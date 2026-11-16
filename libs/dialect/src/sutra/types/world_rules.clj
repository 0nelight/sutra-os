(ns sutra.types.world-rules
  (:require [sutra.ref.install :as ri]
            [sutra.types.link-side :as link-side]
            [sutra.types.nested :as nested]
            [sutra.types.world-indexes :as indexes]
            [sutra.types.schema :as sch]
            [sutra.types.transform-io :as tio]))

(set! *warn-on-reflection* true)

(defn- outputs-for
  [schema kind target]
  (sort-by (comp str first) (sch/outputs-for (sch/index schema) kind target)))

(defn producer-primary-key
  ([schema ot] (producer-primary-key schema ot (partial outputs-for schema)))
  ([schema ot outs]
   (if-let [authored (get-in schema [:object-types ot :backing])]
     (:primary-key authored)
     (some (fn [[_ _ out]] (when (not= :supplement (:role out)) (:primary-key out)))
           (outs :object-type ot)))))

(defn object-primary-key
  ([schema ot] (object-primary-key schema ot (partial outputs-for schema)))
  ([schema ot outs]
   (or (get-in schema [:object-types ot :primary-key])
       (producer-primary-key schema ot outs))))

(defn object-key
  ([schema ot] (object-key schema ot (partial outputs-for schema)))
  ([schema ot outs]
   (let [spec (get-in schema [:object-types ot])
         pk   (object-primary-key schema ot outs)]
     (cond
       (:object-key spec)
       (:object-key spec)

       pk
       {:properties (if (sequential? pk) (mapv keyword pk) [(keyword pk)])
        :source     :primary-key}

       (and (seq (:match-keys spec)) (not (:resolve-entity spec)))
       {:properties (mapv keyword (:match-keys spec)) :source :match-keys}))))

(defn fk-link-spec
  ([schema conn] (fk-link-spec schema conn (partial outputs-for schema)))
  ([schema conn outs]
   (when-let [[_ t out] (first (outs :link-type conn))]
     (let [lt      (get-in schema [:link-types conn])
           in-type (get-in t [:input :object-type])
           fk-side (cond (= in-type (:from-object-type lt)) :from
                         (= in-type (:to-object-type lt))   :to)]
       (when (and fk-side (:fk-attribute out))
         (let [pk-type (case fk-side :from (:to-object-type lt) :to (:from-object-type lt))]
           {:fk-type      in-type
            :pk-type      pk-type
            :fk-side      fk-side
            :fk-attribute (:fk-attribute out)
            :pk-attribute (or (object-primary-key schema pk-type outs)
                              (let [[k & more] (:properties (object-key schema pk-type outs))]
                                (when-not more k)))}))))))

(defn implementers
  ([schema iface] (implementers schema (ri/ancestors-of schema) iface))
  ([schema ancestors iface]
   (vec (sort-by str (for [[ot-kw ot] (:object-types schema)
                           :when (some #{iface} (ri/effective-implements schema ancestors ot))]
                       ot-kw)))))

(defn- attrs [xs]
  (mapv (fn [{:keys [attribute-name attribute-type]}] {:name attribute-name :type attribute-type})
        xs))

(defn- one-attr
  [a]
  (cond (keyword? a) a
        (and (sequential? a) (= 1 (count a))) (keyword (first a))
        (string? a) (keyword a)))

(defn signature
  [W]
  (let [anc     (ri/ancestors-of W)
        indexed (indexes/columns W)]
    {:types      (into {} (for [[T spec] (:object-types W)
                                :let [props (:properties (object-key W T))]]
                            [T (cond-> {:attrs (attrs (:attribute-list spec))}
                                 (seq props)       (assoc :key (mapv keyword props))
                                 (get indexed T)   (assoc :index (get indexed T))
                                 (:title-key spec) (assoc :title (:title-key spec)))]))
     :interfaces (into {}
                       (for [[I _] (:interfaces W)]
                         [I
                          {:props        (attrs (:property-list (ri/effective-interface W anc I)))
                           :implementers (implementers W anc I)}]))
     :links      (into {} (for [[L spec] (:link-types W)
                                :let [fk (fk-link-spec W L)]]
                            [L (cond-> {:from (:from-object-type spec) :to (:to-object-type spec)
                                        :cardinality (or (:cardinality spec) :many-to-many)}
                                 (and (not fk) (:fk spec)) (assoc :fk (:fk spec))
                                 fk (assoc :fk
                                           (cond-> {:holder (:fk-type fk) :target (:pk-type fk)
                                                    :side (:fk-side fk)
                                                    :attr (keyword (:fk-attribute fk))}
                                             (one-attr (:pk-attribute fk))
                                             (assoc :target-attr
                                                    (one-attr (:pk-attribute fk))))))]))}))

(defn- reach [succ seeds]
  (loop [order [] seen #{} todo (vec seeds) i 0]
    (if (< i (count todo))
      (let [x (nth todo i)]
        (if (contains? seen x)
          (recur order seen todo (inc i))
          (recur (conj order x) (conj seen x) (into todo (succ x)) (inc i))))
      order)))

(def ^:private facts-of
  {:transforms   (fn [k e] (for [out (tio/outputs-of e)
                                 :let [target (get out (:kind out))]
                                 :when (some? target)]
                             [[:producers [(:kind out) target]] k]))
   :link-types   (fn [k e] (for [end (link-side/ends e)]
                             [[:ends end] k]))
   :object-types (fn [k e] (concat (for [i (nested/seqable (:implements e))] [[:implements i] k])
                                   (when-let [n (indexes/type-name e)] [[[:names n] k]])))
   :interfaces   (fn [k e] (for [p (nested/seqable (:extends e))] [[:children p] k]))
   :applications (fn [k e] (for [root (keys (indexes/app-wants e))] [[:wanted-by root] k]))})

(def ^:private read-sections (keys facts-of))

(defn- facts [sec k e] (when e ((facts-of sec) k e)))

(defn- aux-of [W]
  (reduce (fn [aux [path x]] (nested/conj-in aux path x))
          {}
          (for [sec read-sections [k e] (get W sec) fact (facts sec k e)] fact)))

(defn- aux-step [aux sec k e0 e1]
  (nested/refile-in aux (facts sec k e0) (facts sec k e1)))

(defn- outs-in
  [W aux]
  (fn [kind target]
    (for [t (sort-by str (get-in aux [:producers [kind target]]))
          :let [te (get-in W [:transforms t])]
          out (tio/outputs-of te)
          :when (and (= kind (:kind out)) (= target (get out (:kind out))))]
      [t te out])))

(defn- ancestors-in [W I]
  (let [ifs (:interfaces W)]
    (into #{} (filter #(contains? ifs %))
          (reach #(get-in ifs [% :extends]) (get-in ifs [I :extends])))))

(defn- descendants-in [aux I]
  (rest (reach #(get-in aux [:children %]) [I])))

(defn- index-of [W aux T]
  (let [spec (get-in W [:object-types T])
        n    (indexes/type-name spec)]
    (when (and n (= T (last (sort-by str (get-in aux [:names n])))))
      (indexes/type-columns spec
                            (into #{} (mapcat #(get (indexes/app-wants
                                                     (get-in W [:applications %]))
                                                    n))
                                  (get-in aux [:wanted-by n]))))))

(defn- type-sig [W aux T]
  (let [spec  (get-in W [:object-types T])
        props (:properties (object-key W T (outs-in W aux)))
        idx   (index-of W aux T)]
    (cond-> {:attrs (attrs (:attribute-list spec))}
      (seq props)       (assoc :key (mapv keyword props))
      idx               (assoc :index idx)
      (:title-key spec) (assoc :title (:title-key spec)))))

(defn- interface-sig [W aux I]
  (let [anc {I (ancestors-in W I)}]
    {:props        (attrs (:property-list (ri/effective-interface W anc I)))
     :implementers (vec (sort-by str (into #{} (mapcat #(get-in aux [:implements %]))
                                           (cons I (descendants-in aux I)))))}))

(defn- link-sig [W aux L]
  (let [spec (get-in W [:link-types L])
        fk   (fk-link-spec W L (outs-in W aux))]
    (cond-> {:from (:from-object-type spec) :to (:to-object-type spec)
             :cardinality (or (:cardinality spec) :many-to-many)}
      (and (not fk) (:fk spec)) (assoc :fk (:fk spec))
      fk (assoc :fk (cond-> {:holder (:fk-type fk) :target (:pk-type fk) :side (:fk-side fk)
                             :attr (keyword (:fk-attribute fk))}
                      (one-attr (:pk-attribute fk))
                      (assoc :target-attr (one-attr (:pk-attribute fk))))))))

(defn sig-init
  [W]
  {:sig (signature W) :aux (aux-of W)})

(defn- section-map! [W s]
  (let [x (get W s)]
    (when-not (or (nil? x) (map? x))
      (throw (ex-info (str "the section " s " is no map")
                      {:precondition :section-not-a-map :section s})))))

(defn- eff-implements [W T]
  (let [ifs (:interfaces W)]
    (for [d (nested/seqable (get-in W [:object-types T :implements]))
          :when (contains? ifs d)
          i (cons d (ancestors-in W d))]
      i)))

(defn sig-step
  [{:keys [sig aux]} W0 W nodes]
  (doseq [s read-sections] (section-map! W0 s) (section-map! W s))
  (let [nodes  (distinct (filter (fn [[s]] (some #{s} read-sections)) nodes))
        aux1   (reduce (fn [a [s k]] (aux-step a s k (get-in W0 [s k]) (get-in W [s k])))
                       aux nodes)
        in     (fn [sec] (keep (fn [[s k]] (when (= s sec) k)) nodes))
        targets (fn [kind]
                  (for [t (in :transforms)
                        e [(get-in W0 [:transforms t]) (get-in W [:transforms t])]
                        out (tio/outputs-of e)
                        :when (= kind (:kind out))
                        :let [x (get out kind)]
                        :when (some? x)]
                    x))
        key-ts  (into (set (in :object-types)) (targets :object-type))
        named   (fn [Wx k] (some-> (get-in Wx [:object-types k]) indexes/type-name))
        names   (into #{} (concat (keep #(named W0 %) (in :object-types))
                                  (keep #(named W %) (in :object-types))
                                  (for [a (in :applications)
                                        e [(get-in W0 [:applications a])
                                           (get-in W [:applications a])]
                                        :when e
                                        root (keys (indexes/app-wants e))]
                                    root)))
        types   (into key-ts (mapcat #(concat (get-in aux [:names %]) (get-in aux1 [:names %])))
                      names)
        links   (into (set (in :link-types))
                      (concat (targets :link-type)
                              (mapcat #(concat (get-in aux [:ends %]) (get-in aux1 [:ends %]))
                                      key-ts)))
        ifaces  (into #{} (concat
                           (for [J (in :interfaces)
                                 x (concat [J]
                                           (descendants-in aux J) (descendants-in aux1 J)
                                           (ancestors-in W0 J) (ancestors-in W J))]
                             x)
                           (for [T (in :object-types)
                                 x (concat (eff-implements W0 T) (eff-implements W T))]
                             x)))
        put     (fn [m sec ks f]
                  (reduce (fn [m k]
                            (if (contains? (get W sec) k) (assoc m k (f W aux1 k)) (dissoc m k)))
                          (or m {}) ks))]
    (with-meta {:aux aux1
                :sig (-> sig
                         (update :types put :object-types types type-sig)
                         (update :interfaces put :interfaces ifaces interface-sig)
                         (update :links put :link-types links link-sig))}
      {:touched {:types types :interfaces ifaces :links links}})))

(defn store-opts
  [clock]
  {:derive    #(ri/derived {} % nil)
   :upgrade   ri/rekey-legacy
   :mint      (fn [old w] (ri/mint-names {:sigma clock} w old))
   :signature signature
   :sig-view  {:init sig-init :step sig-step}})
