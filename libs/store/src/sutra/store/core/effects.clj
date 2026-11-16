(ns sutra.store.core.effects
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [sutra.store.api :as api]
            [sutra.store.edits :as edits]
            [sutra.store.lib :as lib]
            [sutra.store.semantics :as sem]
            [sutra.store.spi :as spi]
            [sutra.store.values :as values]
            [sutra.ref.objectset :as ros]
            [sutra.types.object-set :as os]
            [sutra.types.value-types :as vt]
            [sutra.store.core :as-alias core]
            [sutra.store.core.engine :refer [scan]]
            [sutra.store.core.kv :refer [change-k detached-k edit-key edit-prefix edited-k kv-fx
                                         kv-get kv-scan]]
            [sutra.store.core.reads :refer [links rows]]))

(set! *warn-on-reflection* true)

(defn ->object [T {:keys [id values markings]}]
  (cond-> {:id id :object-type-ref T :values values} (seq markings) (assoc :markings markings)))

(defn ->link [L {:keys [id from to markings]}]
  (cond-> {:id id :link-type-ref L :from-object-id from :to-object-id to}
    (seq markings) (assoc :markings markings)))

(defn canon
  [W section x]
  (or (lib/rid-of W section x) x))

(defn canonical-ir
  [W ir]
  (let [root #(or (lib/rid-of W :object-types %) (lib/rid-of W :interfaces %) %)]
    (if (:op ir)
      (os/rewrite-bottom-up
       (fn [n]
         (cond-> n
           (and (#{:scan :static} (:op n)) (:root n)) (update :root root)
           (and (#{:search-around :with} (:op n))
                (:dir n)) (update :link #(canon W :link-types %))))
       ir)
      (cond-> ir (:root ir) (update :root root)))))

(defn canonical-fence
  [W [k x & more :as f]]
  (case k
    :type      (into [k (canon W :object-types x)] more)
    :object    (into [k (canon W :object-types x)] more)
    :link-type (into [k (canon W :link-types x)] more)
    f))

(defn canonical-step
  [W0 W [k T & more :as step]]
  (case k
    (:drop-type)      (into [k (canon W0 :object-types T)] more)
    (:drop-link-type) (into [k (canon W0 :link-types T)] more)
    (:rename-attribute :drop-attribute :retype-attribute :clear-attribute)
    (into [k (canon W :object-types (canon W0 :object-types T))] more)
    step))

(defn- touching-types
  [W T]
  (let [rid-of (fn [x] (or (lib/rid-of W :object-types x) x))
        t      (rid-of T)]
    (for [[L spec] (:link-types W)
          :when (some #(= t (rid-of %)) [(:from-object-type spec) (:to-object-type spec)])]
      L)))

(defn- stored-types
  [W sig T]
  (remove #(get-in sig [:links % :fk]) (touching-types W T)))

(defn- markings-of [ms] (when (seq ms) (set ms)))

(defn- present!
  [row T id]
  (or row
      (api/refuse! :stale-object (str "StaleObject: " T " " id " is gone")
                   {:sutra/stale-object true :type T :id id :missing true})))

(defn step
  [stored ov {:keys [op type] {:keys [id values markings add remove from-id to-id]} :payload}]
  (let [row (fn [] (let [r (get-in ov [:rows type id] ::core/unknown)]
                     (case r ::core/unknown (stored type id) ::core/gone nil r)))]
    (case op
      :create-object
      (assoc-in ov [:rows type id] (cond-> {:id id :values (or values {})}
                                     (seq markings) (assoc :markings (markings-of markings))))
      :update-object
      (assoc-in ov [:rows type id] (update (present! (row) type id) :values merge values))
      :update-markings
      (let [r  (present! (row) type id)
            ms (apply disj (into (set (:markings r)) add) remove)]
        (assoc-in ov [:rows type id] (if (seq ms) (assoc r :markings ms) (dissoc r :markings))))
      :delete-object
      (-> ov (assoc-in [:rows type id] ::core/gone) (update-in [:gone type] (fnil conj #{}) id))
      :create-link
      (assoc-in ov [:links type id] (cond-> {:id id :from from-id :to to-id}
                                      (seq markings) (assoc :markings (markings-of markings))))
      :delete-link
      (assoc-in ov [:links type id] ::core/gone))))

(defn cascade
  [W sig touching ov]
  (reduce-kv
   (fn [ov T ids]
     (let [ids (into #{} (map str) ids)]
       (reduce (fn [ov L]
                 (let [stored (concat (filter #(ids (:from %)) (get touching [L :from]))
                                      (filter #(ids (:to %)) (get touching [L :to])))
                       made   (for [[lid l] (get-in ov [:links L])
                                    :when (and (map? l) (or (ids (:from l)) (ids (:to l))))]
                                lid)]
                   (reduce #(assoc-in %1 [:links L %2] ::core/gone) ov
                           (concat (map :id stored) made))))
               ov (stored-types W sig T))))
   ov (:gone ov)))

(defn row-effects
  [c {:keys [rows links]}]
  (let [by (fn [m gone?]
             (for [[x by-id] m
                   :let [vs (into []
                                  (keep (fn [[id v]]
                                          (when (= gone? (= ::core/gone v)) (if gone? id v))))
                                  by-id)]
                   :when (seq vs)]
               [x vs]))]
    (concat (for [[L ids] (by links true)]  [:delete-links c L ids])
            (for [[T ids] (by rows true)]   [:delete-rows c T ids])
            (for [[T rs] (by rows false)]   [:write-rows c T rs])
            (for [[L ls] (by links false)]  [:write-links c L ls]))))

(defn- millisecond-values
  [values]
  (walk/postwalk sem/millisecond values))

(defn- decimal-scale
  [t]
  (when (= :decimal (vt/base t))
    (let [{:keys [precision scale]} (vt/col-type-of t)] [precision (or scale 0)])))

(defn- stored-value
  [t v]
  (cond
    (nil? v) v
    (decimal-scale t)
    (if-not (number? v)
      v
      (let [[p s] (decimal-scale t)
            d     (ros/cast-value [:decimal s] v)]
        (when (> (- (.precision ^java.math.BigDecimal d) (.scale ^java.math.BigDecimal d)) (- p s))
          (api/refuse! :malformed
                       (str v " does not fit " (pr-str t) ": at most " (- p s)
                            " digits before the point")
                       {:value v :type t}))
        d))
    (and (vector? t) (= :array (first t)) (sequential? v))
    (do (when (some nil? v)
          (api/refuse! :malformed (str "an array holds no nil element: " (pr-str v))
                       {:value v :type t}))
        (mapv #(stored-value (second t) %) v))
    (and (vector? t) (= :struct (first t)) (map? v))
    (reduce-kv (fn [m k ft] (cond-> m (contains? m k) (update k #(stored-value ft %)))) v
               (second t))
    :else v))

(defn- stored-values
  [W T id values]
  (let [types (into {} (map (juxt :attribute-name :attribute-type))
                    (get-in W [:object-types T :attribute-list]))
        keys  (set (get-in W [:object-types T :object-key :properties]))]
    (when-let [p (some-> id str values/key-problem)]
      (api/refuse! :malformed
                   (str "the id of a " (or (get-in W [:object-types T :object-type-name]) T) ": "
                        (values/explain p))
                   {:type T :value-class (:class p)}))
    (reduce-kv (fn [m a v]
                 (let [h (if-let [t (types a)] (stored-value t v) v)]
                   (when-let [p (if (keys a) (values/key-problem h) (values/problem h))]
                     (api/refuse! :malformed
                                  (str (or (get-in W [:object-types T :object-type-name]) T) "."
                                       (name a)
                                       " of " id ": " (values/explain p)
                                       "; no store keeps it unchanged")
                                  {:type T :attribute a :id id :value-class (:class p)}))
                   (cond-> m (not (identical? h v)) (assoc a h))))
               values values)))

(defn canonical-op
  [W op]
  (let [link? (get-in api/op-descriptors [(:op op) :link?])
        op    (cond-> op
                (:type op) (update :type #(canon W (if link? :link-types :object-types) %)))]
    (cond-> op
      (get-in op [:payload :values])  (update-in [:payload :values] millisecond-values)
      (and (get-in op [:payload :values]) (not link?))
      (update-in [:payload :values] #(stored-values W (:type op) (get-in op [:payload :id]) %))
      (get-in op [:payload :id])      (update-in [:payload :id] str)
      (get-in op [:payload :from-id]) (update-in [:payload :from-id] str)
      (get-in op [:payload :to-id])   (update-in [:payload :to-id] str))))

(defn- feed-value
  [v]
  (cond
    (instance? java.sql.Timestamp v)    (java.util.Date. (.getTime ^java.sql.Timestamp v))
    (instance? java.time.Instant v)     (java.util.Date/from ^java.time.Instant v)
    (instance? java.time.temporal.Temporal v) (str v)
    :else                               v))

(defn- merge-edit
  [a b]
  (if (:created? b)
    (assoc b :deleted? false)
    (cond-> (merge a (select-keys b [:rev :at :from :to]))
      (seq (:attrs b))  (update :attrs (fnil into #{}) (:attrs b))
      (seq (:values b)) (update :values merge (:values b))
      (seq (:attrs-at b)) (update :attrs-at merge (:attrs-at b))
      (:deleted? b)     (assoc :deleted? true))))

(defn- edit-of [{:keys [op] {:keys [values from-id to-id]} :payload} rev at]
  (let [vs (update-vals (or values {}) feed-value)]
    (case op
      :create-object   {:created? true :attrs (set (keys vs)) :values vs :rev rev :at at
                        :created-at at :attrs-at (zipmap (keys vs) (repeat at))}
      :update-object   {:attrs (set (keys vs)) :values vs :rev rev :at at
                        :attrs-at (zipmap (keys vs) (repeat at))}
      :update-markings {:attrs #{:markings} :rev rev}
      :delete-object   {:deleted? true :rev rev}
      :create-link     {:created? true :rev rev :from (str from-id) :to (str to-id)}
      :delete-link     {:deleted? true :rev rev})))

(defn- combine-edits
  [a b]
  (let [[old new] (if (< (:rev b 0) (:rev a 0)) [b a] [a b])]
    (cond-> (merge old new)
      (or (:attrs old) (:attrs new))   (assoc :attrs (into (set (:attrs old)) (:attrs new)))
      (or (:values old) (:values new)) (assoc :values (merge (:values old) (:values new)))
      (or (:attrs-at old) (:attrs-at new))
      (assoc :attrs-at (merge (:attrs-at old) (:attrs-at new))))))

(defn rename-in
  [x a b]
  (cond (set? x)        (cond-> x (contains? x a) (-> (disj a) (conj b)))
        (contains? x a) (-> x (dissoc a) (assoc b (get x a)))
        :else           x))

(defn rename-edit [e [a b]]
  (cond-> e
    (:attrs e)  (update :attrs rename-in a b)
    (:values e) (update :values rename-in a b)
    (:attrs-at e) (update :attrs-at rename-in a b)))

(defn- detached? [db h b T] (some? (kv-get db h (detached-k b T))))

(defn edit-entries
  [db h b root detached? T]
  (let [p   (edit-prefix T)
        one (fn [part] (into {} (kv-scan db h [:edited part] {:after p :upto (str p "\uffff")})))]
    (merge-with combine-edits (when (and root (not detached?)) (one root)) (one b))))

(defn edits-of
  [db h b root T ids]
  (let [n   (count (edit-prefix T))
        es  (edit-entries db h b root (and root (detached? db h b T)) T)
        all (into {} (map (fn [[k v]] [(edn/read-string (subs k n)) v])) es)]
    (if ids (select-keys all (map str ids)) all)))

(defn- index-op
  [stored edits ts-attr {:keys [op type] {:keys [id values] :as payload} :payload :as o}]
  (let [e     (get edits [type (str id)])
        r     (edits/resolve-object e (when-not (= :delete-object op) (or values {}))
                                    (when e (edits/source-times (ts-attr type) payload)))
        held  (:held r)
        had   (:values (stored type id))
        same? (fn [[k v]] (and (contains? had k) (sem/same-data? v (get had k))))
        keep* (fn [vs] (into {} (remove same?) (apply dissoc vs held)))
        held? (or (:deleted? e) (:created? e))
        o     (cond-> (update o :payload dissoc :stamps)
                (seq (:superseded r)) (assoc :edit-superseded (vec (:superseded r))))]
    (case op
      :create-object (cond (not (:follows-source? r)) nil
                           (stored type id) (let [vs (keep* values)]
                                              (when (seq vs)
                                                (-> o (assoc :op :update-object)
                                                    (assoc-in [:payload :values] vs)
                                                    (update :payload dissoc :markings))))
                           :else
                           (update-in o [:payload :values]
                                      #(merge (keep* %) (select-keys (:values e) held))))
      :update-object (when (:follows-source? r)
                       (let [vs (keep* values)] (when (seq vs) (assoc-in o [:payload :values] vs))))
      :delete-object (when (:follows-source? r) o)
      :create-link   (when-not held? o)
      :delete-link   (when-not (:created? e) o)
      o)))

(defn change-effects
  [b rev at origin ops edited stored]
  (concat
   (for [[T os] (group-by :type ops)]
     (kv-fx
      (change-k T rev)
      {:branch b :origin origin
       :entries (mapv (fn [{:keys [op edit-superseded]
                            {:keys [id values from-id to-id add remove]} :payload}]
                        (cond-> {:id (str id) :op op}
                          (and (= :edit origin) (= :update-object op) (seq values))
                          (assoc :before
                                 (let [row (:values (get stored [T (str id)]))]
                                   (into {} (map (fn [a] [a (feed-value (get row a))]))
                                         (keys values))))
                          edit-superseded (assoc :edit-superseded edit-superseded)
                          (seq values) (assoc :attrs (vec (keys values)))
                          (and (= :edit origin)
                               (seq values)) (assoc :values (update-vals values feed-value))
                          (seq add)    (assoc :add (vec add))
                          (seq remove) (assoc :remove (vec remove))
                          from-id      (assoc :from (str from-id) :to (str to-id))))
                      os)}))
   (when (= :edit origin)
     (let [merged (reduce (fn [m {:keys [type payload] :as o}]
                            (let [k (edit-key type (:id payload))]
                              (assoc m k (merge-edit (get m k (get edited k)) (edit-of o rev at)))))
                          {} ops)]
       (for [[k e] merged] (kv-fx [:edited b k] e))))))

(defn fold-targets
  [ops creates?]
  (->> ops
       (filter #(get-in api/op-descriptors [(:op %) :object?]))
       (remove #(and (= :create-object (:op %)) (not creates?)))
       (group-by :type)
       (into {} (map (fn [[T os]] [T (vec (distinct (map (comp str :id :payload) os)))])))))

(defn deleted-by
  [ops]
  (into {} (map (fn [[T os]] [T (mapv (comp str :id :payload) os)]))
        (group-by :type (filter #(= :delete-object (:op %)) ops))))

(defn read-rows
  [db h c targets]
  (into {} (for [[T ids] targets r (when (seq ids) (rows db h c T ids))] [[T (str (:id r))] r])))

(defn read-touching
  [db h {:keys [W sig] :as c} deleted]
  (reduce (fn [m [T ids]]
            (let [gone (set ids)]
              (reduce (fn [m L]
                        (let [ls (links db h c L {:ends ids})]
                          (-> m
                              (update [L :from] (fnil into []) (filter (comp gone :from)) ls)
                              (update [L :to] (fnil into []) (filter (comp gone :to)) ls))))
                      m (stored-types W sig T))))
          {} deleted))

(defn index-ops
  [W ops stored edits]
  (into []
        (keep (partial index-op (fn [T id] (get stored [T (str id)])) edits
                       #(edits/timestamp-attr (get-in W [:object-types % :edit-conflict])))) ops))

(defn- adopted-attr
  [W T a]
  (let [norm #(str/replace (name %) "_" "-")]
    (or (some #(when (= (norm %) (norm a)) %)
              (map :attribute-name (get-in W [:object-types T :attribute-list])))
        a)))

(defn canonical-adoption
  [W {:keys [type id values] :as e}]
  (let [T  (canon W :object-types type)
        id (str id)]
    (cond-> (assoc e :type T :id id)
      values (assoc :values (->> (update-keys values (partial adopted-attr W T))
                                 millisecond-values
                                 (stored-values W T id))))))

(defn adopted-restores
  [adopt rows]
  (into [] (keep (fn [{:keys [type id kind values]}]
                   (let [row (get rows [type id])
                         vs  (when row
                               (into {} (remove (fn [[a v]] (= v (get-in row [:values a]))))
                                     values))]
                     (case kind
                       :delete (when row {:op :delete-object :type type :payload {:id id}})
                       :create
                       (cond (nil? row) {:op :create-object :type type
                                         :payload {:id id :values (or values {})}}
                             (seq vs)   {:op :update-object :type type
                                         :payload {:id id :values vs}})
                       :update (when (seq vs)
                                 {:op :update-object :type type :payload {:id id :values vs}})))))
        adopt))

(defn- adopted-edit
  [{:keys [kind values at]} rev]
  (cond-> {:rev rev}
    at               (assoc :at at)
    (= :create kind) (assoc :created? true :deleted? false)
    (= :delete kind) (assoc :deleted? true)
    (and (not= :delete kind) (seq values))
    (assoc :attrs (set (keys values)) :values (update-vals values feed-value))))

(defn adopt-effects
  [b rev adopt]
  (concat
   (for [{:keys [type id] :as a} adopt] (kv-fx (edited-k b type id) (adopted-edit a rev)))
   (for [[T as] (group-by :type adopt)]
     (kv-fx (change-k T rev)
            {:branch b :origin :edit
             :entries (mapv (fn [{:keys [id kind values]}]
                              (cond-> {:id id
                                       :op (case kind :create :create-object :update :update-object
                                                 :delete :delete-object)}
                                (and (not= :delete kind) (seq values))
                                (assoc :attrs (vec (keys values)) :values
                                       (update-vals values feed-value))))
                            as)}))))

(defn- state-value [s] (cond (keyword? s) (name s) (nil? s) nil :else (str s)))

(defn- lifecycle-refusal
  [W ops stored]
  (first
   (for [{:keys [op type payload]} ops
         :when (#{:create-object :update-object} op)
         :let  [lc (get-in W [:object-types type :lifecycle])]
         :when lc
         :let  [prop (keyword (name (:property lc)))
                vs   (:values payload)]
         :when (contains? vs prop)
         :let  [states (mapv state-value (:states lc))
                to     (state-value (get vs prop))
                from   (when (= :update-object op)
                         (some->> (get stored [type (str (:id payload))]) (->object type) :values
                                  prop state-value))
                edges  (into #{}
                             (for [{f :from t :to} (:transitions lc), x f]
                               [(state-value x) (state-value t)]))
                ok?    (cond
                         (= :create-object op) (or (nil? to) (= to (first states)))
                         (= from to)           true
                         (nil? from)           (some #{to} states)
                         :else                 (contains? edges [from to]))]
         :when (not ok?)]
     (api/refused :lifecycle-edge
                  (str "lifecycle of " (pr-str type) ": " (pr-str (:id payload))
                       " may not move " (or from "∅") " → " to " — no declared transition")
                  {:type type :id (:id payload) :from from :to to}))))

(defn fold-refusal
  [W ops stored holders origin]
  (or (when (= :edit origin) (lifecycle-refusal W ops stored))
      (first (for [{:keys [payload] :as op} ops
                   :when (some spi/reserved-keys (keys (:values payload)))]
               (api/refused :malformed "an attribute value under a reserved key"
                            {:op op :reserved spi/reserved-keys})))
      (first (for [{:keys [type payload] :as op} ops
                   :when (and (contains? op :expected-version)
                              (get-in api/op-descriptors [(:op op) :version-checked?]))
                   :let [actual (api/object-version
                                 (some->> (get stored [type (str (:id payload))])
                                          (->object type)))]
                   :when (not= (:expected-version op) actual)]
               (api/refused :stale-object "StaleObject: target changed since it was read"
                            {:sutra/stale-object true :expected (:expected-version op)
                             :actual actual :type type :id (:id payload)})))
      (when (some #(= :create-object (:op %)) ops)
        (when-let [[msg data] (lib/key-refusal
                               (lib/key-collisions W ops (fn [T props tuples]
                                                           (get holders [T props tuples]))))]
          (api/refused :object-key-taken msg data)))))

(defn fk-link-refusal
  [sig ops]
  (first
   (for [{:keys [op type] :as o} ops
         :when (and (get-in api/op-descriptors [op :link?]) (get-in sig [:links type :fk]))]
     (api/refused :unsupported
                  (str "a link derived from a foreign key is not written; set the foreign"
                       " key instead (" (name op) " on " type ")")
                  {:op o}))))

(defn- holders
  [{:keys [engine] :as db} h c T props tuples]
  (let [all (delay (mapv (juxt :id :values) (spi/-rows engine h c T nil)))]
    (into []
          (mapcat (fn [ch]
                    (for [[id v] (scan db h c
                                       {:source  [:rows T]
                                        :where
                                        [(into [:or]
                                               (for [t ch]
                                                 (into [:and] (map #(vector := %1 %2) props t))))]
                                        :project (set props)}
                                       (fn [] @all))]
                      {:id (str id) :key (mapv #(get v %) props)})))
          (partition-all 500 tuples))))

(defn read-holders
  [db h c W ops]
  (let [answers (atom {})]
    (lib/key-collisions W ops (fn [T props tuples]
                                (let [r (holders db h c T props tuples)]
                                  (swap! answers assoc [T props tuples] r)
                                  r)))
    @answers))

(defn ticks
  [W schema ops]
  (concat
   (when schema [[:schema]])
   (for [[k T] (:migration schema)
         :when (some? T)
         t (case k
             :drop-link-type [[:link-type T]]
             :drop-type      (cons [:type T] (map #(vector :link-type %) (touching-types W T)))
             [[:type T]])]
     t)
   (for [{:keys [op type]} ops
         t (cond
             (get-in api/op-descriptors [op :link?]) [[:link-type type]]
             (= :delete-object
                op) (cons [:type type] (map #(vector :link-type %) (touching-types W type)))
             :else [[:type type]])]
     t)))
