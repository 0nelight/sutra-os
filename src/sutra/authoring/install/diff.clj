(ns sutra.authoring.install.diff
  (:require [sutra.authoring.install.live :as live]
            [sutra.catalog.world-install :as world-install]
            [sutra.types.world-rules :as rules]
            [sutra.types.kinds :as kinds]
            [sutra.types.link-side :as ls]
            [sutra.types.rid :as rid]
            [sutra.types.transform-io :as tio]))

(set! *warn-on-reflection* true)

(defn output-ref
  [o]
  (let [k (:kind o)]
    (when (and (kinds/output? k) (get o k))
      [k (get o k)])))

(defn link-outputs-of
  [t-spec]
  (into #{} (comp (keep output-ref)
                  (keep (fn [[k v]] (when (= :link-type k) v))))
        (tio/outputs-of t-spec)))

(defn- producer-index
  [tx-by-name]
  (reduce-kv (fn [idx t-name spec]
               (reduce (fn [idx o]
                         (if-let [ref (output-ref o)]
                           (update idx ref (fnil conj #{}) t-name)
                           idx))
                       idx (tio/outputs-of spec)))
             {} tx-by-name))

(defn live-by-name
  [current]
  (fn [kinds-key name-key] (live/whole-section current kinds-key name-key)))

(def ^:private sections
  (select-keys rid/name-field-of [:object-types :transforms :sources :link-types :interfaces
                                  :value-types :datasets :functions :actions
                                  :applications]))

(defn- author->attribute-list
  [attrs]
  (mapv (fn [[n t opts]]
          (cond-> {:attribute-name n :attribute-type t}
            (seq opts) (assoc :opts opts)))
        attrs))

(defn object-specs
  [specs]
  (reduce-kv (fn [m k spec]
               (assoc m k (cond-> spec
                            (:attributes spec)
                            (-> (assoc :attribute-list
                                       (author->attribute-list (:attributes spec)))
                                (dissoc :attributes))

                            (nil? (:object-type-name spec))
                            (assoc :object-type-name k))))
             {} specs))

(defn- merge-step
  [spec args]
  (merge (cond-> spec (some #(contains? args %) [:output :outputs]) (dissoc :output :outputs))
         args))

(defn- lay
  [specs plan kind]
  (reduce (fn [m {k :kind :keys [entity outcome args]}]
            (if (not= k kind)
              m
              (case outcome
                :deleted            (dissoc m entity)
                (:created :updated) (update m entity merge-step args)
                m)))
          specs plan))

(defn- steps
  [plan outcomes]
  (into #{} (comp (filter (comp outcomes :outcome)) (map (juxt :kind :entity))) plan))

(defn- object-spec [k spec] (get (object-specs {k spec}) k))

(defn- laid
  [m plan kind full]
  (live/patched m
                (reduce (fn [ops {k :kind :keys [entity outcome args]}]
                          (if (not= k kind)
                            ops
                            (case outcome
                              :deleted (assoc ops entity [:drop])
                              (:created :updated)
                              (let [[op v] (get ops entity)
                                    was    (if op (when (= :put op) v) (live/lookup m entity))]
                                (assoc ops entity [:put (merge-step was args)]))
                              ops)))
                        {} plan)
                full))

(defn- producers-view
  [tx cands full]
  (live/looked (fn [ref]
                 (when (vector? ref)
                   (let [out? (fn [k] (some #(= ref %)
                                            (keep output-ref (tio/outputs-of (live/lookup tx k)))))
                         ps   (into #{} (filter out?) (cands ref))]
                     (when (seq ps) [ps]))))
               full))

(defn- before-producers
  [current {:keys [names producing]}]
  (fn [[kind v]]
    (distinct (for [r  (live/keys-named names v)
                    t  (producing kind r)
                    :let [e (get-in current [:transforms t])]
                    :when e]
                (or (:transform-name e) t)))))

(defn- indexed
  [current plan {:keys [names] :as ix}]
  (let [whole  (into {} (for [[kk nk] sections]
                          [kk (delay (live/whole-section current kk nk))]))
        before (into {} (for [[kk nk] sections]
                          [kk (live/section current names kk nk #(force (whole kk)))]))
        staged (fn [kind] (into [] (comp (filter #(and (= kind (:kind %))
                                                       (#{:created :updated} (:outcome %))))
                                         (map :entity))
                                plan))
        lay-v  (fn [kk kind] (let [m (get before kk)]
                               (laid m plan kind (delay (lay (live/whole m) plan kind)))))
        ots    (lay-v :object-types :object-type)
        after  (assoc before
                      :object-types (live/mapped ots object-spec
                                                 (delay (object-specs (live/whole ots))))
                      :transforms   (lay-v :transforms :transform)
                      :functions    (lay-v :functions :function)
                      :actions      (lay-v :actions :action)
                      :applications (lay-v :applications :application))
        cands  (before-producers current ix)]
    {:before (assoc before :producers
                    (producers-view (:transforms before) cands
                                    (delay (producer-index (live/whole (:transforms before))))))
     :after  (assoc after :producers
                    (producers-view (:transforms after)
                                    #(distinct (concat (cands %) (staged :transform)))
                                    (delay (producer-index (live/whole (:transforms after))))))}))

(defn diff
  [text current plan edits ix]
  (let [{:keys [before after]} (indexed current plan ix)]
    {:text      text
     :current   current
     :plan      plan
     :edits     edits
     ::ix       ix
     :before    before
     :after     after
     :staged    (reduce (fn [m {:keys [kind entity outcome args]}]
                          (cond-> m (#{:created :updated} outcome)
                            (assoc-in [kind entity] args)))
                        {} plan)
     :removed   (steps plan #{:deleted})
     :unchanged (steps plan #{:skipped})}))

(defn staged
  [d kind]
  (get-in d [:staged kind]))

(defn attr-names-of [obj] (set (map :attribute-name (:attribute-list obj))))

(declare held-outputs)

(defn pk-attrs
  [d side ot]
  (let [pk (rules/object-primary-key {:object-types (live/select (get-in d [side :object-types])
                                                                [ot])}
                                     ot
                                     (fn [kind target]
                                       (sort-by (comp str first)
                                                (held-outputs d side kind target))))]
    (into #{} (map keyword) (if (sequential? pk) pk (some-> pk vector)))))

(defn links-near
  [{:keys [text before current] ::keys [ix]}]
  (let [{:keys [names link-end]} ix
        lb   (:link-types before)
        nk   (rid/name-field-of :link-types)
        decl (into {} (for [[k from to _ opts] (:links text)]
                        [(rid/->name k) (merge {:from from :to to}
                                               (select-keys opts [:forward-api-name
                                                                  :reverse-api-name]))]))
        held (fn [r] (let [nm (or (get-in current [:link-types r nk]) r)]
                       (when-let [lt (live/lookup lb nm)]
                         [(rid/->name nm) (ls/table-entry lt identity)])))]
    (memoize (fn [t]
               (merge (into {} (keep held)
                            (distinct (mapcat link-end (live/keys-named names t))))
                      decl)))))

(defn changed
  [d kind]
  (into #{} (comp (filter #(and (= kind (:kind %))
                                (#{:created :updated :deleted} (:outcome %))))
                  (map :entity))
        (:plan d)))

(defn node-key
  [d sec r]
  (let [e (get-in d [:current sec r])] (or (get e (rid/name-field-of sec)) r)))

(defn sub-world
  [w base nodes]
  (reduce (fn [m [sec k]] (if-some [e (live/lookup (get w sec) k)] (assoc-in m [sec k] e) m))
          base nodes))

(defn fn-nodes
  [d fs]
  (let [{:keys [fn-named]} (::ix d)]
    (distinct (concat (for [f fs [sec r] (fn-named f)] [sec (node-key d sec r)])
                      (for [k (changed d :function)] [:functions k])
                      (for [k (changed d :transform)] [:transforms k])))))

(defn- keys-of
  [d rs]
  (keep (fn [r] (when-let [e (get-in d [:current :transforms r])] (or (:transform-name e) r)))
        rs))

(defn held-outputs
  [d side kind target]
  (let [tx (get-in d [side :transforms])]
    (vec (for [k (live/lookup (get-in d [side :producers]) [kind target])
               :let [t (live/lookup tx k)]
               o (tio/outputs-of t)
               :when (and (= kind (:kind o)) (= target (get o kind)))]
           [k t o]))))

(defn primary-output
  [d side kind target]
  (->> (held-outputs d side kind target)
       (remove (fn [[_ _ o]] (= :supplement (:role o))))
       (sort-by (comp str first))
       first))

(defn fk-col?
  [d col]
  (let [tx (get-in d [:after :transforms])]
    (boolean (some (fn [k] (some #(some-> (:fk-attribute %) world-install/fk-col (= col))
                                 (tio/outputs-of (live/lookup tx k))))
                   (distinct (concat (keys-of d ((:fk-col (::ix d)) col))
                                     (keys (staged d :transform))))))))

(defn index
  [d]
  (::ix d))

(defn live-keys
  [d n]
  (live/keys-named (:names (::ix d)) n))

(defn- found
  [d look target]
  (distinct (concat (keys-of d (for [r (live/keys-named (:names (::ix d)) target)
                                     t (look r)]
                                 t))
                    (keys (staged d :transform)))))

(defn producing
  [d kind target]
  (found d #((:producing (::ix d)) kind %) target))

(defn dataset-writers
  [d ds]
  (found d (:dataset-out (::ix d)) ds))

(defn backed-by
  [d ds]
  (distinct
   (concat (keep (fn [r] (when-let [e (get-in d [:current :object-types r])]
                           (or (:object-type-name e) r)))
                 (for [r (live/keys-named (:names (::ix d)) ds)
                       t ((:backed-by (::ix d)) r)]
                   t))
           (keys (staged d :object-type)))))

(defn side-by-name-kw
  ([d side kk] (side-by-name-kw d side kk #(keyword (rid/->name %))))
  ([d side kk keyf]
   (let [m (get-in d [side kk])]
     (live/normalized (:current d) (:names (::ix d)) kk (rid/name-field-of kk) keyf m
                      (delay (live/by-name (for [[k e] (live/whole m)] [(keyf k) k e])))))))

(defn by-name-kw
  ([d kk] (side-by-name-kw d :before kk))
  ([d kk keyf] (side-by-name-kw d :before kk keyf)))

(defn known
  [d kk]
  (let [v (by-name-kw d kk)]
    (fn [t] (and (keyword? t) (live/holds? v t)))))
