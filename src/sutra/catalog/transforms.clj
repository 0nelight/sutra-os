(ns sutra.catalog.transforms
  (:require [sutra.kit.refusal :as refusal]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]
            [sutra.types.kinds :as kinds]
            [sutra.types.languages :as langs]
            [sutra.types.link-side :as link-side]
            [clojure.set :as set]
            [clojure.string :as str]
            [sutra.ref.install :as ri]
            [sutra.catalog.interfaces :as interfaces]
            [sutra.catalog.outputs :as outputs]))

(set! *warn-on-reflection* true)

(def ^:private cross-repo-hint
  (str " — object types and link types live in the ONTOLOGY repository"
       " and a code repository may not declare them, so the type has to be"
       " installed BEFORE the code that names it: push the ontology first, then"
       " this repository (and when removing, the other way round)"))

(defn- input-chain-problem
  [ts t-kw]
  (loop [seen #{} cur t-kw]
    (let [t  (get ts cur)
          in (:input t)]
      (cond
        (contains? seen cur)
        (str "its :input chain closes a cycle back on " (pr-str cur)
             " — a transform cannot read, however indirectly, from itself")

        (or (langs/compute-language? (:language t)) (= :polymorphic (:strategy t)))
        nil

        (= :transform (:kind in))
        (when (contains? ts (:transform in))
          (recur (conj seen cur) (:transform in)))

        (kinds/is? :ends-chain? (:kind in))
        nil

        :else
        (str "carries the :input :kind " (pr-str (:kind in))
             ", which backs no read — a transform that is not written in a compute"
             " language reads from a :datasource, a :dataset, an :object-type or"
             " another :transform")))))

(defn explain-transform-refs
  [schema]
  (let [ts  (or (:transforms schema) {})
        ots (set (keys (:object-types schema)))
        cts (set (keys (:link-types schema)))
        dss (set (keys (:datasets schema)))
        declared {:object-types ots :link-types cts :datasets dss
                  :media-sets (set (keys (:media-sets schema)))}
        missing (fn [kind v]
                  (str "names " (name kind) " " (pr-str v)
                       ", which this schema does not declare"
                       (when-not (= :transform kind) cross-repo-hint)))]
    (vec
     (for [[t-kw t] ts
           reason
           (concat
            (let [in (:input t)]
              (cond
                (and (= :transform (:kind in)) (not (contains? ts (:transform in))))
                [(str "its :input " (missing :transform (:transform in)))]

                (and (= :object-type (:kind in)) (not (contains? ots (:object-type in))))
                [(str "its :input " (missing :object-type (:object-type in)))]

                :else nil))
            (when-let [r (input-chain-problem ts t-kw)] [r])
            (for [out (outputs/outputs-of t)
                  :let [[sec v] (kinds/output-slot out)
                        r (if-let [known (get declared sec)]
                            (when-not (or (contains? known v)
                                          (and (kinds/is? :typed-output? (:kind out)) (rid/rid? v)))
                              (str "its :output " (missing (:kind out) v)))
                            (str "its :output carries an unrecognised :kind "
                                 (pr-str (:kind out))))]
                  :when r]
              r))]
       {:transform (or (:transform-name t) t-kw) :reason reason}))))

(defn explain-transform-collisions
  [schema]
  (let [ts (or (:transforms schema) {})
        nm (fn [t-kw] (let [t (get ts t-kw)] (or (:transform-name t) t-kw)))
        resolved? (fn [ot] (:resolve-entity (get-in schema [:object-types ot])))
        clashes (fn [pairs]
                  (->> pairs
                       (reduce (fn [m [v t-kw]] (update m v (fnil conj []) t-kw)) {})
                       (filter (fn [[v ts']] (and (some? v) (> (count ts') 1))))))
        others  (fn [t-kw ts'] (->> ts' (remove #(= % t-kw)) (map nm) distinct vec))]
    (vec
     (concat
      (for [[ot ts'] (clashes (for [[t-kw t] ts
                                    out (outputs/outputs-of t)
                                    :when (and (= :object-type (:kind out))
                                               (not= :supplement (:role out))
                                               (not (resolved? (:object-type out))))]
                                [(:object-type out) t-kw]))
            t-kw ts']
        {:transform (nm t-kw)
         :reason (str "is a PRIMARY producer of object-type " (pr-str ot)
                      ", and so " (pr-str (others t-kw ts')) " already is"
                      " — an object type's rows may have exactly one source of"
                      " truth. Retire the one this replaces in the same push, mark"
                      " the type :resolve-entity if several sources legitimately"
                      " feed it, or make this output :role :supplement if it adds"
                      " COLUMNS rather than rows."
                      " Note that removing the other transform's file does NOT"
                      " retire it — a code repository retires nothing by"
                      " omission; retiring stays an explicit act.")})
      (for [[lt ts'] (clashes (for [[t-kw t] ts
                                    out (outputs/outputs-of t)
                                    :when (= :link-type (:kind out))]
                                [(:link-type out) t-kw]))
            t-kw ts']
        {:transform (nm t-kw)
         :reason (str "produces link-type " (pr-str lt) ", and so does "
                      (pr-str (others t-kw ts'))
                      " — a link has exactly one backing.")})
      (for [[out-name ts'] (clashes (for [[t-kw t] ts, out (outputs/outputs-of t)]
                                      [(:name out) t-kw]))
            t-kw ts']
        {:transform (nm t-kw)
         :reason (str "declares output name " (pr-str out-name) ", and so does "
                      (pr-str (others t-kw ts'))
                      " — an output name IS the `t_<name>` table name and has to"
                      " be unique across the whole instance.")})))))

(defn explain-orphaned-supplements
  [schema]
  (let [ts (or (:transforms schema) {})
        nm (fn [t-kw] (let [t (get ts t-kw)] (or (:transform-name t) t-kw)))
        ot-nm (fn [ot] (or (get-in schema [:object-types ot :object-type-name]) ot))]
    (vec
     (for [[t-kw t] ts
           out (outputs/outputs-of t)
           :when (and (= :supplement (:role out))
                      (not (and (rid/rid? (:object-type out))
                                (not (contains? (:object-types schema) (:object-type out))))))
           :let [ot   (:object-type out)
                 prim (some-> (first (outputs/primary-outputs-for schema :object-type ot)) (nth 2))
                 attrs (set (map :attribute-name
                                 (get-in schema [:object-types ot :attribute-list])))
                 stray (remove attrs (keys (:column-map out)))
                 reason
                 (cond
                   (nil? prim)
                   (str "is a :role :supplement on object-type " (pr-str (ot-nm ot))
                        ", which has NO primary producer — a supplement adds columns"
                        " to rows it does not create, so there is nothing to merge"
                        " onto. If you are taking this type apart, delete THIS"
                        " transform before the primary; if you are building it,"
                        " declare the primary producer first.")

                   (not= (:primary-key out) (:primary-key prim))
                   (str "is a :role :supplement on object-type " (pr-str (ot-nm ot))
                        " keyed on " (pr-str (:primary-key out))
                        ", but its primary is keyed on " (pr-str (:primary-key prim))
                        " — the merge key has to be the same one, or the columns"
                        " land on different rows than they describe.")

                   (seq stray)
                   (str "is a :role :supplement on object-type " (pr-str (ot-nm ot))
                        " carrying " (pr-str (vec stray))
                        ", which the type does not declare as attributes — a"
                        " supplement may only fill columns that already exist.")

                   (nil? (:primary-key out))
                   (str "is a :role :supplement on object-type " (pr-str (ot-nm ot))
                        " and declares no :primary-key — the merge key is what says"
                        " which rows the columns land on, so it has to name the same"
                        " one the primary producer is keyed on.")

                   :else nil)]
           :when reason]
       {:transform (nm t-kw) :reason reason}))))

(defn incremental? [t] (boolean (:incremental t)))

(defn- read-mode
  [t]
  (let [i (:incremental t)] (if (map? i) (:read-mode i :added) :added)))

(defn write-mode
  [t]
  (let [i (:incremental t)] (if (map? i) (:write-mode i :modify) :modify)))

(defn snapshot-write?
  [t]
  (or (not (:incremental t)) (= :replace (write-mode t))))

(defn modify-write?
  [t]
  (not (snapshot-write? t)))

(defn added-read?
  [t]
  (and (incremental? t) (= :added (read-mode t))))

(defn- incremental-flag?
  [t k]
  (let [i (:incremental t)] (boolean (and (map? i) (k i)))))

(defn strict-append?
  [t]
  (incremental-flag? t :strict-append))

(defn allow-retention?
  [t]
  (incremental-flag? t :allow-retention))

(defn require-incremental?
  [t]
  (incremental-flag? t :require-incremental))

(defn snapshot-input?
  [t alias]
  (let [i (:incremental t)]
    (boolean (and (map? i) (some #{(keyword alias)} (:static-inputs i))))))

(defn snapshot-ends-increment?
  [t alias]
  (and (added-read? t) (not (snapshot-input? t alias))))

(defn- compute-deps
  [t]
  (->> (vals (:inputs t))
       (keep (fn [in] (when (= :transform (:kind in))
                        (:transform in))))
       set))

(defn- peel
  [deps label]
  (let [{:keys [levels cycle]} (ri/kahn deps)]
    (when cycle
      (refusal/refuse! :refusal.catalog/transform-cycle {:remaining (vec cycle)} {:label label}))
    levels))

(defn topo-order
  [transforms]
  (let [ts transforms
        deps (into {}
                   (map (fn [[k t]]
                          [k (cond-> (compute-deps t)
                               (= :transform (:kind (:input t)))
                               (conj (:transform (:input t))))])
                        ts))
        all-keys (set (keys ts))]
    (into [] cat (peel (into {} (map (fn [[k d]] [k (set (filter all-keys d))]) deps))
                       "topo-order"))))

(defn deps-with
  [{:keys [producers dataset-producers media-producers link]} t]
  (into
   (cond-> (compute-deps t)
     (= :transform (:kind (:input t))) (conj (:transform (:input t))))
   (concat
    (mapcat (fn [in]
              (when-let [section (kinds/input-names-section in)]
                (when (not= :transforms section)
                  (map first (producers (rid/kinds-key->tag section) (kinds/input-ref-val in))))))
            (cons (:input t) (vals (:inputs t))))
    (mapcat (fn [in] (when (= :media-set-items (:kind in)) (media-producers (:media-set in))))
            (vals (:inputs t)))
    (mapcat (fn [in] (when (= :dataset (:kind in)) (dataset-producers (:dataset in))))
            (cons (:input t) (vals (:inputs t))))
    (mapcat (fn [out]
              (when (= :link-type (:kind out))
                (let [ct (link (:link-type out))]
                  (mapcat #(map first (producers :object-type %))
                          (link-side/ends ct)))))
            (outputs/outputs-of t))
    (mapcat (fn [out]
              (when (and (= :object-type (:kind out)) (= :supplement (:role out)))
                (map first (outputs/primary-outputs (producers :object-type (:object-type out))))))
            (outputs/outputs-of t)))))

(defn full-deps
  ([schema t] (full-deps schema (outputs/producer-index schema) t))
  ([schema index t]
   (deps-with {:producers         #(outputs/outputs-for index %1 %2)
               :dataset-producers #(outputs/dataset-producers schema %)
               :media-producers   (fn [m]
                                    (for [[k other] (:transforms schema)
                                          :when (some #(= m (:media-set %))
                                                      (vals (:media-outputs other)))]
                                      k))
               :link              #(get-in schema [:link-types %])}
              t)))

(defn cycle-core
  [schedule left]
  (loop [core (set left)]
    (let [read-in-core (into #{} (mapcat #(filter core (schedule %))) core)
          kept         (into #{} (filter read-in-core) core)]
      (if (= kept core) core (recur kept)))))

(defn- levels-around-cycles
  [schedule]
  (let [{:keys [cycle] :as sorted} (ri/kahn schedule)]
    (if-not cycle
      sorted
      (let [core (cycle-core schedule cycle)]
        (assoc sorted :levels
               (:levels (ri/kahn (into {}
                                       (for [[k ds] schedule :when (not (core k))]
                                         [k (set/difference ds core)])))))))))

(defn supplements?
  [o]
  (and (= :object-type (:kind o)) (= :supplement (:role o))))

(defn schedule-with
  [supplementing k t deps]
  (-> deps
      (disj k)
      (set/difference (into #{}
                            (comp (filter supplements?)
                                  (mapcat #(supplementing (:object-type %)))
                                  (remove #{k}))
                            (outputs/outputs-of t)))))

(defn graph-of
  [schema]
  (let [ts       (:transforms schema)
        all      (set (keys ts))
        index    (outputs/producer-index schema)
        deps     (into {} (map (fn [[k t]] [k (into #{} (filter all) (full-deps schema index t))]))
                       ts)
        rdeps    (reduce-kv (fn [m k ds]
                              (reduce (fn [m d] (update m d (fnil conj #{}) k))
                                      (update m k #(or % #{}))
                                      ds))
                            {} deps)
        supp-of  (reduce (fn [m [k t]]
                           (reduce (fn [m o]
                                     (if (supplements? o)
                                       (update m (:object-type o) (fnil conj #{}) k)
                                       m))
                                   m (outputs/outputs-of t)))
                         {} ts)
        schedule (into {} (map (fn [[k t]] [k (schedule-with supp-of k t (get deps k))])) ts)]
    (merge {:deps      deps
            :rdeps     rdeps
            :schedule  schedule}
           (update (levels-around-cycles schedule) :levels
                   (fn [ls] (some->> ls (mapv #(vec (sort-by str %)))))))))

(defn build-graph
  [schema]
  (sch/column schema ::graph graph-of))

(defn dep-graph
  [schema]
  (:schedule (build-graph schema)))

(defn topo-levels
  [schema]
  (:levels (build-graph schema)))

(def ^:private read-back-roles
  #{:supplement})

(defn own-output-reads
  [t-kw t]
  (let [writes (into #{[:transforms t-kw]}
                     (comp (remove (comp read-back-roles :role)) (keep kinds/output-target))
                     (outputs/outputs-of t))]
    (for [[alias in] (cons [:input (:input t)] (:inputs t))
          :let  [target (some-> in kinds/input-target)]
          :when (writes target)]
      {:transform t-kw :input alias :target target})))

(defn loop-problems
  [schema]
  (concat
   (for [[t-kw t] (:transforms schema)
         r        (own-output-reads t-kw t)]
     (assoc r :rule :transform-reads-own-output))
   (let [{:keys [cycle schedule]} (build-graph schema)]
     (when-let [c (seq (cycle-core schedule cycle))]
       [{:rule :transform-cycle :cycle (vec (sort-by str c))}]))))

(defn loop-refusal
  [name-of {:keys [rule transform input target cycle]}]
  [(case rule
     :transform-reads-own-output :refusal.foreign/transform-reads-own-output
     :transform-cycle            :refusal.foreign/transform-cycle)
   (case rule
     :transform-reads-own-output
     {:transform (name-of :transforms transform) :input input
      :target    (str (name (first target)) " " (name-of (first target) (second target)))}
     :transform-cycle
     {:cycle (str/join ", " (map #(name-of :transforms %) cycle))})])

(defn upstream-of   [graph targets] (interfaces/closure (:deps graph)  targets))

(defn downstream-of [graph targets] (interfaces/closure (:rdeps graph) targets))

(defn upstream-closure
  [schema target-transforms]
  (upstream-of (build-graph schema) target-transforms))

(defn downstream-closure
  [schema target-transforms]
  (downstream-of (build-graph schema) target-transforms))
