(ns sutra.builds.plan.jobspec
  (:require [sutra.types.entities :as ents]
            [sutra.catalog.llm-models :as llm-models]
            [sutra.kit.digest :as digest]
            [sutra.kit.tree :as tree]
            [sutra.types.kinds :as kinds]
            [sutra.types.transform-io :as tio]))

(set! *warn-on-reflection* true)

(def ^:private sections
  (ents/sections-in :job-spec))

(defn- ref-of
  [target m]
  (when-let [[section v] (target m)]
    (when (sections section) [section (str v)])))

(defn- inputs-of [t] (cond-> (vec (vals (:inputs t))) (:input t) (conj (:input t))))

(def ^:private not-logic [:description :status :deprecation :project :aliases])

(defn id
  [reg t]
  (when t
    (digest/sha256-hex
     (pr-str (tree/canonical {:order pr-str :leaf #(if (set? %) (vec (sort-by pr-str %)) %)}
                             (apply dissoc (llm-models/resolved reg t) not-logic))))))

(defn- worker-type [t]
  (if (some #(= :datasource (:kind %)) (inputs-of t)) :sync :transforms))

(defn- models-of [reg t]
  (vec (for [[alias in] (sort-by (comp str key) (:inputs (llm-models/resolved reg t)))
             :when (and (map? in) (llm-models/language-model-input? in))]
         (cond-> {:alias (name alias) :model (str (some-> (:model in) name))}
           (:source in)         (assoc :source (name (:source in)))
           (:provider-model in) (assoc :provider-model (:provider-model in))))))

(defn spec-of
  [reg t-rid t]
  {:id            (id reg t)
   :transform     (some-> (:transform-name t) name)
   :transform-rid (str t-rid)
   :worker-type   (worker-type t)
   :inputs        (vec (distinct (keep #(ref-of kinds/input-target %) (inputs-of t))))
   :outputs       (vec (distinct (keep #(ref-of kinds/output-target %) (tio/outputs-of t))))
   :logic         (cond-> {:language (some-> (:language t) name)}
                    (:repository t) (assoc :repository (str (:repository t)))
                    (:code t)       (assoc :code-sha (digest/sha256-hex (:code t)))
                    (:query t)      (assoc :query-sha (digest/sha256-hex (:query t)))
                    (seq (models-of reg t)) (assoc :models (models-of reg t)))})

(defn published
  [own base main?]
  (let [base-ts (update-keys (or (:transforms base) {}) str)]
    (into #{} (for [[rid t] (:transforms own)
                    :let [b (get base-ts (str rid))]
                    :when (or main? (nil? b) (not= (id base b) (id own t)))]
                (str rid)))))

(defn- present
  [schema]
  (into (into #{} (for [section sections rid (keys (get schema section))]
                    [section (str rid)]))
        (for [{:keys [dataset-name]} (vals (:datasets schema)) :when dataset-name]
          [:datasets (name dataset-name)])))

(defn resolve-outputs
  [chain present]
  (reduce (fn [m {:keys [branch transforms published] :as link}]
            (reduce (fn [m [rid t]]
                      (if-not (published (str rid))
                        m
                        (let [s (spec-of link rid t)]
                          (reduce (fn [m o]
                                    (if (or (contains? m o) (not (present o)))
                                      m
                                      (assoc m o {:branch branch :spec s})))
                                  m (:outputs s)))))
                    m (sort-by (comp str key) transforms)))
          {} chain))

(defn- first-published
  [chain rid]
  (some (fn [{:keys [branch transforms published] :as link}]
          (when (published rid)
            {:branch branch :spec (spec-of link rid (get (update-keys transforms str) rid))}))
        chain))

(defn- outputless
  [chain]
  (let [own (:transforms (first chain))]
    (into {} (for [[rid t] own
                   :when (empty? (:outputs (spec-of (first chain) rid t)))
                   :let [hit (first-published chain (str rid))]
                   :when hit]
               [(str rid) hit]))))

(defn- one-writer? [[section]] (= :datasets section))

(defn- co-writers
  [chain held]
  (into {} (for [rid (distinct (mapcat #(map str (keys (:transforms %))) chain))
                 :let [hit (first-published chain rid)]
                 :when (some #(and (held %) (not (one-writer? %))) (:outputs (:spec hit)))]
             [rid hit])))

(defn- fallback-datasets
  [chain held chosen]
  (let [refs (into #{} (for [[_ {:keys [spec]}] chosen
                             ref (concat (:inputs spec) (:outputs spec))
                             :when (and (one-writer? ref) (not (held ref)))]
                         (second ref)))]
    (into {} (for [rid refs
                   :let [names? #(or (= rid (str (key %)))
                                     (= rid (some-> (:dataset-name (val %)) name)))
                         hit    (some #(some (fn [e] (when (names? e) e)) (:datasets %)) chain)]
                   :when hit]
               hit))))

(defn- problems
  [build-branch held resolved chosen]
  (let [owner (into {} (for [[o {:keys [spec]}] resolved] [o (:transform-rid spec)]))]
    (concat
     (for [[rid hits] (group-by (comp :transform-rid :spec) (vals resolved))
           :when (< 1 (count (distinct (map :branch hits))))]
       {:problem :split :transform-rid rid :branches (vec (distinct (map :branch hits)))})
     (for [[rid {:keys [branch spec]}] chosen
           :when (not= branch build-branch)
           ref (concat (:inputs spec) (:outputs spec))
           :when (not (held ref))]
       {:problem :missing :transform-rid rid :branch branch :ref ref})
     (for [[rid {:keys [spec]}] chosen
           o (:outputs spec)
           :when (and (one-writer? o) (held o) (not= rid (owner o)))]
       {:problem :claimed :transform-rid rid :ref o :by (owner o)}))))

(defn build-transforms
  [chain schema]
  (let [build-branch (:branch (first chain))
        held     (present schema)
        resolved (resolve-outputs chain held)
        chosen   (merge (outputless chain)
                        (co-writers chain held)
                        (into {} (for [{:keys [branch spec]} (vals resolved)]
                                   [(:transform-rid spec) {:branch branch :spec spec}])))
        declared (fallback-datasets chain held chosen)
        by-link  (into {} (for [{:keys [branch transforms]} chain]
                            [branch (into {} (for [[k t] transforms] [(str k) [k t]]))]))]
    {:transforms (into {} (for [[rid {:keys [branch]}] chosen]
                            (get-in by-link [branch rid])))
     :datasets   declared
     :jobspecs   resolved
     :problems   (vec (problems build-branch
                                (into held (present {:datasets declared}))
                                resolved chosen))}))

(defn rollback-verdict
  [ds {:keys [spec]} built-id]
  (cond
    (:stream ds)
    {:refuse :refusal.builds/rollback-stream}

    (= :restricted-view (:kind ds))
    {:refuse :refusal.builds/rollback-restricted-view}

    (= :virtual (:kind ds))
    {:refuse :refusal.builds/rollback-virtual}

    (and spec built-id (not= built-id (:id spec)))
    {:warning (str "The dataset logic was changed after the selected transaction time."
                   " Rolling back this dataset will not revert logic changes.")}))
