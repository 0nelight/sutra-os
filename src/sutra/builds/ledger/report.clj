(ns sutra.builds.ledger.report
  (:require [sutra.store.access.substrate :as sub]
            [sutra.catalog.outputs :as outputs]
            [sutra.catalog.transforms :as transforms]
            [sutra.catalog.lookup :as lookup]
            [sutra.builds.plan :as plan]
            [sutra.builds.ledger :as ledger]
            [sutra.types.kinds :as kinds]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]))

(set! *warn-on-reflection* true)

(defn- trace-fp-str
  [e]
  (str "rows=" (:rows e)
       (when (seq (:cols e)) (str " cols=" (pr-str (mapv name (:cols e)))))
       (when-let [k (:key e)]
         (str " key=" (name k) " key-nulls=" (:key_nulls e)
              (when (seq (:key_sample e)) (str " sample=" (pr-str (vec (:key_sample e)))))))
       (when (seq (:null_cols e))
         (str " null-cols=" (pr-str (vec (:null_cols e)))))))

(defn resolve-trace-names
  [schema trace]
  (mapv (fn [e]
          (cond-> e
            (:consumer-rid e)
            (assoc :consumer (or (lookup/name-of schema :transforms (:consumer-rid e))
                                 (:consumer-rid e)))
            (:input-dataset-rid e)
            (assoc :input (or (lookup/name-of schema :datasets (:input-dataset-rid e))
                              (:input-dataset-rid e)))))
        trace))

(defn- consumed-edges
  [schema]
  (vec
   (for [[t-kw t] (:transforms schema)
         [_ in]   (:inputs t)
         :let [in-rid (outputs/input-dataset-rid schema in)]
         :when in-rid]
     {:consumer-rid t-kw :input-dataset-rid in-rid})))

(defn- incremental-dataset-edges
  [schema]
  (set (for [[t-kw t] (:transforms schema)
             :when (transforms/added-read? t)
             [alias in] (:inputs t)
             :when (and (kinds/added-read-kind? (:kind in))
                        (not (transforms/snapshot-input? t alias)))]
         [t-kw (outputs/input-dataset-rid schema in)])))

(defn build-consumed
  [schema cursors incomplete]
  (let [inc-edges (incremental-dataset-edges schema)
        cursors   (remove #(contains? incomplete (:consumer-rid %)) cursors)]
    (into (into [] (remove #(inc-edges [(:consumer-rid %) (:input-dataset-rid %)]))
                (consumed-edges schema))
          cursors)))

(defn mark-bootstrap-snapshots
  [run-id booted writes]
  (if (empty? booted)
    (vec writes)
    (let [booted-jobs (into #{} (map #(ledger/job-rid run-id %)) booted)]
      (mapv (fn [w] (cond-> w (booted-jobs (:job-rid w)) (assoc :bootstrap? true)))
            writes))))

(defn index-reconcile-summary
  [schema report]
  (when (seq report)
    (let [idx     (sch/rid-name-index schema)
          name-of (fn [target] (or (some-> (rid/resolve-rid->name idx target) name)
                                   (str target)))
          fulls   (filter #(= :full-compare (:via %)) report)]
      (cond-> {:derivations (frequencies (map :via report))
               :swept (reduce + 0 (keep :removed report))}
        (seq fulls) (assoc :full-compare (mapv (comp name-of :target) fulls))))))

(defn reprocessing-warnings
  [schema {:keys [dropped-cols bootstrapped incomplete]}]
  (let [llm?     (fn [t] (some #(= :language-model (:kind %)) (vals (:inputs t))))
        incr-llm (for [[k t] (:transforms schema)
                       :when (and (:incremental t) (llm? t))
                       :let  [rid (lookup/rid-of schema :transforms k)]
                       :when (or (contains? bootstrapped rid) (contains? incomplete rid))]
                   {:kind         :incremental-llm-full-read
                    :transform    (name k)
                    :bootstrapped (contains? bootstrapped rid)
                    :capped       (contains? incomplete rid)
                    :detail       (str "incremental LLM transform " (name k) " "
                                       (if (contains? bootstrapped rid)
                                         "read its WHOLE input (consumer-pointer at zero) and sent it to the model"
                                         "capped its batch and withheld its cursor")
                                       " — if this recurs every build it re-Groks rows it already "
                                       "has "
                                       "results for. Check `consumption " (name k)
                                       "` / `delta " (name k) " <input>`.")})
        dropped  (for [[t-name cols] dropped-cols :when (seq cols)
                       :let [cols (mapv #(if (keyword? %) (name %) (str %)) cols)]]
                   {:kind      :dropped-output-column
                    :transform (name t-name)
                    :columns   cols
                    :detail    (str (name t-name) " emitted column(s) " cols
                                    " that map to no declared attribute or link-FK — silently "
                                    "DROPPED. If an incremental previous_output() done-filter keys "
                                    "on one, the dedup set is always empty and every row is "
                                    "re-processed. Declare it as an attribute, or dedup on a "
                                    "persisted key.")})]
    (vec (concat dropped incr-llm))))

(defn print-build-trace!
  [schema trace]
  (doseq [{:keys [kind consumer-rid input-dataset-rid incremental?
                  path rows low high prev-out done-rows events]} trace]
    (let [nm (fn [k rid] (or (lookup/name-of schema k rid) rid))]
      (case kind
        :prev-output
        (println (format "[import-trace  prev-output %-24s] done-rows=%s prev-out=%s"
                         (name (nm :transforms consumer-rid)) done-rows prev-out))
        :code
        (let [cons  (name (nm :transforms consumer-rid))
              chat  (count (filter #(= "chat" (:event %)) events))
              other (remove #(#{"chat" "previous_output" "input"} (:event %)) events)]
          (println (format "[import-trace  code %-24s] chat-calls=%s%s"
                           cons chat
                           (if (seq other) (str " events=" (pr-str (vec other))) "")))
          (doseq [e events :when (= "input" (:event e))]
            (println (format "[import-trace    input %-18s %s] %s"
                             (str (:name e)) cons (trace-fp-str e))))
          (doseq [e events :when (= "previous_output" (:event e))]
            (println (format "[import-trace    previous_output %-8s %s] %s"
                             "" cons (trace-fp-str e)))))
        (println (format "[import-trace  read %-24s ← %-16s] path=%s incr=%s rows=%s low=%s high=%s"
                         (name (nm :transforms consumer-rid))
                         (name (nm :datasets input-dataset-rid))
                         (name (or path :?)) (boolean incremental?)
                         rows (or low "-") (or high "-")))))))

(defn print-build-timing!
  [phases timings]
  (doseq [{:keys [phase ms]} phases]
    (println (format "[import-phase  %-20s] %8dms" (name phase) ms)))
  (doseq [{:keys [transform ms rows]} timings]
    (println (format "[import-xf     %-28s] %8dms  rows=%s"
                     (name transform) ms (str rows)))))

(defn standing-links
  [store schema touched]
  (let [touched (set touched)
        lts     (into {} (filter (fn [[_ lt]] (or (touched (lookup/from-type lt))
                                                  (touched (lookup/to-type lt)))))
                      (:link-types schema))]
    (if (empty? lts)
      0
      (reduce + 0 (map #(sub/count-links store %) (keys lts))))))

(defn findings
  [schema scope index-report job-result links-standing]
  (let [trace    (some->> (:trace job-result) (resolve-trace-names schema))
        scope    (some->> scope (plan/resolve-scope-names schema))
        index    (index-reconcile-summary schema index-report)
        warnings (reprocessing-warnings schema job-result)]
    (cond-> {:links-standing links-standing}
      trace          (assoc :trace trace)
      scope          (assoc :scope scope)
      index          (assoc :index-reconcile index)
      (seq warnings) (assoc :warnings warnings))))
