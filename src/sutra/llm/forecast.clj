(ns sutra.llm.forecast
  (:require [sutra.types.column :as column]
            [sutra.store.control :as ctl]
            [clojure.string :as str]
            [sutra.catalog.outputs :as outputs]
            [sutra.catalog.transforms :as transforms]
            [sutra.catalog.lookup :as lookup]
            [sutra.llm.budget :as budget]
            [sutra.catalog.llm-models :as llm-models]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.access.substrate :as sub]
            [sutra.types.llm-forecast :as fc]
            [sutra.types.kinds :as kinds]))

(set! *warn-on-reflection* true)

(defn llm-transforms
  [schema closure]
  (let [ts (:transforms schema)]
    (vec
     (for [k     (transforms/topo-order ts)
           :when (or (nil? closure) (contains? closure k))
           :let  [t (get ts k)
                  models (keep #(when (llm-models/language-model-input? %) (:model %))
                               (vals (:inputs t)))]
           :when (seq models)]
       {:key         k
        :rid         (lookup/rid-of schema :transforms k)
        :transform   (name (or (:transform-name t) k))
        :model       (str (first models))
        :models      (mapv str models)
        :incremental? (boolean (transforms/added-read? t))
        :done-key    (:done-key t)
        :prompt-key  (:prompt-key t)
        :semantic-version (:semantic-version t)
        :max-per-run (:max-per-run t)
        :output-pk   (or (:primary-key t) (:primary-key (first (outputs/outputs-of t))))
        :output-rid  (:dataset (first (outputs/outputs-of t)))
        :inputs      (vec (for [[alias in] (:inputs t)
                                :when (not (llm-models/language-model-input? in))
                                :let  [rid (outputs/input-dataset-rid schema in)]
                                :when rid]
                            {:alias (name alias) :rid rid :kind (:kind in)
                             :added-read? (boolean
                                           (and (transforms/added-read? t)
                                                (kinds/added-read-kind? (:kind in))))}))}))))

(defn- input-rows
  [store ledger t-rid {:keys [rid kind added-read?]}]
  (cond
    (= :object-type kind) {:rows (sub/stored-count store rid)
                           :basis :full}
    (not added-read?)     {:rows (dsets/table-count store rid) :basis :full}
    :else
    (let [d ((:added-changed ledger) t-rid rid)]
      (if (= :bootstrap d)
        {:rows (dsets/table-count store rid) :basis :bootstrap}
        {:rows (count d) :basis :incremental}))))

(defn- driving-input
  [store ledger {:keys [rid inputs]}]
  (let [counted (mapv #(merge % (input-rows store ledger rid %)) inputs)]
    (when (seq counted)
      (assoc (apply max-key :rows counted) :all counted))))

(defn- col
  [k]
  (column/sql-name k))

(defn- declared-input [inputs input]
  (some #(when (= (name input) (:alias %)) %) inputs))

(defn- missing-table? [^Exception e]
  (boolean (re-find #"(?i)table|does not exist" (str (.getMessage e)))))

(defn pending-by-done-key
  [store {:keys [done-key inputs output-rid output-pk]}]

  (when-let [{:keys [input key]} done-key]
    (when-let [in (declared-input inputs input)]
      (when (and output-rid (pos? (dsets/table-count store (:rid in))))
        (let [pk (or output-pk key)]
          (try
            (-> (dsets/query store
                 {"i" (:rid in) "o" output-rid}
                 (str "SELECT count(DISTINCT i." (col key) ") AS pending"
                      " FROM i"
                      " WHERE i." (col key) " IS NOT NULL"
                      "   AND i." (col key) " NOT IN"
                      "       (SELECT o." (col pk) " FROM o"
                      "         WHERE o." (col pk) " IS NOT NULL)")
                 [])
                first :pending long)
            (catch Exception e
              (if (missing-table? e) (dsets/table-count store (:rid in)) (throw e)))))))))

(defn prompt-hash-sql
  [version cols]
  (str "md5(concat(md5('" (long (or version 1)) "'), "
       (str/join ", " (for [c cols] (str "md5(coalesce(i." (col c) ", ''))")))
       "))"))

(defn pending-by-prompt-key
  [store {:keys [prompt-key inputs output-rid semantic-version]}]
  (when-let [{:keys [input columns column]} prompt-key]
    (when-let [in (declared-input inputs input)]
      (let [total (dsets/table-count store (:rid in))]
        (when (and output-rid (pos? total))
          (let [hash-col (col (or column :prompt-hash))]
            (try
              {:total   total
               :output-rows (dsets/table-count store output-rid)
               :pending (-> (dsets/query store
                             {"i" (:rid in) "o" output-rid}
                             (str "SELECT count(*) AS pending"
                                  " FROM i"
                                  " WHERE " (prompt-hash-sql semantic-version columns) " NOT IN"
                                  "       (SELECT o." hash-col " FROM o"
                                  "         WHERE o." hash-col " IS NOT NULL)")
                             [])
                            first :pending long)}
              (catch Exception e
                (if (missing-table? e)
                  {:total total :pending total :output-rows 0}
                  (throw e))))))))))

(defn call-history
  [store schema since]
  (if-let [rid (lookup/rid-of schema :datasets :llm_call_log)]
    (if (zero? (dsets/table-count store rid))
      []
      (dsets/query store
       {"log" rid}
       (str "SELECT caller, model, count(*) AS calls,"
            " sum(prompt_tokens) AS in_tokens, sum(completion_tokens) AS out_tokens,"
            " max(ts) AS last_ts"
            " FROM log"
            " WHERE error IS NULL AND ts >= ?"
            " GROUP BY caller, model")
       [since]))
    []))

(defn- window-rows
  [ledger input-rid since]
  (->> ((:transactions ledger) input-rid)
       (filter #(>= (compare (str (:started_at %)) (str since)) 0))
       (remove #(= "ABORTED" (str (:status %))))
       (map #(long (or (:row_delta %) 0)))
       (filter pos?)
       (reduce + 0)))

(defn- iso-days-ago
  [store days]
  (str (.minus (ctl/instant store) (long days)
               java.time.temporal.ChronoUnit/DAYS)))

(defn estimate
  [store schema closure {:keys [days calls-per-row ledger]}]
  (let [days    (or days fc/default-window-days)
        since   (iso-days-ago store days)
        history (call-history store schema since)
        observed (:observed ledger)
        entries
        (vec
         (for [t (llm-transforms schema closure)]
           (let [sample  (fc/sample (fc/history-for history (:rid t) (:transform t)) (:model t))
                 rate    (llm-models/rate-of schema (:model t))
                 base    (select-keys t [:transform :rid :model :models :incremental?])
                 by-prompt (pending-by-prompt-key store t)
                 pending   (or (:pending by-prompt) (pending-by-done-key store t))
                 obs       (get observed (str (:rid t)))]
             (if pending
               (let [pc (fc/per-call sample)]
                 (merge base
                        (fc/project-drain pending (:max-per-run t) pc rate (:pending obs))
                        {:pending-basis (if by-prompt :prompt-key :done-key)
                         :input    (name (:input (or (:prompt-key t) (:done-key t))))
                         :done-key (:done-key t)
                         :prompt-key (:prompt-key t)
                         :observed obs
                         :hash-mismatch? (boolean
                                          (and by-prompt
                                               (= (:pending by-prompt) (:total by-prompt))
                                               (pos? (long (:output-rows by-prompt 0)))))
                         :per-call pc
                         :rows     pending}))
               (if-let [n (:pending obs)]
                 (let [pc (fc/per-call sample)]
                   (merge base
                          (fc/project-drain n (:max-per-run t) pc rate nil)
                          {:pending-basis :measured
                           :observed obs
                           :per-call pc
                           :rows     n}))
                 (let [drv    (driving-input store ledger t)
                       rows   (:rows drv 0)
                       in-win (when (:rid drv) (window-rows ledger (:rid drv) since))
                       pr     (fc/per-row sample in-win calls-per-row)]
                   (merge base
                          (fc/project rows pr rate)
                          {:pending-basis :delta
                           :basis    (:basis drv :none)
                           :input    (:alias drv)
                           :inputs   (mapv #(select-keys % [:alias :rows :basis]) (:all drv))
                           :per-row  pr
                           :window-rows in-win})))))))
        total (fc/roll-up entries)]
    {:window     {:days days :since since}
     :branch     (:branch store)
     :transforms entries
     :total      total
     :budget     (fc/budget-verdict {:budget (budget/budget-eur store)
                                     :spent  (budget/spent-today store)}
                                    (:cost-next-run total)
                                    (mapv #(assoc % :cost (or (:cost-next-run %) (:cost %)))
                                          entries))}))
