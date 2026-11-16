(ns sutra.llm.cost
  (:require [sutra.kit.noted :as noted]
            [sutra.kit.coerce :refer [->num0]]
            [clojure.string :as str]
            [sutra.catalog.lookup :as lookup]
            [sutra.store.access.datasets :as dsets]
            [sutra.types.llm-cost :as llm-cost]
            [sutra.viz.lineage-projection :as lp]))

(set! *warn-on-reflection* true)

(def ^:private call-cols
  ["ts" "request_id" "caller" "run_id" "source" "model"
   "prompt_tokens" "completion_tokens" "latency_ms" "error"])

(defn- read-rows
  [store schema ds-name cols where params]
  (if-let [rid (lookup/rid-of schema :datasets ds-name)]
    (noted/or-else (str "reading " ds-name) []
      (dsets/query store {"log" rid}
                   (str "SELECT " (str/join ", " cols) " FROM log"
                        (when where (str " WHERE " where)))
                   (vec params)))
    []))

(defn- log-present?
  [schema]
  (boolean (lookup/rid-of schema :datasets :llm_call_log)))

(defn resource-index
  [schema]
  (merge (into {} (for [[rid d] (:datasets schema)]
                    [(llm-cost/caller-key rid) (:dataset-name d)]))
         (into {} (for [[rid t] (:transforms schema)
                        :let [nm (:transform-name t)]
                        :when nm
                        k (llm-cost/caller-keys rid (name nm))]
                    [k nm]))))

(defn node-index
  [schema]
  (merge (into {} (for [[rid d] (:datasets schema)
                        :let [nm (:dataset-name d)]
                        :when nm]
                    [(llm-cost/caller-key rid) (lp/status-key :dataset nm)]))
         (into {} (for [[rid t] (:transforms schema)
                        :let [nm (:transform-name t)]
                        :when nm
                        k (llm-cost/caller-keys rid (name nm))]
                    [k (lp/status-key :transform nm)]))))

(defn- priced-rows
  [store schema {:keys [since until caller run]}]
  (let [index  (resource-index schema)
        rates  (read-rows store schema :llm_model_rates
                          ["model" "rate_in" "rate_out" "currency" "effective_from"]
                          nil nil)
        rows   (read-rows store schema :llm_call_log call-cols nil nil)
        needle (fn [q & sels]
                 (or (str/blank? (str q))
                     (let [n (str/lower-case (str q))]
                       (boolean (some #(str/includes? (str/lower-case (str %)) n) sels)))))]
    {:logged (count rows)
     :rows   (->> (llm-cost/price-calls rates rows)
                  (mapv #(assoc % :resource (llm-cost/resolve-resource index (:caller %))))
                  (filterv #(llm-cost/within-day-window? since until (:ts %)))
                  (filterv #(needle caller (:caller %) (:resource %)))
                  (filterv #(or (str/blank? (str run))
                                (str/starts-with? (str (:run-id %)) (str run)))))}))

(defn- money-fields
  [priced]
  (let [real     (remove #(seq (str (:error %))) priced)
        unpriced (filter #(nil? (:cost %)) real)
        by-ccy   (->> (filter :cost priced)
                      (group-by #(or (:currency %) llm-cost/default-currency))
                      (map (fn [[ccy rs]] [ccy (reduce + 0.0 (map :cost rs))]))
                      (sort-by first))
        total    (when-let [cs (seq (keep :cost priced))] (reduce + 0.0 cs))]
    {:cost             total
     :currency         (or (ffirst by-ccy) llm-cost/default-currency)
     :cost-by-currency (into {} by-ccy)
     :unpriced-calls   (count unpriced)
     :unpriced-models  (vec (distinct (map #(or (:model %) "?") unpriced)))
     :unpriced-earliest
     (into {} (for [[m rs] (group-by #(or (:model %) "?") unpriced)]
                [m (->> rs (keep :ts) sort first)]))}))

(defn cost
  [store schema {:keys [calls? per per-count] :as opts}]
  (let [{:keys [logged rows]} (priced-rows store schema opts)
        pt (long (reduce + 0.0 (map #(->num0 (:prompt-tokens %)) rows)))
        ct (long (reduce + 0.0 (map #(->num0 (:completion-tokens %)) rows)))
        money (money-fields rows)]
    (cond-> (merge money
                   {:calls  (count rows)
                    :errors (count (filter #(seq (str (:error %))) rows))
                    :since  (:since opts) :until (:until opts)
                    :caller (:caller opts) :run (:run opts)
                    :prompt-tokens pt :completion-tokens ct
                    :per per :per-count per-count
                    :cost-per-unit (when (and (:cost money) per-count (pos? (long per-count)))
                                     (/ (:cost money) (long per-count)))
                    :logged  logged
                    :log?    (log-present? schema)
                    :by-resource
                      (vec (llm-cost/rollup :resource #(or (:resource %) (:caller %)) rows))
                    :by-model    (vec (llm-cost/rollup :model :model rows))})
      calls? (assoc :call-rows
                    (mapv #(select-keys % [:ts :caller :resource :model :source
                                           :prompt-tokens :completion-tokens
                                           :cost :currency :latency-ms :error])
                          (reverse (sort-by :ts rows)))))))

(defn usage-by-node
  [store schema]
  (let [{:keys [rows]} (priced-rows store schema {})
        idx (node-index schema)]
    (llm-cost/usage-rollup #(get idx (llm-cost/caller-key (:caller %))) rows)))

(defn trace
  [store schema request-id]
  (let [rows (read-rows store schema :llm_call_log
                        (conj call-cols "input_json" "output_json" "input_sha")
                        "request_id = ?" [(str request-id)])]
    (when-let [r (first rows)]
      (assoc r :resource (llm-cost/resolve-resource (resource-index schema) (:caller r))))))

(defn run-calls
  [store schema run-id]
  (->> (read-rows store schema :llm_call_log (conj call-cols "input_json" "output_json")
                  "run_id = ?" [(str run-id)])
       (sort-by :ts)
       vec))

(defn- hit? [q & sels]
  (let [n (str/lower-case (str q))]
    (boolean (some #(str/includes? (str/lower-case (str %)) n) sels))))

(defn- audit-where
  [store schema index {:keys [caller model errors? since until]}]
  (let [callers (when-not (str/blank? (str caller))
                  (->> (read-rows store schema :llm_call_log ["caller"] nil nil)
                       (map :caller) (remove nil?) distinct
                       (filterv #(hit? caller % (llm-cost/resolve-resource index %)))))
        conds   (cond-> []
                  (not (str/blank? (str since))) (conj ["substr(coalesce(ts, ''), 1, 10) >= ?"
                                                        (str since)])
                  (not (str/blank? (str until))) (conj ["substr(coalesce(ts, ''), 1, 10) <= ?"
                                                        (str until)])
                  (not (str/blank? (str model))) (conj ["strpos(lower(coalesce(model, '')), ?) > 0"
                                                        (str/lower-case (str model))])
                  errors?                        (conj ["coalesce(error, '') <> ''"])
                  callers (conj (into [(str "caller IN (" (str/join ", " (repeat (count callers)
                                                                                "?")) ")")]
                                      callers)))]
    (when-not (and callers (empty? callers))
      [(when (seq conds) (str/join " AND " (map first conds))) (vec (mapcat rest conds))])))

(defn audit
  [store schema {:keys [caller model since until limit] :as opts}]
  (let [index  (resource-index schema)
        lim    (max 1 (or limit 20))
        logged (or (some-> (first (read-rows store schema :llm_call_log ["count(*) AS n"] nil nil))
                           :n long)
                   0)
        [where params :as w] (audit-where store schema index opts)
        tally  (when w
                 (first (read-rows store schema :llm_call_log
                                   ["count(*) AS calls"
                                    "sum(CASE WHEN coalesce(error, '') <> '' THEN 1 ELSE 0 END) AS errors"]
                                   where params)))
        shown  (when w
                 (read-rows store schema :llm_call_log call-cols
                            (str (or where "TRUE") " ORDER BY ts DESC NULLS LAST LIMIT " lim)
                            params))]
    {:calls  (long (or (:calls tally) 0))
     :errors (long (or (:errors tally) 0))
     :shown  (count shown)
     :logged logged
     :log?   (log-present? schema)
     :since  since :until until :caller caller :model model
     :rows   (mapv #(-> (assoc % :resource (llm-cost/resolve-resource index (:caller %)))
                        (select-keys [:request-id :ts :caller :resource :model :source
                                      :prompt-tokens :completion-tokens :latency-ms :error]))
                   shown)}))
