(ns sutra.llm.rates
  (:require [sutra.kit.refusal :as refusal]
            [sutra.store.access.datasets :as dsets]
            [sutra.catalog.system-datasets :as sysds]
            [clojure.string :as str]
            [sutra.types.llm-cost :as llm-cost]))

(set! *warn-on-reflection* true)

(def ^:private llm-rates-rid
  "ri.sutra.main.dataset.225d57a0-b16c-38ad-892c-d7da11d6f4aa")

(defn- flag? [x]
  (cond (boolean? x) x
        (number? x)  (not (zero? x))
        (string? x)  (contains? #{"1" "true" "t"} (str/lower-case (str/trim x)))
        :else        false))

(defn- latest-rates
  [values]
  (into {} (map (fn [[m rs]] [m (last (sort-by #(str (:effective-from %)) rs))]))
        (group-by :model values)))

(defn- same-rate? [{:keys [in out currency deprecated]} cur]
  (and cur
       (== (double in)  (double (:rate-in cur)))
       (== (double out) (double (:rate-out cur)))
       (= (or currency "USD") (or (not-empty (str (:currency cur))) "USD"))
       (= (flag? deprecated) (flag? (:deprecated cur)))))

(defn- rate-rows
  [models latest at]
  (let [now (str at)]
    (vec
     (for [[model {:keys [in out currency deprecated] :as spec}] models
           :let [cur (get latest (name model))]
           :when (not (same-rate? spec cur))
           :let [ccy     (or currency "USD")
                 was-dep (flag? (:deprecated cur))
                 now-dep (flag? deprecated)
                 reason  (cond (and now-dep (not was-dep)) "deprecate"
                               (and was-dep (not now-dep)) "restore"
                               :else                       "register")]]
       {:id     (llm-cost/row-id model now in out ccy)
        :values {:model model :rate_in in :rate_out out
                 :currency ccy :effective_from now :reason reason
                 :deprecated now-dep}}))))

(defn record-llm-rates!
  [store head at]
  (when-let [models (not-empty (:llm-models head))]
    (let [rows (rate-rows models (latest-rates (map :values (dsets/rows store llm-rates-rid))) at)]
      (when (seq rows)
        (dsets/ensure-datasets! store
                                {llm-rates-rid
                                   (get-in sysds/system-datasets [llm-rates-rid :columns])})
        (dsets/write-system-rows! store llm-rates-rid :append rows)))))

(defn reprice-llm-model!
  [store schema {:keys [model in out currency effective-from reason]}]
  (let [nm  (some-> model str str/trim not-empty)
        eff (some-> effective-from str str/trim not-empty)
        rate (fn [k v] (let [d (some-> v str str/trim parse-double)]
                         (when (or (nil? d) (neg? d))
                           (refusal/refuse! :refusal.llm/reprice-negative
                                            {:field k :value v}
                                            {:field-name (name k)}))
                         d))]
    (when-not (and nm eff (some? in) (some? out))
      (refusal/refuse! :refusal.llm/reprice-incomplete
                       {:model model :effective-from effective-from}))
    (when-not (contains? (:llm-models schema) (keyword nm))
      (refusal/refuse! :refusal.llm/reprice-unregistered {:model nm}))
    (let [in*  (rate :in in)
          out* (rate :out out)
          ccy  (or (some-> currency str str/trim not-empty) "USD")
          eff* (if (re-matches #"\d{4}-\d{2}-\d{2}" eff) (str eff "T00:00:00Z") eff)
          id   (llm-cost/row-id nm eff* in* out* ccy)]
      (dsets/ensure-datasets! store
                              {llm-rates-rid
                                 (get-in sysds/system-datasets [llm-rates-rid :columns])})
      (dsets/write-system-rows!
       store llm-rates-rid :append
       [{:id id
         :values {:model nm :rate_in in* :rate_out out* :currency ccy
                  :effective_from eff* :reason (or (not-empty (str reason)) "correction")}}])
      {:model nm :effective-from eff* :in in* :out out* :currency ccy :id id})))
