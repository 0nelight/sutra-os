(ns sutra.builds.ledger.jobs
  (:require [sutra.catalog.backing :as backing]
            [clojure.string :as str]
            [sutra.builds.ledger.facts :as facts]
            [sutra.builds.ledger :as ledger]
            [sutra.builds.ports :as ports]
            [sutra.builds.run :as runs]
            [sutra.store.storage.command :as cmd]
            [sutra.store.control :as ctl]))

(set! *warn-on-reflection* true)

(defn job-facts
  [noted t-kw at]
  (facts/job-facts noted t-kw at))

(defn ledger!
  [store effects]
  (try (cmd/execute! store effects)
       (catch InterruptedException e (throw e))
       (catch Exception e
         (.println System/err
                   (str "  ⚠ ledger write failed (" (str/join ", " (distinct (keep :op effects)))
          "): " (ex-message e)))
         nil)))

(defn open-job-facts
  [build-branch run-id t-kw attempt at]
  {:job-rid (ledger/job-rid run-id t-kw) :run-id run-id :transform-rid t-kw
   :branch build-branch :attempt attempt :at at})

(defn- failed-spans
  [store e]
  (when-let [err (some #(:stderr (ex-data %)) (take-while some? (iterate ex-cause e)))]
    (not-empty (:spans ((ports/compute-runner store :spans) err)))))

(defn record-job-failure!
  [{:keys [store schema run-id]} noted t-kw e]
  (when t-kw
    (let [fs (select-keys (job-facts noted t-kw (ctl/stamp store)) [:job-log :job-spans :at])]
      (ledger! store [(ledger/fail-job (assoc fs
                                              :job-spans (or (:job-spans fs) (failed-spans store e))
                                              :job-rid (ledger/job-rid run-id t-kw)
                                              :error (runs/describe-throwable schema e)))]))))

(defn terminal-job-effects
  [noted run-id branch facts at]
  (for [[t-rid fs] (group-by :transform-rid facts)
        :when t-rid]
    (ledger/finish-job
     (merge (job-facts noted t-rid at)
            {:job-rid (ledger/job-rid run-id t-rid) :run-id run-id
             :transform-rid t-rid :branch branch
             :rows-out (reduce + 0 (keep :written fs))
             :snapshot-id nil :outputs (keep :dataset-rid fs)}))))

(defn publish-failure-effects
  [schema run-id failures at]
  (for [f failures
        :let [t (or (:t-kw f)
                    (when-let [ty (:type f)]
                      (if (= :links (:kind f))
                        (:producer (backing/backing-of schema :link-type ty))
                        (let [b (backing/object-backing schema ty)]
                          (when-not (:authored? b) (:producer b))))))]
        :when t]
    (ledger/fail-job
     {:job-rid (ledger/job-rid run-id t)
      :error (str "published nothing: " (get f :fn) " — " (:error f)
                  (when-let [c (get f :count)] (str " (" c " row(s))")))
      :at at})))

(defn pend-level!
  [store run-id nodes]
  (when run-id
    (when-let [jrs (seq (distinct (keep #(some->> (:t-kw %) (ledger/job-rid run-id)) nodes)))]
      (ledger! store [(ledger/pend-jobs run-id jrs)]))))
