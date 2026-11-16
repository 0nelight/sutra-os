(ns sutra.builds.import.outcome
  (:require [clojure.string :as str]
            [sutra.kit.anomaly :as anomaly]
            [sutra.foundation.contracts :as ct]
            [sutra.foundation.settings :as settings]
            [sutra.store.control :as ctl]
            [sutra.store.storage.command :as cmd]
            [sutra.builds.ledger.jobs :as jl]
            [sutra.builds.ledger.report :as report]
            [sutra.builds.plan.freshness :as fresh]
            [sutra.builds.run.monitor :as bm]))

(set! *warn-on-reflection* true)

(defn- finish-build!
  [{:keys [store schema pa phase-times run-id]} {:keys [noted publish-failures withheld]}]
  (bm/note-final! run-id)
  (bm/unregister! run-id)
  (let [timings (vec (sort-by :ms > (:done @pa)))
        phases  (phase-times)]
    (when (settings/value :import-timing)
      (report/print-build-timing! phases timings))
    (let [job-result (ct/check! :sutra.foundation.contracts/job-result noted)]
      (some->> (:trace job-result) (report/print-build-trace! schema))
      (let [attributed (when (seq publish-failures)
                         (count
                          (cmd/execute! store
                                        (jl/publish-failure-effects
                                         schema run-id publish-failures
                                         (ctl/stamp store)))))]
        (fresh/stamp-fingerprints! store run-id schema withheld job-result)
        {:timings timings :phases phases :job-result job-result :attributed attributed}))))

(defn- publish-failure
  [publish-failures attributed]
  (when (seq publish-failures)
    (anomaly/ex (anomaly/refused
                 :fault
                 (str (count publish-failures) " publish job(s) landed nothing: "
                      (str/join ", " (map (comp str :fn) publish-failures)))
                 {:publish-failures publish-failures
                  :jobs-attributed? (pos? (long (or attributed 0)))}))))

(defn build-result
  [{:keys [store schema scope run-id] :as env}
   {:keys [counts rows stage-writes backing-writes pk-index-named index-report
           publish-failures unpublished job-failure] :as state}]
  (let [links-standing (report/standing-links store schema (keys rows))
        {:keys [timings phases job-result attributed]} (finish-build! env state)
        failure        (or job-failure (publish-failure publish-failures attributed))
        findings       (report/findings schema scope index-report job-result links-standing)]
    (merge {:counts         (assoc (merge-with - counts unpublished) :links-standing links-standing)
            :pk-index       pk-index-named
            :touched-types  (vec (keys rows))
            :timings        timings
            :phase-timings  phases
            :dataset-writes (report/mark-bootstrap-snapshots run-id (:bootstrapped job-result)
                                                             (concat stage-writes backing-writes))
            :consumed       (report/build-consumed schema (:consumed-cursors job-result)
                                                   (:incomplete job-result))
            :summary        findings}
           findings
           (when failure {:failure failure}))))
