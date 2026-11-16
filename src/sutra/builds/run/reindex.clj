(ns sutra.builds.run.reindex
  (:require [sutra.store.seat :as seat]
            [sutra.builds.import :as importer]
            [sutra.builds.import.sources :as import-sources]
            [sutra.builds.run.monitor :as bm]
            [sutra.builds.ledger.marks :as marks]))

(set! *warn-on-reflection* true)

(defn reindex-if-changed!
  [store schema branch]
  (let [sig (import-sources/source-signature store schema branch)]
    (cond
      (nil? sig)             {:reindexed? false :reason :no-plan}
      (bm/any-build-active? store) {:reindexed? false :reason :busy}
      :else
      (let [r (marks/consume! store branch :reindex sig
                              #(importer/build! store branch {:origin "reindex poller"}
                                                :fire))]
        (case r
          ::marks/first {:reindexed? false :reason :baseline}
          ::marks/same  {:reindexed? false :reason :unchanged}
          (assoc r :reindexed? true :reason :changed))))))

(def module
  {:name :reindex
   :jobs [{:name  :reindex :every "SUTRA_REINDEX_MS"
           :tick  (seat/on-branch
                   "reindex"
                   (fn [store branch]
                     (when (:reindexed?
                            (reindex-if-changed! store (seat/branch-schema store) branch))
                       (println (str "[reindex] " branch
                                     " source changed → reindexed")))))}]})
