(ns sutra.sources.export
  (:require [sutra.sources.protocols :as api]
            [sutra.sources.datasources :as ds]
            [sutra.sources.policy :as policy]
            [sutra.kit.refusal :as refusal]))

(set! *warn-on-reflection* true)

(def batch-rows
  500)

(defn- assert-data-exportable!
  [src markings]
  (let [allowed (set (:export-markings src))
        leaking (remove allowed markings)]
    (when (seq leaking)
      (refusal/refuse! :refusal.foreign/export-markings
                       {:source (:source-name src) :leaking (vec leaking)
                        :export-markings allowed}
                       {:source (str (:source-name src))}))))

(defn export-rows!
  [auth src {:keys [table rows truncate? markings]}]
  (ds/assert-source-write-cleared! auth src)
  (policy/assert-export-allowed! src)
  (assert-data-exportable! src markings)
  (api/with-source [s (partial ds/connect auth) src]
    (when truncate?
      (api/write! s {:table table :truncate true}))
    {:truncated? (boolean truncate?)
     :written    (reduce (fn [n batch]
                           (+ n (or (:insert-count (api/write! s {:table table :rows (vec batch)}))
                                    0)))
                         0
                         (partition-all batch-rows rows))}))

(def module
  {:name     :foreign-export
   :provides {:foreign/export-rows export-rows!}})
