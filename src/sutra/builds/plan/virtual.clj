(ns sutra.builds.plan.virtual
  (:require [sutra.sources.api :as sources]
            [sutra.kit.refusal :as refusal]
            [sutra.catalog.values :as values]
            [sutra.catalog.lookup :as lookup]
            [sutra.types.virtual :as virtual]))

(set! *warn-on-reflection* true)

(defn spec-of
  [schema ds-ref]
  (let [ds (get-in schema [:datasets (or (lookup/rid-of schema :datasets ds-ref) ds-ref)])]
    (when (virtual/virtual? ds) ds)))

(defn- source-spec
  [schema ds]
  (or (get (values/sources->datasources schema) (virtual/source-name ds))
      (refusal/refuse! :refusal.builds/virtual-unknown-source {:source (virtual/source-name ds)})))

(defn- read-rows
  [s spec ds sc]
  (let [t   (virtual/table ds)
        key (or (virtual/declared-key ds) (sources/scan-key spec t))]
    (if key
      (mapv second (sources/entries s t (assoc sc :order [[:db/key :asc]]) key))
      (sources/scan s t sc))))

(defn rows
  ([connect schema ds] (rows connect schema ds nil))
  ([connect schema ds limit]
   (let [spec (source-spec schema ds)]
     (sources/with-source [s connect spec]
       (read-rows s spec ds (virtual/read-scan ds limit))))))

(defn- probe!
  [connect schema ds]
  (let [spec (source-spec schema ds)
        t    (virtual/table ds)
        c    (virtual/probe-column ds)]
    (sources/with-source [s connect spec]
      (cond
        (and c (sources/capable? spec :sql))
        (let [r (first (sources/read s
                                 (sources/probe-sql (:type spec) t (virtual/probe-sql-column ds))))]
          (virtual/probe-signature ds {:max (:m r) :count (:n r)}))

        (and (nil? c) (sources/capable? spec :token))
        (virtual/token-signature (sources/token s t))

        :else
        (virtual/signature-of-rows ds (read-rows s spec ds (cond-> {} c (assoc :project #{c}))))))))

(defonce ^:private probes (atom {}))

(defn signature
  ([connect schema ds] (signature connect schema ds false))
  ([connect schema ds now?]
   (when (virtual/detected? ds)
     (let [k   [(virtual/source-name ds) (virtual/table ds) (get-in ds [:virtual :detect])]
           now (System/currentTimeMillis)
           {:keys [at sig]} (get @probes k)]
       (if (or now? (virtual/probe-due? ds at now))
         (let [sig (try [:virtual (probe! connect schema ds)]
                        (catch Exception e
                          (swap! probes assoc k {:at now :sig nil})
                          (throw e)))]
           (swap! probes assoc k {:at now :sig sig})
           sig)
         sig)))))
