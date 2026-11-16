(ns sutra.builds.plan.unit-tests
  (:require [sutra.kit.noted :as noted]
            [sutra.kit.refusal :as refusal]
            [clojure.string :as str]
            [sutra.builds.plan.inputs :as inputs]
            [sutra.builds.import.materialize :as mat]
            [sutra.parser.params :as params]
            [sutra.store.access.datasets :as dsets]
            [sutra.catalog.outputs :as outputs]
            [sutra.catalog.lookup :as lookup]
            [sutra.types.expectations :as xp]
            [sutra.types.languages :as langs]
            [sutra.types.kinds :as kinds]
            [sutra.types.naming :as naming]
            [sutra.types.unit-tests :as ut]))

(set! *warn-on-reflection* true)

(defn- value-type [v]
  (cond (boolean? v) :boolean (integer? v) :long (number? v) :decimal :else :string))

(defn- typed
  [t v]
  (let [t (if (vector? t) (first t) t)]
    (cond
      (not (number? v))                      v
      (= :decimal t)                         (bigdec v)
      (#{:double :float} t)                  (double v)
      (and (#{:long :integer :short :byte} t) (== v (long v))) (long v)
      :else                                  v)))

(defn- declared-columns
  [schema in]
  (case (kinds/physical (keyword (:kind in)))
    :landed (:columns (mat/landing schema (:object-type in) 0 ""))
    :dataset-table
    (get-in schema [:datasets (lookup/rid-of schema :datasets (:dataset in)) :columns])
    :transform-table
    (let [t (get-in schema [:transforms (outputs/transform-input-key schema in)])]
      (for [c (:output-columns t) :when (sequential? c)]
        {:attribute-name (first c) :attribute-type (second c)}))
    nil))

(defn- scratch-input
  [schema in rows]
  (let [decl     (vec (declared-columns schema in))
        by-col   (into {} (map (fn [c] [(xp/col-key (:attribute-name c)) (:attribute-name c)]))
                       decl)
        extra    (for [[k v] (apply merge-with #(or %1 %2) {} rows)
                       :when (not (contains? by-col (xp/col-key k)))]
                   {:attribute-name (keyword (name k)) :attribute-type (value-type v)})
        cols     (into decl (distinct) extra)
        key-of   (fn [k] (get by-col (xp/col-key k) (keyword (name k))))
        type-of  (into {} (map (juxt :attribute-name :attribute-type)) cols)]
    [cols (mapv (fn [r] (-> (into {} (map (fn [[k v]] (let [c (key-of k)]
                                                        [(xp/col-key c) (typed (type-of c) v)])))
                                  r)
                            (assoc naming/pk-col (str (random-uuid)))))
                rows)]))

(defn- scratch-name [alias]
  (str mat/landing-prefix "ut_" (str/replace (name alias) #"[^a-zA-Z0-9]" "_") "_"
       (Long/toString (quot (System/currentTimeMillis) 1000) 36) "_"
       (subs (str (random-uuid)) 0 8)))

(defn- run-sql
  [store schema t rows-of]
  (let [names (volatile! [])]
    (try
      (let [tables (into {}
                         (for [[alias in] (:inputs t)
                               :let [nm (scratch-name alias)
                                     [cols rows] (scratch-input schema in (rows-of alias))]]
                           (do (vswap! names conj nm)
                               (dsets/write-scratch-rows! store nm cols rows)
                               [(name alias) {:table nm}])))]
        (mat/run-query! store (keyword (:language t)) tables
                        (:query (params/substitute-transform t))))
      (finally
        (noted/quietly "unit test scratch" (dsets/drop-scratch! store @names))))))

(defn- received
  [store schema t-kw t {:keys [inputs output]}]
  (let [by-alias (into {} (map (fn [[a rs]] [(xp/col-key a) rs])) inputs)
        rows-of  #(get by-alias (xp/col-key %) [])]
    (cond
      (langs/query-language? (keyword (:language t))) (run-sql store schema t rows-of)
      (langs/code-language? (:language t))
      (inputs/run-code-on-inputs! store (inputs/code-env store) schema t-kw rows-of output)
      :else (refusal/refuse! :refusal.builds/unit-test-language
                             {:transform t-kw} {:language (:language t)}))))

(defn run-test
  [store schema t-kw t u]
  (merge {:name (:name u)}
         (try (ut/compare-rows (:expect u) (received store schema t-kw t u))
              (catch InterruptedException e (throw e))
              (catch Exception e
                {:passed? false :detail (str "the transform failed: " (ex-message e))
                 :missing [] :unexpected []}))))

(declare run-tests)

(defn of-transforms
  [store schema transform-refs]
  (into [] (keep (fn [r] (when (seq (:tests (second (inputs/transform-entry schema r))))
                           (run-tests store schema r))))
        (distinct transform-refs)))

(defn run-tests
  [store schema transform-ref]
  (let [[t-kw t] (inputs/transform-entry schema transform-ref)]
    (when-not t
      (refusal/refuse! :refusal.builds/unit-test-unknown-transform {:transform transform-ref}))
    (let [rs (mapv #(run-test store schema t-kw t %) (:tests t))]
      {:transform (name (or (:transform-name t) transform-ref))
       :tests     rs
       :passed    (count (filter :passed? rs))
       :failed    (count (remove :passed? rs))})))
