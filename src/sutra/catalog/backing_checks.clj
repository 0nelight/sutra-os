(ns sutra.catalog.backing-checks
  (:require [sutra.kit.refusal :as refusal]
            [clojure.string :as str]
            [sutra.types.languages :as langs]
            [sutra.types.kinds :as kinds]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]
            [sutra.catalog.outputs :as outputs]))

(set! *warn-on-reflection* true)

(defn- require-object-backing-transform!
  [type-kw schema]
  (let [match (outputs/primary-output-for schema :object-type type-kw)]
    (when-not match
      (refusal/refuse! :refusal.catalog/object-type-unfed {:object-type type-kw}))
    (let [[_ t output] match
          {:keys [primary-key column-map]} output
          {:keys [source input language inputs query code external-systems]} t
          {:keys [table]} source
          src-q (:query source)
          compute? (langs/compute-language? language)
          legacy-ok?
          (and (= :datasource (:kind input))
               (keyword? (:datasource input))
               (or (seq table) (seq src-q)))
          dataset-ok?
          (and (= :dataset (:kind input))
               (some? (:dataset input)))
          compute-ok?
          (and compute?
               (if (seq external-systems)
                 (or (nil? inputs) (map? inputs))
                 (and (map? inputs) (seq inputs)))
               (seq (if (= :query (langs/body-field language)) query code)))]
      (when-not (and (or legacy-ok? dataset-ok? compute-ok?)
                     primary-key
                     (or (nil? column-map) (map? column-map)))
        (refusal/refuse! :refusal.catalog/object-output-incomplete
                         {:object-type type-kw :transform t :output output}
                         {:languages (set langs/compute-language-names)})))))

(defn- require-output-keys!
  [type-kw t output ks style]
  (when-not (every? #(keyword? (get output %)) ks)
    (refusal/refuse! :refusal.catalog/link-output-incomplete
                     {:link-type type-kw :transform t :output output}
                     {:missing (str/join " / " (map str ks)) :style style})))

(defn unbacked-link
  [type-kw]
  (refusal/message :refusal.catalog/link-unbacked {:link-type type-kw}))

(defn- require-link-backing-transform!
  [type-kw schema]
  (let [match (outputs/find-output-for schema :link-type type-kw)]
    (when-not match
      (refusal/refuse! :refusal.catalog/link-unbacked {:link-type type-kw}))
    (let [[_ t output] match
          in-kind (get-in t [:input :kind])]
      (cond
        (= :polymorphic (:strategy t))
        (when-not (and (vector? (:sources t)) (seq (:sources t))
                       (every? (fn [s] (and (let [ft (:from-type s)]
                                              (or (keyword? ft) (rid/rid? ft)))
                                            (keyword? (:from-key-column s))
                                            (keyword? (:to-key-column s))))
                               (:sources t)))
          (refusal/refuse! :refusal.catalog/polymorphic-source-incomplete
                           {:link-type type-kw :transform t}))

        (= :resolve-by-shared-property (:strategy t))
        (require-output-keys! type-kw t output
                              [:from-key-column :match-column :shared-property]
                              "resolve-by-shared-property")

        (or (langs/compute-language? (:language t))
            (kinds/source-rows? in-kind))
        (require-output-keys! type-kw t output
                              [:from-key-column :to-key-column] "join-table")

        (= :object-type in-kind)
        (do
          (require-output-keys! type-kw t output [:fk-attribute] "endpoint-fk")
          (doseq [[k v] {:on-null       (:on-null output)
                         :on-unresolved (:on-unresolved output)
                         :on-duplicate  (:on-duplicate output)}
                  :when (some? v)]
            (when-not (= :skip v)
              (refusal/refuse! :refusal.catalog/fk-link-on-missing
                               {:link-type type-kw :key k :value v}))))

        :else
        (refusal/refuse! :refusal.catalog/link-input-kind-unknown
                         {:link-type type-kw :transform t} {:kind in-kind})))))

(defn validate-external-sources!
  [schema]
  (let [schema (sch/indexed schema)]
    (doseq [[k _spec] (:object-types schema)
            :when (outputs/find-output-for schema :object-type k)]
      (require-object-backing-transform! k schema))
    (doseq [[k _spec] (:link-types schema)]
      (require-link-backing-transform! k schema))))
