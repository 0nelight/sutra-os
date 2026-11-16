(ns sutra.catalog.backing
  (:require [sutra.types.backing :as backing]
            [sutra.types.languages :as langs]
            [sutra.catalog.lookup :as lookup]
            [sutra.catalog.outputs :as outputs]
            [sutra.catalog.transforms :as transforms]))

(set! *warn-on-reflection* true)

(defn endpoint-fk-problem
  [schema ct-kw ct trans output]
  (let [shown (fn [x] (or (lookup/name-of schema nil x) x))
        {:keys [from-object-type to-object-type cardinality]} ct
        fk-side-type (get-in trans [:input :object-type])
        fk-attribute (:fk-attribute output)
        on-other-side? (or (= fk-side-type from-object-type)
                           (= fk-side-type to-object-type))
        ots (:object-types schema)
        fk-ot (get ots fk-side-type)]
    (cond
      (not (keyword? fk-attribute))
      "endpoint-fk output is missing :fk-attribute"
      (backing/endpoint-fk-output-problem output)
      (backing/endpoint-fk-output-problem output)
      (= :many-to-many cardinality)
      ":many-to-many cannot be expressed by a single FK column — use :join"
      (not (contains? ots from-object-type))
      (str "from-endpoint " (pr-str (shown from-object-type)) " is not a declared object-type")
      (not (contains? ots to-object-type))
      (str "to-endpoint " (pr-str (shown to-object-type)) " is not a declared object-type")
      (not on-other-side?)
      (str "endpoint-fk input " (pr-str (shown fk-side-type)) " is neither endpoint of the link")
      (let [fk-side (if (= fk-side-type from-object-type) :from :to)]
        (case cardinality
          :many-to-one (not= :from fk-side)
          :one-to-many (not= :to fk-side)
          false))
      (str "link " (pr-str (shown ct-kw)) " is " (pr-str cardinality) " but carries its FK on "
           (pr-str (shown fk-side-type))
           ", the ONE side — that realises the opposite cardinality. Move the FK"
           " attribute to the many side, or flip :cardinality.")
      (nil? fk-ot)
      (str "FK-side object-type " (pr-str (shown fk-side-type)) " not found")
      :else
      (let [attrs (set (map :attribute-name (:attribute-list fk-ot)))
            [_ _ fk-out] (outputs/primary-output-for schema :object-type fk-side-type)
            cmap (:column-map fk-out)]
        (cond
          (not (contains? attrs fk-attribute))
          (str ":fk-attribute " (pr-str fk-attribute)
               " is not a declared attribute on " (pr-str (shown fk-side-type)))
          (some #(= fk-attribute (first %)) (:derived (lookup/attrs-index schema fk-side-type)))
          (str "FK link " (pr-str (shown ct-kw)) " joins on " (pr-str (shown fk-side-type)) "."
               (name fk-attribute)
               ", which is :derived — computed at read time, it has no stored column, so"
               " the link would stay empty. Store the value through a supplement transform"
               " on "
               (pr-str (shown fk-side-type)) " (:role :supp) and drop its :derived.")
          (and (map? cmap) (not (contains? cmap fk-attribute)))
          (str ":fk-attribute " (pr-str fk-attribute)
               " is absent from " (pr-str (shown fk-side-type)) "'s producer :column-map")
          :else nil)))))

(defn link-backing
  [schema ct-kw]
  (when-let [[_ trans output] (outputs/find-output-for schema :link-type ct-kw)]
    (let [shared (or (:shared-property output) (:shared-property trans)
                     (get-in trans [:external-source :shared-property])
                     (get-in output [:external-source :shared-property]))
          kind   (cond
                   (= :polymorphic (:strategy trans))                :polymorphic
                   (= :resolve-by-shared-property (:strategy trans))  :resolve-by-shared-property
                   (langs/compute-language? (:language trans))        :join-table
                   (= :object-type (get-in trans [:input :kind]))     :fk
                   :else                                              :source-provided)]
      (cond-> {:kind kind :producer (:transform-name trans)}
        (= kind :resolve-by-shared-property)
        (assoc :shared-property shared)

        (= kind :join-table)
        (assoc :language        (:language trans)
               :from-key-column (:from-key-column output)
               :to-key-column   (:to-key-column output))

        (and (= kind :fk) (:fk-attribute output))
        (assoc :fk-attribute (:fk-attribute output))))))

(defn- backing-facts
  [[t-kw t out]]
  {:producer     t-kw
   :dataset      (:dataset out)
   :incremental? (boolean (:incremental t))
   :snapshot?    (transforms/snapshot-write? t)
   :modify?      (transforms/modify-write? t)
   :external?    (boolean (some #{:datasource :source} (map :kind (vals (:inputs t)))))})

(def ^:private backing-fact-keys
  [:producer :dataset :incremental? :snapshot? :modify? :external?])

(defn- dataset-backing
  [schema {:keys [dataset primary-key column-map]}]
  (let [prods (outputs/dataset-producers schema dataset)
        ts    (keep (:transforms schema) prods)]
    {:producer     (when (= 1 (count prods)) (first prods))
     :dataset      dataset
     :primary-key  primary-key
     :column-map   column-map
     :role         :primary
     :authored?    true
     :incremental? false
     :snapshot?    true
     :modify?      false
     :external?    (boolean (some (fn [t] (some #{:datasource :source}
                                                (map :kind (vals (:inputs t)))))
                                  ts))}))

(defn object-backing
  [schema ot-kw]
  (if-let [authored (get-in schema [:object-types ot-kw :backing])]
    (dataset-backing schema authored)
    (when-let [[_ _ out :as trip] (outputs/primary-output-for schema :object-type ot-kw)]
      (assoc (backing-facts trip)
             :primary-key (:primary-key out)
             :column-map  (:column-map out)
             :role        (:role out :primary)))))

(defn authored-backing-dataset
  [schema ot-kw]
  (get-in schema [:object-types ot-kw :backing :dataset]))

(defn types-backed-by
  [schema ds]
  (into #{} (keep (fn [[ot _]] (when (= ds (authored-backing-dataset schema ot)) ot)))
        (:object-types schema)))

(defn backing-of
  [schema kind target-kw]
  (if (= :object-type kind)
    (some-> (object-backing schema target-kw) (select-keys backing-fact-keys))
    (some-> (outputs/primary-output-for schema kind target-kw) backing-facts)))

(defn identity-rule
  [obj-spec primary-key]
  (if (:resolve-entity obj-spec)
    [:entity-resolution (vec (:match-keys obj-spec))]
    [:primary-key primary-key]))

(defn identity-rule-determinate?
  [[kind v]]
  (or (= kind :entity-resolution) (some? v)))
