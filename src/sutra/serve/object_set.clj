(ns sutra.serve.object-set
  (:require [sutra.catalog.lookup :as lookup]
            [sutra.kit.refusal :as refusal]
            [sutra.objects.reads :as reads]))

(set! *warn-on-reflection* true)

(def ^:private member-cap
  10000)

(def ^:private comparisons
  {"eq" := "gt" :> "gte" :>= "lt" :< "lte" :<=
   "startsWith" :starts-with "containsAllTermsInOrder" :contains-ci})

(defn- where-of
  [{:strs [type field value] :as clause}]
  (let [attr (when (and (string? field) (seq field)) (keyword field))]
    (cond
      (and attr (comparisons type))
      {attr {(comparisons type) value}}

      (and attr (= "isNull" type))
      {attr {:nil? (boolean value)}}

      (and attr (= "in" type) (sequential? value) (seq value))
      {:or (mapv (fn [v] {attr {:= v}}) value)}

      (and (#{"and" "or"} type) (sequential? value) (seq value))
      {(keyword type) (mapv where-of value)}

      (and (= "not" type) (= "eq" (get value "type")) (string? (get value "field")))
      {(keyword (get value "field")) {:not= (get value "value")}}

      :else (refusal/refuse! :refusal.object-set/invalid-filter {::error "INVALID_FILTER"}
                             {:what clause}))))

(defn- both
  [a b]
  (if (and a b) {:and [a b]} (or a b)))

(defn- parse-set
  [{:strs [type objectType objectSet where] :as object-set}]
  (case type
    "base"   (if (string? objectType)
               {:object-type objectType}
               (refusal/refuse! :refusal.object-set/invalid-object-type
                                {::error "INVALID_OBJECT_TYPE"} {:what objectType}))
    "filter" (let [inner (parse-set objectSet)]
               (assoc inner :where (both (:where inner) (where-of where))))
    (refusal/refuse! :refusal.object-set/not-supported {::error "OBJECT_SET_NOT_SUPPORTED"}
                     {:what object-set})))

(defn parse
  [{:strs [objectSet propertySet]}]
  (cond-> (parse-set objectSet)
    (seq propertySet) (assoc :properties (mapv keyword propertySet))))

(defn error-of
  [e]
  (::error (ex-data e)))

(defn members
  [store schema rid where]
  (let [rows (reads/objects-matching store schema rid {:where where :limit member-cap} {})]
    (when (< (count rows) member-cap)
      (into #{} (map (comp str :id)) rows))))

(defn addressable?
  [schema rid]
  (or (not-any? #(= :id (:attribute-name %)) (lookup/attrs schema rid))
      (= :id (get-in schema [:object-types rid :primary-key]))))

(defn matching
  [store schema rid where ids]
  (reads/objects-matching store schema rid
                          {:where (both {:or (mapv (fn [i] {:id {:= i}}) ids)} where)}
                          {}))

(defn- object-update
  [nm properties {:keys [id values]}]
  {:type "object" :state "ADDED_OR_UPDATED"
   :object (assoc (if properties (select-keys values properties) values)
                  :__apiName nm :__primaryKey (str id))})

(defn- removal
  [nm id]
  {:type "object" :state "REMOVED" :object {:__apiName nm :__primaryKey id}})

(defn updates
  [{:keys [where properties]} nm known ids rows]
  (let [here   (into #{} (map (comp str :id)) rows)
        gone   (remove here (map str ids))
        left   (cond->> gone (and where known) (filter known))]
    {:updates (into (mapv #(object-update nm properties %) rows)
                    (map #(removal nm %))
                    left)
     :members (when known (reduce disj (into known here) gone))}))
