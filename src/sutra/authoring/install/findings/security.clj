(ns sutra.authoring.install.findings.security
  (:require [clojure.string :as str]
            [sutra.authoring.install.findings :as fnd]
            [sutra.kit.coerce :refer [->name-kw]]
            [sutra.types.attribute :as attribute]
            [sutra.types.granular :as granular]
            [sutra.types.transform-io :as tio]))

(set! *warn-on-reflection* true)

(defn- property-names
  [spec]
  (into #{} (keep #(if (keyword? %) % (attribute/attr-name %))) (:attributes spec)))

(defn- column-names
  [spec]
  (when (seq (:columns spec)) (into #{} (keep :attribute-name) (:columns spec))))

(defn- object-findings
  [[k spec]]
  (when-let [security (not-empty (:security spec))]
    (for [p (granular/security-problems security (property-names spec)
                                        (some-> (:primary-key spec) ->name-kw))]
      {:rule :security-policy :entity-class :object-types :entity k ::fnd/ctx p})))

(defn- restricted-view?
  [spec]
  (= :restricted-view (some-> (:kind spec) ->name-kw)))

(defn- view-findings
  [datasets [k spec]]
  (when (restricted-view? spec)
    (let [x       (some-> (get-in spec [:restricted-view :dataset]) ->name-kw)
          backing (get datasets x)]
      (if (or (nil? backing) (restricted-view? backing))
        [{:rule :restricted-view-backing :entity-class :datasets :entity k
          ::fnd/ctx {:dataset x :view? (some? backing)}}]
        (for [p (granular/policy-problems (get-in spec [:restricted-view :policy])
                                          (column-names backing) :view)]
          {:rule :security-policy :entity-class :datasets :entity k ::fnd/ctx p})))))

(defn- view-inputs
  [datasets [t spec]]
  (for [in (tio/inputs-of spec)
        :let [x (some-> (when (map? in) (:dataset in)) ->name-kw)]
        :when (and x (restricted-view? (get datasets x)))]
    {:rule :restricted-view-as-input :entity-class :transforms :entity t
     ::fnd/ctx {:dataset x}}))

(def ^:private problem-words
  {:malformed            "is not a policy (`true`, `[:and …]`, `[:or …]`, `[:not p]` or `[op a b]`)"
   :unknown-op           "uses an operator granular policies do not have"
   :bad-term             "compares a term that is neither `[:user k]`, `[:property k]`, `[:column k]` nor a literal"
   :unknown-field        "names a field the row does not have — a missing value fails every comparison, so the policy would hide every row"
   :ordering-not-supported "orders (<, <=, >, >=) inside an object or property security policy, which is not allowed"
   :constant-comparison  "compares two literals, which decides the same for every row and every user"
   :negated-membership   "negates a group, marking or organization membership — a scoped token lacks the attribute, the negation passes and grants more than meant"
   :too-many-comparisons "has more comparisons than a policy may carry"
   :too-heavy            "outweighs the comparison budget (constant 1, collection 1000, marking 3000; under 10000)"
   :property-policy-without-object-policy "has property policies but no object `:policy` beside them (`:policy true` admits every object)"
   :property-policy-empty "guards properties with neither a `:policy` nor `:markings`"
   :property-policy-guards-nothing "names no properties"
   :primary-key-guarded  "guards the primary key, which no property policy may"
   :unknown-property     "guards a property the type does not declare"
   :property-guarded-twice "guards one property in two property policies; a property belongs to at most one"
   :mandatory-control-invalid "names a mandatory control property the type does not declare, its primary key, or a kind other than :markings or :organizations"})

(defn- detail
  [{:keys [problem at op field property policies weight max count] :as p}]
  (str/join ", "
            (remove nil?
                    [(when (contains? p :policy) (str "policy " (name (:policy p))))
                     (when at (str "at " (pr-str at)))
                     (when op (str "op " op))
                     (when field (str "field " field))
                     (when property (str "property " property))
                     (when policies (str "policies " (pr-str policies)))
                     (when weight (str "weight " weight " of " max))
                     (when (and count (= :too-many-comparisons problem))
                       (str count " of " max))])))

(def security
  {:find (fn [{:keys [text]}]
           (let [datasets (into {} (map (fn [[k v]] [(->name-kw k) v])) (:datasets text))]
             (concat (mapcat object-findings (:objects text))
                     (mapcat (partial view-findings datasets) (:datasets text))
                     (mapcat (partial view-inputs datasets) (:transforms text)))))
   :self #{:security-policy}
   :says
   {:security-policy
    (fn [{nm :entity p ::fnd/ctx}]
      (let [d (detail p)]
        (str "The security policy of " (name nm) " "
             (get problem-words (:problem p) (name (:problem p)))
             (when (seq d) (str " (" d ")")) ".")))

    :restricted-view-backing
    (fn [{nm :entity {:keys [dataset view?]} ::fnd/ctx}]
      (str "Restricted view " (name nm) " reads " (some-> dataset name) ", which "
           (if view? "is itself a restricted view" "is no declared dataset")
           ". A restricted view stands on the dataset whose rows it filters."))

    :restricted-view-as-input
    (fn [{nm :entity {:keys [dataset]} ::fnd/ctx}]
      (str "Transform " (name nm) " reads restricted view " (name dataset) ". A restricted"
           " view filters reads per user and cannot feed a build;"
           " read its backing dataset, and mark the output."))}})
