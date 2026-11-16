(ns sutra.catalog.constraints
  (:require [sutra.kit.refusal :as refusal]
            [sutra.catalog.values :as values]
            [sutra.catalog.lookup :as lookup]
            [sutra.types.expr :as expr]
            [sutra.types.value-types :as vt]
            [clojure.string :as str]
            [sutra.ref.params :as rp]))

(set! *warn-on-reflection* true)

(defn unknown-type
  [obj-type]
  (refusal/refusal :refusal.catalog/unknown-type {:type obj-type :unknown-type obj-type}))

(defn elsewhere-hint
  [branches]
  (when (seq branches)
    (str " — exists on branch " (str/join ", " branches) " — pass --branch " (first branches))))

(defn with-message
  [constraints {:keys [attribute constraint] :as viol} v]
  (let [nm (name attribute)
        c  (get constraints constraint)]
    (assoc viol :message
           (case constraint
             :required   (str nm " is required")
             :one-of     (str nm " = " (pr-str v) " is not one of " (pr-str (vec c)))
             :min-length (str nm " is shorter than " c " characters")
             :max-length (str nm " is longer than " c " characters")
             :regex      (str nm " = " (pr-str v) " does not match " (pr-str c))
             :format     (str nm " = " (pr-str v) " is not a valid " (name c))
             :min        (str nm " = " v " is below the minimum " c)
             :max        (str nm " = " v " is above the maximum " c)))))

(defn violations
  ([attribute-list values] (violations attribute-list values {}))
  ([attribute-list values {:keys [require-complete?] :or {require-complete? true}}]
   (let [vs (rp/violations attribute-list values require-complete?)]
     (if (empty? vs)
       vs
       (let [cs (into {} (map (juxt :attribute-name (comp :constraints :opts))) attribute-list)]
         (mapv #(with-message (cs (:attribute %)) % (get values (:attribute %))) vs))))))

(defn expectation-violations
  [checks values]
  (reduce
   (fn [acc {:keys [name expr inputs on-error] :or {on-error :fail}}]
     (let [inputs-present? (or (empty? inputs)
                               (every? #(contains? values %) inputs))
           holds? (when inputs-present?
                    (try (expr/pred values expr)
                         (catch Exception _ ::skip)))]
       (if (or (not inputs-present?) (= holds? ::skip) holds?)
         acc
         (conj acc {:check     name
                    :on-error  on-error
                    :message   (str name " failed on this row")}))))
   []
   checks))

(defn check!
  ([attribute-list ot values] (check! attribute-list ot values {}))
  ([attribute-list ot values opts]
   (let [vs (violations attribute-list values opts)]
     (when (seq vs)
       (refusal/refuse! :refusal.catalog/constraint-violation
                        {:type ::violation :object-type ot :violations vs}
                        {:type-name (name ot) :messages (str/join "; " (map :message vs))}))
     values)))

(defn row-validator
  [schema obj-type]
  (let [spec     (lookup/get-type schema :object obj-type)
        _        (when-not spec
                   (throw (unknown-type obj-type)))
        attrs-ok (values/attrs-checker schema obj-type)
        vals-ok  (values/vals-checker schema obj-type)
        con-list (vt/resolve-value-types (:attribute-list spec) (:value-types schema))
        checks   (seq (:checks spec))]
    (fn [values complete?]
      (let [{:keys [problem undeclared]} (attrs-ok values)]
        (when problem
          (let [[code params] (values/bad-attrs-refusal schema obj-type values)]
            (refusal/refuse! code {:type obj-type :undeclared undeclared} params))))
      (when-not (vals-ok values)
        (let [[code params] (values/bad-vals-refusal schema obj-type values)]
          (refusal/refuse! code
                           {:type obj-type :bad-values (values/bad-vals schema obj-type values)}
                           params)))
      (check! con-list obj-type values {:require-complete? complete?})
      (when checks
        (let [vs    (expectation-violations checks values)
              fails (filter #(= :fail (:on-error %)) vs)]
          (doseq [w (filter #(= :warn (:on-error %)) vs)]
            (println (str "[expect-warn] " (name obj-type) ": " (:message w))))
          (when (seq fails)
            (refusal/refuse! :refusal.catalog/expectation-failed
                             {:type :sutra.catalog.constraints/violation
                              :object-type obj-type :violations fails}
                             {:type-name (name obj-type)
                              :messages  (str/join "; " (map :message fails))})))))))
