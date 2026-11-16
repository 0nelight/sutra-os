(ns sutra.catalog.values
  (:require [sutra.types.value-types :as vt]
            [clojure.string :as str]
            [sutra.catalog.lookup :as lookup]))

(set! *warn-on-reflection* true)

(defn- temporal-val?
  [v]
  (or (inst? v) (string? v) (instance? java.time.temporal.Temporal v)))

(def ^:private class-checks
  {:text    string?
   :number  number?
   :boolean #(or (true? %) (false? %))
   :keyword #(or (keyword? %) (string? %))
   :uuid    #(or (uuid? %) (string? %))
   :temporal temporal-val?
   :geo     string?
   :media   string?
   :series  string?})

(defn sources->datasources
  [schema]
  (into {} (for [[_rid s] (:sources schema)]
             [(:source-name s) s])))

(defn public-source
  [s]
  (cond-> (dissoc s :secret-cipher :secrets :password)
    (seq (:secret-cipher s)) (assoc :sealed? true)))

(defn public-datasources
  [schema]
  (update-vals (sources->datasources schema) public-source))

(defn attrs-checker
  [schema tn]
  (let [declared (some->> (get-in schema [:object-types tn])
                          :attribute-list
                          (into #{} (map :attribute-name)))]
    (fn [vals]
      (let [undeclared (vec (sort-by str (remove (or declared #{}) (keys vals))))]
        {:declared   declared
         :undeclared undeclared
         :problem    (cond (nil? declared)  :no-such-object-type
                           (seq undeclared) :undeclared-attributes)}))))

(defn attrs-check
  [schema tn vals]
  ((attrs-checker schema tn) vals))

(defn bad-attrs-refusal
  [schema tn vals]
  (let [nm (or (:object-type-name (get-in schema [:object-types tn])) tn)
        {:keys [declared undeclared problem]} (attrs-check schema tn vals)
        listed (if (seq undeclared)
                 (str/join ", " undeclared)
                 "(none — the row was rejected empty-handed)")]
    (if (= :no-such-object-type problem)
      [:refusal.catalog/bad-attributes-no-type {:name nm :listed listed}]
      [:refusal.catalog/bad-attributes
       {:name nm :listed listed :declared (str/join ", " (sort-by str declared))}])))

(def ^:private scalar-check
  (memoize (fn [et] (class-checks (vt/class-of et)))))

(defn value-ok?
  [et v]
  (if (vector? et)
    (case (first et)
      :array  (and (sequential? v) (every? #(value-ok? (second et) %) v))
      :struct (and (map? v)
                   (every? (fn [[k fv]]
                             (if-let [ft (get (second et) k)]
                               (or (nil? fv) (value-ok? ft fv))
                               true))
                           v))
      :decimal (number? v)
      :vector  (and (sequential? v) (every? number? v))
      true)
    (if-let [p (scalar-check et)] (p v) true)))

(defn vals-checker
  [schema tn]
  (let [tm (lookup/attr-type-map schema tn)]
    (fn [vals]
      (when tm
        (every? (fn [[k v]] (if-let [et (tm k)] (or (nil? v) (value-ok? et v)) true))
                vals)))))

(defn bad-vals
  [schema tn vals]
  (let [tm (lookup/attr-type-map schema tn)]
    (vec (for [[k v] vals
               :let  [et (get tm k)]
               :when (and et (some? v) (not (value-ok? et v)))]
           {:attribute k :declared et :value v}))))

(defn bad-vals-refusal
  [schema tn vals]
  (let [nm  (or (:object-type-name (get-in schema [:object-types tn])) tn)
        bad (bad-vals schema tn vals)
        one (fn [{:keys [attribute declared value]}]
              (str attribute " is declared " (pr-str declared) " but got "
                   (.getSimpleName (class value)) " "
                   (let [s (pr-str value)]
                     (if (> (count s) 120) (str (subs s 0 120) "…") s))))]
    [:refusal.catalog/bad-value-types
     {:name   nm
      :listed (if (seq bad)
                (str/join "; " (map one bad))
                "(none — the row was rejected empty-handed)")}]))
