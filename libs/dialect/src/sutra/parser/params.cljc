(ns sutra.parser.params
  (:require [sutra.kit.anomaly :as anomaly]
            [clojure.string :as str]))

(def ^:const param-types
  #{:string :number :boolean})

(def token-pattern
  #"\$\{\s*([A-Za-z_][A-Za-z0-9_.*+!?-]*)\s*\}")

(defn refs
  [body]
  (->> (re-seq token-pattern (str body))
       (map (comp keyword second))
       distinct
       vec))

(defn declared
  [t]
  (or (:parameters t) {}))

(defn unknown-refs
  [params body]
  (vec (remove #(contains? params %) (refs body))))

(defn valueless
  [params]
  (vec (sort (keep (fn [[k v]] (when-not (contains? v :default) k)) params))))

(defn type-error
  [type v]
  (case type
    :string  (when-not (or (string? v) (keyword? v) (number? v))
               "expected a string")
    :number  (when-not (or (number? v)
                           (and (string? v)
                                (re-matches #"[-+]?\d+(\.\d+)?([eE][-+]?\d+)?" (str/trim v))))
               "expected a number")
    :boolean (when-not (or (boolean? v)
                           (contains? #{"true" "false"} (str/lower-case (str v))))
               "expected true or false")
    (str "unknown parameter type " (pr-str type)
         " — one of " (pr-str (vec (sort param-types))))))

(defn- ->number [v]
  (if (number? v)
    v
    #?(:clj  (let [s (str/trim (str v))]
               (if (re-matches #"[-+]?\d+" s) (parse-long s) (parse-double s)))
       :cljs (js/parseFloat (str/trim (str v))))))

(defn- ->boolean [v]
  (if (boolean? v) v (= "true" (str/lower-case (str v)))))

(defn sql-literal
  [type v]
  (if (nil? v)
    "NULL"
    (if-let [err (type-error type v)]
      (anomaly/coded! :refusal.dialect/parameter-invalid :incorrect
        "parameter value {value:pr} is not valid: {error}"
        {:type type :value v} {:error err})
      (case type
        :string  (str "'" (str/replace (if (keyword? v) (name v) (str v)) "'" "''") "'")
        :number  (str (->number v))
        :boolean (if (->boolean v) "TRUE" "FALSE")))))

(defn with-overrides
  [t overrides]
  (if (empty? overrides)
    t
    (update t :parameters
            (fn [ps]
              (reduce-kv (fn [m k v]
                           (cond-> m (contains? m k) (assoc-in [k :default] v)))
                         (or ps {}) overrides)))))

(defn bind
  [transforms values]
  (if (empty? values)
    transforms
    (update-vals transforms #(cond-> % (some (declared %) (keys values)) (with-overrides values)))))

(defn undeclared
  [transforms values]
  (let [known (into #{} (mapcat (comp keys declared)) (vals transforms))]
    (vec (sort (remove known (keys values))))))

(defn substitute
  [params body]
  (if (or (empty? params) (str/blank? (str body)))
    body
    (let [unknown (unknown-refs params body)
          used    (refs body)
          missing (vec (sort (filter #(and (contains? params %)
                                           (not (contains? (get params %) :default)))
                                     used)))]
      (when (seq unknown)
        (anomaly/coded! :refusal.dialect/undeclared-parameters :incorrect
          "transform body references undeclared parameter(s) {names} — declare them under :parameters"
          {:unknown unknown}
          {:names (str/join ", " (map #(str "${" (name %) "}") unknown))}))
      (when (seq missing)
        (anomaly/coded! :refusal.dialect/parameters-without-value :incorrect
          "parameter(s) {names} have no value — a parameter without one is a placeholder and cannot be built"
          {:missing missing} {:names (str/join ", " (map name missing))}))
      (str/replace (str body) token-pattern
                   (fn [m]
                     (let [k (keyword (second m))
                           {:keys [type default]} (get params k)]
                       (sql-literal type default)))))))

(defn substitute-transform
  [t]
  (let [ps (declared t)]
    (cond-> t
      (and (seq ps) (seq (str (:query t))))
      (assoc :query (substitute ps (:query t))))))

(defn errors
  [t-name t]
  (let [ps (declared t)]
    (vec
     (concat
      (for [[k {:keys [type default]}] ps
            :let [msg (cond
                        (not (contains? param-types type))
                        (str "unknown type " (pr-str type) " — one of "
                             (pr-str (vec (sort param-types))))
                        (and (contains? (get ps k) :default) (some? default))
                        (type-error type default))]
            :when msg]
        (str "transform " (name t-name) " parameter " (name k) ": " msg))
      (for [ref (unknown-refs ps (:query t))]
        (str "transform " (name t-name) " references undeclared parameter ${"
             (name ref) "}"))))))
