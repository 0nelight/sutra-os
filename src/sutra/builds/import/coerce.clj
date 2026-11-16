(ns sutra.builds.import.coerce
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [sutra.builds.plan :as plan]
            [sutra.kit.time :as time]
            [sutra.types.decimal :as d]
            [sutra.types.value-types :as vt]))

(set! *warn-on-reflection* true)

(defn- coerce-number
  [v]
  (cond
    (nil? v) nil
    (number? v) v
    (string? v) (or (parse-long v) (parse-double v) v)
    :else v))

(defn- coerce-exact
  [v]
  (cond
    (nil? v)    nil
    (number? v) v
    (string? v) (or (d/exact v) v)
    :else       v))

(defn- coerce-string [v] (when (some? v) (str v)))

(defn- coerce-boolean [v]
  (cond (boolean? v) v
        (number? v) (not (zero? v))
        (string? v) (contains? #{"1" "t" "true" "yes" "y"}
                               (str/lower-case v))
        :else (boolean v)))

(def ^:private ts-fmt
  (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss"))

(defn- coerce-temporal
  [date-only?]
  (fn [v]
    (when (some? v)
      (let [t (time/->utc-local v)]
        (cond
          (nil? t)   v
          date-only? (str (.toLocalDate t))
          :else      (.format t ts-fmt))))))

(def ^:private coercions
  {:exact    coerce-exact
   :number   coerce-number
   :boolean  coerce-boolean
   :instant  (coerce-temporal false)
   :date     (coerce-temporal true)
   :string   coerce-string})

(defn- coerce-structured
  [v]
  (if (and (string? v) (re-find #"^\s*[\[{]" v))
    (try (json/read-str v :key-fn keyword) (catch Exception _ v))
    v))

(defn- coercion-fn
  [to]
  (if (plan/structured-type? to)
    coerce-structured
    (coercions (vt/coerce-of (plan/value-type-head to)))))

(def ^:private coercion-for
  (memoize (fn [to] (or (coercion-fn to) identity))))

(defn- apply-transform [row {:keys [op column to value]}]
  (case op
    :cast (cond-> row (contains? row column) (update column (coercion-for to)))
    :default (update row column (fn [v] (if (nil? v) value v)))
    :rename (-> row (assoc to (get row column)) (dissoc column))
    row))

(defn apply-transforms [row transforms]
  (reduce apply-transform row transforms))
