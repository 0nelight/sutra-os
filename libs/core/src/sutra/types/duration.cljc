(ns sutra.types.duration
  (:require [clojure.string :as str]
            [sutra.kit.refusal :as refusal]))

(def ^:private unit-ms
  {"ms" 1 "s" 1000 "m" 60000 "min" 60000 "h" 3600000 "d" 86400000
   "MILLISECONDS" 1 "SECONDS" 1000 "MINUTES" 60000 "HOURS" 3600000 "DAYS" 86400000
   "WEEKS" 604800000})

(defn- bad [v]
  (refusal/refuse! :refusal.types/not-a-duration {:duration v}))

(defn ->ms
  [v]
  (cond
    (nil? v) nil
    (integer? v) (if (neg? v) (bad v) (long v))
    (map? v) (let [u (some-> (or (:unit v) (get v "unit")) name str/upper-case)
                   n (or (:value v) (get v "value"))
                   f (get unit-ms u)]
               (if (and f (integer? n) (not (neg? n))) (* (long n) (long f)) (bad v)))
    (string? v) (let [s (str/trim v)]
                  (if-let [[_ n u] (re-matches #"(\d+)\s*([a-zA-Z]*)" s)]
                    (let [f (if (str/blank? u) 1 (or (get unit-ms (str/lower-case u)) (bad v)))]
                      (* #?(:clj (Long/parseLong n) :cljs (js/parseInt n 10)) (long f)))
                    (bad v)))
    :else (bad v)))

(defn valid?
  [v]
  (try (->ms v) true (catch #?(:clj Exception :cljs :default) _ false)))

(defn render
  [ms]
  (when ms
    (let [ms (long ms)]
      (cond
        (zero? ms) "0s"
        (zero? (mod ms 3600000)) (str (quot ms 3600000) "h")
        (zero? (mod ms 60000)) (str (quot ms 60000) "m")
        (zero? (mod ms 1000)) (str (quot ms 1000) "s")
        :else (str ms "ms")))))
