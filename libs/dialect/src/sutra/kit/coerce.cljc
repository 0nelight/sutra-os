(ns sutra.kit.coerce
  (:require [clojure.string :as str]))

(defn ->kw
  [x]
  (cond
    (keyword? x) x
    (nil? x)     nil
    (symbol? x)  (keyword x)
    :else        (keyword (str x))))

(defn ->name-kw
  [x]
  (some-> x name keyword))

(defn drop-empty
  [m]
  (into {} (remove (fn [[_ v]] (or (nil? v) (and (coll? v) (empty? v))))) m))

(defn ->num
  [x]
  (cond
    (number? x) (double x)
    (and (string? x) (not (str/blank? x)))
    #?(:clj  (try (Double/parseDouble (str/trim x)) (catch NumberFormatException _ nil))
       :cljs (let [n (js/parseFloat (str/trim x))] (when-not (js/isNaN n) n)))
    :else nil))

(defn ->num0 [x]
  (or (->num x) 0.0))
