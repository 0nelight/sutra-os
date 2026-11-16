(ns sutra.store.values
  (:import (java.nio.charset StandardCharsets)
           (java.time Instant)
           (java.time.temporal ChronoUnit)))

(set! *warn-on-reflection* true)

(def ^:const max-ident-bytes 63)

(def ^:const max-key-bytes
  900)

(defn- text-problem [^String s]
  (let [n (.length s)]
    (loop [i 0]
      (when (< i n)
        (let [c (.charAt s i)]
          (cond
            (= c (char 0)) {:class :text/nul :at i}
            (Character/isHighSurrogate c)
            (if (and (< (inc i) n) (Character/isLowSurrogate (.charAt s (inc i))))
              (recur (+ i 2))
              {:class :text/surrogate :at i})
            (Character/isLowSurrogate c) {:class :text/surrogate :at i}
            :else (recur (inc i))))))))

(defn problem
  [v]
  (cond
    (nil? v) nil
    (string? v) (text-problem v)
    (or (instance? Double v) (instance? Float v))
    (when (or (Double/isNaN (double v)) (Double/isInfinite (double v))) {:class :double/non-finite})
    (ratio? v) {:class :number/ratio}
    (map? v) (some (fn [[k x]] (some-> (or (problem k) (problem x)) (update :path #(cons k %)))) v)
    (coll? v) (some (fn [[i x]] (some-> (problem x) (update :path #(cons i %))))
                    (map-indexed vector v))
    :else nil))

(defn ident-problem
  [nm]
  (let [s (if (keyword? nm) (name nm) (str nm))]
    (or (text-problem s)
        (when (> (alength (.getBytes s StandardCharsets/UTF_8)) max-ident-bytes)
          {:class :ident/too-long :bytes (alength (.getBytes s StandardCharsets/UTF_8))}))))

(defn key-problem
  [k]
  (if (string? k)
    (or (text-problem k)
        (let [n (alength (.getBytes ^String k StandardCharsets/UTF_8))]
          (when (> n max-key-bytes) {:class :key/too-long :bytes n})))
    (problem k)))

(def ^:private says
  {:text/nul         "text holds a NUL (U+0000)"
   :text/surrogate   "text holds a lone UTF-16 surrogate, which is no Unicode character"
   :double/non-finite "a double is NaN or infinite"
   :number/ratio     "a ratio has no exact decimal; give a decimal or a double"
   :ident/too-long   (str "a name is longer than " max-ident-bytes " bytes of UTF-8")
   :key/too-long     (str "a key is longer than " max-key-bytes
                          " bytes of UTF-8, more than every store can index")})

(defn explain
  [p]
  (str (says (:class p))
       (when-let [at (:at p)] (str " at character " at))
       (when (seq (:path p)) (str " in " (pr-str (vec (:path p)))))))

(defn dataset-value
  [v]
  (cond
    (instance? java.sql.Timestamp v)
    (let [t ^java.sql.Timestamp v
          n (.getNanos t)]
      (if (zero? (rem n 1000)) t
        (doto (java.sql.Timestamp. (.getTime t)) (.setNanos (* 1000 (quot n 1000))))))
    (instance? Instant v) (.truncatedTo ^Instant v ChronoUnit/MICROS)
    (ratio? v) (try (.decimalValue ^clojure.lang.Ratio v) (catch ArithmeticException _ v))
    :else v))
