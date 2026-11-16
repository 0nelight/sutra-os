(ns sutra.store.semantics
  (:require [sutra.ref.expr :as rexpr]))

(set! *warn-on-reflection* true)

(def semantics
  {:classes  [:nil :bool :num :text :inst :coll :other]
   :equality {:num   :numeric
              :inst  :millisecond
              :cross :as-text
              :nil   :never-equal
              :coll  :element-wise}
   :order    {:nil   :last-asc
              :nan   :above-numbers
              :text  :codepoint
              :cross :by-class
              :ties  :by-key}
   :numeric  {:decimal      :exact
              :avg          :sum/count
              :int-overflow :refuse}
   :compute  {:nil          :propagates
              :zero-divisor :nil
              :uncast       {:quotient :double}
              :cast         {:arithmetic :exact
                             :casts      #{:integer :double :decimal}
                             :rounding   :half-away-from-zero}}
   :array    {:contains :any-element}})

(defn canon
  [x]
  (cond (uuid? x) (str x) (keyword? x) (str (symbol x)) :else x))

(defn class-of [x]
  (let [x (canon x)]
    (cond (nil? x) :nil (boolean? x) :bool (number? x) :num (string? x) :text
          (inst? x) :inst (coll? x) :coll :else :other)))

(def ^:private class-rank (zipmap (:classes semantics) (range)))

(defn- code-points ^ints [^String s] (.toArray (.codePoints s)))

(defn compare-values
  [a b]
  (let [a (canon a) b (canon b) ca (class-of a) cb (class-of b)]
    (cond
      (not= ca cb)  (compare (class-rank ca) (class-rank cb))
      (= :nil ca)   0
      (= :text ca)  (java.util.Arrays/compare (code-points a) (code-points b))
      (= :inst ca)  (compare (inst-ms a) (inst-ms b))
      (= :num
         ca)   (let [na (and (float? a) (Double/isNaN (double a)))
                     nb (and (float? b) (Double/isNaN (double b)))]
                      (cond (and na nb) 0 na 1 nb -1 :else (compare a b)))
      (= :coll ca)  (loop [xs (seq a) ys (seq b)]
                      (cond (and (empty? xs) (empty? ys)) 0
                            (empty? xs) -1
                            (empty? ys) 1
                            :else (let [c (compare-values (first xs) (first ys))]
                                    (if (zero? c) (recur (rest xs) (rest ys)) c))))
      :else         (compare a b))))

(defn compare-in
  [dir a b]
  (let [c (cond (and (nil? a) (nil? b)) 0 (nil? a) 1 (nil? b) -1 :else (compare-values a b))]
    (if (= :desc dir) (- c) c)))

(defn millisecond
  [x]
  (cond (instance? java.time.Instant
                   x)  (.truncatedTo ^java.time.Instant x java.time.temporal.ChronoUnit/MILLIS)
        (instance? java.sql.Timestamp x) (if (zero? (rem (.getNanos ^java.sql.Timestamp x) 1000000))
                                           x
                                           (java.sql.Timestamp. (.getTime ^java.sql.Timestamp x)))
        :else                            x))

(defn group-key
  [x]
  (cond
    (inst? x) (java.time.Instant/ofEpochMilli (inst-ms x))
    (and (number? x)
         (not (and (float? x) (or (Double/isNaN (double x)) (Double/isInfinite (double x))))))
    (let [d (.stripTrailingZeros (bigdec x))]
      (if (<= (.scale d) 0)
        (let [i (.toBigIntegerExact d)] (if (< (.bitLength i) 64) (long i) (bigint i)))
        (if (decimal? x) d x)))
    (sequential? x) (mapv group-key x)
    :else x))

(defn equal?
  [a b]
  (let [a (canon a) b (canon b)]
    (cond (or (nil? a) (nil? b)) false
          (and (coll? a) (coll? b)) (and (= (count a) (count b)) (every? true? (map equal? a b)))
          :else (rexpr/same-value? a b))))

(defn same-data?
  [a b]
  (cond (and (number? a) (number? b)) (== a b)
        (and (inst? a) (inst? b)) (= (inst-ms a) (inst-ms b))
        (and (map? a) (map? b)) (let [a (update-keys a group-key) b (update-keys b group-key)]
                                  (and (= (set (keys a)) (set (keys b)))
                                       (every? #(same-data? (get a %) (get b %)) (keys a))))
        (and (sequential? a)
             (sequential? b)) (and (= (count a) (count b)) (every? true? (map same-data? a b)))
        :else (= a b)))

(defn check-overrides!
  [overrides]
  (when (seq overrides)
    (throw (ex-info (str "a store engine may not override the semantics: " (pr-str overrides))
                    {:semantics-overrides overrides})))
  overrides)
