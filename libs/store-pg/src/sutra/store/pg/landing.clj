(ns sutra.store.pg.landing
  (:require [sutra.types.coltype :as ctype])
  (:import [java.math BigDecimal]
           [java.time LocalDate LocalDateTime]))

(set! *warn-on-reflection* true)

(def ^:private max-decimal-digits
  38)

(defn- decimal-shape
  [^BigDecimal v]
  [(max 1 (- (.precision v) (.scale v))) (max 0 (.scale v))])

(defn- widen-decimal
  [a b]
  (let [ints (max (- (:precision a 0) (:scale a 0))
                  (- (:precision b 0) (:scale b 0)))
        scale (max (:scale a 0) (:scale b 0))]
    (ctype/col-type :decimal {:precision (+ ints scale) :scale scale})))

(defn storable
  [t]
  (if (= :decimal (:kind t))
    (let [ints (- (:precision t 0) (:scale t 0))]
      (ctype/col-type :decimal {:precision max-decimal-digits
                             :scale (min (:scale t 0)
                                         (max 0 (- max-decimal-digits ints)))}))
    t))

(defn narrowed
  [types]
  (into []
        (keep (fn [[col t]]
                (let [s (storable t)]
                  (when (and (= :decimal (:kind t))
                             (< (:scale s 0) (:scale t 0)))
                    {:column (str col) :inferred t :stored s}))))
        types))

(defn type-of
  [v]
  (if (instance? BigDecimal v)
    (let [[ints scale] (decimal-shape v)]
      (ctype/col-type :decimal {:precision (+ ints scale) :scale scale}))
    (ctype/col-type
     (condp instance? v
       Boolean       :boolean
       Long          :bigint
       Integer       :bigint
       Short         :bigint
       Double        :double
       Float         :double
       java.sql.Date :date
       LocalDate     :date
       java.sql.Timestamp :timestamp
       LocalDateTime :timestamp
       :varchar))))

(def ^:private long-as-decimal
  (ctype/col-type :decimal {:precision 19 :scale 0}))

(defn- widen-numeric
  [a b]
  (let [ka (:kind a) kb (:kind b)
        numeric? #{:bigint :decimal :double}]
    (when (and (numeric? ka) (numeric? kb))
      (cond
        (= :double ka)                a
        (= :double kb)                b
        (= :decimal ka kb)            (widen-decimal a b)
        (= :decimal ka)               (widen-decimal a long-as-decimal)
        (= :decimal kb)               (widen-decimal long-as-decimal b)
        :else                         a))))

(defn- see-value
  [have v]
  (let [t (type-of v)]
    (cond
      (nil? have)              t
      (= (:kind have) (:kind t)) (if (= :decimal (:kind t)) (widen-decimal have t) have)
      :else                    (or (widen-numeric have t) have))))

(defn col-types
  [rows]
  (let [typed (persistent!
               (reduce (fn [m row]
                         (reduce-kv (fn [m k v]
                                      (assoc! m k (if (nil? v)
                                                    (get m k)
                                                    (see-value (get m k) v))))
                                    m row))
                       (transient {})
                       rows))]
    (reduce-kv (fn [m k t] (if t m (assoc m k (ctype/col-type :varchar))))
               typed typed)))

(defn dedupe-by-pk
  [pk lrows]
  (vec (vals (reduce (fn [m r] (assoc m (get r pk) r)) {} lrows))))

(defn shape-groups
  [pk lrows]
  (for [[shape rows] (group-by (comp set keys) lrows)]
    [(into [pk] (sort (disj shape pk))) rows]))

