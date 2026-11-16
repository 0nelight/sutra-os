(ns sutra.calx.fn.daycount
  (:require [clojure.string :as str]
            [sutra.calx.value :as v]
            [sutra.calx.fn.util :as u])
  (:import (java.time DateTimeException YearMonth)))

(set! *warn-on-reflection* true)

(def max-serial
  2958465)

(defn leap-year? [y] (.isLeap (java.time.Year/of (int y))))

(defn days-in-month [y m] (.lengthOfMonth (YearMonth/of (int y) (int m))))

(defn ymd
  [serial]
  (if (zero? serial) [1900 1 0] (v/serial->ymd serial)))

(defn ->serial
  [y m d]
  (try
    (let [s (long (v/date->serial y m d))]
      (if (<= 0 s max-serial) s v/ERR-NUM))
    (catch DateTimeException _ v/ERR-NUM)
    (catch ArithmeticException _ v/ERR-NUM)))

(defn add-months
  ([serial n] (add-months serial n false))
  ([serial n eom?]
   (let [[y m d] (ymd serial)
         idx (+ (* 12 y) (dec m) n)
         y' (quot idx 12) m' (inc (mod idx 12))]
     (if (or (< y' 1899) (> y' 9999))
       v/ERR-NUM
       (let [dim (days-in-month y' m')]
         (->serial y' m' (if eom? dim (min d dim))))))))

(defn last-day-of-month? [serial]
  (let [[y m d] (ymd serial)] (= d (days-in-month y m))))

(defn- last-of-feb? [y m d] (and (= m 2) (>= d (days-in-month y 2))))

(def ^:private month-numbers
  (zipmap ["jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec"] (range 1 13)))

(defn- month-number [loc s]
  (let [s (str/lower-case s)
        named (fn [names] (some (fn [[i n]] (when (= s (str/lower-case n)) (inc i)))
                                (map-indexed vector names)))]
    (or (named (:month-names loc))
        (named (:month-abbr loc))
        (when (>= (count s) 3) (month-numbers (subs s 0 3))))))

(defn- valid-date [y m d]
  (when (and m (<= 1 m 12) (<= 1 d (days-in-month y m)))
    (double (v/date->serial y m d))))

(def ^:private date-patterns
  [[#"(?i)(\d{1,2})\.?[-\s/]+(\p{L}{3,})\.?[-\s/,]+(\d{4})"
    (fn [loc d m y] [y (month-number loc m) d])]
   [#"(?i)(\p{L}{3,})\.?\s+(\d{1,2}),?\s+(\d{4})"
    (fn [loc m d y] [y (month-number loc m) d])]
   [#"(\d{4})/(\d{1,2})/(\d{1,2})"
    (fn [_ y m d] [y m d])]])

(defn- parse-date-only [loc s]
  (or (when-let [n (v/parse-date-time loc s)] (when (== n (Math/floor n)) n))
      (some (fn [[re f]]
              (when-let [[_ & groups] (re-matches re (str/trim s))]
                (let [[y m d] (f loc (nth groups 0) (nth groups 1) (nth groups 2))]
                  (valid-date (parse-long y) (if (string? m) (parse-long m) m) (parse-long d)))))
            date-patterns)))

(defn parse-date-time
  ([s] (parse-date-time nil s))
  ([loc s]
   (when (string? s)
     (or (v/parse-date-time loc s)
         (parse-date-only loc s)
         (when-let [[_ d t] (re-matches #"(?s)\s*(.*?\S)\s+(\d{1,2}:\d{2}.*)" s)]
           (let [dv (parse-date-only loc d)
                 tv (v/parse-date-time loc t)]
             (when (and dv tv (< tv 1.0)) (+ dv tv))))))))

(defn to-serial-number
  ([x] (to-serial-number x 0.0))
  ([x default]
   (let [x (if (v/array? x) (ffirst x) x)]
     (if (string? x)
       (or (v/parse-number x) (parse-date-time x) v/ERR-VALUE)
       (u/num x default)))))

(defn serial
  ([x] (serial x 0.0))
  ([x default]
   (u/check [n (to-serial-number x default)]
     (if (and (>= n 0.0) (< n (inc max-serial)))
       (long (Math/floor n))
       v/ERR-NUM))))

(defn days360
  [start end european?]
  (let [[y1 m1 d1] (ymd start)
        [y2 m2 d2] (ymd end)
        d1 (if (or (= d1 31) (and (not european?) (last-of-feb? y1 m1 d1))) 30 d1)
        d2 (if (and (= d2 31) (or european? (= d1 30))) 30 d2)]
    (+ (* 360 (- y2 y1)) (* 30 (- m2 m1)) (- d2 d1))))

(defn- days-30-360-us
  [start end]
  (let [[y1 m1 d1] (ymd start)
        [y2 m2 d2] (ymd end)
        feb1 (last-of-feb? y1 m1 d1)
        feb2 (last-of-feb? y2 m2 d2)
        [d1 d2] (cond (and (= d1 31) (= d2 31)) [30 30]
                      (= d1 31) [30 d2]
                      (and (= d1 30) (= d2 31)) [30 30]
                      (and feb1 feb2) [30 30]
                      feb1 [30 d2]
                      :else [d1 d2])]
    (+ (* 360 (- y2 y1)) (* 30 (- m2 m1)) (- d2 d1))))

(defn- actual-actual
  [start end]
  (let [[y1 m1 d1] (ymd start)
        [y2 m2 d2] (ymd end)
        days (double (- end start))]
    (if (or (= y1 y2)
            (and (= (inc y1) y2) (or (> m1 m2) (and (= m1 m2) (>= d1 d2)))))
      (let [feb29? (some (fn [y] (and (leap-year? y)
                                      (let [mar1 (->serial y 3 1)]
                                        (and (< start mar1) (>= end mar1)))))
                         [y1 y2])]
        (/ days
           (if (or (and (= y1 y2) (leap-year? y1)) feb29? (and (= m2 2) (= d2 29))) 366.0 365.0)))
      (let [years (inc (- y2 y1))
            year-days (- (->serial (inc y2) 1 1) (->serial y1 1 1))]
        (/ days (/ year-days (double years)))))))

(defn valid-basis? [b] (<= 0 b 4))

(defn day-count
  [start end basis]
  (case (int basis)
    0 (days-30-360-us start end)
    4 (days360 start end true)
    (- end start)))

(defn yearfrac
  [start end basis]
  (let [[s e] (if (> start end) [end start] [start end])]
    (case (int basis)
      0 (/ (days-30-360-us s e) 360.0)
      1 (actual-actual s e)
      2 (/ (- e s) 360.0)
      3 (/ (- e s) 365.0)
      4 (/ (days360 s e true) 360.0))))
