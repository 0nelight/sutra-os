(ns sutra.calx.fn.date
  (:require [clojure.string :as str]
            [sutra.calx.value :as v :refer [error?]]
            [sutra.calx.fn.util :as u]
            [sutra.calx.fn.daycount :as dc])
  (:import (java.time.temporal IsoFields)))

(set! *warn-on-reflection* true)

(defn- scalar
  [[lo hi] coercers f & {:keys [lift local?] :or {lift :all}}]
  (let [impl (fn [f args]
               (let [xs (mapv (fn [coerce a] (coerce a)) coercers (concat args (repeat v/omitted)))]
                 (or (some #(when (error? %) %) xs)
                     (u/result (apply f xs)))))]
    (cond-> {:arity [lo hi]
             :lift  lift
             :impl  (fn [& args] (impl f args))}
      local? (assoc :ctx? true :impl (fn [ctx & args] (impl (partial f (:locale ctx)) args))))))

(defn- int-arg [default] #(u/int* % default))
(def ^:private date-arg dc/serial)

(defn- text-only
  [x]
  (let [x (if (v/array? x) (ffirst x) x)]
    (if (string? x) x v/ERR-VALUE)))

(defn- day-of-week
  [serial]
  (mod (+ serial 6) 7))

(defn- date* [y m d]
  (if (or (neg? y) (>= y 10000))
    v/ERR-NUM
    (let [y (if (< y 1900) (+ y 1900) y)]
      (u/check [first-of-month (dc/->serial y m 1)]
        (let [s (+ first-of-month (dec d))]
          (if (<= 0 s dc/max-serial) s v/ERR-NUM))))))

(defn- time* [h m s]
  (let [total (+ (* 3600 h) (* 60 m) s)]
    (if (or (> h 32767) (neg? total))
      v/ERR-NUM
      (/ (double (mod total 86400)) 86400.0))))

(defn- datevalue [loc s]
  (if-let [n (dc/parse-date-time loc s)] (Math/floor n) v/ERR-VALUE))

(defn- timevalue [loc s]
  (if-let [n (dc/parse-date-time loc s)] (- n (Math/floor n)) v/ERR-VALUE))

(defn- component [i] (fn [serial] (nth (dc/ymd serial) i)))

(defn- time-arg
  [x]
  (u/check [n (dc/to-serial-number x)]
    (if (and (>= n 0.0) (< n (inc dc/max-serial))) n v/ERR-NUM)))

(defn- time-component
  [unit]
  (fn [n]
    (let [secs (mod (Math/round (* (- n (Math/floor n)) 86400.0)) 86400)]
      (case unit
        :hour   (quot secs 3600)
        :minute (mod (quot secs 60) 60)
        :second (mod secs 60)))))

(def ^:private weekday-types
  {1 [0 1], 2 [1 1], 3 [1 0], 11 [1 1], 12 [2 1], 13 [3 1], 14 [4 1], 15 [5 1], 16 [6 1], 17 [0 1]})

(defn- weekday [serial type]
  (if-let [[start base] (weekday-types type)]
    (+ base (mod (- (day-of-week serial) start) 7))
    v/ERR-NUM))

(defn- isoweeknum [serial]
  (.get (v/serial->local-date serial) IsoFields/WEEK_OF_WEEK_BASED_YEAR))

(def ^:private weeknum-starts
  {1 0, 2 1, 11 1, 12 2, 13 3, 14 4, 15 5, 16 6, 17 0})

(defn- weeknum [serial type]
  (if (= type 21)
    (isoweeknum serial)
    (if-let [start (weeknum-starts type)]
      (let [jan1 (dc/->serial (first (dc/ymd serial)) 1 1)
            offset (mod (- (day-of-week jan1) start) 7)]
        (inc (quot (+ (- serial jan1) offset) 7)))
      v/ERR-NUM)))

(defn- datedif [start end unit]
  (if (> start end)
    v/ERR-NUM
    (let [[y1 m1 d1] (dc/ymd start)
          [y2 m2 d2] (dc/ymd end)
          months (- (+ (* 12 (- y2 y1)) (- m2 m1)) (if (< d2 d1) 1 0))]
      (case (str/upper-case unit)
        "Y"  (quot months 12)
        "M"  months
        "D"  (- end start)
        "MD" (if (>= d2 d1)
               (- d2 d1)
               (u/check [anchor (dc/->serial y2 (dec m2) d1)] (- end anchor)))
        "YM" (mod months 12)
        "YD" (u/check [anniversary (dc/->serial y2 m1 d1)
                       anniversary
                       (if (> anniversary end) (dc/->serial (dec y2) m1 d1) anniversary)]
               (- end anniversary))
        v/ERR-NUM))))

(defn- yearfrac [start end basis]
  (if (dc/valid-basis? basis) (dc/yearfrac start end basis) v/ERR-NUM))

(defn- weekend-days
  [x]
  (let [x (if (v/array? x) (ffirst x) x)]
    (cond
      (v/omitted? x) #{5 6}
      (string? x) (if (and (re-matches #"[01]{7}" x) (not= x "1111111"))
                    (set (keep-indexed (fn [i c] (when (= c \1) i)) x))
                    v/ERR-VALUE)
      :else (u/check [code (u/int* x 1)]
              (cond (<= 1 code 7)   #{(mod (+ code 4) 7) (mod (+ code 5) 7)}
                    (<= 11 code 17) #{(mod (- code 12) 7)}
                    :else v/ERR-NUM)))))

(defn- holiday-set
  [x]
  (if (v/omitted? x)
    #{}
    (reduce (fn [acc h]
              (if (nil? h)
                acc
                (let [s (dc/serial h)] (if (error? s) (reduced s) (conj acc s)))))
            #{} (u/flat x))))

(defn- workday-pred [weekend holidays]
  (fn [serial]
    (not (or (weekend (mod (+ serial 5) 7)) (holidays serial)))))

(defn- networkdays* [start end weekend holidays]
  (u/check [weekend (weekend-days weekend)
            holidays (holiday-set holidays)]
    (let [[lo hi sign] (if (<= start end) [start end 1] [end start -1])
          weekday? (workday-pred weekend #{})
          n (inc (- hi lo))
          full-weeks (quot n 7)
          tail (count (filter weekday? (range (+ lo (* 7 full-weeks)) (inc hi))))
          off (count (filter #(and (<= lo % hi) (weekday? %)) holidays))]
      (* sign (- (+ (* full-weeks (- 7 (count weekend))) tail) off)))))

(defn- workday* [start days weekend holidays]
  (u/check [weekend (weekend-days weekend)
            holidays (holiday-set holidays)]
    (cond
      (zero? days) start
      (> (abs days) dc/max-serial) v/ERR-NUM
      :else
      (let [step (if (pos? days) 1 -1)
            result (->> (iterate #(+ % step) (+ start step))
                        (take-while #(<= 0 % dc/max-serial))
                        (filter (workday-pred weekend holidays))
                        (drop (dec (abs days)))
                        first)]
        (or result v/ERR-NUM)))))

(def functions
  {"DATE"      (scalar [3 3] [(int-arg 0) (int-arg 0) (int-arg 0)] date*)
   "DATEVALUE" (scalar [1 1] [text-only] datevalue :local? true)
   "TIME"      (scalar [3 3] [(int-arg 0) (int-arg 0) (int-arg 0)] time*)
   "TIMEVALUE" (scalar [1 1] [text-only] timevalue :local? true)
   "TODAY"     {:arity [0 0] :ctx? true :volatile true
                :impl (fn [ctx] (Math/floor (double (:now ctx))))}
   "NOW"       {:arity [0 0] :ctx? true :volatile true :impl (fn [ctx] (double (:now ctx)))}

   "YEAR"   (scalar [1 1] [date-arg] (component 0))
   "MONTH"  (scalar [1 1] [date-arg] (component 1))
   "DAY"    (scalar [1 1] [date-arg] (component 2))
   "HOUR"   (scalar [1 1] [time-arg] (time-component :hour))
   "MINUTE" (scalar [1 1] [time-arg] (time-component :minute))
   "SECOND" (scalar [1 1] [time-arg] (time-component :second))

   "WEEKDAY"    (scalar [1 2] [date-arg (int-arg 1)] weekday)
   "WEEKNUM"    (scalar [1 2] [date-arg (int-arg 1)] weeknum)
   "ISOWEEKNUM" (scalar [1 1] [date-arg] isoweeknum)

   "EDATE"   (scalar [2 2] [date-arg (int-arg 0)] #(dc/add-months %1 %2))
   "EOMONTH" (scalar [2 2] [date-arg (int-arg 0)] #(dc/add-months %1 %2 true))

   "DATEDIF"  (scalar [3 3] [date-arg date-arg u/text] datedif)
   "DAYS"     (scalar [2 2] [date-arg date-arg] -)
   "DAYS360"  (scalar [2 3] [date-arg date-arg #(u/bool % false)] dc/days360)
   "YEARFRAC" (scalar [2 3] [date-arg date-arg (int-arg 0)] yearfrac)

   "NETWORKDAYS"      (scalar [2 3] [date-arg date-arg identity]
                              #(networkdays* %1 %2 v/omitted %3) :lift #{0 1})
   "NETWORKDAYS.INTL" (scalar [2 4] [date-arg date-arg identity identity]
                              networkdays* :lift #{0 1 2})
   "WORKDAY"          (scalar [2 3] [date-arg (int-arg 0) identity]
                              #(workday* %1 %2 v/omitted %3) :lift #{0 1})
   "WORKDAY.INTL"     (scalar [2 4] [date-arg (int-arg 0) identity identity]
                              workday* :lift #{0 1 2})})
