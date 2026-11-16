(ns blueprint-cljs.date-utils
  "Date math, formatting and locale names for the datetime components — Blueprint's
   `common/dateUtils.ts`, `timeUnit.ts`, `dateRangeSelectionStrategy.ts` and the date-fns
   formats it uses, on plain js/Date. A range is `[start end]`, either may be nil."
  (:refer-clojure :exclude [clone])
  (:require [clojure.string :as str]))

(def default-locale "en-US")

;; ------------------------------------------------------------------------ basics

(defn clone [^js d] (js/Date. (.getTime d)))

(defn valid?
  [d]
  (and (instance? js/Date d) (not (js/isNaN (.getTime d)))))

(def invalid-date (js/Date. js/NaN))

(defn date
  ([y m] (js/Date. y m 1))
  ([y m d] (js/Date. y m d)))

(defn start-of-day [^js d] (js/Date. (.getFullYear d) (.getMonth d) (.getDate d)))

(defn month-start [^js d] (js/Date. (.getFullYear d) (.getMonth d) 1))

(defn month-end [^js d] (js/Date. (.getFullYear d) (inc (.getMonth d)) 0))

(defn add-days [^js d n] (js/Date. (.getFullYear d) (.getMonth d) (+ (.getDate d) n)))

(defn add-months
  "First of the month `n` months after `d`'s month."
  [^js d n]
  (js/Date. (.getFullYear d) (+ (.getMonth d) n) 1))

(defn same-day?
  [^js a ^js b]
  (and (some? a) (some? b)
       (= (.getFullYear a) (.getFullYear b))
       (= (.getMonth a) (.getMonth b))
       (= (.getDate a) (.getDate b))))

(defn same-month?
  [^js a ^js b]
  (and (some? a) (some? b)
       (= (.getFullYear a) (.getFullYear b))
       (= (.getMonth a) (.getMonth b))))

(defn same-time?
  [^js a ^js b]
  (and (some? a) (some? b)
       (= (.getHours a) (.getHours b))
       (= (.getMinutes a) (.getMinutes b))
       (= (.getSeconds a) (.getSeconds b))
       (= (.getMilliseconds a) (.getMilliseconds b))))

(defn month-diff
  "Calendar months from `a` to `b` (date-fns differenceInCalendarMonths b a)."
  [^js a ^js b]
  (+ (* 12 (- (.getFullYear b) (.getFullYear a))) (- (.getMonth b) (.getMonth a))))

(defn day-key [^js d] (.getTime (start-of-day d)))

(defn day-in-range?
  "Blueprint `isDayInRange`: inclusive by day; `exclusive?` leaves out both ends."
  ([d range] (day-in-range? d range false))
  ([d [s e] exclusive?]
   (if (or (nil? d) (nil? s) (nil? e))
     false
     (let [k (day-key d)]
       (and (<= (day-key s) k (day-key e))
            (or (not exclusive?) (and (not (same-day? s d)) (not (same-day? d e)))))))))

(defn day-range-in-range?
  [[s e] outer]
  (and (or (nil? s) (day-in-range? s outer))
       (or (nil? e) (day-in-range? e outer))))

(defn month-in-range?
  [d [s e]]
  (and (some? d) (some? s) (some? e)
       (<= (.getTime (month-start s)) (.getTime (month-start d)) (.getTime (month-start e)))))

(defn ranges-equal?
  [[s1 e1] [s2 e2]]
  (and (or (and (nil? s1) (nil? s2)) (and s1 s2 (same-day? s1 s2)))
       (or (and (nil? e1) (nil? e2)) (and e1 e2 (same-day? e1 e2)))))

(defn date-between
  [[^js s ^js e]]
  (js/Date. (+ (.getTime s) (* 0.5 (- (.getTime e) (.getTime s))))))

(defn date-time
  "Blueprint `getDateTime`: the day of `d` at the time of `t` (midnight without `t`)."
  [^js d ^js t]
  (cond
    (nil? d) nil
    (nil? t) (js/Date. (.getFullYear d) (.getMonth d) (.getDate d) 0 0 0 0)
    :else    (js/Date. (.getFullYear d) (.getMonth d) (.getDate d)
                       (.getHours t) (.getMinutes t) (.getSeconds t) (.getMilliseconds t))))

(defn today? [d] (same-day? d (js/Date.)))

(defn month-changed?
  [^js a ^js b]
  (or (not= (nil? a) (nil? b)) (not= (some-> b .getMonth) (some-> a .getMonth))))

(defn time-changed?
  [^js a ^js b]
  (or (not= (nil? a) (nil? b))
      (not= (some-> b .getHours) (some-> a .getHours))
      (not= (some-> b .getMinutes) (some-> a .getMinutes))
      (not= (some-> b .getSeconds) (some-> a .getSeconds))
      (not= (some-> b .getMilliseconds) (some-> a .getMilliseconds))))

(defn clamp-date
  [^js d ^js lo ^js hi]
  (cond
    (and lo (< (.getTime d) (.getTime lo))) lo
    (and hi (> (.getTime d) (.getTime hi))) hi
    :else d))

(defn default-max-date
  []
  (let [d (js/Date.)] (.setMonth d (+ (.getMonth d) 6)) d))

(defn default-min-date
  []
  (let [d (js/Date.)]
    (.setFullYear d (- (.getFullYear d) 20))
    (.setMonth d 0 1)
    d))

(defn today-at-midnight [] (start-of-day (js/Date.)))

;; ------------------------------------------------------------------------- time

(defn time-only
  [^js d]
  (js/Date. 0 0 0 (.getHours d) (.getMinutes d) (.getSeconds d) (.getMilliseconds d)))

(defn default-min-time [] (js/Date. 0 0 0 0 0 0 0))
(defn default-max-time [] (js/Date. 0 0 0 23 59 59 999))

(defn time-in-range?
  [d lo hi]
  (let [t  (.getTime (time-only d))
        lo (.getTime (time-only lo))
        hi (.getTime (time-only hi))]
    (if (<= hi lo)
      (or (>= t lo) (<= t hi))
      (and (>= t lo) (<= t hi)))))

(defn time-in-range
  "Blueprint `getTimeInRange`."
  [d lo hi]
  (cond
    (same-time? lo hi)            hi
    (time-in-range? d lo hi)      d
    (>= (.getTime (time-only d)) (.getTime (time-only hi))) hi
    :else                         lo))

(defn hour-12 [h] (let [x (mod h 12)] (if (zero? x) 12 x)))
(defn hour-24 [h pm?] (let [x (if (= h 12) 0 h)] (if pm? (+ x 12) x)))
(defn pm? [h] (>= h 12))
(defn meridiem-hour [h to-pm?] (if to-pm? (+ (mod h 12) 12) (mod h 12)))

(def time-units
  {:hour24 {:class "hour" :min 0 :max 23 :label "hours (24hr clock)"}
   :hour12 {:class "hour" :min 1 :max 12 :label "hours (12hr clock)"}
   :minute {:class "minute" :min 0 :max 59 :label "minutes"}
   :second {:class "second" :min 0 :max 59 :label "seconds"}
   :ms     {:class "millisecond" :min 0 :max 999 :label "milliseconds"}})

(defn unit-valid?
  [unit n]
  (let [{:keys [min max]} (time-units unit)]
    (and (number? n) (not (js/isNaN n)) (<= min n max))))

(defn wrap-unit
  [unit n]
  (let [{:keys [min max]} (time-units unit)]
    (cond (> n max) min (< n min) max :else n)))

(defn get-unit
  [unit ^js d]
  (case unit
    :hour24 (.getHours d)
    :hour12 (hour-12 (.getHours d))
    :minute (.getMinutes d)
    :second (.getSeconds d)
    :ms     (.getMilliseconds d)))

(defn set-unit!
  [unit n ^js d pm?]
  (case unit
    :hour24 (.setHours d n)
    :hour12 (.setHours d (hour-24 n pm?))
    :minute (.setMinutes d n)
    :second (.setSeconds d n)
    :ms     (.setMilliseconds d n))
  d)

(defn pad [n width] (.padStart (str n) width "0"))

;; ------------------------------------------------------------------ format/parse

(def default-formats
  {nil          "yyyy-MM-dd"
   :minute      "yyyy-MM-dd HH:mm"
   :second      "yyyy-MM-dd HH:mm:ss"
   :millisecond "yyyy-MM-dd HH:mm:ss.SSS"})

(defn precision
  "Blueprint's precision from `:time-precision`, else `:time-picker-props`, where a non-empty
   map without `:precision` means :minute."
  [time-precision time-picker-props]
  (some-> (or time-precision
              (:precision time-picker-props)
              (when (seq time-picker-props) :minute))
          keyword))

(defn default-format
  [time-precision time-picker-props]
  (get default-formats (precision time-precision time-picker-props)))

(def ^:private token-re #"yyyy|yy|MM|M|dd|d|HH|H|mm|m|ss|s|SSS|'[^']*'|.")

(defn- tokens [pattern] (re-seq token-re pattern))

(defn format-date
  "date-fns `format` for the numeric tokens yyyy yy MM M dd d HH H mm m ss s SSS and
   'quoted' text."
  [^js d pattern]
  (apply str
         (for [t (tokens pattern)]
           (case t
             "yyyy" (pad (.getFullYear d) 4)
             "yy"   (pad (mod (.getFullYear d) 100) 2)
             "MM"   (pad (inc (.getMonth d)) 2)
             "M"    (inc (.getMonth d))
             "dd"   (pad (.getDate d) 2)
             "d"    (.getDate d)
             "HH"   (pad (.getHours d) 2)
             "H"    (.getHours d)
             "mm"   (pad (.getMinutes d) 2)
             "m"    (.getMinutes d)
             "ss"   (pad (.getSeconds d) 2)
             "s"    (.getSeconds d)
             "SSS"  (pad (.getMilliseconds d) 3)
             (if (str/starts-with? t "'") (subs t 1 (dec (count t))) t)))))

(def ^:private token-digits
  {"yyyy" 4 "yy" 2 "MM" 2 "M" 2 "dd" 2 "d" 2 "HH" 2 "H" 2 "mm" 2 "m" 2 "ss" 2 "s" 2
   "SSS" 3})

(defn- escape-re [s] (str/replace s #"[.*+?^${}()|\[\]\\]" "\\$&"))

(defn parse-date
  "date-fns `parse` for the tokens of `format-date`: an invalid Date when `s` does not
   match `pattern` or names no real day."
  [s pattern]
  (let [ts (tokens pattern)
        re (re-pattern
            (str "^"
                 (apply str (for [t ts]
                              (if-let [n (token-digits t)]
                                (str "(\\d{1," n "})")
                                (escape-re (if (str/starts-with? t "'")
                                             (subs t 1 (dec (count t))) t)))))
                 "$"))
        m  (re-find re (str/trim (str s)))]
    (if-not m
      invalid-date
      (let [vs  (zipmap (filter token-digits ts) (map js/parseInt (rest m)))
            g   (fn [& ks] (some vs ks))
            y   (or (g "yyyy") (some-> (g "yy") (+ 2000)) (.getFullYear (js/Date.)))
            mo  (dec (or (g "MM" "M") 1))
            dd  (or (g "dd" "d") 1)
            h   (or (g "HH" "H") 0)
            mi  (or (g "mm" "m") 0)
            se  (or (g "ss" "s") 0)
            ms  (or (g "SSS") 0)
            res (js/Date. y mo dd h mi se ms)]
        (.setFullYear res y)
        (if (and (= (.getMonth res) mo) (= (.getDate res) dd) (< h 24) (< mi 60) (< se 60))
          res
          invalid-date)))))

;; ------------------------------------------------------------------ locale names

(defn- dtf [locale opts] (js/Intl.DateTimeFormat. (or locale default-locale) (clj->js opts)))

(defn month-name [d locale] (.format (dtf locale {:month "long"}) d))

(defn caption [d locale] (.format (dtf locale {:month "long" :year "numeric"}) d))

(defn weekday-long [d locale] (.format (dtf locale {:weekday "long"}) d))

(defn weekday-short
  "date-fns `EEEEEE`: two letters (Su Mo … / So Mo …)."
  [d locale]
  (subs (.format (dtf locale {:weekday "short"}) d) 0 2))

(defn day-label [d locale] (.format (dtf locale {:dateStyle "full"}) d))

(defn first-day-of-week
  "0 = Sunday … 6 = Saturday, from the locale's week info where the browser has it."
  [locale]
  (let [l    (try (js/Intl.Locale. (or locale default-locale)) (catch :default _ nil))
        info (when l
               (cond
                 (fn? (.-getWeekInfo l)) (.getWeekInfo l)
                 (some? (.-weekInfo l))  (.-weekInfo l)))
        fd   (some-> info .-firstDay)]
    (if fd (mod fd 7) 0)))

;; --------------------------------------------------------- range selection logic

(defn- other-boundary [b] (if (= b :start) :end :start))
(defn- boundary-date [b [s e]] (if (= b :start) s e))
(defn- range-for [b d other] (if (= b :start) [d other] [other d]))
(defn- overlapping? [b ^js d ^js other]
  (if (= b :start) (> (.getTime d) (.getTime other)) (< (.getTime d) (.getTime other))))

(defn- create-range
  [^js a ^js b single?]
  (cond
    (and (not single?) (same-day? a b)) [nil nil]
    (< (.getTime a) (.getTime b))        [a b]
    :else                                [b a]))

(defn- next-state-default
  [[s e] day single?]
  (cond
    (and (nil? s) (nil? e)) {:range [day nil] :boundary :start}
    (and s (nil? e))        {:range (create-range day s single?) :boundary :end}
    (and (nil? s) e)        {:range (create-range day e single?) :boundary :start}
    :else
    (let [start? (same-day? s day) end? (same-day? e day)]
      (cond
        (and start? end?) {:range [nil nil] :boundary :start}
        start?            {:range [nil e] :boundary :start}
        end?              {:range [s nil] :boundary :end}
        :else             {:range [day nil] :boundary :start}))))

(defn- next-state-for-boundary
  [rng day single? b]
  (let [bd (boundary-date b rng)
        ob (other-boundary b)
        od (boundary-date ob rng)]
    (cond
      (and (nil? bd) (nil? od))
      {:boundary b :range (range-for b day nil)}

      (and bd (nil? od))
      {:boundary b :range (range-for b (if (same-day? bd day) nil day) nil)}

      (and (nil? bd) od)
      (cond
        (same-day? day od)
        (if single?
          {:boundary b :range (range-for b od od)}
          {:boundary ob :range (range-for b nil nil)})
        (overlapping? b day od) {:boundary ob :range (range-for b od day)}
        :else                   {:boundary b :range (range-for b day od)})

      :else
      (cond
        (same-day? bd day)
        {:boundary b :range (range-for b nil (if (same-day? bd od) nil od))}
        (same-day? day od)
        (if single?
          {:boundary b :range (range-for b od od)}
          {:boundary ob :range (range-for b bd nil)})
        (overlapping? b day od) {:boundary b :range (range-for b day nil)}
        :else                   {:boundary b :range (range-for b day od)}))))

(defn next-range-state
  "Blueprint `DateRangeSelectionStrategy.getNextState` → {:range [s e] :boundary b}."
  [current-range day single? boundary]
  (if boundary
    (next-state-for-boundary current-range day single? (keyword boundary))
    (next-state-default current-range day single?)))
