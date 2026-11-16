(ns sutra.calx.value
  (:require [clojure.string :as str])
  (:import (java.math BigDecimal MathContext RoundingMode)))

(set! *warn-on-reflection* true)

(defrecord CellError [type])

(def error-strings
  {:null  "#NULL!"  :div0 "#DIV/0!" :value "#VALUE!" :ref   "#REF!"
   :name  "#NAME?"  :num  "#NUM!"   :na    "#N/A"    :spill "#SPILL!"
   :calc  "#CALC!"  :cycle "#CYCLE!" :error "#ERROR!"})

(def ^:private string->error-type
  (into {} (map (fn [[k v]] [v k])) error-strings))

(def ^:private error-instances
  (into {} (map (fn [k] [k (->CellError k)])) (keys error-strings)))

(defn err
  [type]
  (or (error-instances type) (->CellError type)))

(defn error? [x] (instance? CellError x))

(defn error-string
  ([e] (error-string nil e))
  ([loc ^CellError e]
   (or (get-in loc [:error-text (:type e)]) (get error-strings (:type e) "#ERROR!"))))

(defn parse-error
  ([s] (parse-error nil s))
  ([loc s]
   (let [up (str/upper-case s)]
     (some-> (or (get-in loc [:errors up]) (string->error-type up)) err))))

(defmethod print-method CellError [e ^java.io.Writer w]
  (.write w (str "#calx/error " (pr-str (:type e)))))

(def ERR-DIV0  (err :div0))
(def ERR-VALUE (err :value))
(def ERR-REF   (err :ref))
(def ERR-NAME  (err :name))
(def ERR-NUM   (err :num))
(def ERR-NA    (err :na))
(def ERR-NULL  (err :null))
(def ERR-SPILL (err :spill))
(def ERR-CALC  (err :calc))
(def ERR-CYCLE (err :cycle))

(defrecord Ref [sheet r1 c1 r2 c2])

(defn ref? [x] (instance? Ref x))
(defn single-cell? [^Ref r] (and (= (:r1 r) (:r2 r)) (= (:c1 r) (:c2 r))))

(defrecord Lambda [params body env])

(defn lambda? [x] (instance? Lambda x))

(def omitted ::omitted)
(defn omitted? [x] (identical? x omitted))

(defn array? [x] (vector? x))
(defn rows [a] (count a))
(defn cols [a] (count (first a)))

(defn ->array
  [x]
  (if (array? x) x [[x]]))

(defn array-values
  [a]
  (if (array? a) (apply concat a) (list a)))

(defn amap2
  [f a]
  (mapv (fn [row] (mapv f row)) a))

(defn transpose [a]
  (if (empty? a) a (apply mapv vector a)))

(defn make-array2
  [nr nc f]
  (mapv (fn [r] (mapv (fn [c] (f r c)) (range nc))) (range nr)))

(defn scalarize
  [x]
  (if (and (array? x) (= 1 (count x)) (= 1 (count (first x))))
    (ffirst x)
    x))

(defn normalize-number
  [x]
  (let [d (double x)]
    (cond (or (Double/isNaN d) (Double/isInfinite d)) ERR-NUM
          (== d 0.0) 0.0
          :else d)))

(def ^:private mc15 (MathContext. 15 RoundingMode/HALF_EVEN))

(defn format-number
  ([x]
   (let [d (double x)]
     (cond
       (== d 0.0) "0"
       (and (== d (Math/rint d)) (< (Math/abs d) 1e15)) (str (long d))
       :else
       (let [bd (.stripTrailingZeros (.round (BigDecimal. d) mc15))
             a  (Math/abs d)]
         (if (or (>= a 1e21) (< a 1e-9))
           (let [s (-> (String/format java.util.Locale/ROOT "%.14E" (object-array [d]))
                       (str/replace #"\.?0+E" "E")
                       (str/replace #"E\+?(-?)0*(\d)" "E$1$2"))]
             (if (str/includes? s "E-") s (str/replace s "E" "E+")))
           (.toPlainString bd))))))
  ([loc x]
   (let [s (format-number x) dec (:decimal loc \.)]
     (if (= dec \.) s (str/replace s "." (str dec))))))

(def ^:private number-re
  #"^\s*([+-]?)(\d+(?:\.\d*)?|\.\d+)(?:[eE]([+-]?\d+))?\s*(%?)\s*$")

(defn- en-us-digits
  [loc s]
  (let [dec (:decimal loc \.)
        grp (java.util.regex.Pattern/quote (str (:group loc \,)))
        s   (str/replace s (re-pattern (str "(?<=\\d)" grp "(?=\\d{3}(\\D|$))")) "")]
    (cond
      (= dec \.)            s
      (str/includes? s ".") nil
      :else                 (str/replace s (str dec) "."))))

(defn parse-number
  ([s] (parse-number nil s))
  ([loc s]
   (when (string? s)
     (when-let [s (en-us-digits loc s)]
       (when-let [[_ sign digits exp pct] (re-matches number-re s)]
         (let [n (Double/parseDouble (str sign digits (when exp (str "e" exp))))]
           (if (seq pct) (/ n 100.0) n)))))))

(def ^:private epoch-day-of-1899-12-30 (.toEpochDay (java.time.LocalDate/of 1899 12 30)))

(defn date->serial
  [y m d]
  (let [first-of-month (-> (java.time.LocalDate/of (int y) 1 1) (.plusMonths (long (dec m))))
        n (- (.toEpochDay first-of-month) epoch-day-of-1899-12-30)
        base (if (< n 61) (dec n) n)]
    (double (+ base (dec d)))))

(defn serial->local-date
  ^java.time.LocalDate [serial]
  (let [n (long (Math/floor (double serial)))
        n (if (< n 61) (inc n) n)]
    (java.time.LocalDate/ofEpochDay (+ epoch-day-of-1899-12-30 n))))

(defn serial->ymd [serial]
  (let [n (long (Math/floor (double serial)))]
    (if (= n 60)
      [1900 2 29]
      (let [d (serial->local-date serial)]
        [(.getYear d) (.getMonthValue d) (.getDayOfMonth d)]))))

(defn time->fraction [h m s]
  (/ (+ (* 3600.0 h) (* 60.0 m) s) 86400.0))

(def ^:private iso-date-re
  #"^\s*(\d{4})-(\d{1,2})-(\d{1,2})(?:[ T](\d{1,2}):(\d{2})(?::(\d{2}(?:\.\d+)?))?)?\s*$")
(def ^:private dmy-date-re
  #"^\s*(\d{1,2})\.(\d{1,2})\.(\d{4}|\d{2})(?:\s+(\d{1,2}):(\d{2})(?::(\d{2}(?:[.,]\d+)?))?)?\s*$")
(def ^:private slash-date-re #"^\s*(\d{1,2})/(\d{1,2})/(\d{4})\s*$")
(def ^:private time-re #"^\s*(\d{1,2}):(\d{2})(?::(\d{2}(?:\.\d+)?))?\s*([aApP][mM])?\s*$")

(defn- valid-ymd? [y m d]
  (and (<= 1 m 12) (<= 1 d (.lengthOfMonth (java.time.YearMonth/of (int y) (int m))))))

(defn- full-year
  [^String y]
  (let [n (parse-long y)] (cond (> (count y) 2) n (< n 30) (+ 2000 n) :else (+ 1900 n))))

(defn- date-time
  [y mo d h mi sec]
  (let [y (full-year y) mo (parse-long mo) d (parse-long d)]
    (when (and (valid-ymd? y mo d) (or (nil? h) (and (< (parse-long h) 24) (< (parse-long mi) 60))))
      (+ (date->serial y mo d)
         (if h
           (time->fraction (parse-long h) (parse-long mi)
                           (if sec (parse-double (str/replace sec "," ".")) 0))
           0)))))

(defn parse-date-time
  ([s] (parse-date-time nil s))
  ([loc s]
   (when (string? s)
     (or
      (when-let [[_ y mo d h mi sec] (re-matches iso-date-re s)] (date-time y mo d h mi sec))
      (when-let [[_ a b y] (re-matches slash-date-re s)]
        (let [[d mo] (if (= :dmy (:date-order loc)) [a b] [b a])] (date-time y mo d nil nil nil)))
      (when-let [[_ d mo y h mi sec] (re-matches dmy-date-re s)] (date-time y mo d h mi sec))
      (when-let [[_ h mi sec ampm] (re-matches time-re s)]
        (let [h (parse-long h) mi (parse-long mi)
              h (cond (nil? ampm) h
                      (= "a" (str/lower-case (subs ampm 0 1))) (mod h 12)
                      :else (+ 12 (mod h 12)))]
          (when (and (< h 24) (< mi 60))
            (time->fraction h mi (if sec (parse-double sec) 0)))))))))

(defn serial->iso
  [serial]
  (let [s     (double serial)
        day   (Math/floor s)
        secs  (Math/round (* (- s day) 86400.0))
        [day secs] (if (= secs 86400) [(inc day) 0] [day secs])
        clock (format "%02d:%02d:%02d" (quot secs 3600) (mod (quot secs 60) 60) (mod secs 60))]
    (if (zero? day)
      clock
      (let [[y m d] (serial->ymd day)
            date (format "%04d-%02d-%02d" y m d)]
        (if (zero? secs) date (str date " " clock))))))

(def ^:private boolean-words {"TRUE" true "FALSE" false})

(defn boolean-text
  ([b] (boolean-text nil b))
  ([loc b] (get-in loc [:boolean-text (boolean b)] (if b "TRUE" "FALSE"))))

(defn text->boolean
  ([s] (text->boolean nil s))
  ([loc s]
   (let [up (str/upper-case (str/trim s))]
     (get (:booleans loc) up (get boolean-words up)))))

(defn to-number
  ([v] (to-number nil v))
  ([loc v]
   (cond
     (number? v) (double v)
     (nil? v) 0.0
     (boolean? v) (if v 1.0 0.0)
     (string? v) (if (str/blank? v)
                   ERR-VALUE
                   (or (parse-number loc v) (parse-date-time loc v) ERR-VALUE))
     (error? v) v
     :else ERR-VALUE)))

(defn to-text
  ([v] (to-text nil v))
  ([loc v]
   (cond
     (string? v) v
     (nil? v) ""
     (number? v) (format-number loc v)
     (boolean? v) (boolean-text loc v)
     (error? v) v
     :else ERR-VALUE)))

(defn to-bool
  ([v] (to-bool nil v))
  ([loc v]
   (cond
     (boolean? v) v
     (nil? v) false
     (number? v) (not (zero? (double v)))
     (string? v) (let [b (text->boolean loc v)] (if (nil? b) ERR-VALUE b))
     (error? v) v
     :else ERR-VALUE)))

(defn display
  ([v] (display nil v))
  ([loc v]
   (cond
     (error? v) (error-string loc v)
     (array? v) (display loc (ffirst v))
     (lambda? v) (error-string loc ERR-CALC)
     :else (to-text loc v))))

(defn- type-rank [v]
  (cond (number? v) 0 (string? v) 1 (boolean? v) 2 :else 3))

(defn compare-values
  [a b]
  (let [a (if (nil? a) (cond (string? b) "" (boolean? b) false :else 0.0) a)
        b (if (nil? b) (cond (string? a) "" (boolean? a) false :else 0.0) b)
        ra (type-rank a) rb (type-rank b)]
    (if (not= ra rb)
      (compare ra rb)
      (case (int ra)
        0 (compare (double a) (double b))
        1 (compare (str/lower-case a) (str/lower-case b))
        2 (compare a b)
        0))))

(defn values-equal? [a b] (zero? (compare-values a b)))

(defn parse-input
  ([input] (parse-input input {}))
  ([input {:keys [locale parse-dates?] :or {parse-dates? true}}]
   (cond
     (not (string? input)) {:value (cond (number? input) (normalize-number input)
                                         (or (nil? input) (boolean? input) (error? input)) input
                                         :else (str input))}
     (and (str/starts-with? input "=") (> (count input) 1)) {:formula input}
     (str/starts-with? input "'") {:value (subs input 1)}
     (= "" input) {:value nil}
     :else
     (let [b (text->boolean locale input)]
       (cond
         (some? b) {:value b}
         (parse-error locale (str/trim input)) {:value (parse-error locale (str/trim input))}
         :else (if-let [n (parse-number locale input)]
                 {:value n}
                 (if-let [d (and parse-dates? (parse-date-time locale input))]
                   {:value d :date? true}
                   {:value input})))))))
