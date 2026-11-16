(ns sutra.calx.format
  (:require [clojure.string :as str]
            [sutra.calx.value :as v])
  (:import (java.math BigDecimal MathContext RoundingMode)))

(set! *warn-on-reflection* true)

(def ^:private condition-re #"(<=|>=|<>|<|>|=)\s*([+-]?(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?)")

(def color-names
  {"BLACK" :black "BLUE" :blue "CYAN" :cyan "GREEN" :green "MAGENTA" :magenta "RED" :red
   "WHITE" :white "YELLOW" :yellow})

(def ^:private basic-colors [:black :white :red :green :blue :yellow :magenta :cyan])

(defn- bracket-token
  [content]
  (let [up (str/upper-case content)]
    (cond
      (re-matches #"H+|M+|S+" up)
      [:elapsed ({\H :h \M :m \S :s} (first up)) (count up)]

      (re-matches condition-re content)
      (let [[_ op n] (re-matches condition-re content)] [:condition [op (parse-double n)]])

      (str/starts-with? content "$")
      (let [sym (second (re-matches #"(?s)\$([^-]*).*" content))]
        (when (seq sym) [:lit sym]))

      (color-names up) [:color (color-names up)]

      (re-matches #"COLOR([1-9]|[1-4]\d|5[0-6])" up)
      (let [n (parse-long (subs up 5))] [:color (get basic-colors (dec n) n)])

      :else nil)))

(defn- starts-with-ci? [^String s i ^String prefix]
  (.regionMatches s true (int i) prefix 0 (count prefix)))

(defn- run-length
  [^String s i]
  (let [c (Character/toLowerCase (.charAt s i))]
    (loop [j i]
      (if (and (< j (count s)) (= c (Character/toLowerCase (.charAt s j)))) (recur (inc j))
        (- j i)))))

(def ^:private date-letters {\y :y \m :m \d :d \h :h \s :s})

(defn- tokenize
  [^String s]
  (let [n (count s)]
    (loop [i 0 out []]
      (if (>= i n)
        out
        (let [c (.charAt s i)
              lc (Character/toLowerCase c)
              nxt (when (< (inc i) n) (.charAt s (inc i)))]
          (cond
            (= c \") (let [j (long (or (str/index-of s "\"" (inc i)) n))]
                       (recur (inc j) (conj out [:lit (subs s (inc i) j)])))
            (#{\\ \!} c) (recur (+ i 2) (cond-> out nxt (conj [:lit (str nxt)])))
            (= c \_) (recur (+ i 2) (conj out [:lit " "]))
            (= c \*) (recur (+ i 2) out)
            (= c \[) (let [j (long (or (str/index-of s "]" i) n))]
                       (recur (inc j)
                              (if-let [t (bracket-token (subs s (inc i) (min j n)))] (conj out t)
                                out)))
            (starts-with-ci? s i "General") (recur (+ i 7) (conj out [:general]))
            (starts-with-ci? s i "AM/PM") (recur (+ i 5) (conj out [:ampm "AM/PM"]))
            (starts-with-ci? s i "A/P") (recur (+ i 3) (conj out [:ampm (subs s i (+ i 3))]))
            (#{\0 \# \?} c) (recur (inc i) (conj out [:digit c]))
            (= c \.) (recur (inc i) (conj out [:point]))
            (= c \,) (recur (inc i) (conj out [:comma]))
            (= c \%) (recur (inc i) (conj out [:percent]))
            (= c \/) (recur (inc i) (conj out [:slash]))
            (= c \@) (recur (inc i) (conj out [:at]))
            (and (= lc \e) (#{\+ \-} nxt)) (recur (+ i 2) (conj out [:exp (str c nxt)]))
            (date-letters lc) (let [k (long (run-length s i))]
                                (recur (+ i k) (conj out [:date (date-letters lc) k])))
            :else (recur (inc i) (conj out [:lit (str c)]))))))))

(defn- split-sections
  [^String s]
  (loop [i 0 start 0 acc []]
    (if (>= i (count s))
      (conj acc (subs s start))
      (case (.charAt s i)
        \" (recur (inc (long (or (str/index-of s "\"" (inc i)) (count s)))) start acc)
        (\\ \!) (recur (+ i 2) start acc)
        \[ (recur (inc (long (or (str/index-of s "]" i) (count s)))) start acc)
        \; (recur (inc i) (inc i) (conj acc (subs s start i)))
        (recur (inc i) start acc)))))

(defn- tag [t] (first t))
(defn- digit? [t] (= :digit (tag t)))
(defn- time-token? [t] (#{:date :elapsed :ampm} (tag t)))

(defn- literal-text
  [[k x]]
  (case k
    :lit x :digit (str x) :point "." :comma "," :percent "%" :slash "/" :exp x :at "@" ""))

(defn- resolve-minutes
  [tokens]
  (let [timeish (keep-indexed (fn [i t] (when (time-token? t) i)) tokens)
        unit-at (fn [i] (when i (let [[k u] (nth tokens i)] (when (#{:date :elapsed} k) u))))
        neighbours (into {}
                         (map (fn [[a b c]] [b [a c]])
                              (partition 3 1 (concat [nil] timeish [nil]))))]
    (vec (map-indexed
          (fn [i [k u n :as t]]
            (if (and (= k :date) (= u :m) (<= n 2)
                     (let [[p q] (neighbours i)]
                       (or (= :h (unit-at p)) (= :s (unit-at q)))))
              [:date :min n]
              t))
          tokens))))

(defn- date-section-tokens
  [tokens]
  (loop [[t & more :as ts] (resolve-minutes tokens) out []]
    (cond
      (empty? ts) out
      (and (= :point (tag t)) (= [:digit \0] (first more)))
      (let [zeros (take-while #(= [:digit \0] %) more)]
        (recur (drop (count zeros) more) (conj out [:subsec (count zeros)])))
      (#{:date :elapsed :ampm} (tag t)) (recur more (conj out t))
      :else (recur more (conj out [:lit (literal-text t)])))))

(defn- classify-commas
  [tokens]
  (let [tv (vec tokens)
        roles (map-indexed
               (fn [i t]
                 (if (not= :comma (tag t))
                   t
                   (let [next-t (first (drop-while #(= :comma (tag %)) (subvec tv (inc i))))
                         prev-t (first (drop-while #(= :comma (tag %)) (rseq (subvec tv 0 i))))
                         in-int? (not-any? #(#{:point :exp} (tag %)) (subvec tv 0 i))]
                     (cond
                       (and in-int? (digit? next-t) (digit? prev-t)) [:group]
                       (and (digit? prev-t) (not (digit? next-t))) [:scale]
                       :else [:lit ","]))))
               tv)]
    [(vec (remove #(#{:group :scale} (tag %)) roles))
     (boolean (some #(= :group (tag %)) roles))
     (count (filter #(= :scale (tag %)) roles))]))

(defn- split-at-tag
  [k tokens]
  (let [[a b] (split-with #(not= k (tag %)) tokens)]
    [(vec a) (when (seq b) (vec (rest b)))]))

(defn- number-layout [tokens]
  (let [[tokens grouping? scale] (classify-commas tokens)
        [mantissa exp-part] (split-at-tag :exp tokens)
        [int-part frac-part] (split-at-tag :point mantissa)
        int-part (if (some digit? int-part) int-part (conj int-part [:digit \#]))]
    {:integer int-part
     :point? (some? frac-part)
     :frac (or frac-part [])
     :exp (some #(when (= :exp (tag %)) (second %)) tokens)
     :exp-digits (or exp-part [])
     :grouping? grouping?
     :scale scale
     :percent (count (filter #(= :percent (tag %)) tokens))}))

(defn- fraction-layout [tokens]
  (let [[left right] (split-at-tag :slash tokens)
        int+lits (vec (reverse (drop-while digit? (reverse left))))
        num (subvec left (count int+lits))
        fixed? (re-matches #"[1-9]" (literal-text (first right)))
        den (vec (take-while #(or (digit? %) (re-matches #"\d" (literal-text %))) right))
        den (if fixed? den (vec (take-while digit? right)))]
    {:integer int+lits
     :whole? (boolean (some digit? int+lits))
     :num num
     :den den
     :fixed-den (when fixed? (parse-long (apply str (map literal-text den))))
     :tail (subvec right (count den))}))

(defn- parse-section [text]
  (let [raw (tokenize text)
        condition (some #(when (= :condition (tag %)) (second %)) raw)
        color (some #(when (= :color (tag %)) (second %)) raw)
        tokens (vec (remove #(#{:condition :color} (tag %)) raw))
        has? (fn [k] (some #(= k (tag %)) tokens))
        base (cond-> {:tokens tokens :condition condition} color (assoc :color color))]
    (cond
      (has? :general) (assoc base :kind :general)
      (some time-token? tokens) (assoc base :kind :date :tokens (date-section-tokens tokens))
      (and (has? :slash) (some digit? tokens)
           (let [[l r] (split-at-tag :slash tokens)]
             (and (digit? (peek l))
                  (or (digit? (first r)) (re-matches #"[1-9]" (literal-text (first r)))))))
      (merge base {:kind :fraction} (fraction-layout tokens))
      (and (has? :exp) (some digit? tokens)) (merge base {:kind :scientific} (number-layout tokens))
      (some digit? tokens) (merge base {:kind :number} (number-layout tokens))
      (has? :at) (assoc base :kind :text)
      :else (assoc base :kind :literal))))

(def parse-format
  (memoize (fn [fmt] (mapv parse-section (split-sections fmt)))))

(def ^:private mc15 (MathContext. 15 RoundingMode/HALF_EVEN))

(defn- decimal
  ^BigDecimal [x places]
  (-> (BigDecimal/valueOf (double x)) (.round mc15) (.setScale (int places) RoundingMode/HALF_UP)))

(defn- split-digits
  [^BigDecimal bd]
  (let [[i f] (str/split (.toPlainString bd) #"\." 2)]
    [(if (= i "0") "" i) (or f "")]))

(defn- pad-char [placeholder]
  (case placeholder \0 "0" \? " " ""))

(defn- fill-integer
  [tokens ^String digits group]
  (let [slots (count (filter digit? tokens))
        k (count digits)
        positions (fn [j] (if (and (zero? j) (> k slots))
                            (range (dec k) (- slots 2) -1)
                            [(- slots 1 j)]))
        render-pos (fn [placeholder p]
                     (let [ch (if (< p k) (str (.charAt digits (- k 1 p))) (pad-char placeholder))]
                       (if (and group (pos? p) (zero? (mod p 3)) (re-matches #"\d" ch))
                         (str ch group)
                         ch)))]
    (loop [[t & more :as ts] tokens j 0 sb (StringBuilder.)]
      (cond
        (empty? ts) (str sb)
        (digit? t) (recur more (inc j)
                          (reduce #(.append ^StringBuilder %1 ^String (render-pos (second t) %2)) sb
                                  (positions j)))
        :else (recur more j (.append sb ^String (literal-text t)))))))

(defn- fill-fraction
  [tokens ^String digits]
  (let [placeholders (map second (filter digit? tokens))
        trimmed (->> (map vector placeholders digits)
                     reverse
                     (take-while (fn [[p d]] (and (= d \0) (not= p \0))))
                     count)
        keep-n (- (count placeholders) trimmed)]
    (loop [[t & more :as ts] tokens j 0 sb (StringBuilder.)]
      (cond
        (empty? ts) (str sb)
        (digit? t) (recur more (inc j) (.append sb ^String (if (< j keep-n)
                                                             (str (.charAt digits j))
                                                             (pad-char (second t)))))
        :else (recur more j (.append sb ^String (literal-text t)))))))

(defn- digit-count [tokens] (count (filter digit? tokens)))

(defn- scaled [x {:keys [percent scale]}]
  (* x (Math/pow 100.0 percent) (Math/pow 1000.0 (- scale))))

(defn- group-sep [loc grouping?] (when grouping? (str (:group loc \,))))
(defn- decimal-sep [loc] (str (:decimal loc \.)))

(defn- render-number [loc {:keys [integer frac point? grouping?] :as sec} x]
  (let [[i f] (split-digits (decimal (scaled x sec) (digit-count frac)))]
    (str (fill-integer integer i (group-sep loc grouping?))
         (when point? (str (decimal-sep loc) (fill-fraction frac f))))))

(defn- render-scientific [loc {:keys [integer frac point? exp exp-digits grouping?] :as sec} x]
  (let [x (scaled x sec)
        ints (map second (filter digit? integer))
        n (max 1 (count ints))
        eng? (some #{\#} ints)
        places (digit-count frac)
        bd (.round (BigDecimal/valueOf (double x)) mc15)
        e (if (zero? (.signum bd)) 0
              (let [s (.stripTrailingZeros bd)] (- (.precision s) (.scale s) 1)))
        limit (.pow BigDecimal/TEN (int n))
        [mant e] (loop [e (if eng? (* n (Math/floorDiv (long e) (long n))) (- e (dec n)))]
                   (let [m (.setScale (.movePointLeft bd (int e)) (int places)
                                      RoundingMode/HALF_UP)]
                     (if (and (>= (.compareTo m limit) 0) (pos? (.signum bd)))
                       (recur (+ e (if eng? n 1)))
                       [m e])))
        [i f] (split-digits mant)
        e (if (zero? (.signum bd)) 0 e)]
    (str (fill-integer integer i (group-sep loc grouping?))
         (when point? (str (decimal-sep loc) (fill-fraction frac f)))
         (subs exp 0 1)
         (cond (neg? e) "-" (= \+ (.charAt ^String exp 1)) "+" :else "")
         (fill-integer exp-digits (if (zero? e) "" (str (Math/abs (long e)))) nil))))

(defn- best-fraction
  [^double x max-den]
  (reduce (fn [[_ _ best-err :as best] d]
            (let [n (Math/round (* x d))
                  err (Math/abs (- x (/ (double n) d)))]
              (if (< err best-err) [n d err] best)))
          [(Math/round x) 1 (Math/abs (- x (Math/round x)))]
          (range 2 (inc max-den))))

(defn- render-fraction [{:keys [integer whole? num den fixed-den tail]} x]
  (let [w (if whole? (Math/floor x) 0.0)
        f (- x w)
        [n d] (if fixed-den
                [(Math/round (* f fixed-den)) fixed-den]
                (best-fraction f (dec (long (Math/pow 10 (count den))))))
        [w n] (if (and whole? (= n d)) [(inc w) 0] [w n])
        whole-digits (cond (pos? w) (str (long w)) (and whole? (zero? n)) "0" :else "")
        frac-part (if fixed-den
                    (str (fill-integer num (str n) nil) "/" (apply str (map literal-text den)))
                    (let [ds (str d)]
                      (str (fill-integer num (str n) nil) "/" ds
                           (apply str (map pad-char (drop (count ds) (map second den)))))))]
    (str (fill-integer integer whole-digits nil)
         (if (and whole? (zero? n))
           (apply str (repeat (count frac-part) " "))
           frac-part)
         (apply str (map literal-text tail)))))

(def ^:private month-names
  ["January" "February" "March" "April" "May" "June" "July" "August" "September" "October"
   "November" "December"])
(def ^:private day-names ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"])

(def ^:private max-serial 2958466.0)

(defn- date-fields
  [x subsec]
  (let [unit (long (Math/pow 10 subsec))
        per-day (* 86400 unit)
        total (Math/round (* (double x) per-day))
        days (Math/floorDiv total per-day)
        secs (quot (- total (* days per-day)) unit)
        [y mo d] (if (zero? days) [1900 1 0] (v/serial->ymd days))]
    {:y y :mo mo :d d :wd (mod (+ days 6) 7)
     :h (quot secs 3600) :mi (mod (quot secs 60) 60) :s (mod secs 60)
     :frac (mod total unit) :subsec subsec
     :total-secs (quot total unit)}))

(defn- zero-pad [n width] (let [s (str n)] (str (apply str (repeat (- width (count s)) "0")) s)))

(defn- name-of
  [loc k abbr-k en i n]
  (let [full (get (k loc) i (en i))]
    (case (int n)
      3 (or (get (abbr-k loc) i) (subs full 0 (min 3 (count full))))
      4 full
      5 (subs full 0 1))))

(defn- render-date-token [loc fields twelve-hour? [k u n :as t]]
  (let [{:keys [y mo d wd h mi s frac subsec total-secs]} fields]
    (case k
      :date (case u
              :y (if (<= n 2) (zero-pad (mod y 100) 2) (str y))
              :m (case (int (min n 5))
                   1 (str mo) 2 (zero-pad mo 2)
                   (3 4 5) (name-of loc :month-names :month-abbr month-names (dec mo) (min n 5)))
              :d (case (int (min n 4))
                   1 (str d) 2 (zero-pad d 2)
                   (3 4) (name-of loc :day-names :day-abbr day-names wd (min n 4)))
              :h (let [hh (if twelve-hour? (let [m (mod h 12)] (if (zero? m) 12 m)) h)]
                   (zero-pad hh (min n 2)))
              :min (zero-pad mi (min n 2))
              :s (zero-pad s (min n 2)))
      :elapsed (zero-pad (case u :h (quot total-secs 3600) :m (quot total-secs 60) :s total-secs) n)
      :subsec (str (decimal-sep loc) (subs (zero-pad frac subsec) 0 u))
      :ampm (let [pm? (>= h 12)]
              (if (= "AM/PM" u)
                (if pm? "PM" "AM")
                (str (.charAt ^String u (if pm? 2 0)))))
      :lit u)))

(defn- render-date
  [loc {:keys [tokens]} x]
  (when (and (>= x 0) (< x max-serial))
    (let [subsec (reduce max 0 (keep (fn [[k n]] (when (= k :subsec) n)) tokens))
          fields (date-fields x subsec)
          twelve-hour? (some #(= :ampm (tag %)) tokens)]
      (apply str (map #(render-date-token loc fields twelve-hour? %) tokens)))))

(defn- render-literal [{:keys [tokens]} text]
  (apply str (map #(case (tag %) :at text :general text (literal-text %)) tokens)))

(defn- render-section
  [loc section x]
  (case (:kind section)
    :general (render-literal section (v/format-number loc x))
    :number (render-number loc section x)
    :scientific (render-scientific loc section x)
    :fraction (render-fraction section x)
    :date (render-date loc section x)
    (:text :literal) (render-literal section (v/format-number loc x))))

(defn- condition-holds? [[op n] x]
  (case op
    "<" (< x n) "<=" (<= x n) ">" (> x n) ">=" (>= x n) "=" (== x n) "<>" (not= x n)))

(defn- choose-numeric
  [sections x]
  (let [numeric (vec (remove #(= :text (:kind %)) (take 3 sections)))
        signed (fn [s] [s (abs x) (neg? x)])]
    (cond
      (empty? numeric) [{:kind :general :tokens [[:general]]} (abs x) (neg? x)]

      (some :condition numeric)
      (signed (or (some #(when (and (:condition %) (condition-holds? (:condition %) x)) %) numeric)
                  (first (remove :condition numeric))
                  (peek numeric)))

      :else
      (case (count numeric)
        1 (signed (first numeric))
        2 (if (neg? x) [(second numeric) (- x) false] (signed (first numeric)))
        (cond (pos? x) (signed (first numeric))
              (neg? x) [(second numeric) (- x) false]
              :else (signed (nth numeric 2)))))))

(defn- text-section [sections]
  (if (>= (count sections) 4)
    (nth sections 3)
    (some #(when (= :text (:kind %)) %) sections)))

(defn date-format? [fmt]
  (and (not (str/blank? fmt)) (= :date (:kind (first (parse-format fmt))))))

(defn general? [fmt]
  (or (str/blank? fmt) (= "general" (str/lower-case (str/trim fmt)))))

(defn system-format
  [loc fmt]
  (get (:system-formats loc) fmt fmt))

(defn- number-render [loc x fmt]
  (let [x   (double x)
        fmt (system-format loc fmt)]
    (if (general? fmt)
      {:text (v/format-number loc x)}
      (let [[section value negate?] (choose-numeric (parse-format fmt) x)]
        (cond-> {:text (if (= :date (:kind section))
                         (render-section loc section (if negate? (- value) value))
                         (when-let [s (render-section loc section value)]
                           (if (and negate? (pos? value)) (str "-" s) s)))}
          (:color section) (assoc :color (:color section)))))))

(defn format-number
  ([x fmt] (format-number nil x fmt))
  ([loc x fmt] (:text (number-render loc x fmt))))

(defn format-text
  [text fmt]
  (if-let [section (when-not (general? fmt) (text-section (parse-format fmt)))]
    (render-literal section text)
    text))

(defn render
  ([value fmt] (render nil value fmt))
  ([loc value fmt]
   (let [value (if (v/array? value) (ffirst value) value)]
     (cond
       (nil? value) {:text ""}
       (v/error? value) {:text (v/error-string loc value)}
       (number? value) (update (number-render loc value fmt) :text #(or % "#####"))
       (boolean? value) {:text (v/boolean-text loc value)}
       (string? value) (let [section (when-not (general? fmt) (text-section (parse-format fmt)))]
                         (cond-> {:text (format-text value fmt)}
                           (:color section) (assoc :color (:color section))))
       :else {:text (v/display loc value)}))))

(defn format-value
  ([value fmt] (format-value nil value fmt))
  ([loc value fmt] (:text (render loc value fmt))))
