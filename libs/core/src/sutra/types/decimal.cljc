(ns sutra.types.decimal
  (:require [clojure.string :as str]
            [sutra.types.value-types :as vt]))

(declare cmp-plain)

#?(:cljs
   (deftype ExactDecimal [s]
     Object
     (toString [_] s)
     IEquiv
     (-equiv [_ o] (and (instance? ExactDecimal o) (= s (.-s ^ExactDecimal o))))
     IHash
     (-hash [_] (hash s))
     IComparable
     (-compare [_ o]
               (or (cmp-plain s (if (instance? ExactDecimal o) (.-s ^ExactDecimal o) (str o))) 0))
     IPrintWithWriter
     (-pr-writer [_ w _] (-write w (str "#exact \"" s "\"")))))

(defn exact?
  [x]
  #?(:cljs (instance? ExactDecimal x)
     :clj  (or (instance? java.math.BigDecimal x)
               (instance? java.math.BigInteger x)
               (instance? clojure.lang.BigInt x))))

(def ^:private digits "0123456789")

(defn- digit-at [s i] (str/index-of digits (subs s i (inc i))))
(defn- digit-str [n] (subs digits n (inc n)))

(defn- strip-leading
  [s]
  (let [t (str/replace s #"^0+" "")] (if (str/blank? t) "0" t)))

(defn- split-at-dot [p]
  (if-let [i (str/index-of p ".")] [(subs p 0 i) (subs p (inc i))] [p ""]))

(defn- zeros [n] (apply str (repeat n "0")))

(defn- all-zero? [s] (or (str/blank? s) (nil? (re-find #"[1-9]" s))))

(defn- shift
  [i f e]
  (cond
    (zero? e) [i f]
    (pos? e)  (let [n (min e (count f))]
                [(str i (subs f 0 n) (zeros (- e n))) (subs f n)])
    :else     (let [e (- e), n (min e (count i)), cut (- (count i) n)]
                [(subs i 0 cut) (str (zeros (- e n)) (subs i cut) f)])))

(defn- trim-frac
  [p]
  (if (str/index-of p ".")
    (let [t (str/replace p #"0+$" "")]
      (if (str/ends-with? t ".") (subs t 0 (dec (count t))) t))
    p))

(defn- parts
  [s]
  (when-let [[_ sign i f e] (re-matches #"([+-]?)(\d*)(?:\.(\d*))?(?:[eE]([+-]?\d+))?" s)]
    (let [i (or i "") f (or f "")]
      (when (or (seq i) (seq f))
        [sign i f (if (seq e) (parse-long e) 0)]))))

(defn plain
  [x]
  (cond
    (nil? x)    nil
    (string? x) (when-let [[sg i f e] (parts (str/trim x))]
                  (let [[i f] (shift i f e)
                        b     (let [i (strip-leading i)]
                                (if (str/blank? f) i (str i "." f)))]
                    (if (= sg "-") (str "-" b) b)))
    #?@(:cljs [(instance? ExactDecimal x) (.-s ^ExactDecimal x)]
        :clj  [(instance? java.math.BigDecimal x) (.toPlainString ^java.math.BigDecimal x)
               (ratio? x) (recur (double x))])
    (number? x) (some-> (plain (str x)) trim-frac)
    :else       nil))

(defn magnitude
  [x]
  (some-> (plain x) (str/replace #"^-" "")))

(defn- cmp-abs
  [a b]
  (let [[ai af] (split-at-dot a) [bi bf] (split-at-dot b)
        ai (strip-leading ai)    bi (strip-leading bi)
        n  (max (count af) (count bf))
        c  (cond
             (not= (count ai) (count bi)) (compare (count ai) (count bi))
             (not= ai bi)                 (compare ai bi)
             :else (compare (str af (zeros (- n (count af))))
                            (str bf (zeros (- n (count bf))))))]
    (cond (pos? c) 1 (neg? c) -1 :else 0)))

(defn cmp-plain
  [a b]
  (when (and a b)
    (let [na (and (str/starts-with? a "-") (not (all-zero? a)))
          nb (and (str/starts-with? b "-") (not (all-zero? b)))]
      (cond
        (and na (not nb)) -1
        (and nb (not na)) 1
        :else (let [c (cmp-abs (str/replace a #"^-" "") (str/replace b #"^-" ""))]
                (if na (- c) c))))))

(defn cmp
  [a b]
  (cmp-plain (plain a) (plain b)))

(defn cmp-magnitude
  [a b]
  (cmp-plain (magnitude a) (magnitude b)))

(defn integral?
  [x]
  (boolean (when-let [p (plain x)] (all-zero? (second (split-at-dot p))))))

(defn plain-integral
  [x]
  (when-let [p (plain x)]
    (let [neg (str/starts-with? p "-")
          i   (strip-leading (first (split-at-dot (str/replace p #"^-" ""))))]
      (if (and neg (not= i "0")) (str "-" i) i))))

(defn- inc-digits
  [s]
  (loop [i (dec (count s)) tail ""]
    (if (neg? i)
      (str "1" tail)
      (let [d (digit-at s i)]
        (if (= d 9)
          (recur (dec i) (str "0" tail))
          (str (subs s 0 i) (digit-str (inc d)) tail))))))

(defn round-half-up
  [p scale]
  (let [scale (max 0 (int scale))
        [i f] (split-at-dot p)
        i     (strip-leading i)
        keep  (subs (str f (zeros scale)) 0 scale)
        up?   (and (> (count f) scale) (>= (digit-at f scale) 5))
        ds    (cond-> (str i keep) up? inc-digits)
        cut   (- (count ds) scale)]
    (if (pos? scale)
      (str (strip-leading (subs ds 0 cut)) "." (subs ds cut))
      (strip-leading ds))))

(defn exact
  [s]
  (when-let [p (plain s)]
    #?(:clj  (java.math.BigDecimal. ^String p)
       :cljs (->ExactDecimal p))))

(defn lossy-number
  [x]
  (when-let [p (plain x)]
    #?(:clj (Double/parseDouble p) :cljs (js/parseFloat p))))

(defn host-number
  [x]
  (if (number? x) x (lossy-number x)))

(defn- canonical
  [p]
  (when p
    (let [neg   (str/starts-with? p "-")
          [i f] (split-at-dot (str/replace p #"^-" ""))
          f     (str/replace f #"0+$" "")
          i     (strip-leading i)
          b     (if (str/blank? f) i (str i "." f))]
      (if (and neg (not= b "0")) (str "-" b) b))))

(defn carry
  [s]
  (when-let [p (plain s)]
    (let [d (lossy-number p)]
      (if (= (canonical p) (canonical (plain d))) d (exact p)))))

(def max-safe-integer
  9007199254740991)

(defn numeric-value-type?
  [t]
  (boolean (when (or (keyword? t) (string? t))
             (contains? vt/numeric-types (keyword (name t))))))
