(ns sutra.calx.fn.engineering
  (:require [clojure.string :as str]
            [sutra.calx.value :as v :refer [error?]]
            [sutra.calx.fn.util :as u])
  (:import (java.math BigDecimal MathContext RoundingMode)))

(set! *warn-on-reflection* true)

(defn- present [x] (when-not (or (nil? x) (v/omitted? x)) x))

(def ^:private base-specs
  {:bin {:radix 2 :bits 10}
   :oct {:radix 8 :bits 30}
   :hex {:radix 16 :bits 40}})

(defn- base-range [{:keys [bits]}]
  (let [half (bit-shift-left 1 (dec bits))] [(- half) (dec half)]))

(defn- digits-arg
  [x]
  (let [x (v/scalarize x)]
    (cond (boolean? x) v/ERR-VALUE
          (number? x) (if (== x (Math/rint x)) (str (long x)) (str x))
          :else (u/text x))))

(defn- parse-base
  [s base]
  (let [{:keys [radix bits]} (base-specs base)
        s (str/trim s)]
    (cond
      (= "" s) 0
      (> (count s) 10) v/ERR-NUM
      (not-every? #(<= 0 (Character/digit (char %) (int radix))) s) v/ERR-NUM
      :else (let [n (Long/parseLong s radix)]
              (if (>= n (bit-shift-left 1 (dec bits))) (- n (bit-shift-left 1 bits)) n)))))

(defn- places-arg [places]
  (if-let [p (present places)]
    (u/check [p (u/num p)]
      (let [p (u/trunc p)] (if (<= 1 p 10) (long p) v/ERR-NUM)))
    nil))

(defn- format-base
  [n base places]
  (let [{:keys [radix bits] :as spec} (base-specs base)
        [lo hi] (base-range spec)]
    (u/check [places (places-arg places)]
      (cond
        (not (<= lo n hi)) v/ERR-NUM
        (neg? n) (str/upper-case (Long/toString (+ n (bit-shift-left 1 bits)) (int radix)))
        :else (let [s (str/upper-case (Long/toString n (int radix)))]
                (cond (nil? places) s
                      (> (count s) places) v/ERR-NUM
                      :else (str (apply str (repeat (- places (count s)) \0)) s)))))))

(defn- from-decimal [base]
  {:arity [1 2] :lift :all
   :impl (fn [x & [places]]
           (u/check [n (u/num x)]
             (let [[lo hi] (base-range (base-specs base))]
               (if (<= lo n (inc hi))
                 (format-base (long (u/trunc n)) base places)
                 v/ERR-NUM))))})

(defn- to-decimal [base]
  {:arity [1 1] :lift :all
   :impl (fn [x] (u/check [s (digits-arg x) n (parse-base s base)] (double n)))})

(defn- base->base [from to]
  {:arity [1 2] :lift :all
   :impl (fn [x & [places]]
           (u/check [s (digits-arg x) n (parse-base s from)]
             (format-base n to places)))})

(def ^:private max-bits 281474976710655.0)

(defn- bit-operand [x]
  (u/check [n (u/num x)]
    (if (and (<= 0 n max-bits) (== n (Math/floor n))) (long n) v/ERR-NUM)))

(defn- bitwise [f]
  {:arity [2 2] :lift :all
   :impl (fn [a b] (u/check [a (bit-operand a) b (bit-operand b)] (double (f a b))))})

(defn- shift [sign]
  {:arity [2 2] :lift :all
   :impl (fn [x amount]
           (u/check [n (bit-operand x) s (u/num amount)]
             (let [s (* sign (long (u/trunc s)))]
               (cond
                 (> (abs s) 53) v/ERR-NUM
                 (neg? s) (double (bit-shift-right n (- s)))
                 :else (let [r (* (double n) (Math/pow 2 s))]
                         (if (> r max-bits) v/ERR-NUM r))))))})

(def ^:private two-over-sqrt-pi (/ 2.0 (Math/sqrt Math/PI)))

(defn- erf-series
  [x]
  (let [x2 (* x x)]
    (loop [n 0 term x acc x]
      (let [term (/ (* term 2.0 x2) (+ (* 2 n) 3))
            acc' (+ acc term)]
        (if (or (== acc acc') (> n 500))
          (* two-over-sqrt-pi (Math/exp (- x2)) acc')
          (recur (inc n) term acc'))))))

(defn- erfc-cf
  [x]
  (let [tail (reduce (fn [acc k] (/ (* 0.5 k) (+ x acc))) 0.0 (range 200 0 -1))]
    (/ (Math/exp (- (* x x))) (* (Math/sqrt Math/PI) (+ x tail)))))

(defn erfc [x]
  (cond (neg? x) (- 2.0 (erfc (- x)))
        (< x 2.0) (- 1.0 (erf-series x))
        :else (erfc-cf x)))

(defn erf [x]
  (let [a (Math/abs (double x))
        r (if (< a 2.0) (erf-series a) (- 1.0 (erfc-cf a)))]
    (if (neg? x) (- r) r)))

(def ^:private units
  (let [length {"m" [1.0 true] "mi" [1609.344] "Nmi" [1852.0] "in" [0.0254] "ft" [0.3048]
                "yd" [0.9144] "ang" [1e-10 true] "ell" [1.143] "ly" [9.4607304725808e15 true]
                "parsec" [3.08567758128155e16 true] "pc" [3.08567758128155e16 true]
                "Pica" [(/ 0.0254 72)] "Picapt" [(/ 0.0254 72)] "pica" [(/ 0.0254 6)]
                "survey_mi" [1609.34721869444]}
        mass {"g" [1.0 true] "lbm" [453.59237] "ozm" [28.349523125] "u" [1.66053906660e-24 true]
              "stone" [6350.29318] "ton" [907184.74] "uk_ton" [1016046.9088] "LTON" [1016046.9088]
              "grain" [0.06479891] "cwt" [45359.237] "shweight" [45359.237] "uk_cwt" [50802.34544]
              "lcwt" [50802.34544] "hweight" [50802.34544] "sg" [14593.9029372064]}
        time {"sec" [1.0 true] "s" [1.0 true] "mn" [60.0] "min" [60.0] "hr" [3600.0]
              "day" [86400.0] "d" [86400.0] "yr" [31557600.0]}
        volume {"l" [1.0 true] "L" [1.0 true] "lt" [1.0 true] "gal" [3.785411784] "qt" [0.946352946]
                "pt" [0.473176473] "us_pt" [0.473176473] "uk_pt" [0.56826125] "cup" [0.2365882365]
                "oz" [0.0295735295625] "tbs" [0.01478676478125] "tsp" [0.00492892159375]
                "uk_gal" [4.54609] "uk_qt" [1.1365225] "barrel" [158.987294928]
                "m3" [1000.0 true 3] "ft3" [28.316846592] "in3" [0.016387064]}
        energy {"J" [1.0 true] "e" [1e-7 true] "c" [4.184 true] "cal" [4.1868 true]
                "eV" [1.602176634e-19 true] "ev" [1.602176634e-19 true]
                "HPh" [2684519.53769617] "hh" [2684519.53769617]
                "Wh" [3600.0 true] "wh" [3600.0 true] "flb" [0.0421401100938048]
                "BTU" [1055.05585262] "btu" [1055.05585262]}
        pressure {"Pa" [1.0 true] "p" [1.0 true] "atm" [101325.0 true] "at" [101325.0 true]
                  "mmHg" [133.322368421053 true] "psi" [6894.75729316836] "Torr" [133.322368421053]}
        force {"N" [1.0 true] "dyn" [1e-5 true] "dy" [1e-5 true] "lbf" [4.4482216152605]
               "pond" [9.80665e-3 true]}
        power {"W" [1.0 true] "w" [1.0 true] "HP" [745.69987158227] "h" [745.69987158227]
               "PS" [735.49875]}
        magnetism {"T" [1.0 true] "ga" [1e-4 true]}
        speed {"m/s" [1.0 true] "m/sec" [1.0 true] "m/h" [(/ 1.0 3600) true]
               "m/hr" [(/ 1.0 3600) true]
               "mph" [0.44704] "kn" [(/ 1852.0 3600)] "admkn" [0.514773333333333]}
        area {"m2" [1.0 true 2] "ha" [10000.0] "ar" [100.0 true] "uk_acre" [4046.8564224]
              "us_acre" [4046.87260987425] "ft2" [0.09290304] "in2" [0.00064516] "yd2" [0.83612736]
              "mi2" [2589988.110336] "Morgen" [2500.0]}]
    (into {}
          (for [[cat table] {:length length :mass mass :time time :volume volume :energy energy
                             :pressure pressure :force force :power power :magnetism magnetism
                             :speed speed :area area}
                [unit [factor prefixable? power]] table]
            [unit
             {:category cat :factor factor :prefixable? (boolean prefixable?)
              :power (or power 1)}]))))

(def ^:private temperatures
  (let [c {:to-k #(+ % 273.15) :from-k #(- % 273.15)}
        f {:to-k #(+ (/ (* (- % 32) 5) 9) 273.15) :from-k #(+ (/ (* (- % 273.15) 9) 5) 32)}
        k {:to-k identity :from-k identity}
        rank {:to-k #(/ (* % 5) 9) :from-k #(/ (* % 9) 5)}
        reau {:to-k #(+ (* % 1.25) 273.15) :from-k #(/ (- % 273.15) 1.25)}]
    {"C" c "cel" c "F" f "fah" f "K" k "kel" k "Rank" rank "Reau" reau}))

(def ^:private prefixes
  {"Y" 1e24 "Z" 1e21 "E" 1e18 "P" 1e15 "T" 1e12 "G" 1e9 "M" 1e6 "k" 1e3 "h" 1e2 "da" 1e1 "e" 1e1
   "d" 1e-1 "c" 1e-2 "m" 1e-3 "u" 1e-6 "n" 1e-9 "p" 1e-12 "f" 1e-15 "a" 1e-18 "z" 1e-21 "y" 1e-24})

(defn- resolve-unit
  [name]
  (or (some-> (temperatures name) (assoc :category :temperature))
      (units name)
      (some (fn [[p mult]]
              (when (str/starts-with? name p)
                (let [base (subs name (count p))]
                  (if-let [{:keys [prefixable? power] :as u} (units base)]
                    (when prefixable? (update u :factor * (Math/pow mult power)))
                    (when (#{"K" "kel"} base)
                      {:category :temperature
                       :to-k #(* % mult) :from-k #(/ % mult)})))))
            prefixes)))

(def ^:private mc15 (MathContext. 15 RoundingMode/HALF_UP))

(defn- d15 [x]
  (let [x (double x)]
    (if (or (zero? x) (Double/isNaN x) (Double/isInfinite x)) x
        (.doubleValue (.round (BigDecimal/valueOf x) mc15)))))

(defn- convert [x from to]
  (u/check [n (u/num x) from (u/text from) to (u/text to)]
    (let [a (resolve-unit from) b (resolve-unit to)]
      (cond
        (or (nil? a) (nil? b) (not= (:category a) (:category b))) v/ERR-NA
        (= :temperature (:category a)) (u/result (d15 ((:from-k b) ((:to-k a) n))))
        :else (u/result (d15 (/ (* n (:factor a)) (:factor b))))))))

(def ^:private num-re "(?:\\d+\\.?\\d*|\\.\\d+)(?:[eE][+-]?\\d+)?")
(def ^:private real-re (re-pattern (str "([+-]?" num-re ")")))
(def ^:private full-re (re-pattern (str "([+-]?" num-re ")([+-])(" num-re ")?([ij])")))
(def ^:private imag-re (re-pattern (str "([+-]?)(" num-re ")?([ij])")))

(defn- parse-complex-text [s]
  (let [num (fn [sign digits]
              (* (if (= sign "-") -1.0 1.0) (if digits (Double/parseDouble digits) 1.0)))]
    (if-let [[_ re] (re-matches real-re s)]
      {:re (Double/parseDouble re) :im 0.0}
      (if-let [[_ re sign im sfx] (re-matches full-re s)]
        {:re (Double/parseDouble re) :im (num sign im) :suffix sfx}
        (if-let [[_ sign im sfx] (re-matches imag-re s)]
          {:re 0.0 :im (num sign im) :suffix sfx}
          v/ERR-NUM)))))

(defn- complex
  [x]
  (let [x (v/scalarize x)]
    (cond
      (error? x) x
      (or (nil? x) (v/omitted? x)) {:re 0.0 :im 0.0}
      (number? x) {:re (double x) :im 0.0}
      (string? x) (if (= "" x) {:re 0.0 :im 0.0} (parse-complex-text x))
      :else v/ERR-VALUE)))

(defn- combined-suffix
  [zs]
  (let [sfx (set (keep :suffix zs))]
    (if (> (count sfx) 1) v/ERR-VALUE (or (first sfx) "i"))))

(defn- fmt-part [x] (v/format-number (d15 x)))

(defn- format-complex
  [{:keys [re im suffix]}]
  (let [re (d15 re) im (d15 im) sfx (or suffix "i")]
    (cond
      (or (Double/isNaN re) (Double/isInfinite re) (Double/isNaN im)
          (Double/isInfinite im)) v/ERR-NUM
      (zero? im) (fmt-part re)
      :else (str (when-not (zero? re) (fmt-part re))
                 (cond (== im 1.0) (if (zero? re) "" "+")
                       (== im -1.0) "-"
                       :else (str (when (and (pos? im) (not (zero? re))) "+") (fmt-part im)))
                 sfx))))

(defn- z [re im suffix] {:re re :im im :suffix suffix})

(defn- z-mul [{a :re b :im} {c :re d :im}] [(- (* a c) (* b d)) (+ (* a d) (* b c))])

(defn- z-abs [{:keys [re im]}] (Math/hypot re im))
(defn- z-arg [{:keys [re im]}] (Math/atan2 im re))

(defn- z-polar [r theta suffix] (z (* r (Math/cos theta)) (* r (Math/sin theta)) suffix))

(defn- complex-fn
  [f]
  {:arity [1 1] :lift :all
   :impl (fn [x]
           (u/check [w (complex x) r (f w)]
             (if (map? r) (format-complex (assoc r :suffix (:suffix w))) (u/result r))))})

(defn- complex-args
  [args]
  (let [xs (mapcat #(if (v/array? %) (remove nil? (u/flat %)) [%]) args)
        zs (map complex xs)]
    (or (first (filter error? zs)) zs)))

(defn- complex-fold [f init]
  {:arity [1 255]
   :impl (fn [& args]
           (u/check [zs (complex-args args) sfx (combined-suffix zs)]
             (let [[re im] (reduce f init zs)] (format-complex (z re im sfx)))))})

(defn- complex-binary [f]
  {:arity [2 2] :lift :all
   :impl (fn [a b]
           (u/check [p (complex a) q (complex b) sfx (combined-suffix [p q]) r (f p q)]
             (format-complex (assoc r :suffix sfx))))})

(defn- z-div [{a :re b :im} {c :re d :im :as q}]
  (let [den (+ (* c c) (* d d))]
    (if (zero? den)
      v/ERR-NUM
      (z (/ (+ (* a c) (* b d)) den) (/ (- (* b c) (* a d)) den) nil))))

(defn- z-ln [w]
  (if (zero? (z-abs w)) v/ERR-NUM (z (Math/log (z-abs w)) (z-arg w) nil)))

(defn- z-log-base [base]
  (fn [w] (u/check [l (z-ln w)] (z (/ (:re l) (Math/log base)) (/ (:im l) (Math/log base)) nil))))

(defn- z-power [w n]
  (cond (and (zero? (z-abs w)) (<= n 0)) v/ERR-NUM
        (zero? (z-abs w)) (z 0.0 0.0 nil)
        :else (z-polar (Math/pow (z-abs w) n) (* n (z-arg w)) nil)))

(defn- complex-text-arg [x]
  (let [x (v/scalarize x)] (if (boolean? x) v/ERR-VALUE x)))

(def functions
  {"DEC2BIN" (from-decimal :bin)
   "DEC2OCT" (from-decimal :oct)
   "DEC2HEX" (from-decimal :hex)
   "BIN2DEC" (to-decimal :bin)
   "OCT2DEC" (to-decimal :oct)
   "HEX2DEC" (to-decimal :hex)
   "BIN2OCT" (base->base :bin :oct)
   "BIN2HEX" (base->base :bin :hex)
   "OCT2BIN" (base->base :oct :bin)
   "OCT2HEX" (base->base :oct :hex)
   "HEX2BIN" (base->base :hex :bin)
   "HEX2OCT" (base->base :hex :oct)

   "BITAND"    (bitwise bit-and)
   "BITOR"     (bitwise bit-or)
   "BITXOR"    (bitwise bit-xor)
   "BITLSHIFT" (shift 1)
   "BITRSHIFT" (shift -1)

   "DELTA"  {:arity [1 2] :lift :all
             :impl (fn [a & [b]]
                     (u/check [a (u/num a) b (u/num (or b v/omitted))] (if (== a b) 1.0 0.0)))}
   "GESTEP" {:arity [1 2] :lift :all
             :impl (fn [n & [s]]
                     (u/check [n (u/num n) s (u/num (or s v/omitted))] (if (>= n s) 1.0 0.0)))}

   "ERF"          {:arity [1 2] :lift :all
                   :impl (fn [lo & [hi]]
                           (u/check [lo (u/num lo)]
                             (if-let [hi (present hi)]
                               (u/check [hi (u/num hi)] (u/result (- (erf hi) (erf lo))))
                               (u/result (erf lo)))))}
   "ERF.PRECISE"  (u/num-fn [1 1] erf)
   "ERFC"         (u/num-fn [1 1] erfc)
   "ERFC.PRECISE" (u/num-fn [1 1] erfc)

   "CONVERT" {:arity [3 3] :lift :all :impl convert}

   "COMPLEX" {:arity [2 3] :lift :all
              :impl (fn [re im & [sfx]]
                      (u/check [re (u/num (complex-text-arg re)) im (u/num (complex-text-arg im))
                                sfx (let [s (present sfx)] (if s (u/text s) "i"))]
                        (cond (not (#{"i" "j" ""} sfx)) v/ERR-VALUE
                              :else (format-complex (z re im (if (= "" sfx) "i" sfx))))))}
   "IMREAL"      (complex-fn :re)
   "IMAGINARY"   (complex-fn :im)
   "IMABS"       (complex-fn z-abs)
   "IMARGUMENT"  (complex-fn #(if (zero? (z-abs %)) v/ERR-DIV0 (z-arg %)))
   "IMCONJUGATE" (complex-fn #(update % :im -))
   "IMSUM"       (complex-fold (fn [[re im] w] [(+ re (:re w)) (+ im (:im w))]) [0.0 0.0])
   "IMPRODUCT"   (complex-fold (fn [acc w] (z-mul {:re (acc 0) :im (acc 1)} w)) [1.0 0.0])
   "IMSUB"       (complex-binary (fn [p q] (z (- (:re p) (:re q)) (- (:im p) (:im q)) nil)))
   "IMDIV"       (complex-binary z-div)
   "IMSQRT"      (complex-fn #(z-polar (Math/sqrt (z-abs %)) (/ (z-arg %) 2) nil))
   "IMEXP"       (complex-fn #(z-polar (Math/exp (:re %)) (:im %) nil))
   "IMLN"        (complex-fn z-ln)
   "IMLOG10"     (complex-fn (z-log-base 10.0))
   "IMLOG2"      (complex-fn (z-log-base 2.0))
   "IMSIN"       (complex-fn (fn [{a :re b :im}]
                               (z (* (Math/sin a) (Math/cosh b)) (* (Math/cos a) (Math/sinh b))
                                  nil)))
   "IMCOS"       (complex-fn (fn [{a :re b :im}]
                               (z (* (Math/cos a) (Math/cosh b)) (- (* (Math/sin a) (Math/sinh b)))
                                  nil)))
   "IMPOWER"     {:arity [2 2] :lift :all
                  :impl (fn [x n]
                          (u/check [w (complex x) n (u/num n) r (z-power w n)]
                            (format-complex (assoc r :suffix (:suffix w)))))}})
