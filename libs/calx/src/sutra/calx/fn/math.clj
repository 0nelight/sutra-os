(ns sutra.calx.fn.math
  (:require [clojure.string :as str]
            [sutra.calx.value :as v :refer [error?]]
            [sutra.calx.eval :as ev]
            [sutra.calx.fn.util :as u])
  (:import (java.math BigDecimal MathContext RoundingMode)))

(set! *warn-on-reflection* true)

(def ^:private mc15 (MathContext. 15 RoundingMode/HALF_UP))

(defn- bd15
  ^BigDecimal [x]
  (.round (BigDecimal/valueOf (double x)) mc15))

(defn- d15
  [x]
  (let [x (double x)]
    (if (or (zero? x) (Double/isNaN x) (Double/isInfinite x)) x (.doubleValue (bd15 x)))))

(defn round-decimal
  [x digits ^RoundingMode mode]
  (let [x (double x)]
    (if (zero? x)
      0.0
      (let [digits (int (max -330 (min 330 digits)))]
        (.doubleValue (.setScale (bd15 x) digits mode))))))

(defn- nums
  [& xs]
  (let [ns (map #(u/num %) xs)]
    (or (first (filter error? ns)) (vec ns))))

(def ^:private max-exact 9007199254740992.0)

(defn- num-fn
  [[lo hi] f & opts]
  (update (apply u/num-fn [lo hi] f opts) :impl
          (fn [impl] (fn [& args] (apply impl (take hi (concat args (repeat v/omitted))))))))

(defn- agg
  [f]
  {:arity [1 255] :args :refs
   :impl (fn [ctx & args]
           (u/with-collected [xs (u/collect ctx args)]
             (u/result (f xs))))})

(defn- sum [xs] (reduce + 0.0 xs))
(defn- product [xs] (if (empty? xs) 0.0 (reduce * 1.0 xs)))

(defn- grid
  [x]
  (u/->2d (if (v/omitted? x) v/ERR-VALUE x)))

(defn- first-err [a] (some #(when (error? %) %) (u/flat a)))

(defn- sumproduct [& args]
  (let [grids (map grid args)
        dims (map (juxt v/rows v/cols) grids)]
    (cond
      (not (apply = dims)) v/ERR-VALUE
      :else
      (or (some first-err grids)
          (let [as-num (fn [x] (if (number? x) (double x) 0.0))]
            (u/result
             (reduce + 0.0 (apply map (fn [& xs] (reduce * 1.0 (map as-num xs)))
                                  (map u/flat grids)))))))))

(defn- paired-sum
  [f]
  {:arity [2 2]
   :impl (fn [a b]
           (let [xs (u/flat (grid a)) ys (u/flat (grid b))]
             (cond
               (not= (count xs) (count ys)) v/ERR-NA
               :else (or (some first-err [[xs] [ys]])
                         (u/result (reduce + 0.0
                                           (keep (fn [[x y]] (when (and (number? x) (number? y))
                                                                 (f (double x) (double y))))
                                                 (map vector xs ys))))))))})

(defn- seriessum [x n m coeffs]
  (u/check [[x n m] (nums x n m)
            cs (let [cs (u/flat (grid coeffs))]
                 (or (first-err [cs])
                     (if (every? number? cs) cs v/ERR-VALUE)))]
    (u/result (reduce + 0.0
                      (map-indexed (fn [i a] (* (double a) (Math/pow x (+ n (* i m))))) cs)))))

(defn- area
  [ctx x]
  (if (v/ref? x)
    {:dims [(inc (- (:r2 x) (:r1 x))) (inc (- (:c2 x) (:c1 x)))] :grid (ev/ref-values ctx x)}
    (let [g (u/->2d (ev/deref-value ctx x))]
      {:dims [(v/rows g) (v/cols g)] :grid g})))

(defn- resize
  [ctx x [nr nc]]
  (if (v/ref? x)
    (area ctx (assoc x :r2 (+ (:r1 x) (dec nr)) :c2 (+ (:c1 x) (dec nc))))
    (area ctx x)))

(defn- cells
  [areas]
  (let [[nr nc] (:dims (first areas))
        gr (min nr (apply max (map (comp v/rows :grid) areas)))
        gc (min nc (apply max (map (comp v/cols :grid) areas)))]
    (for [r (range gr) c (range gc)] [r c])))

(defn- at [{g :grid} [r c]] (get-in g [r c]))

(defn- conditional-sum
  [sum-area tests]
  (reduce (fn [acc rc]
            (if (every? (fn [[a pred]] (pred (at a rc))) tests)
              (let [x (at sum-area rc)]
                (cond (number? x) (+ acc (double x))
                      (error? x) (reduced x)
                      :else acc))
              acc))
          0.0
          (cells (cons sum-area (map first tests)))))

(defn- criteria
  [ctx x]
  (let [c (ev/deref-value ctx x)] (if (v/omitted? c) nil c)))

(defn- sumif [ctx rng crit & [sum-rng]]
  (let [a (area ctx rng)
        s (if (and sum-rng (not (v/omitted? sum-rng))) (resize ctx sum-rng (:dims a)) a)]
    (ev/broadcast (fn [c]
                    (u/result (conditional-sum s [[a (u/criteria-pred (:locale ctx) c)]])))
                  [(criteria ctx crit)])))

(defn- sumifs [ctx sum-rng & pairs]
  (if (odd? (count pairs))
    v/ERR-VALUE
    (let [s (area ctx sum-rng)
          ranges (map #(area ctx %) (take-nth 2 pairs))
          crits (map #(criteria ctx %) (take-nth 2 (rest pairs)))]
      (if-not (every? #(= (:dims s) (:dims %)) ranges)
        v/ERR-VALUE
        (ev/broadcast (fn [& cs]
                        (let [preds (map #(u/criteria-pred (:locale ctx) %) cs)]
                          (u/result (conditional-sum s (map vector ranges preds)))))
                      crits)))))

(defn- mean [xs] (if (empty? xs) v/ERR-DIV0 (/ (sum xs) (count xs))))

(defn- sq-dev [xs] (let [m (/ (sum xs) (count xs))] (sum (map #(let [d (- % m)] (* d d)) xs))))

(defn- variance [sample?]
  (fn [xs]
    (let [n (count xs) dof (if sample? (dec n) n)]
      (if (< dof 1) v/ERR-DIV0 (/ (sq-dev xs) dof)))))

(defn- stdev [sample?]
  (let [variance-fn (variance sample?)]
    (fn [xs] (let [x (variance-fn xs)] (if (error? x) x (Math/sqrt x))))))

(defn- extreme [f] (fn [xs] (if (empty? xs) 0.0 (reduce f xs))))

(defn- percentile-inc [xs p]
  (let [s (vec (sort xs)) n (count s)]
    (if (or (zero? n) (< p 0) (> p 1))
      v/ERR-NUM
      (let [h (* p (dec n)) lo (long (Math/floor h)) frac (- h lo)]
        (if (>= lo (dec n))
          (peek s)
          (+ (s lo) (* frac (- (s (inc lo)) (s lo)))))))))

(defn- percentile-exc [xs p]
  (let [s (vec (sort xs)) n (count s) h (* p (inc n))]
    (if (or (zero? n) (<= p 0) (>= p 1) (< h 1) (> h n))
      v/ERR-NUM
      (let [lo (long (Math/floor h)) frac (- h lo)]
        (if (>= lo n)
          (peek s)
          (+ (s (dec lo)) (* frac (- (s lo) (s (dec lo))))))))))

(defn- median [xs] (if (empty? xs) v/ERR-NUM (percentile-inc xs 0.5)))

(defn- mode-sngl [xs]
  (let [freqs (frequencies xs)
        best (apply max 0 (vals freqs))]
    (if (< best 2) v/ERR-NA (first (filter #(= best (freqs %)) xs)))))

(defn- kth [order]
  (fn [xs k]
    (let [k (long (Math/ceil k)) s (vec (sort order xs))]
      (if (<= 1 k (count s)) (s (dec k)) v/ERR-NUM))))

(defn- quartile [pct valid?]
  (fn [xs q]
    (let [q (u/trunc q)]
      (if (valid? q) (pct xs (/ q 4.0)) v/ERR-NUM))))

(def ^:private aggregators
  {1  {:kind :numeric :f mean}
   2  {:kind :count}
   3  {:kind :counta}
   4  {:kind :numeric :f (extreme max)}
   5  {:kind :numeric :f (extreme min)}
   6  {:kind :numeric :f product}
   7  {:kind :numeric :f (stdev true)}
   8  {:kind :numeric :f (stdev false)}
   9  {:kind :numeric :f sum}
   10 {:kind :numeric :f (variance true)}
   11 {:kind :numeric :f (variance false)}
   12 {:kind :numeric :f median}
   13 {:kind :numeric :f mode-sngl}
   14 {:kind :numeric-k :f (kth >)}
   15 {:kind :numeric-k :f (kth <)}
   16 {:kind :numeric-k :f percentile-inc}
   17 {:kind :numeric-k :f (quartile percentile-inc #(<= 0 % 4))}
   18 {:kind :numeric-k :f percentile-exc}
   19 {:kind :numeric-k :f (quartile percentile-exc #(<= 1 % 3))}})

(defn- run-aggregator
  [{:keys [kind f]} values ignore-errors? & [k]]
  (let [values (if ignore-errors? (remove error? values) values)
        numbers (keep #(when (number? %) (double %)) values)]
    (case kind
      :count (double (count numbers))
      :counta (double (count values))
      (or (some #(when (error? %) %) values)
          (u/result (if (= kind :numeric-k) (f numbers k) (f numbers)))))))

(def ^:private nested-re #"(?i)\b(SUBTOTAL|AGGREGATE)\s*\(")

(defn- subtotal-values
  [ctx args skip-nested?]
  (let [formula-at (or (:formula-at ctx) (constantly nil))
        nested? (fn [sheet r c] (when-let [f (formula-at sheet r c)] (re-find nested-re f)))]
    (mapcat (fn [a]
              (cond
                (v/omitted? a) []
                (v/ref? a) (for [[r c x] (ev/ref-cells ctx a)
                                 :when (not (and skip-nested? (nested? (:sheet a) r c)))]
                             x)
                (v/array? a) (remove nil? (u/flat a))
                :else [a]))
            args)))

(defn- int-arg [ctx x] (u/int* (ev/deref-value ctx x)))

(defn- subtotal [ctx fnum & refs]
  (u/check [n (int-arg ctx fnum)
            spec (or (aggregators (if (> n 100) (- n 100) n)) v/ERR-VALUE)]
    (if (or (> n 111) (< 11 n 101) (empty? refs))
      v/ERR-VALUE
      (run-aggregator spec (subtotal-values ctx refs true) false))))

(defn- aggregate [ctx fnum opt & args]
  (u/check [n (int-arg ctx fnum)
            o (if (v/omitted? opt) 0 (int-arg ctx opt))
            spec (or (aggregators n) v/ERR-VALUE)]
    (if-not (<= 0 o 7)
      v/ERR-VALUE
      (let [ignore-errors? (contains? #{2 3 6 7} o)
            skip-nested? (<= o 3)]
        (if (= :numeric-k (:kind spec))
          (let [[arr k] args]
            (if (or (nil? k) (v/omitted? k) (> (count args) 2))
              v/ERR-VALUE
              (u/check [k (u/num (ev/deref-value ctx k))]
                (run-aggregator spec (subtotal-values ctx [arr] skip-nested?) ignore-errors? k))))
          (run-aggregator spec (subtotal-values ctx args skip-nested?) ignore-errors?))))))

(defn- round-fn [mode]
  (num-fn [1 2] (fn [x d] (round-decimal x (u/trunc d) mode))))

(defn- signs-conflict? [n s] (and (pos? n) (neg? s)))

(defn- ceiling [n s]
  (cond (zero? s) 0.0
        (signs-conflict? n s) v/ERR-NUM
        :else (d15 (* (Math/ceil (d15 (/ n s))) s))))

(defn- floor* [n s]
  (cond (zero? n) 0.0
        (zero? s) v/ERR-DIV0
        (signs-conflict? n s) v/ERR-NUM
        :else (d15 (* (Math/floor (d15 (/ n s))) s))))

(defn- ceiling-math [n s mode]
  (let [s (Math/abs (double s))]
    (cond (zero? s) 0.0
          (and (neg? n) (not (zero? mode))) (- (d15 (* (Math/ceil (d15 (/ (- n) s))) s)))
          :else (d15 (* (Math/ceil (d15 (/ n s))) s)))))

(defn- floor-math [n s mode]
  (let [s (Math/abs (double s))]
    (cond (zero? s) 0.0
          (and (neg? n) (not (zero? mode))) (- (d15 (* (Math/floor (d15 (/ (- n) s))) s)))
          :else (d15 (* (Math/floor (d15 (/ n s))) s)))))

(defn- mround [n m]
  (cond (zero? m) 0.0
        (and (not (zero? n)) (not= (pos? n) (pos? m))) v/ERR-NUM
        :else (d15 (* (round-decimal (/ n m) 0 RoundingMode/HALF_UP) m))))

(defn- away-to-multiple
  [x ok?]
  (let [m (Math/ceil (Math/abs (double x)))
        m (if (ok? (long m)) m (inc m))]
    (if (neg? x) (- m) m)))

(defn- fact [n]
  (cond (neg? n) v/ERR-NUM
        (> n 170) v/ERR-NUM
        :else (reduce * 1.0 (range 1 (inc (long n))))))

(defn- factdouble [n]
  (cond (neg? n) v/ERR-NUM
        (> n 300) v/ERR-NUM
        :else (reduce * 1.0 (range (long n) 0 -2))))

(defn- combin* [n k]
  (let [k (min k (- n k))
        r (reduce (fn [acc i] (/ (* acc (- n i)) (inc i))) 1.0 (range k))]
    (if (< r max-exact) (Math/rint r) r)))

(defn- combin [n k]
  (let [n (u/trunc n) k (u/trunc k)]
    (if (or (neg? n) (neg? k) (< n k)) v/ERR-NUM (combin* n k))))

(defn- combina [n k]
  (let [n (u/trunc n) k (u/trunc k)]
    (cond (or (neg? n) (neg? k)) v/ERR-NUM
          (zero? k) 1.0
          (zero? n) 0.0
          :else (combin* (+ n k -1) k))))

(defn- permut [n k]
  (let [n (u/trunc n) k (u/trunc k)]
    (if (or (neg? n) (neg? k) (< n k))
      v/ERR-NUM
      (reduce * 1.0 (range (inc (- n k)) (inc n))))))

(defn- permutationa [n k]
  (let [n (u/trunc n) k (u/trunc k)]
    (if (or (neg? n) (neg? k)) v/ERR-NUM (Math/pow n k))))

(defn- integers
  [xs]
  (if (some #(or (neg? %) (>= % max-exact)) xs)
    v/ERR-NUM
    (mapv #(long (u/trunc %)) xs)))

(defn- gcd2 [a b] (if (zero? b) a (recur b (mod a b))))

(defn- multinomial [xs]
  (u/check [ns (integers xs)]
    (first (reduce (fn [[acc total] n]
                     (let [total (+ total n)] [(* acc (combin* (double total) (double n))) total]))
                   [1.0 0] ns))))

(defn- int-agg [f]
  (agg (fn [xs] (u/check [ns (integers xs)] (double (f ns))))))

(defn- lcm [ns]
  (if (some zero? ns)
    0
    (reduce (fn [a b] (let [r (* (/ a (gcd2 a b)) (double b))]
                        (if (>= r max-exact) (reduced ##Inf) (long r))))
            1 ns)))

(defn- matrix
  [x]
  (let [g (grid x)]
    (if (every? number? (u/flat g)) (v/amap2 double g) v/ERR-VALUE)))

(defn- square? [m] (= (v/rows m) (v/cols m)))

(defn- mmult [a b]
  (u/check [a (matrix a) b (matrix b)]
    (if (not= (v/cols a) (v/rows b))
      v/ERR-VALUE
      (let [bt (v/transpose b)]
        (mapv (fn [row] (mapv (fn [col] (u/result (reduce + 0.0 (map * row col)))) bt)) a)))))

(defn- pivot-row
  [m k]
  (apply max-key #(Math/abs (double (get-in m [% k]))) (range k (count m))))

(defn- swap-rows [m i j] (if (= i j) m (assoc m i (m j) j (m i))))

(defn- determinant [m]
  (let [n (count m)]
    (loop [m m k 0 det 1.0]
      (if (= k n)
        det
        (let [p (pivot-row m k)
              m (swap-rows m k p)
              det (if (= p k) det (- det))
              pv (get-in m [k k])]
          (if (zero? pv)
            0.0
            (recur (reduce (fn [m i]
                             (let [f (/ (get-in m [i k]) pv)]
                               (update m i #(mapv - % (mapv (partial * f) (m k))))))
                           m (range (inc k) n))
                   (inc k)
                   (* det pv))))))))

(defn- mdeterm [x]
  (u/check [m (matrix x)]
    (if (square? m) (u/result (d15 (determinant m))) v/ERR-VALUE)))

(defn- minverse [x]
  (u/check [m (matrix x)]
    (if-not (square? m)
      v/ERR-VALUE
      (let [n (count m)
            scale (apply max 1e-300 (map #(Math/abs (double %)) (u/flat m)))
            aug (mapv (fn [i row] (into row (map #(if (= i %) 1.0 0.0) (range n)))) (range) m)]
        (loop [a aug k 0]
          (if (= k n)
            (mapv (fn [row] (mapv u/result (subvec row n))) a)
            (let [a (swap-rows a k (pivot-row a k))
                  pv (get-in a [k k])]
              (if (<= (Math/abs (double pv)) (* 1e-14 scale))
                v/ERR-NUM
                (let [pivot (mapv #(/ % pv) (a k))
                      a (assoc a k pivot)]
                  (recur (reduce (fn [a i]
                                   (if (= i k)
                                     a
                                     (let [f (get-in a [i k])]
                                       (update a i #(mapv - % (mapv (partial * f) pivot))))))
                                 a (range n))
                         (inc k)))))))))))

(defn- munit [n]
  (u/check [n (u/int* n)]
    (if (< n 1) v/ERR-VALUE (v/make-array2 n n (fn [r c] (if (= r c) 1.0 0.0))))))

(defn- base [n radix & [min-len]]
  (u/check [[n radix min-len] (nums n radix min-len)]
    (let [n (u/trunc n) radix (u/trunc radix) min-len (u/trunc min-len)]
      (if (or (neg? n) (>= n max-exact) (not (<= 2 radix 36)) (not (<= 0 min-len 255)))
        v/ERR-NUM
        (let [s (str/upper-case (Long/toString (long n) (int radix)))]
          (str (apply str (repeat (- min-len (count s)) \0)) s))))))

(defn- decimal [text radix]
  (u/check [t (u/text text) r (u/num radix)]
    (let [r (u/trunc r) t (str/trim t)]
      (cond
        (not (<= 2 r 36)) v/ERR-NUM
        (> (count t) 255) v/ERR-VALUE
        (= "" t) 0.0
        :else (let [ds (map #(Character/digit (char %) (int r)) t)]
                (if (some neg? ds)
                  v/ERR-NUM
                  (let [x (reduce (fn [acc d] (+ (* acc r) d)) 0.0 ds)]
                    (if (>= x max-exact) v/ERR-NUM x))))))))

(def ^:private roman-chars [\M \D \C \L \X \V \I])
(def ^:private roman-values [1000 500 100 50 10 5 1])

(defn- roman*
  [n form]
  (let [max-idx (dec (count roman-values))
        sb (StringBuilder.)]
    (loop [i 0 n n]
      (if (> i (quot max-idx 2))
        (str sb)
        (let [idx (* 2 i)
              digit (quot n (roman-values idx))]
          (if (= 4 (mod digit 5))
            (let [idx2 (if (= digit 4) (dec idx) (- idx 2))
                  idx (loop [idx idx steps 0]
                        (if (and (< steps form) (< idx max-idx)
                                 (<= (- (roman-values idx2) (roman-values (inc idx))) n))
                          (recur (inc idx) (inc steps))
                          idx))]
              (.append sb (roman-chars idx))
              (.append sb (roman-chars idx2))
              (recur (inc i) (- (+ n (roman-values idx)) (roman-values idx2))))
            (do
              (when (> digit 4) (.append sb (roman-chars (dec idx))))
              (dotimes [_ (mod digit 5)] (.append sb (roman-chars idx)))
              (recur (inc i) (mod n (roman-values idx))))))))))

(defn- roman [n & [form]]
  (u/check [n (u/num n)
            form (cond (or (nil? form) (v/omitted? form)) 0.0
                       (boolean? form) (if form 0.0 4.0)
                       :else (u/num form))]
    (let [n (long (u/trunc n)) form (long (u/trunc form))]
      (if (and (<= 0 n 3999) (<= 0 form 4)) (roman* n form) v/ERR-VALUE))))

(def ^:private roman-digit {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000})

(defn- arabic [text]
  (u/check [t (u/text text)]
    (let [t (str/upper-case (str/trim t))
          neg? (str/starts-with? t "-")
          ds (map roman-digit (if neg? (subs t 1) t))]
      (cond
        (> (count t) 255) v/ERR-VALUE
        (some nil? ds) v/ERR-VALUE
        :else (let [x (reduce + 0 (map (fn [d nxt] (if (and nxt (< d nxt)) (- d) d))
                                       ds (concat (rest ds) [nil])))]
                (double (if neg? (- x) x)))))))

(defn- nonzero [f] (fn [x] (if (zero? x) v/ERR-DIV0 (f x))))

(defn- trig-limited
  [f]
  (fn [x] (if (>= (Math/abs (double x)) 134217728.0) v/ERR-NUM (f x))))

(defn- num1 [f] (num-fn [1 1] f))

(def functions
  {"SUM"     (agg sum)
   "SUMSQ"   (agg #(sum (map (fn [x] (* x x)) %)))
   "PRODUCT" (agg product)
   "SUMPRODUCT" {:arity [1 255] :impl sumproduct}
   "SUMIF"   {:arity [2 3] :args :refs :impl sumif}
   "SUMIFS"  {:arity [3 255] :args :refs :impl sumifs}
   "SUMX2MY2" (paired-sum (fn [x y] (- (* x x) (* y y))))
   "SUMX2PY2" (paired-sum (fn [x y] (+ (* x x) (* y y))))
   "SUMXMY2"  (paired-sum (fn [x y] (let [d (- x y)] (* d d))))
   "SERIESSUM" {:arity [4 4] :lift #{0 1 2} :impl seriessum}

   "SUBTOTAL"  {:arity [2 255] :args :refs :impl subtotal}
   "AGGREGATE" {:arity [3 255] :args :refs :impl aggregate}

   "ABS"    (num1 #(Math/abs (double %)))
   "SIGN"   (num1 #(Math/signum (double %)))
   "SQRT"   (num1 #(if (neg? %) v/ERR-NUM (Math/sqrt %)))
   "SQRTPI" (num1 #(if (neg? %) v/ERR-NUM (Math/sqrt (* % Math/PI))))
   "POWER"  (num-fn [2 2] (fn [x y]
                              (cond (and (zero? x) (zero? y)) v/ERR-NUM
                                    (and (zero? x) (neg? y)) v/ERR-DIV0
                                    :else (Math/pow x y))))
   "EXP"    (num1 #(Math/exp %))
   "LN"     (num1 #(if (pos? %) (Math/log %) v/ERR-NUM))
   "LOG10"  (num1 #(if (pos? %) (Math/log10 %) v/ERR-NUM))
   "LOG"    (num-fn [1 2] (fn [x b]
                              (cond (or (<= x 0) (<= b 0)) v/ERR-NUM
                                    (== b 1) v/ERR-DIV0
                                    (== b 10) (Math/log10 x)
                                    :else (/ (Math/log x) (Math/log b))))
                      :defaults [nil 10])
   "PI"     {:arity [0 0] :impl (constantly Math/PI)}

   "INT"      (num1 #(Math/floor %))
   "TRUNC"    (num-fn [1 2] (fn [x d] (round-decimal x (u/trunc d) RoundingMode/DOWN)))
   "MOD"      (num-fn [2 2] (fn [n d]
                                (if (zero? d)
                                  v/ERR-DIV0
                                  (let [r (- n (* d (Math/floor (d15 (/ n d)))))]
                                    (if (== (abs r) (abs d)) 0.0 r)))))
   "QUOTIENT" (num-fn [2 2] (fn [n d] (if (zero? d) v/ERR-DIV0 (u/trunc (d15 (/ n d))))))
   "ROUND"     (round-fn RoundingMode/HALF_UP)
   "ROUNDUP"   (round-fn RoundingMode/UP)
   "ROUNDDOWN" (round-fn RoundingMode/DOWN)
   "MROUND"    (num-fn [2 2] mround)
   "CEILING"          (num-fn [2 2] ceiling)
   "CEILING.MATH"     (num-fn [1 3] ceiling-math :defaults [nil 1 0])
   "CEILING.PRECISE"  (num-fn [1 2] #(ceiling-math %1 %2 0) :defaults [nil 1])
   "ISO.CEILING"      (num-fn [1 2] #(ceiling-math %1 %2 0) :defaults [nil 1])
   "FLOOR"            (num-fn [2 2] floor*)
   "FLOOR.MATH"       (num-fn [1 3] floor-math :defaults [nil 1 0])
   "FLOOR.PRECISE"    (num-fn [1 2] #(floor-math %1 %2 0) :defaults [nil 1])
   "EVEN" (num1 #(away-to-multiple % even?))
   "ODD"  (num1 #(if (zero? %) 1.0 (away-to-multiple % odd?)))

   "FACT"         (num1 #(fact (u/trunc %)))
   "FACTDOUBLE"   (num1 #(factdouble (u/trunc %)))
   "COMBIN"       (num-fn [2 2] combin)
   "COMBINA"      (num-fn [2 2] combina)
   "PERMUT"       (num-fn [2 2] permut)
   "PERMUTATIONA" (num-fn [2 2] permutationa)
   "MULTINOMIAL"  (agg multinomial)
   "GCD"          (int-agg #(reduce gcd2 0 %))
   "LCM"          (int-agg lcm)

   "RAND" {:arity [0 0] :ctx? true :volatile true :impl (fn [ctx] ((:rand ctx)))}
   "RANDBETWEEN" {:arity [2 2] :ctx? true :volatile true :lift :all
                  :impl (fn [ctx lo hi]
                          (u/check [[lo hi] (nums lo hi)]
                            (let [lo (Math/ceil lo) hi (Math/floor hi)]
                              (if (> lo hi)
                                v/ERR-NUM
                                (+ lo (Math/floor (* ((:rand ctx)) (inc (- hi lo)))))))))}

   "DEGREES" (num1 #(Math/toDegrees %))
   "RADIANS" (num1 #(Math/toRadians %))
   "SIN"   (num1 #(Math/sin %))
   "COS"   (num1 #(Math/cos %))
   "TAN"   (num1 #(Math/tan %))
   "ASIN"  (num1 #(if (<= -1 % 1) (Math/asin %) v/ERR-NUM))
   "ACOS"  (num1 #(if (<= -1 % 1) (Math/acos %) v/ERR-NUM))
   "ATAN"  (num1 #(Math/atan %))
   "ATAN2" (num-fn [2 2] (fn [x y] (if (and (zero? x) (zero? y)) v/ERR-DIV0 (Math/atan2 y x))))
   "SINH"  (num1 #(Math/sinh %))
   "COSH"  (num1 #(Math/cosh %))
   "TANH"  (num1 #(Math/tanh %))
   "ASINH" (num1 #(let [x (double %)]
                    (* (Math/signum x) (Math/log (+ (Math/abs x) (Math/sqrt (inc (* x x))))))))
   "ACOSH" (num1 #(if (< % 1) v/ERR-NUM (Math/log (+ % (Math/sqrt (dec (* % %)))))))
   "ATANH" (num1 #(if (< -1 % 1) (* 0.5 (Math/log (/ (+ 1 %) (- 1 %)))) v/ERR-NUM))
   "COT"   (num1 (trig-limited (nonzero #(/ 1.0 (Math/tan %)))))
   "COTH"  (num1 (trig-limited (nonzero #(/ 1.0 (Math/tanh %)))))
   "CSC"   (num1 (trig-limited (nonzero #(/ 1.0 (Math/sin %)))))
   "CSCH"  (num1 (trig-limited (nonzero #(/ 1.0 (Math/sinh %)))))
   "SEC"   (num1 (trig-limited #(/ 1.0 (Math/cos %))))
   "SECH"  (num1 (trig-limited #(/ 1.0 (Math/cosh %))))
   "ACOT"  (num1 #(- (/ Math/PI 2) (Math/atan %)))
   "ACOTH" (num1 #(if (<= (Math/abs (double %)) 1) v/ERR-NUM
                    (* 0.5 (Math/log (/ (+ % 1) (- % 1))))))

   "MMULT"    {:arity [2 2] :impl mmult}
   "MDETERM"  {:arity [1 1] :impl mdeterm}
   "MINVERSE" {:arity [1 1] :impl minverse}
   "MUNIT"    {:arity [1 1] :impl munit}

   "BASE"    {:arity [2 3] :lift :all :impl base}
   "DECIMAL" {:arity [2 2] :lift :all :impl decimal}
   "ROMAN"   {:arity [1 2] :lift :all :impl roman}
   "ARABIC"  {:arity [1 1] :lift :all :impl arabic}})
