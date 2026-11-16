(ns sutra.calx.fn.stats
  (:require [sutra.calx.value :as v :refer [error?]]
            [sutra.calx.eval :as ev]
            [sutra.calx.fn.util :as u]))

(set! *warn-on-reflection* true)

(defn- first-error [xs] (some #(when (error? %) %) xs))

(defmacro ^:private guard
  [bad? & body]
  `(if ~bad? v/ERR-NUM (do ~@body)))

(defn- sq ^double [^double x] (* x x))

(defn- integer-valued? [^double x] (== x (Math/floor x)))

(def ^:private eps 1e-16)
(def ^:private tiny 1e-300)
(def ^:private max-iter 100000)

(def ^:private lanczos
  [0.99999999999980993 676.5203681218851 -1259.1392167224028 771.32342877765313
   -176.61502916214059 12.507343278686905 -0.13857109526572012
   9.9843695780195716e-6 1.5056327351493116e-7])

(defn- lanczos-sum ^double [^double z]
  (reduce (fn [^double acc i] (+ acc (/ (double (nth lanczos i)) (+ z i))))
          (double (first lanczos)) (range 1 9)))

(defn- log-gamma
  ^double [^double x]
  (if (< x 0.5)
    (- (Math/log (/ Math/PI (Math/abs (Math/sin (* Math/PI x))))) (log-gamma (- 1.0 x)))
    (let [z (- x 1.0)
          t (+ z 7.5)]
      (+ (* 0.5 (Math/log (* 2.0 Math/PI)))
         (* (+ z 0.5) (Math/log t))
         (- t)
         (Math/log (lanczos-sum z))))))

(defn- gamma-fn
  ^double [^double x]
  (cond
    (and (integer-valued? x) (<= 1.0 x 171.0))
    (reduce * 1.0 (range 1.0 x))

    (< x 0.5)
    (/ Math/PI (* (Math/sin (* Math/PI x)) (gamma-fn (- 1.0 x))))

    :else
    (let [z (- x 1.0)
          t (+ z 7.5)
          p (Math/pow t (* 0.5 (+ z 0.5)))]
      (* (Math/sqrt (* 2.0 Math/PI)) p (* p (Math/exp (- t))) (lanczos-sum z)))))

(defn- log-beta ^double [^double a ^double b]
  (- (+ (log-gamma a) (log-gamma b)) (log-gamma (+ a b))))

(defn- clamp-tiny ^double [^double x] (if (< (Math/abs x) tiny) tiny x))

(defn- gamma-series
  ^double [^double a ^double x]
  (loop [n 0 ap a del (/ 1.0 a) sum (/ 1.0 a)]
    (let [ap (+ ap 1.0)
          del (* del (/ x ap))
          sum (+ sum del)]
      (if (or (< (Math/abs del) (* (Math/abs sum) eps)) (> n max-iter))
        (* sum (Math/exp (- (* a (Math/log x)) x (log-gamma a))))
        (recur (inc n) ap del sum)))))

(defn- gamma-cf
  ^double [^double a ^double x]
  (let [b (- (+ x 1.0) a)
        d (/ 1.0 (clamp-tiny b))]
    (loop [i 1 b b c (/ 1.0 tiny) d d h d]
      (let [an (* (- i) (- i a))
            b (+ b 2.0)
            d (/ 1.0 (clamp-tiny (+ (* an d) b)))
            c (clamp-tiny (+ b (/ an c)))
            del (* d c)
            h (* h del)]
        (if (or (< (Math/abs (- del 1.0)) eps) (> i max-iter))
          (* h (Math/exp (- (* a (Math/log x)) x (log-gamma a))))
          (recur (inc i) b c d h))))))

(defn- gamma-p
  ^double [^double a ^double x]
  (cond (<= x 0.0) 0.0
        (< x (+ a 1.0)) (gamma-series a x)
        :else (- 1.0 (gamma-cf a x))))

(defn- gamma-q
  ^double [^double a ^double x]
  (cond (<= x 0.0) 1.0
        (< x (+ a 1.0)) (- 1.0 (gamma-series a x))
        :else (gamma-cf a x)))

(defn- beta-cf
  ^double [^double a ^double b ^double x]
  (let [qab (+ a b) qap (+ a 1.0) qam (- a 1.0)
        d (/ 1.0 (clamp-tiny (- 1.0 (/ (* qab x) qap))))]
    (loop [m 1 c 1.0 d d h d]
      (let [m2 (* 2.0 m)
            aa (/ (* m (- b m) x) (* (+ qam m2) (+ a m2)))
            d (/ 1.0 (clamp-tiny (+ 1.0 (* aa d))))
            c (clamp-tiny (+ 1.0 (/ aa c)))
            h (* h d c)
            aa (- (/ (* (+ a m) (+ qab m) x) (* (+ a m2) (+ qap m2))))
            d (/ 1.0 (clamp-tiny (+ 1.0 (* aa d))))
            c (clamp-tiny (+ 1.0 (/ aa c)))
            del (* d c)
            h (* h del)]
        (if (or (< (Math/abs (- del 1.0)) eps) (>= m max-iter))
          h
          (recur (inc m) c d h))))))

(defn- beta-reg
  ^double [^double a ^double b ^double x]
  (cond
    (<= x 0.0) 0.0
    (>= x 1.0) 1.0
    :else
    (let [front (Math/exp (- (+ (* a (Math/log x)) (* b (Math/log1p (- x)))) (log-beta a b)))]
      (if (< x (/ (+ a 1.0) (+ a b 2.0)))
        (/ (* front (beta-cf a b x)) a)
        (- 1.0 (/ (* front (beta-cf b a (- 1.0 x))) b))))))

(defn- erfc
  ^double [^double x]
  (if (neg? x)
    (+ 1.0 (gamma-p 0.5 (* x x)))
    (gamma-q 0.5 (* x x))))

(defn- horner
  ^double [coeffs ^double x]
  (reduce (fn [^double acc ^double c] (+ (* acc x) c)) 0.0 (rseq coeffs)))

(def ^:private as241
  {:a [3.3871328727963666080e0 1.3314166789178437745e+2 1.9715909503065514427e+3
       1.3731693765509461125e+4 4.5921953931549871457e+4 6.7265770927008700853e+4
       3.3430575583588128105e+4 2.5090809287301226727e+3]
   :b [1.0 4.2313330701600911252e+1 6.8718700749205790830e+2 5.3941960214247511077e+3
       2.1213794301586595867e+4 3.9307895800092710610e+4 2.8729085735721942674e+4
       5.2264952788528545610e+3]
   :c [1.42343711074968357734e0 4.63033784615654529590e0 5.76949722146069140550e0
       3.64784832476320460504e0 1.27045825245236838258e0 2.41780725177450611770e-1
       2.27238449892691845833e-2 7.74545014278341407640e-4]
   :d [1.0 2.05319162663775882187e0 1.67638483018380384940e0 6.89767334985100004550e-1
       1.48103976427480074590e-1 1.51986665636164571966e-2 5.47593808499534494600e-4
       1.05075007164441684324e-9]
   :e [6.65790464350110377720e0 5.46378491116411436990e0 1.78482653991729133580e0
       2.96560571828504891230e-1 2.65321895265761230930e-2 1.24266094738807843860e-3
       2.71155556874348757815e-5 2.01033439929228813265e-7]
   :f [1.0 5.99832206555887937690e-1 1.36929880922735805310e-1 1.48753612908506148525e-2
       7.86869131145613259100e-4 1.84631831751005468180e-5 1.42151175831644588870e-7
       2.04426310338993978564e-15]})

(defn- norm-s-inv
  ^double [^double p]
  (let [{:keys [a b c d e f]} as241
        q (- p 0.5)]
    (if (<= (Math/abs q) 0.425)
      (let [r (- 0.180625 (* q q))]
        (/ (* q (horner a r)) (horner b r)))
      (let [r (Math/sqrt (- (Math/log (if (neg? q) p (- 1.0 p)))))
            x (if (<= r 5.0)
                (let [r (- r 1.6)] (/ (horner c r) (horner d r)))
                (let [r (- r 5.0)] (/ (horner e r) (horner f r))))]
        (if (neg? q) (- x) x)))))

(defn- bisect
  ^double [f ^double lo ^double hi]
  (let [lo-neg? (neg? (double (f lo)))]
    (loop [lo lo hi hi i 0]
      (let [mid (* 0.5 (+ lo hi))]
        (if (or (<= mid lo) (>= mid hi) (> i 2000))
          mid
          (let [fm (double (f mid))]
            (cond (zero? fm) mid
                  (= (neg? fm) lo-neg?) (recur mid hi (inc i))
                  :else (recur lo mid (inc i)))))))))

(defn- quantile
  [cdf sf p q lo hi]
  (let [p (double p) q (double q) lo (double lo) hi (double hi)
        [g target] (if (<= p 0.5) [cdf p] [#(- (double (sf %))) (- q)])
        g (fn [x] (- (double (g x)) (double target)))
        hi (if (Double/isInfinite hi)
             (loop [h (max 1.0 (* 2.0 lo))]
               (if (or (>= (g h) 0.0) (> h 1e300)) h (recur (* 2.0 h))))
             hi)]
    (bisect g lo hi)))

(def ^:private sqrt-2pi (Math/sqrt (* 2.0 Math/PI)))

(defn- norm-s-pdf ^double [^double z] (/ (Math/exp (* -0.5 z z)) sqrt-2pi))
(defn- norm-s-cdf ^double [^double z] (* 0.5 (erfc (- (/ z (Math/sqrt 2.0))))))

(defn- gamma-pdf
  ^double [^double x ^double a ^double b]
  (if (zero? x)
    (cond (< a 1.0) ##Inf (== a 1.0) (/ 1.0 b) :else 0.0)
    (Math/exp (- (* (- a 1.0) (Math/log x)) (/ x b) (* a (Math/log b)) (log-gamma a)))))

(defn- gamma-inv
  ^double [^double p ^double q ^double a]
  (quantile #(gamma-p a %) #(gamma-q a %) p q 0.0 ##Inf))

(defn- beta-pdf ^double [^double x ^double a ^double b]
  (if (or (zero? x) (== x 1.0))
    (/ (* (Math/pow x (- a 1.0)) (Math/pow (- 1.0 x) (- b 1.0))) (Math/exp (log-beta a b)))
    (Math/exp (- (+ (* (- a 1.0) (Math/log x)) (* (- b 1.0) (Math/log1p (- x)))) (log-beta a b)))))

(defn- beta-inv ^double [^double p ^double a ^double b]
  (quantile #(beta-reg a b %) #(beta-reg b a (- 1.0 (double %))) p (- 1.0 p) 0.0 1.0))

(defn- t-tail
  ^double [^double x ^double df]
  (let [half (* 0.5 (beta-reg (* 0.5 df) 0.5 (/ df (+ df (* x x)))))]
    (if (neg? x) (- 1.0 half) half)))

(defn- t-pdf ^double [^double x ^double df]
  (Math/exp (- (log-gamma (* 0.5 (+ df 1.0)))
               (log-gamma (* 0.5 df))
               (* 0.5 (Math/log (* df Math/PI)))
               (* 0.5 (+ df 1.0) (Math/log1p (/ (* x x) df))))))

(defn- t-inv
  ^double [^double p ^double df]
  (if (== p 0.5)
    0.0
    (let [q (if (< p 0.5) p (- 1.0 p))
          x (bisect #(- q (t-tail % df)) 0.0
                    (loop [h 1.0] (if (or (<= (t-tail h df) q) (> h 1e300)) h (recur (* 2.0 h)))))]
      (if (< p 0.5) (- x) x))))

(defn- f-cdf ^double [^double x ^double d1 ^double d2]
  (if (<= x 0.0) 0.0 (beta-reg (* 0.5 d1) (* 0.5 d2) (/ (* d1 x) (+ (* d1 x) d2)))))

(defn- f-sf ^double [^double x ^double d1 ^double d2]
  (if (<= x 0.0) 1.0 (beta-reg (* 0.5 d2) (* 0.5 d1) (/ d2 (+ d2 (* d1 x))))))

(defn- f-pdf ^double [^double x ^double d1 ^double d2]
  (if (zero? x)
    (cond (< d1 2.0) ##Inf (== d1 2.0) 1.0 :else 0.0)
    (Math/exp (- (+ (* 0.5 d1 (Math/log (/ d1 d2))) (* (- (* 0.5 d1) 1.0) (Math/log x)))
                 (* 0.5 (+ d1 d2) (Math/log1p (/ (* d1 x) d2)))
                 (log-beta (* 0.5 d1) (* 0.5 d2))))))

(defn- f-inv ^double [^double p ^double q ^double d1 ^double d2]
  (quantile #(f-cdf % d1 d2) #(f-sf % d1 d2) p q 0.0 ##Inf))

(defn- log-choose ^double [^double n ^double k]
  (- (log-gamma (+ n 1.0)) (log-gamma (+ k 1.0)) (log-gamma (+ (- n k) 1.0))))

(defn- binom-pmf ^double [^double k ^double n ^double p]
  (cond (zero? p) (if (zero? k) 1.0 0.0)
        (== p 1.0) (if (== k n) 1.0 0.0)
        :else (Math/exp (+ (log-choose n k) (* k (Math/log p)) (* (- n k) (Math/log1p (- p)))))))

(defn- binom-cdf ^double [^double k ^double n ^double p]
  (cond (>= k n) 1.0
        (zero? p) 1.0
        (== p 1.0) 0.0
        :else (beta-reg (- n k) (+ k 1.0) (- 1.0 p))))

(defn- poisson-pmf ^double [^double k ^double m]
  (if (zero? m)
    (if (zero? k) 1.0 0.0)
    (Math/exp (- (* k (Math/log m)) m (log-gamma (+ k 1.0))))))

(defn- hypgeom-pmf ^double [^double s ^double n ^double big-m ^double big-n]
  (Math/exp (- (+ (log-choose big-m s) (log-choose (- big-n big-m) (- n s)))
               (log-choose big-n n))))

(def ^:private a-opts
  {:text-in-refs :zero :bool-in-refs :number})

(defn- sample
  ([ctx arg] (sample ctx arg {}))
  ([ctx arg opts] (u/collect ctx [arg] opts)))

(defn- coerce-scalars
  [xs defaults]
  (map-indexed (fn [i x] (if (v/omitted? x) (get defaults i 0.0) (u/num x))) xs))

(defn- lift-scalars
  [ctx f args defaults]
  (ev/broadcast (fn [& xs]
                  (let [ns (coerce-scalars xs defaults)]
                    (or (first-error ns) (u/result (apply f ns)))))
                (map #(ev/deref-value ctx %) args)))

(defn- agg-fn
  ([f] (agg-fn f {}))
  ([f opts]
   {:arity [1 255] :args :refs
    :impl (fn [ctx & args]
            (u/with-collected [xs (u/collect ctx args opts)]
              (u/result (f xs))))}))

(defn- data-fn
  [[lo hi] n f & {:keys [defaults opts]}]
  {:arity [lo hi] :args :refs
   :impl (fn [ctx & args]
           (let [[ds ss] (split-at n args)
                 samples (mapv #(sample ctx % opts) ds)]
             (or (first-error samples)
                 (lift-scalars ctx #(apply f (concat samples %&)) ss defaults))))})

(defn- region
  [ctx a]
  (cond
    (v/ref? a) (let [{:keys [r1 c1 r2 c2]} a]
                 {:dims [(inc (- r2 r1)) (inc (- c2 c1))] :vals (ev/ref-values ctx a)})
    (error? a) a
    (v/omitted? a) {:dims [1 1] :vals [[nil]]}
    :else (let [x (u/->2d a)] {:dims [(v/rows x) (v/cols x)] :vals x})))

(defn- cell-count [{[r c] :dims}] (* (long r) (long c)))

(defn- zip-regions
  [regions]
  (let [vals (map :vals regions)
        nr (apply max (map count vals))
        nc (apply max (map #(count (first %)) vals))]
    (for [i (range nr) j (range nc)]
      (mapv #(get-in % [i j]) vals))))

(defn- zip-cells
  [ra rb]
  (cond
    (= (:dims ra) (:dims rb)) (zip-regions [ra rb])
    (= (cell-count ra) (cell-count rb)) (map vector (u/flat (:vals ra)) (u/flat (:vals rb)))
    :else v/ERR-NA))

(defn- paired
  [ctx a b]
  (u/check [ra (region ctx a)
            rb (region ctx b)
            tuples (zip-cells ra rb)
            _ (or (first-error (apply concat tuples)) true)]
    (let [ps (filter (fn [[x y]] (and (number? x) (number? y))) tuples)]
      [(mapv (comp double first) ps) (mapv (comp double second) ps)])))

(defn- pair-fn
  ([f] (pair-fn [2 2] f []))
  ([arity f defaults]
   {:arity arity :args :refs
    :impl (fn [ctx a b & scalars]
            (let [ps (paired ctx a b)]
              (if (error? ps)
                ps
                (lift-scalars ctx #(apply f (concat ps %&)) scalars defaults))))}))

(defn- padded
  [args n]
  (take n (concat args (repeat v/omitted))))

(defn- num-fn
  [arity f & [defaults]]
  (u/num-fn arity f :defaults (vec defaults)))

(defn- cumulative? [^double flag] (not (zero? flag)))

(defn- sum ^double [xs] (reduce + 0.0 xs))

(defn- mean [xs] (if (empty? xs) v/ERR-DIV0 (/ (sum xs) (count xs))))

(defn- devsq ^double [xs]
  (let [m (double (mean xs))] (reduce (fn [^double acc ^double x] (+ acc (sq (- x m)))) 0.0 xs)))

(defn- variance
  [xs ddof]
  (let [n (count xs)]
    (if (<= n ddof) v/ERR-DIV0 (/ (devsq xs) (- n ddof)))))

(defn- stdev [xs ddof]
  (u/check [var (variance xs ddof)] (Math/sqrt var)))

(defn- interpolate
  [sorted ^double h]
  (let [i (long (Math/floor h))
        f (- h i)
        lo (double (nth sorted i))]
    (if (or (zero? f) (>= (inc i) (count sorted)))
      lo
      (+ lo (* f (- (double (nth sorted (inc i))) lo))))))

(defn- percentile-inc [xs ^double k]
  (guard (or (empty? xs) (< k 0.0) (> k 1.0))
    (interpolate (vec (sort xs)) (* k (dec (count xs))))))

(defn- percentile-exc [xs ^double k]
  (let [n (count xs)
        h (- (* k (inc n)) 1.0)]
    (guard (or (zero? n) (<= k 0.0) (>= k 1.0) (< h 0.0) (> h (dec n)))
      (interpolate (vec (sort xs)) h))))

(defn- median [xs] (percentile-inc xs 0.5))

(defn- quartile [pct-fn lo hi]
  (fn [xs q]
    (let [q (u/trunc q)]
      (guard (or (< q lo) (> q hi)) (pct-fn xs (/ q 4.0))))))

(defn- truncate-digits
  ^double [^double x ^long digits]
  (let [m (Math/pow 10.0 digits)]
    (/ (Math/floor (+ (* x m) 1e-9)) m)))

(defn- percentrank
  [rank-of]
  (fn [xs x & [sig]]
    (let [sig (u/trunc (double (or sig 3.0)))
          sorted (vec (sort xs))
          n (count sorted)
          rank (fn [v] (rank-of (count (filter #(< % v) sorted)) n))]
      (cond
        (or (zero? n) (< sig 1.0)) v/ERR-NUM
        (or (< x (first sorted)) (> x (peek sorted))) v/ERR-NA
        :else
        (let [r (if (some #(== % x) sorted)
                  (rank x)
                  (let [lo (last (filter #(< % x) sorted))
                        hi (first (filter #(> % x) sorted))
                        rlo (double (rank lo))]
                    (+ rlo (* (/ (- x lo) (- hi lo)) (- (double (rank hi)) rlo)))))]
          (u/result (truncate-digits r (long sig))))))))

(defn- nth-largest [xs k]
  (let [k (long (Math/ceil k)) n (count xs)]
    (guard (or (< k 1) (> k n)) (nth (sort > xs) (dec k)))))

(defn- nth-smallest [xs k]
  (let [k (long (Math/ceil k)) n (count xs)]
    (guard (or (< k 1) (> k n)) (nth (sort xs) (dec k)))))

(defn- rank
  [avg? x xs order]
  (let [x (double x)
        ties (count (filter #(== % x) xs))
        better (count (filter (if (zero? (double order)) #(> % x) #(< % x)) xs))]
    (cond (zero? ties) v/ERR-NA
          avg? (+ better (/ (inc ties) 2.0))
          :else (+ better 1.0))))

(defn- modes
  [xs]
  (let [freq (frequencies xs)
        top (apply max 0 (vals freq))]
    (when (> top 1)
      (->> xs distinct (filter #(= top (freq %))) vec))))

(defn- geomean [xs]
  (guard (or (empty? xs) (some #(<= % 0.0) xs))
    (Math/exp (/ (sum (map #(Math/log %) xs)) (count xs)))))

(defn- harmean [xs]
  (guard (or (empty? xs) (some #(<= % 0.0) xs))
    (/ (count xs) (sum (map #(/ 1.0 %) xs)))))

(defn- avedev [xs]
  (guard (empty? xs)
    (let [m (double (mean xs))] (/ (sum (map #(Math/abs (- % m)) xs)) (count xs)))))

(defn- trimmean [xs pct]
  (let [n (count xs)]
    (guard (or (zero? n) (< pct 0.0) (>= pct 1.0))
      (let [k (long (Math/floor (/ (* n pct) 2.0)))]
        (mean (->> xs sort (drop k) (take (- n (* 2 k)))))))))

(defn- standardized-moment-sum
  [xs ddof p]
  (u/check [s (stdev xs ddof)]
    (if (zero? s)
      v/ERR-DIV0
      (let [m (double (mean xs))] (sum (map #(Math/pow (/ (- % m) s) p) xs))))))

(defn- skew [xs]
  (let [n (count xs)]
    (if (< n 3)
      v/ERR-DIV0
      (u/check [m3 (standardized-moment-sum xs 1 3)]
        (* (/ n (* (- n 1.0) (- n 2.0))) m3)))))

(defn- skew-p [xs]
  (if (empty? xs)
    v/ERR-DIV0
    (u/check [m3 (standardized-moment-sum xs 0 3)] (/ m3 (count xs)))))

(defn- kurt [xs]
  (let [n (double (count xs))]
    (if (< n 4)
      v/ERR-DIV0
      (u/check [m4 (standardized-moment-sum xs 1 4)]
        (- (* (/ (* n (+ n 1)) (* (- n 1) (- n 2) (- n 3))) m4)
           (/ (* 3 (sq (- n 1))) (* (- n 2) (- n 3))))))))

(defn- moments
  [xs ys]
  (let [n (count xs)
        mx (/ (sum xs) n)
        my (/ (sum ys) n)]
    {:n n :mx mx :my my
     :sxx (sum (map #(sq (- % mx)) xs))
     :syy (sum (map #(sq (- % my)) ys))
     :sxy (sum (map #(* (- %1 mx) (- %2 my)) xs ys))}))

(defn- with-moments
  [xs ys min-n f]
  (if (< (count xs) (max 1 min-n)) v/ERR-DIV0 (f (moments xs ys))))

(defn- correl [xs ys]
  (with-moments xs ys 2
    (fn [{:keys [sxx syy sxy]}]
      (if (or (zero? sxx) (zero? syy)) v/ERR-DIV0 (/ sxy (Math/sqrt (* sxx syy)))))))

(defn- slope [ys xs]
  (with-moments xs ys 1
    (fn [{:keys [sxx sxy]}] (if (zero? sxx) v/ERR-DIV0 (/ sxy sxx)))))

(defn- intercept [ys xs]
  (with-moments xs ys 1
    (fn [{:keys [sxx sxy mx my]}] (if (zero? sxx) v/ERR-DIV0 (- my (* (/ sxy sxx) mx))))))

(defn- steyx [ys xs]
  (with-moments xs ys 3
    (fn [{:keys [n sxx syy sxy]}]
      (if (zero? sxx) v/ERR-DIV0 (Math/sqrt (/ (- syy (/ (sq sxy) sxx)) (- n 2.0)))))))

(defn- covariance [ddof]
  (fn [xs ys]
    (with-moments xs ys (inc ddof)
      (fn [{:keys [n sxy]}] (/ sxy (- n ddof))))))

(defn- forecast [ys xs x]
  (with-moments xs ys 1
    (fn [{:keys [sxx sxy mx my]}]
      (if (zero? sxx) v/ERR-DIV0 (let [b (/ sxy sxx)] (+ (- my (* b mx)) (* b x)))))))

(defn- count-impl
  [ctx & args]
  (double
   (reduce + (for [a args]
               (cond
                 (v/omitted? a) 1
                 (v/ref? a) (count (filter (comp number? peek) (ev/ref-cells ctx a)))
                 (v/array? a) (count (filter number? (u/flat a)))
                 (or (nil? a) (error? a)) 0
                 (error? (v/to-number a)) 0
                 :else 1)))))

(defn- counta-impl [ctx & args]
  (double
   (reduce + (for [a args]
               (cond
                 (v/ref? a) (count (ev/ref-cells ctx a))
                 (v/array? a) (count (remove nil? (u/flat a)))
                 (nil? a) 0
                 :else 1)))))

(defn- blank? [x] (or (nil? x) (= "" x)))

(defn- countblank-impl [ctx a]
  (double
   (cond
     (v/ref? a) (let [{:keys [r1 c1 r2 c2]} a]
                  (- (* (inc (- r2 r1)) (inc (- c2 c1)))
                     (count (remove (comp blank? peek) (ev/ref-cells ctx a)))))
     (v/array? a) (count (filter blank? (u/flat a)))
     :else (if (blank? a) 1 0))))

(defn- criterion [ctx c]
  (let [c (ev/deref-value ctx c)] (if (v/omitted? c) 0.0 c)))

(defn- resize
  [a [nr nc]]
  (cond
    (v/ref? a) (assoc a :r2 (+ (:r1 a) (dec nr)) :c2 (+ (:c1 a) (dec nc)))
    (v/array? a) (v/make-array2 nr nc (fn [r c] (get-in a [r c])))
    :else a))

(defn- conditional
  [ctx target pairs finish]
  (let [regions (map #(region ctx %) (cond->> (map first pairs) target (cons target)))]
    (cond
      (first-error regions) (first-error regions)
      (apply not= (map :dims regions)) v/ERR-VALUE
      :else
      (let [rows (vec (zip-regions regions))
            tail (- (cell-count (first regions)) (count rows))
            crits (map #(criterion ctx (second %)) pairs)]
        (ev/broadcast
         (fn [& cs]
           (let [preds (mapv #(u/criteria-pred (:locale ctx) %) cs)
                 ok? (fn [cells] (every? true? (map #(boolean (%1 %2)) preds cells)))
                 selected (if target
                            (keep (fn [row] (when (ok? (rest row)) (first row))) rows)
                            (filter ok? rows))]
             (finish selected (if (ok? (repeat nil)) tail 0))))
         crits)))))

(defn- criteria-pairs [args]
  (when (and (seq args) (even? (count args))) (partition 2 args)))

(defn- count-matches [selected tail] (double (+ (count selected) tail)))

(defn- numeric-finish
  [f empty]
  (fn [selected _]
    (or (first-error selected)
        (let [xs (map double (filter number? selected))]
          (if (empty? xs) empty (u/result (f xs)))))))

(defn- ifs-fn
  [finish]
  {:arity [3 255] :args :refs
   :impl (fn [ctx target & args]
           (if-let [pairs (criteria-pairs args)]
             (conditional ctx target pairs finish)
             v/ERR-VALUE))})

(defn- t-test
  [pairs samples tails type]
  (let [tails (u/trunc tails) type (u/trunc type)
        p-of (fn [t df] (if (or (Double/isNaN t) (Double/isInfinite t))
                          v/ERR-DIV0
                          (* tails (t-tail (Math/abs (double t)) df))))]
    (cond
      (not (#{1.0 2.0} tails)) v/ERR-NUM
      (== type 1.0)
      (let [[xs ys] @pairs d (mapv - xs ys) n (count d)]
        (if (< n 2)
          v/ERR-DIV0
          (p-of (/ (double (mean d)) (/ (double (stdev d 1)) (Math/sqrt n))) (dec n))))
      (#{2.0 3.0} type)
      (let [[as bs] @samples n1 (count as) n2 (count bs)]
        (if (or (< n1 2) (< n2 2))
          v/ERR-DIV0
          (let [m1 (double (mean as)) m2 (double (mean bs))
                v1 (double (variance as 1)) v2 (double (variance bs 1))]
            (if (== type 2.0)
              (let [df (+ n1 n2 -2)
                    pooled (/ (+ (* (dec n1) v1) (* (dec n2) v2)) df)]
                (p-of (/ (- m1 m2) (Math/sqrt (* pooled (+ (/ 1.0 n1) (/ 1.0 n2))))) df))
              (let [a (/ v1 n1) b (/ v2 n2)
                    df (/ (sq (+ a b)) (+ (/ (sq a) (dec n1)) (/ (sq b) (dec n2))))]
                (p-of (/ (- m1 m2) (Math/sqrt (+ a b))) df))))))
      :else v/ERR-NUM)))

(defn- f-test [xs ys]
  (if (or (< (count xs) 2) (< (count ys) 2))
    v/ERR-DIV0
    (let [v1 (double (variance xs 1)) v2 (double (variance ys 1))]
      (if (or (zero? v1) (zero? v2))
        v/ERR-DIV0
        (let [f (/ v1 v2) d1 (dec (count xs)) d2 (dec (count ys))]
          (* 2.0 (min (f-cdf f d1 d2) (f-sf f d1 d2))))))))

(defn- z-test [xs x & [sigma]]
  (let [n (count xs)]
    (if (zero? n)
      v/ERR-NA
      (u/check [s (if sigma (double sigma) (stdev xs 1))]
        (if (zero? s)
          v/ERR-DIV0
          (- 1.0 (norm-s-cdf (/ (- (double (mean xs)) x) (/ s (Math/sqrt n))))))))))

(defn- chisq-test-impl [ctx actual expected]
  (u/check [ra (region ctx actual)
            re (region ctx expected)
            _ (if (= (:dims ra) (:dims re)) true v/ERR-NA)
            tuples (zip-regions [ra re])
            _ (or (first-error (apply concat tuples)) true)]
    (let [[r c] (:dims ra)
          df (if (and (> r 1) (> c 1)) (* (dec r) (dec c)) (dec (max r c)))
          ps (filter (fn [[a e]] (and (number? a) (number? e))) tuples)]
      (cond
        (< df 1) v/ERR-NA
        (some (fn [[_ e]] (zero? (double e))) ps) v/ERR-DIV0
        :else (let [stat (sum (map (fn [[a e]] (/ (sq (- a e)) e)) ps))]
                (u/result (gamma-q (/ df 2.0) (/ stat 2.0))))))))

(defn- prob [xs ps lower upper]
  (let [upper (or upper lower)]
    (cond
      (some #(or (< % 0.0) (> % 1.0)) ps) v/ERR-NUM
      (> (Math/abs (- (sum ps) 1.0)) 1e-7) v/ERR-NUM
      :else (sum (map (fn [x p] (if (<= lower x upper) p 0.0)) xs ps)))))

(defn- frequency-impl [ctx data bins]
  (u/check [xs (sample ctx data)
            bs (sample ctx bins)]
    (let [order (sort-by bs (range (count bs)))
          slot (fn [x] (or (first (filter #(<= x (nth bs %)) order)) (count bs)))
          counts (frequencies (map slot xs))]
      (mapv (fn [i] [(double (get counts i 0))]) (range (inc (count bs)))))))

(defn- numeric-grid
  [a]
  (let [cells (u/flat a)]
    (or (first-error cells)
        (if (every? number? cells) (v/amap2 double a) v/ERR-VALUE))))

(defn- invert-matrix
  [m]
  (let [n (count m)
        aug (mapv (fn [i row] (into (vec row) (map #(if (= i %) 1.0 0.0) (range n)))) (range n) m)]
    (loop [a aug col 0]
      (if (= col n)
        (mapv #(subvec % n) a)
        (let [piv (apply max-key #(Math/abs (double (get-in a [% col]))) (range col n))
              pv (double (get-in a [piv col]))]
          (if (< (Math/abs pv) 1e-12)
            nil
            (let [a (assoc a piv (a col) col (mapv #(/ % pv) (a piv)))
                  prow (a col)]
              (recur (mapv (fn [i row]
                             (if (= i col)
                               row
                               (let [f (double (nth row col))] (mapv #(- %1 (* f %2)) row prow))))
                           (range n) a)
                     (inc col)))))))))

(defn- design
  [ky kx]
  (u/check [y (numeric-grid ky)]
    (let [[r c] [(v/rows y) (v/cols y)]
          orient (if (and (= r 1) (> c 1)) :row :col)
          ys (vec (u/flat y))
          n (count ys)]
      (if (nil? kx)
        {:y ys :x (mapv #(vector (double (inc %))) (range n)) :orient orient}
        (u/check [x (numeric-grid kx)]
          (let [[xr xc] [(v/rows x) (v/cols x)]]
            (cond
              (= [r c] [xr xc]) {:y ys :x (mapv vector (u/flat x)) :orient orient}
              (and (= orient :col) (= xr n)) {:y ys :x x :orient orient}
              (and (= orient :row) (= xc n)) {:y ys :x (v/transpose x) :orient orient}
              :else v/ERR-REF)))))))

(defn- dot ^double [a b] (reduce + 0.0 (map * a b)))

(defn- ols
  [{:keys [y x]} const?]
  (let [n (count y)
        k (count (first x))
        xm (if const? (mapv #(/ (sum (map (fn [row] (nth row %)) x)) n) (range k))
             (vec (repeat k 0.0)))
        ym (if const? (/ (sum y) n) 0.0)
        xc (mapv #(mapv - % xm) x)
        yc (mapv #(- % ym) y)
        s (vec (for [i (range k)]
                 (vec (for [j (range k)] (sum (map #(* (nth % i) (nth % j)) xc))))))
        sinv (invert-matrix s)
        df (- n k (if const? 1 0))]
    (cond
      (nil? sinv) v/ERR-NUM
      (< df 0) v/ERR-NUM
      :else
      (let [xty (mapv (fn [i] (sum (map #(* (nth %1 i) %2) xc yc))) (range k))
            b (mapv #(dot % xty) sinv)
            b0 (if const? (- ym (dot b xm)) 0.0)
            ssresid (sum (map (fn [row yv] (sq (- yv b0 (dot b row)))) x y))
            sstot (sum (map #(sq (- % ym)) y))
            ssreg (- sstot ssresid)
            sey (Math/sqrt (/ ssresid df))]
        {:b b :b0 b0 :k k :df df :ssresid ssresid :ssreg ssreg :sey sey
         :r2 (/ ssreg sstot)
         :f (/ (/ ssreg k) (/ ssresid df))
         :se (mapv #(* sey (Math/sqrt (get-in sinv [% %]))) (range k))
         :se0 (when const? (* sey (Math/sqrt (+ (/ 1.0 n) (dot xm (mapv #(dot % xm) sinv))))))}))))

(defn- linest-array
  [{:keys [b b0 k df ssresid ssreg sey r2 f se se0]} stats? coef]
  (let [width (inc k)
        row (fn [& xs] (vec (take width (concat (map u/result xs) (repeat v/ERR-NA)))))
        first-row (mapv (comp u/result coef) (conj (vec (rseq b)) b0))]
    (if stats?
      [first-row
       (conj (mapv u/result (rseq se)) (if se0 (u/result se0) v/ERR-NA))
       (row r2 sey)
       (row f (double df))
       (row ssreg ssresid)]
      [first-row])))

(defn- log-y
  [d]
  (if (some #(<= % 0.0) (:y d)) v/ERR-NUM (update d :y #(mapv (fn [y] (Math/log y)) %))))

(defn- regression-fn
  [log?]
  {:arity [1 4]
   :impl (fn [& args]
           (let [[ky kx const stats] (padded args 4)
                 const? (u/bool const true)
                 stats? (u/bool stats false)]
             (u/check [_ const? _ stats?
                       d (design (u/->2d ky) (u/->2d kx))
                       d (if log? (log-y d) d)
                       fit (ols d const?)]
               (linest-array fit stats? (if log? #(Math/exp %) identity)))))})

(defn- predict
  [{:keys [b b0 k]} d new-x orient]
  (let [f (fn [row] (+ b0 (dot b row)))]
    (if (nil? new-x)
      (let [ys (mapv f (:x d))]
        (if (= orient :row) [ys] (mapv vector ys)))
      (u/check [nx (numeric-grid new-x)]
        (cond
          (= k 1) (v/amap2 #(f [%]) nx)
          (and (= orient :col) (= (v/cols nx) k)) (mapv #(vector (f %)) nx)
          (and (= orient :row) (= (v/rows nx) k)) [(mapv f (v/transpose nx))]
          :else v/ERR-REF)))))

(defn- trend-fn
  [log?]
  {:arity [1 4]
   :impl (fn [& args]
           (let [[ky kx nx const] (padded args 4)
                 const? (u/bool const true)]
             (u/check [_ const?
                       d (design (u/->2d ky) (u/->2d kx))
                       d' (if log? (log-y d) d)
                       fit (ols d' const?)
                       out (predict fit d (u/->2d nx) (:orient d))]
               (v/amap2 #(u/result (if log? (Math/exp %) %)) out))))})

(defn- norm-dist [x m sd c]
  (guard (<= sd 0.0)
    (let [z (/ (- x m) sd)]
      (if (cumulative? c) (norm-s-cdf z) (/ (norm-s-pdf z) sd)))))

(defn- norm-inv [p m sd]
  (guard (or (<= p 0.0) (>= p 1.0) (<= sd 0.0)) (+ m (* sd (norm-s-inv p)))))

(defn- lognorm-dist [x m sd c]
  (guard (or (<= x 0.0) (<= sd 0.0))
    (let [z (/ (- (Math/log x) m) sd)]
      (if (cumulative? c) (norm-s-cdf z) (/ (norm-s-pdf z) (* x sd))))))

(defn- lognorm-inv [p m sd]
  (u/check [z (norm-inv p m sd)] (Math/exp z)))

(defn- expon-dist [x lambda c]
  (guard (or (< x 0.0) (<= lambda 0.0))
    (if (cumulative? c) (- (Math/expm1 (- (* lambda x)))) (* lambda (Math/exp (- (* lambda x)))))))

(defn- poisson-dist [x m c]
  (let [x (u/trunc x)]
    (guard (or (< x 0.0) (< m 0.0))
      (if (cumulative? c) (gamma-q (+ x 1.0) m) (poisson-pmf x m)))))

(defn- binom-dist [s n p c]
  (let [s (u/trunc s) n (u/trunc n)]
    (guard (or (< s 0.0) (> s n) (< p 0.0) (> p 1.0))
      (if (cumulative? c) (binom-cdf s n p) (binom-pmf s n p)))))

(defn- binom-dist-range [n p s & [s2]]
  (let [n (u/trunc n) s (u/trunc s) s2 (if s2 (u/trunc s2) s)]
    (guard (or (< n 0.0) (< p 0.0) (> p 1.0) (< s 0.0) (> s n) (< s2 s) (> s2 n))
      (sum (map #(binom-pmf % n p) (range s (inc s2)))))))

(defn- binom-inv [n p alpha]
  (let [n (u/trunc n)]
    (guard (or (< n 0.0) (< p 0.0) (> p 1.0) (< alpha 0.0) (> alpha 1.0))
      (loop [k 0.0 acc 0.0]
        (let [acc (+ acc (binom-pmf k n p))]
          (if (or (>= acc (- alpha 1e-14)) (>= k n)) k (recur (inc k) acc)))))))

(defn- negbinom-dist [f s p c]
  (let [f (u/trunc f) s (u/trunc s)]
    (guard (or (< f 0.0) (< s 1.0) (< p 0.0) (> p 1.0))
      (if (cumulative? c)
        (beta-reg s (+ f 1.0) p)
        (Math/exp (+ (log-choose (+ f s -1.0) (- s 1.0)) (* s (Math/log p))
                     (* f (Math/log1p (- p)))))))))

(defn- hypgeom-dist [s n big-m big-n c]
  (let [[s n big-m big-n] (map u/trunc [s n big-m big-n])
        lo (max 0.0 (- n (- big-n big-m)))]
    (guard (or (< s 0.0) (> s (min n big-m)) (< s lo)
               (<= n 0.0) (> n big-n) (<= big-m 0.0) (> big-m big-n) (<= big-n 0.0))
      (if (cumulative? c)
        (min 1.0 (sum (map #(hypgeom-pmf % n big-m big-n) (range lo (inc s)))))
        (hypgeom-pmf s n big-m big-n)))))

(defn- gamma-dist [x a b c]
  (guard (or (< x 0.0) (<= a 0.0) (<= b 0.0))
    (if (cumulative? c) (gamma-p a (/ x b)) (gamma-pdf x a b))))

(defn- gamma-dist-inv [p a b]
  (guard (or (< p 0.0) (>= p 1.0) (<= a 0.0) (<= b 0.0))
    (if (zero? p) 0.0 (* b (gamma-inv p (- 1.0 p) a)))))

(defn- beta-dist [x a b c lo hi]
  (guard (or (<= a 0.0) (<= b 0.0) (>= lo hi) (< x lo) (> x hi))
    (let [z (/ (- x lo) (- hi lo))]
      (if (cumulative? c) (beta-reg a b z) (/ (beta-pdf z a b) (- hi lo))))))

(defn- beta-dist-inv [p a b lo hi]
  (guard (or (<= p 0.0) (> p 1.0) (<= a 0.0) (<= b 0.0) (>= lo hi))
    (+ lo (* (- hi lo) (if (== p 1.0) 1.0 (beta-inv p a b))))))

(defn- bad-df? [df] (or (< df 1.0) (> df 1e10)))

(defn- chisq-dist [x df c]
  (let [df (u/trunc df)]
    (guard (or (< x 0.0) (bad-df? df))
      (if (cumulative? c) (gamma-p (/ df 2.0) (/ x 2.0)) (gamma-pdf x (/ df 2.0) 2.0)))))

(defn- chisq-dist-rt [x df]
  (let [df (u/trunc df)]
    (guard (or (< x 0.0) (bad-df? df)) (gamma-q (/ df 2.0) (/ x 2.0)))))

(defn- chisq-inv [p df]
  (let [df (u/trunc df)]
    (guard (or (< p 0.0) (>= p 1.0) (bad-df? df))
      (if (zero? p) 0.0 (* 2.0 (gamma-inv p (- 1.0 p) (/ df 2.0)))))))

(defn- chisq-inv-rt [q df]
  (let [df (u/trunc df)]
    (guard (or (<= q 0.0) (> q 1.0) (bad-df? df))
      (if (== q 1.0) 0.0 (* 2.0 (gamma-inv (- 1.0 q) q (/ df 2.0)))))))

(defn- t-dist [x df c]
  (let [df (u/trunc df)]
    (guard (< df 1.0)
      (if (cumulative? c) (- 1.0 (t-tail x df)) (t-pdf x df)))))

(defn- t-dist-2t [x df]
  (let [df (u/trunc df)]
    (guard (or (< x 0.0) (< df 1.0)) (min 1.0 (* 2.0 (t-tail x df))))))

(defn- t-dist-rt [x df]
  (let [df (u/trunc df)] (guard (< df 1.0) (t-tail x df))))

(defn- tdist-legacy [x df tails]
  (let [tails (u/trunc tails)]
    (guard (or (< x 0.0) (not (#{1.0 2.0} tails)))
      (u/check [p (t-dist-rt x df)] (* tails p)))))

(defn- t-inv-left [p df]
  (let [df (u/trunc df)] (guard (or (<= p 0.0) (>= p 1.0) (< df 1.0)) (t-inv p df))))

(defn- t-inv-2t [p df]
  (let [df (u/trunc df)]
    (guard (or (<= p 0.0) (> p 1.0) (< df 1.0))
      (if (== p 1.0) 0.0 (- (t-inv (/ p 2.0) df))))))

(defn- f-dist [x d1 d2 c]
  (let [d1 (u/trunc d1) d2 (u/trunc d2)]
    (guard (or (< x 0.0) (bad-df? d1) (bad-df? d2))
      (if (cumulative? c) (f-cdf x d1 d2) (f-pdf x d1 d2)))))

(defn- f-dist-rt [x d1 d2]
  (let [d1 (u/trunc d1) d2 (u/trunc d2)]
    (guard (or (< x 0.0) (bad-df? d1) (bad-df? d2)) (f-sf x d1 d2))))

(defn- f-dist-inv [p d1 d2]
  (let [d1 (u/trunc d1) d2 (u/trunc d2)]
    (guard (or (< p 0.0) (>= p 1.0) (bad-df? d1) (bad-df? d2))
      (if (zero? p) 0.0 (f-inv p (- 1.0 p) d1 d2)))))

(defn- f-dist-inv-rt [q d1 d2]
  (let [d1 (u/trunc d1) d2 (u/trunc d2)]
    (guard (or (<= q 0.0) (> q 1.0) (bad-df? d1) (bad-df? d2))
      (if (== q 1.0) 0.0 (f-inv (- 1.0 q) q d1 d2)))))

(defn- weibull-dist [x a b c]
  (guard (or (< x 0.0) (<= a 0.0) (<= b 0.0))
    (let [z (Math/pow (/ x b) a)]
      (if (cumulative? c)
        (- (Math/expm1 (- z)))
        (* (/ a (Math/pow b a)) (Math/pow x (- a 1.0)) (Math/exp (- z)))))))

(defn- confidence-norm [alpha sd n]
  (let [n (u/trunc n)]
    (guard (or (<= alpha 0.0) (>= alpha 1.0) (<= sd 0.0) (< n 1.0))
      (/ (* (norm-s-inv (- 1.0 (/ alpha 2.0))) sd) (Math/sqrt n)))))

(defn- confidence-t [alpha sd n]
  (let [n (u/trunc n)]
    (cond
      (or (<= alpha 0.0) (>= alpha 1.0) (<= sd 0.0) (< n 1.0)) v/ERR-NUM
      (== n 1.0) v/ERR-DIV0
      :else (/ (* (- (t-inv (/ alpha 2.0) (dec n))) sd) (Math/sqrt n)))))

(defn- gamma-excel [x]
  (guard (and (<= x 0.0) (integer-valued? x)) (gamma-fn x)))

(defn- gammaln [x] (guard (<= x 0.0) (log-gamma x)))

(def ^:private descriptive
  (let [avg (fn [xs] (mean xs))
        maxf (fn [xs] (if (empty? xs) 0.0 (apply max xs)))
        minf (fn [xs] (if (empty? xs) 0.0 (apply min xs)))
        var-s #(variance % 1) var-p #(variance % 0)
        sd-s #(stdev % 1) sd-p #(stdev % 0)
        mode-sngl #(or (first (modes %)) v/ERR-NA)]
    {"AVERAGE"  (agg-fn avg)
     "AVERAGEA" (agg-fn avg a-opts)
     "MAX"      (agg-fn maxf)
     "MAXA"     (agg-fn maxf a-opts)
     "MIN"      (agg-fn minf)
     "MINA"     (agg-fn minf a-opts)
     "MEDIAN"   (agg-fn median)
     "MODE"      (agg-fn mode-sngl)
     "MODE.SNGL" (agg-fn mode-sngl)
     "MODE.MULT" {:arity [1 255] :args :refs
                  :impl (fn [ctx & args]
                          (u/with-collected [xs (u/collect ctx args)]
                            (if-let [ms (modes xs)] (mapv vector ms) v/ERR-NA)))}
     "GEOMEAN"  (agg-fn geomean)
     "HARMEAN"  (agg-fn harmean)
     "AVEDEV"   (agg-fn avedev)
     "DEVSQ"    (agg-fn #(if (empty? %) v/ERR-NUM (devsq %)))
     "KURT"     (agg-fn kurt)
     "SKEW"     (agg-fn skew)
     "SKEW.P"   (agg-fn skew-p)
     "STDEV"    (agg-fn sd-s)
     "STDEV.S"  (agg-fn sd-s)
     "STDEVA"   (agg-fn sd-s a-opts)
     "STDEVP"   (agg-fn sd-p)
     "STDEV.P"  (agg-fn sd-p)
     "STDEVPA"  (agg-fn sd-p a-opts)
     "VAR"      (agg-fn var-s)
     "VAR.S"    (agg-fn var-s)
     "VARA"     (agg-fn var-s a-opts)
     "VARP"     (agg-fn var-p)
     "VAR.P"    (agg-fn var-p)
     "VARPA"    (agg-fn var-p a-opts)
     "COUNT"      {:arity [1 255] :args :refs :impl count-impl}
     "COUNTA"     {:arity [1 255] :args :refs :impl counta-impl}
     "COUNTBLANK" {:arity [1 1] :args :refs :impl countblank-impl}}))

(def ^:private conditionals
  {"COUNTIF"   {:arity [2 2] :args :refs
                :impl (fn [ctx rng crit] (conditional ctx nil [[rng crit]] count-matches))}
   "COUNTIFS"  {:arity [2 254] :args :refs
                :impl (fn [ctx & args]
                        (if-let [pairs (criteria-pairs args)]
                          (conditional ctx nil pairs count-matches)
                          v/ERR-VALUE))}
   "AVERAGEIF" {:arity [2 3] :args :refs
                :impl (fn [ctx rng crit & [avg]]
                        (let [r (region ctx rng)
                              target (if (and avg (not (v/omitted? avg))) (resize avg (:dims r))
                                       rng)]
                          (if (error? r)
                            r
                            (conditional ctx target [[rng crit]]
                                         (numeric-finish #(/ (sum %) (count %)) v/ERR-DIV0)))))}
   "AVERAGEIFS" (ifs-fn (numeric-finish #(/ (sum %) (count %)) v/ERR-DIV0))
   "MAXIFS"     (ifs-fn (numeric-finish #(apply max %) 0.0))
   "MINIFS"     (ifs-fn (numeric-finish #(apply min %) 0.0))})

(def ^:private order-stats
  (let [rank-spec (fn [avg?]
                    {:arity [2 3] :args :refs
                     :impl (fn [ctx x ref & [order]]
                             (u/with-collected [xs (sample ctx ref)]
                               (lift-scalars ctx #(rank avg? %1 xs %2) [x (or order v/omitted)]
                                             [0.0 0.0])))})
        pct-inc (data-fn [2 2] 1 percentile-inc)
        pct-exc (data-fn [2 2] 1 percentile-exc)
        q-inc (data-fn [2 2] 1 (quartile percentile-inc 0 4))
        prank-inc (data-fn [2 3] 1 (percentrank (fn [less n] (if (= n 1) 1.0 (/ less (dec n)))))
                           :defaults [0.0 nil])
        prank-exc (data-fn [2 3] 1 (percentrank (fn [less n] (/ (inc less) (inc n))))
                           :defaults [0.0 nil])]
    {"LARGE"            (data-fn [2 2] 1 nth-largest)
     "SMALL"            (data-fn [2 2] 1 nth-smallest)
     "TRIMMEAN"         (data-fn [2 2] 1 trimmean)
     "RANK"             (rank-spec false)
     "RANK.EQ"          (rank-spec false)
     "RANK.AVG"         (rank-spec true)
     "PERCENTILE"       pct-inc
     "PERCENTILE.INC"   pct-inc
     "PERCENTILE.EXC"   pct-exc
     "QUARTILE"         q-inc
     "QUARTILE.INC"     q-inc
     "QUARTILE.EXC"     (data-fn [2 2] 1 (quartile percentile-exc 1 3))
     "PERCENTRANK"      prank-inc
     "PERCENTRANK.INC"  prank-inc
     "PERCENTRANK.EXC"  prank-exc
     "FREQUENCY"        {:arity [2 2] :args :refs :impl frequency-impl}}))

(def ^:private paired-stats
  (let [correl-spec (pair-fn correl)
        forecast-spec {:arity [3 3] :args :refs
                       :impl (fn [ctx x ky kx]
                               (let [ps (paired ctx kx ky)]
                                 (if (error? ps)
                                   ps
                                   (let [[xs ys] ps]
                                     (lift-scalars ctx #(forecast ys xs %) [x] [])))))}
        covar-p (pair-fn (covariance 0))]
    {"CORREL"           correl-spec
     "PEARSON"          correl-spec
     "RSQ"              (pair-fn #(u/check [r (correl %1 %2)] (sq r)))
     "COVAR"            covar-p
     "COVARIANCE.P"     covar-p
     "COVARIANCE.S"     (pair-fn (covariance 1))
     "SLOPE"            (pair-fn slope)
     "INTERCEPT"        (pair-fn intercept)
     "STEYX"            (pair-fn steyx)
     "FORECAST"         forecast-spec
     "FORECAST.LINEAR"  forecast-spec
     "PROB"             (pair-fn [3 4] (fn [xs ps lower & [upper]] (prob xs ps lower upper))
                                 [0.0 nil])
     "LINEST"           (regression-fn false)
     "LOGEST"           (regression-fn true)
     "TREND"            (trend-fn false)
     "GROWTH"           (trend-fn true)}))

(def ^:private tests
  (let [t-test-spec {:arity [4 4] :args :refs
                     :impl (fn [ctx a b tails type]
                             (let [pairs (delay (paired ctx a b))
                                   samples (delay (let [as (sample ctx a) bs (sample ctx b)]
                                                    (or (first-error [as bs]) [as bs])))
                                   checked (fn [d] (when (error? @d) @d))]
                               (lift-scalars ctx
                                             (fn [tails type]
                                               (or (if (== 1.0 (u/trunc type)) (checked pairs)
                                                     (checked samples))
                                                   (t-test pairs samples tails type)))
                                             [tails type] [])))}
        f-test-spec (data-fn [2 2] 2 f-test)
        z-test-spec (data-fn [2 3] 1 z-test :defaults [0.0 nil])
        chisq-test-spec {:arity [2 2] :args :refs :impl chisq-test-impl}]
    {"T.TEST" t-test-spec "TTEST" t-test-spec
     "F.TEST" f-test-spec "FTEST" f-test-spec
     "Z.TEST" z-test-spec "ZTEST" z-test-spec
     "CHISQ.TEST" chisq-test-spec "CHITEST" chisq-test-spec}))

(def ^:private scalar-stats
  {"STANDARDIZE"     (num-fn [3 3] (fn [x m sd] (guard (<= sd 0.0) (/ (- x m) sd))))
   "FISHER"          (num-fn [1 1] (fn [x] (guard (or (<= x -1.0) (>= x 1.0))
                                             (* 0.5 (Math/log (/ (+ 1.0 x) (- 1.0 x)))))))
   "FISHERINV"       (num-fn [1 1] #(Math/tanh %))
   "GAMMA"           (num-fn [1 1] gamma-excel)
   "GAMMALN"         (num-fn [1 1] gammaln)
   "GAMMALN.PRECISE" (num-fn [1 1] gammaln)
   "PHI"             (num-fn [1 1] norm-s-pdf)
   "GAUSS"           (num-fn [1 1] #(- (norm-s-cdf %) 0.5))})

(def ^:private distributions
  {"NORM.DIST"     (num-fn [4 4] norm-dist)
   "NORMDIST"      (num-fn [4 4] norm-dist)
   "NORM.INV"      (num-fn [3 3] norm-inv)
   "NORMINV"       (num-fn [3 3] norm-inv)
   "NORM.S.DIST"   (num-fn [2 2] #(norm-dist %1 0.0 1.0 %2))
   "NORMSDIST"     (num-fn [1 1] norm-s-cdf)
   "NORM.S.INV"    (num-fn [1 1] #(norm-inv % 0.0 1.0))
   "NORMSINV"      (num-fn [1 1] #(norm-inv % 0.0 1.0))
   "LOGNORM.DIST"  (num-fn [4 4] lognorm-dist)
   "LOGNORMDIST"   (num-fn [3 3] #(lognorm-dist %1 %2 %3 1.0))
   "LOGNORM.INV"   (num-fn [3 3] lognorm-inv)
   "LOGINV"        (num-fn [3 3] lognorm-inv)
   "EXPON.DIST"    (num-fn [3 3] expon-dist)
   "EXPONDIST"     (num-fn [3 3] expon-dist)
   "POISSON.DIST"  (num-fn [3 3] poisson-dist)
   "POISSON"       (num-fn [3 3] poisson-dist)
   "BINOM.DIST"    (num-fn [4 4] binom-dist)
   "BINOMDIST"     (num-fn [4 4] binom-dist)
   "BINOM.DIST.RANGE" {:arity [3 4] :lift :all
                       :impl (fn [& args]
                               (let [ns (coerce-scalars args [0.0 0.0 0.0 nil])]
                                 (or (first-error ns) (u/result (apply binom-dist-range ns)))))}
   "BINOM.INV"     (num-fn [3 3] binom-inv)
   "CRITBINOM"     (num-fn [3 3] binom-inv)
   "NEGBINOM.DIST" (num-fn [4 4] negbinom-dist)
   "NEGBINOMDIST"  (num-fn [3 3] #(negbinom-dist %1 %2 %3 0.0))
   "HYPGEOM.DIST"  (num-fn [5 5] hypgeom-dist)
   "HYPGEOMDIST"   (num-fn [4 4] #(hypgeom-dist %1 %2 %3 %4 0.0))
   "GAMMA.DIST"    (num-fn [4 4] gamma-dist)
   "GAMMADIST"     (num-fn [4 4] gamma-dist)
   "GAMMA.INV"     (num-fn [3 3] gamma-dist-inv)
   "GAMMAINV"      (num-fn [3 3] gamma-dist-inv)
   "BETA.DIST"     (num-fn [4 6] beta-dist [0 0 0 0 0 1])
   "BETADIST"      (num-fn [3 5] #(beta-dist %1 %2 %3 1.0 %4 %5) [0 0 0 0 1])
   "BETA.INV"      (num-fn [3 5] beta-dist-inv [0 0 0 0 1])
   "BETAINV"       (num-fn [3 5] beta-dist-inv [0 0 0 0 1])
   "CHISQ.DIST"    (num-fn [3 3] chisq-dist)
   "CHISQ.DIST.RT" (num-fn [2 2] chisq-dist-rt)
   "CHIDIST"       (num-fn [2 2] chisq-dist-rt)
   "CHISQ.INV"     (num-fn [2 2] chisq-inv)
   "CHISQ.INV.RT"  (num-fn [2 2] chisq-inv-rt)
   "CHIINV"        (num-fn [2 2] chisq-inv-rt)
   "T.DIST"        (num-fn [3 3] t-dist)
   "T.DIST.2T"     (num-fn [2 2] t-dist-2t)
   "T.DIST.RT"     (num-fn [2 2] t-dist-rt)
   "TDIST"         (num-fn [3 3] tdist-legacy)
   "T.INV"         (num-fn [2 2] t-inv-left)
   "T.INV.2T"      (num-fn [2 2] t-inv-2t)
   "TINV"          (num-fn [2 2] t-inv-2t)
   "F.DIST"        (num-fn [4 4] f-dist)
   "F.DIST.RT"     (num-fn [3 3] f-dist-rt)
   "FDIST"         (num-fn [3 3] f-dist-rt)
   "F.INV"         (num-fn [3 3] f-dist-inv)
   "F.INV.RT"      (num-fn [3 3] f-dist-inv-rt)
   "FINV"          (num-fn [3 3] f-dist-inv-rt)
   "WEIBULL.DIST"  (num-fn [4 4] weibull-dist)
   "WEIBULL"       (num-fn [4 4] weibull-dist)
   "CONFIDENCE.NORM" (num-fn [3 3] confidence-norm)
   "CONFIDENCE"      (num-fn [3 3] confidence-norm)
   "CONFIDENCE.T"    (num-fn [3 3] confidence-t)})

(def functions
  (merge descriptive conditionals order-stats paired-stats tests scalar-stats distributions))
