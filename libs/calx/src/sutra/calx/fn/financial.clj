(ns sutra.calx.fn.financial
  (:refer-clojure :exclude [num])
  (:require [sutra.calx.value :as v :refer [error?]]
            [sutra.calx.eval :as ev]
            [sutra.calx.fn.util :as u]
            [sutra.calx.fn.daycount :as dc]))

(set! *warn-on-reflection* true)

(defn- scalar
  [[lo hi] coercers f]
  {:arity [lo hi]
   :lift  :all
   :impl  (fn [& args]
            (let [xs (mapv (fn [coerce a] (coerce a)) coercers (concat args (repeat v/omitted)))]
              (or (some #(when (error? %) %) xs)
                  (u/result (apply f xs)))))})

(defn- num
  ([] (num 0.0))
  ([default] #(u/num % default)))

(def ^:private date dc/serial)

(defn- int-arg [default] #(u/int* % default))

(defn- bool-arg [default] #(u/bool % default))

(defn- pay-type [t] (if (zero? t) 0 1))

(defn- sum [xs] (reduce + 0.0 xs))

(defn- growth [r n] (Math/pow (+ 1.0 r) n))

(defn- newton
  [f guess]
  (loop [x guess, i 0]
    (if (>= i 100)
      v/ERR-NUM
      (let [[y dy] (f x)
            x' (- x (/ y dy))]
        (cond
          (or (Double/isNaN x') (Double/isInfinite x') (<= x' -1.0)) v/ERR-NUM
          (< (abs (- x' x)) 1e-10) x'
          :else (recur x' (inc i)))))))

(defn- numeric-derivative
  [g]
  (fn [x]
    (let [h (* 1e-6 (max 1.0 (abs x)))]
      [(g x) (/ (- (g (+ x h)) (g (- x h))) (* 2.0 h))])))

(defn- fv* [r n pmt pv type]
  (let [t (pay-type type)]
    (if (zero? r)
      (- (+ pv (* pmt n)))
      (let [q (growth r n)]
        (- (+ (* pv q) (* pmt (+ 1.0 (* r t)) (/ (- q 1.0) r))))))))

(defn- pv* [r n pmt fv type]
  (let [t (pay-type type)]
    (if (zero? r)
      (- (+ fv (* pmt n)))
      (let [q (growth r n)]
        (/ (- (+ fv (* pmt (+ 1.0 (* r t)) (/ (- q 1.0) r)))) q)))))

(defn- pmt* [r n pv fv type]
  (let [t (pay-type type)]
    (cond
      (zero? n) v/ERR-NUM
      (zero? r) (- (/ (+ pv fv) n))
      :else (let [q (growth r n)]
              (/ (- (* r (+ fv (* pv q))))
                 (* (+ 1.0 (* r t)) (- q 1.0)))))))

(defn- nper* [r pmt pv fv type]
  (let [t (pay-type type)]
    (if (zero? r)
      (if (zero? pmt) v/ERR-NUM (- (/ (+ pv fv) pmt)))
      (let [k (/ (* pmt (+ 1.0 (* r t))) r)]
        (/ (Math/log (/ (- k fv) (+ k pv))) (Math/log (+ 1.0 r)))))))

(defn- ipmt* [r per n pv fv type]
  (let [t (pay-type type)]
    (if (or (< per 1) (> per n))
      v/ERR-NUM
      (u/check [pmt (pmt* r n pv fv t)]
        (* r (cond
               (< per 2) (if (= t 1) 0.0 (- pv))
               (= t 1)   (- (fv* r (- per 2) pmt pv 1) pmt)
               :else     (fv* r (dec per) pmt pv 0)))))))

(defn- ppmt* [r per n pv fv type]
  (u/check [pmt (pmt* r n pv fv type)
            ipmt (ipmt* r per n pv fv type)]
    (- pmt ipmt)))

(defn- rate* [n pmt pv fv type guess]
  (let [t (pay-type type)
        f (fn [r]
            (if (< (abs r) 1e-12)
              (+ pv (* pmt n) fv)
              (let [q (growth r n)]
                (+ (* pv q) (* pmt (+ 1.0 (* r t)) (/ (- q 1.0) r)) fv))))]
    (if (<= n 0) v/ERR-NUM (newton (numeric-derivative f) guess))))

(defn- cumulative
  [per-fn]
  (fn [r n pv start end type]
    (if (or (<= r 0) (<= n 0) (<= pv 0) (< start 1) (< end start) (> end n) (not (#{0 1} type)))
      v/ERR-NUM
      (sum (map #(per-fn r % n pv 0.0 type) (range start (inc end)))))))

(defn- ispmt* [r per n pv]
  (if (zero? n) v/ERR-DIV0 (* pv r (- (/ per n) 1.0))))

(defn- pduration* [r pv fv]
  (if (or (<= r 0) (<= pv 0) (<= fv 0))
    v/ERR-NUM
    (/ (- (Math/log fv) (Math/log pv)) (Math/log (+ 1.0 r)))))

(defn- rri* [n pv fv]
  (if (or (<= n 0) (zero? pv)) v/ERR-NUM (- (Math/pow (/ fv pv) (/ 1.0 n)) 1.0)))

(defn- discounted-sum
  [r cfs offset]
  (sum (map-indexed (fn [i cf] (/ cf (growth r (+ i offset)))) cfs)))

(defn- mixed-signs? [xs] (and (some pos? xs) (some neg? xs)))

(defn- npv-impl [ctx rate & values]
  (u/check [r (u/num (ev/deref-value ctx rate))
            cfs (u/collect ctx values)]
    (if (== r -1.0) v/ERR-DIV0 (u/result (discounted-sum r cfs 1)))))

(defn- irr-impl [ctx values & [guess]]
  (u/check [cfs (u/collect ctx [values])
            guess (u/num (if guess (ev/deref-value ctx guess) v/omitted) 0.1)]
    (if-not (mixed-signs? cfs)
      v/ERR-NUM
      (u/result
       (newton (fn [r]
                 [(discounted-sum r cfs 0)
                  (sum (map-indexed (fn [i cf] (/ (* (- i) cf) (growth r (inc i)))) cfs))])
               guess)))))

(defn- mirr-impl [ctx values finance-rate reinvest-rate]
  (u/check [cfs (u/collect ctx [values])
            fr (u/num (ev/deref-value ctx finance-rate))
            rr (u/num (ev/deref-value ctx reinvest-rate))]
    (let [n (count cfs)
          inflows (- (* (discounted-sum rr (map #(max % 0.0) cfs) 1) (growth rr n)))
          outflows (* (discounted-sum fr (map #(min % 0.0) cfs) 1) (+ 1.0 fr))]
      (if (or (< n 2) (zero? inflows) (zero? outflows))
        v/ERR-DIV0
        (u/result (- (Math/pow (/ inflows outflows) (/ 1.0 (dec n))) 1.0))))))

(defn- dated-flows
  [values dates]
  (let [vs (u/flat (u/->2d values))
        ds (u/flat (u/->2d dates))]
    (if (not= (count vs) (count ds))
      v/ERR-NUM
      (u/check [cfs
                (reduce (fn [acc x] (if (number? x) (conj acc (double x)) (reduced v/ERR-VALUE))) []
                        vs)
                days
                (reduce (fn [acc d]
                          (let [s (dc/serial d)] (if (error? s) (reduced s) (conj acc s)))) [] ds)]
        (let [d0 (first days)]
          (if (some #(< % d0) days)
            v/ERR-NUM
            (mapv (fn [cf d] [cf (/ (- d d0) 365.0)]) cfs days)))))))

(defn- xnpv-sum [r flows]
  (sum (map (fn [[cf t]] (/ cf (growth r t))) flows)))

(defn- xnpv* [rate values dates]
  (u/check [r (u/num rate)
            flows (dated-flows values dates)]
    (if (<= r -1.0) v/ERR-NUM (u/result (xnpv-sum r flows)))))

(defn- xirr* [values dates & [guess]]
  (u/check [flows (dated-flows values dates)
            guess (u/num (or guess v/omitted) 0.1)]
    (if-not (mixed-signs? (map first flows))
      v/ERR-NUM
      (u/result
       (newton (fn [r]
                 [(xnpv-sum r flows)
                  (sum (map (fn [[cf t]] (/ (* (- t) cf) (growth r (inc t)))) flows))])
               guess)))))

(defn- fvschedule* [principal schedule]
  (u/check [p (u/num principal)
            rates (reduce (fn [acc x]
                            (let [r (if (nil? x) 0.0 (v/to-number x))]
                              (if (error? r) (reduced r) (conj acc r))))
                          [] (u/flat (u/->2d schedule)))]
    (u/result (reduce (fn [acc r] (* acc (+ 1.0 r))) p rates))))

(defn- sln* [cost salvage life]
  (if (zero? life) v/ERR-DIV0 (/ (- cost salvage) life)))

(defn- syd* [cost salvage life per]
  (if (or (<= life 0) (<= per 0) (> per life) (neg? salvage))
    v/ERR-NUM
    (/ (* (- cost salvage) (+ (- life per) 1.0) 2.0) (* life (+ life 1.0)))))

(defn- db* [cost salvage life period month]
  (if (or (<= cost 0) (neg? salvage) (> salvage cost) (<= life 0) (<= period 0)
          (< month 1) (> month 12) (> period (inc life)))
    v/ERR-NUM
    (let [rate (/ (Math/round (* 1000.0 (- 1.0 (Math/pow (/ salvage cost) (/ 1.0 life))))) 1000.0)
          first-year (/ (* cost rate month) 12.0)
          schedule (iterate (fn [[_ acc]] (let [d (* (- cost acc) rate)] [d (+ acc d)]))
                            [first-year first-year])
          p (long (Math/floor period))]
      (if (> period life)
        (let [[_ acc] (nth schedule (dec (long (Math/floor life))))]
          (/ (* (- cost acc) rate (- 12.0 month)) 12.0))
        (first (nth schedule (dec p)))))))

(defn- ddb-period
  [cost salvage life period factor]
  (let [rate (/ factor life)
        [rate old] (if (>= rate 1.0)
                     [1.0 (if (== period 1.0) cost 0.0)]
                     [rate (* cost (Math/pow (- 1.0 rate) (dec period)))])
        new (* cost (Math/pow (- 1.0 rate) period))]
    (max 0.0 (if (< new salvage) (- old salvage) (- old new)))))

(defn- ddb* [cost salvage life period factor]
  (if (or (neg? cost) (neg? salvage) (<= life 0) (<= period 0) (> period life) (<= factor 0))
    v/ERR-NUM
    (ddb-period cost salvage life period factor)))

(defn- vdb-whole
  [cost salvage life life-left periods factor]
  (let [end (long (Math/ceil periods))]
    (loop [i 1, total 0.0, book (- cost salvage), sln nil]
      (if (> i end)
        total
        (let [ddb (when-not sln (ddb-period cost salvage life i factor))
              line (or sln (/ book (- life-left (dec i))))
              switch? (or sln (> line ddb))
              term (if switch? line ddb)
              term (if (= i end) (* term (+ periods 1.0 (- end))) term)]
          (recur (inc i) (+ total term) (if switch? book (- book ddb)) (when switch? line)))))))

(defn- vdb* [cost salvage life start end factor no-switch?]
  (if (or (neg? start) (< end start) (> end life) (neg? cost) (> salvage cost) (<= factor 0))
    v/ERR-NUM
    (let [int-start (Math/floor start)
          int-end (Math/ceil end)]
      (if no-switch?
        (sum (for [i (range (inc (long int-start)) (inc (long int-end)))
                   :let [term (ddb-period cost salvage life i factor)]]
               (cond (= i (inc (long int-start))) (* term (- (min end (inc int-start)) start))
                     (= i (long int-end)) (* term (+ end 1.0 (- int-end)))
                     :else term)))
        (let [depreciated-to (fn [p] (vdb-whole cost salvage life life p factor))
              partial (fn [whole-start frac]
                        (* frac (vdb-whole (- cost (depreciated-to whole-start)) salvage life
                                           (- life whole-start) 1.0 factor)))
              head (if (== start int-start) 0.0 (partial int-start (- start int-start)))
              tail (if (== end int-end) 0.0 (partial (dec int-end) (- int-end end)))
              cost' (- cost (depreciated-to int-start))]
          (- (vdb-whole cost' salvage life (- life int-start) (- int-end int-start) factor)
             head tail))))))

(defn- effect* [nominal npery]
  (if (or (<= nominal 0) (< npery 1))
    v/ERR-NUM
    (- (Math/pow (+ 1.0 (/ nominal npery)) npery) 1.0)))

(defn- nominal* [effect npery]
  (if (or (<= effect 0) (< npery 1))
    v/ERR-NUM
    (* npery (- (Math/pow (+ 1.0 effect) (/ 1.0 npery)) 1.0))))

(defn- dollar-convert
  [x fraction scale]
  (cond
    (neg? fraction) v/ERR-NUM
    (zero? fraction) v/ERR-DIV0
    :else (let [pow10 (Math/pow 10.0 (Math/ceil (Math/log10 fraction)))
                a (abs x)
                whole (Math/floor a)]
            (* (Math/signum (double x)) (+ whole (scale (- a whole) fraction pow10))))))

(defn- dollarde* [x fraction] (dollar-convert x fraction (fn [frac f p] (/ (* frac p) f))))
(defn- dollarfr* [x fraction] (dollar-convert x fraction (fn [frac f p] (/ (* frac f) p))))

(defn- tbill-days
  [settlement maturity]
  (if (or (>= settlement maturity) (> maturity (dc/add-months settlement 12)))
    v/ERR-NUM
    (- maturity settlement)))

(defn- tbillprice* [settlement maturity discount]
  (u/check [dsm (tbill-days settlement maturity)]
    (let [price (* 100.0 (- 1.0 (/ (* discount dsm) 360.0)))]
      (if (or (<= discount 0) (<= price 0)) v/ERR-NUM price))))

(defn- tbillyield* [settlement maturity price]
  (u/check [dsm (tbill-days settlement maturity)]
    (if (<= price 0) v/ERR-NUM (* (/ (- 100.0 price) price) (/ 360.0 dsm)))))

(defn- tbilleq* [settlement maturity discount]
  (u/check [dsm (tbill-days settlement maturity)]
    (cond
      (<= discount 0) v/ERR-NUM
      (<= dsm 182) (/ (* 365.0 discount) (- 360.0 (* discount dsm)))
      :else (let [price (* 100.0 (- 1.0 (/ (* discount dsm) 360.0)))
                  t (/ dsm 365.0)]
              (/ (+ (- t) (Math/sqrt (- (* t t) (* (- (* 2.0 t) 1.0) (- 1.0 (/ 100.0 price))))))
                 (- t 0.5))))))

(defn- security-yf
  [settlement maturity basis]
  (if (or (>= settlement maturity) (not (dc/valid-basis? basis)))
    v/ERR-NUM
    (dc/yearfrac settlement maturity basis)))

(defn- disc* [settlement maturity price redemption basis]
  (u/check [yf (security-yf settlement maturity basis)]
    (if (or (<= price 0) (<= redemption 0)) v/ERR-NUM (/ (/ (- redemption price) redemption) yf))))

(defn- intrate* [settlement maturity investment redemption basis]
  (u/check [yf (security-yf settlement maturity basis)]
    (if (or (<= investment 0) (<= redemption 0)) v/ERR-NUM
      (/ (/ (- redemption investment) investment) yf))))

(defn- received* [settlement maturity investment discount basis]
  (u/check [yf (security-yf settlement maturity basis)]
    (let [d (- 1.0 (* discount yf))]
      (if (or (<= investment 0) (<= discount 0) (<= d 0)) v/ERR-NUM (/ investment d)))))

(defn- pricedisc* [settlement maturity discount redemption basis]
  (u/check [yf (security-yf settlement maturity basis)]
    (if (or (<= discount 0) (<= redemption 0)) v/ERR-NUM (* redemption (- 1.0 (* discount yf))))))

(defn- yielddisc* [settlement maturity price redemption basis]
  (u/check [yf (security-yf settlement maturity basis)]
    (if (or (<= price 0) (<= redemption 0)) v/ERR-NUM (/ (/ (- redemption price) price) yf))))

(defn- pricemat* [settlement maturity issue rate yld basis]
  (u/check [yf-sm (security-yf settlement maturity basis)]
    (if (or (neg? rate) (neg? yld) (> issue settlement))
      v/ERR-NUM
      (let [yf-im (dc/yearfrac issue maturity basis)
            yf-is (dc/yearfrac issue settlement basis)]
        (- (/ (+ 100.0 (* yf-im rate 100.0)) (+ 1.0 (* yf-sm yld)))
           (* yf-is rate 100.0))))))

(defn- accrintm* [issue settlement rate par basis]
  (u/check [yf (security-yf issue settlement basis)]
    (if (or (<= rate 0) (<= par 0)) v/ERR-NUM (* par rate yf))))

(defn- accrint* [issue first-interest settlement rate par frequency basis calc-method?]
  (let [start (if (and (not calc-method?) (> settlement first-interest)) first-interest issue)]
    (u/check [yf (security-yf start settlement basis)]
      (if (or (<= rate 0) (<= par 0) (not (#{1 2 4} frequency)))
        v/ERR-NUM
        (* par rate yf)))))

(defn- coupon-schedule
  [settlement maturity frequency basis]
  (if (or (>= settlement maturity) (not (#{1 2 4} frequency)) (not (dc/valid-basis? basis)))
    v/ERR-NUM
    (let [months (quot 12 frequency)
          eom? (dc/last-day-of-month? maturity)]
      (loop [k 1, ncd maturity]
        (u/check [pcd (dc/add-months maturity (- (* k months)) eom?)]
          (if (<= pcd settlement)
            {:pcd pcd :ncd ncd :n k}
            (recur (inc k) pcd)))))))

(defn- coupon-days
  [settlement maturity frequency basis]
  (u/check [sched (coupon-schedule settlement maturity frequency basis)]
    (let [{:keys [pcd ncd]} sched
          e (case (int basis)
              1 (double (- ncd pcd))
              3 (/ 365.0 frequency)
              (/ 360.0 frequency))
          a (dc/day-count pcd settlement basis)
          dsc (if (#{0 4} basis) (- e a) (- ncd settlement))]
      (assoc sched :a a :e e :dsc dsc))))

(defn- coupon-fn [k]
  (fn [settlement maturity frequency basis]
    (u/check [cd (coupon-days settlement maturity frequency basis)] (get cd k))))

(defn- bond-price
  [settlement maturity rate yld redemption frequency basis]
  (u/check [days (coupon-days settlement maturity frequency basis)]
    (let [{:keys [a e dsc n]} days
          coupon (/ (* 100.0 rate) frequency)
          y (/ yld frequency)
          accrued (* coupon (/ a e))
          t (/ dsc e)]
      (if (= n 1)
        (- (/ (+ redemption coupon) (+ 1.0 (* t y))) accrued)
        (+ (/ redemption (growth y (+ (dec n) t)))
           (sum (map #(/ coupon (growth y (+ (dec %) t))) (range 1 (inc n))))
           (- accrued))))))

(defn- price* [settlement maturity rate yld redemption frequency basis]
  (if (or (neg? rate) (neg? yld) (<= redemption 0))
    v/ERR-NUM
    (bond-price settlement maturity rate yld redemption frequency basis)))

(defn- yield* [settlement maturity rate price redemption frequency basis]
  (if (or (neg? rate) (<= price 0) (<= redemption 0))
    v/ERR-NUM
    (u/check [days (coupon-days settlement maturity frequency basis)]
      (if (= 1 (:n days))
        (let [{:keys [a e dsc]} days
              c (/ rate frequency)
              paid (+ (/ price 100.0) (* (/ a e) c))]
          (* (/ (- (+ (/ redemption 100.0) c) paid) paid) (/ (* frequency e) dsc)))
        (newton (numeric-derivative
                 #(- (bond-price settlement maturity rate % redemption frequency basis) price))
                (max rate 0.05))))))

(defn- duration* [settlement maturity coupon yld frequency basis]
  (if (or (neg? coupon) (neg? yld))
    v/ERR-NUM
    (u/check [days (coupon-days settlement maturity frequency basis)]
      (let [{:keys [e dsc n]} days
            c (/ (* 100.0 coupon) frequency)
            y (+ 1.0 (/ yld frequency))
            flows (map (fn [k] [(+ (dec k) (/ dsc e)) (if (= k n) (+ c 100.0) c)])
                       (range 1 (inc n)))
            pv (sum (map (fn [[t cf]] (/ cf (Math/pow y t))) flows))
            weighted (sum (map (fn [[t cf]] (/ (* t cf) (Math/pow y t))) flows))]
        (/ weighted pv frequency)))))

(defn- mduration* [settlement maturity coupon yld frequency basis]
  (u/check [d (duration* settlement maturity coupon yld frequency basis)]
    (/ d (+ 1.0 (/ yld frequency)))))

(def ^:private coupon-args [date date (int-arg 0) (int-arg 0)])

(def functions
  {
   "PMT"      (scalar [3 5] [(num) (num) (num) (num) (num)] pmt*)
   "IPMT"     (scalar [4 6] [(num) (num) (num) (num) (num) (num)] ipmt*)
   "PPMT"     (scalar [4 6] [(num) (num) (num) (num) (num) (num)] ppmt*)
   "PV"       (scalar [3 5] [(num) (num) (num) (num) (num)] pv*)
   "FV"       (scalar [3 5] [(num) (num) (num) (num) (num)] fv*)
   "NPER"     (scalar [3 5] [(num) (num) (num) (num) (num)] nper*)
   "RATE"     (scalar [3 6] [(num) (num) (num) (num) (num) (num 0.1)] rate*)
   "CUMIPMT"  (scalar [6 6] [(num) (num) (num) (int-arg 0) (int-arg 0) (int-arg 0)]
                      (cumulative ipmt*))
   "CUMPRINC" (scalar [6 6] [(num) (num) (num) (int-arg 0) (int-arg 0) (int-arg 0)]
                      (cumulative ppmt*))
   "ISPMT"    (scalar [4 4] [(num) (num) (num) (num)] ispmt*)
   "PDURATION" (scalar [3 3] [(num) (num) (num)] pduration*)
   "RRI"      (scalar [3 3] [(num) (num) (num)] rri*)

   "NPV"        {:arity [2 255] :args :refs :impl npv-impl}
   "IRR"        {:arity [1 2] :args :refs :impl irr-impl}
   "MIRR"       {:arity [3 3] :args :refs :impl mirr-impl}
   "XNPV"       {:arity [3 3] :lift #{0} :impl xnpv*}
   "XIRR"       {:arity [2 3] :impl xirr*}
   "FVSCHEDULE" {:arity [2 2] :lift #{0} :impl fvschedule*}

   "SLN" (scalar [3 3] [(num) (num) (num)] sln*)
   "SYD" (scalar [4 4] [(num) (num) (num) (num)] syd*)
   "DB"  (scalar [4 5] [(num) (num) (num) (num) (int-arg 12)] db*)
   "DDB" (scalar [4 5] [(num) (num) (num) (num) (num 2.0)] ddb*)
   "VDB" (scalar [5 7] [(num) (num) (num) (num) (num) (num 2.0) (bool-arg false)] vdb*)

   "EFFECT"   (scalar [2 2] [(num) (int-arg 0)] effect*)
   "NOMINAL"  (scalar [2 2] [(num) (int-arg 0)] nominal*)
   "DOLLARDE" (scalar [2 2] [(num) (int-arg 0)] dollarde*)
   "DOLLARFR" (scalar [2 2] [(num) (int-arg 0)] dollarfr*)

   "TBILLEQ"    (scalar [3 3] [date date (num)] tbilleq*)
   "TBILLPRICE" (scalar [3 3] [date date (num)] tbillprice*)
   "TBILLYIELD" (scalar [3 3] [date date (num)] tbillyield*)

   "DISC"      (scalar [4 5] [date date (num) (num) (int-arg 0)] disc*)
   "INTRATE"   (scalar [4 5] [date date (num) (num) (int-arg 0)] intrate*)
   "RECEIVED"  (scalar [4 5] [date date (num) (num) (int-arg 0)] received*)
   "PRICEDISC" (scalar [4 5] [date date (num) (num) (int-arg 0)] pricedisc*)
   "YIELDDISC" (scalar [4 5] [date date (num) (num) (int-arg 0)] yielddisc*)
   "PRICEMAT"  (scalar [5 6] [date date date (num) (num) (int-arg 0)] pricemat*)
   "ACCRINTM"  (scalar [3 5] [date date (num) (num 1000.0) (int-arg 0)] accrintm*)
   "ACCRINT"   (scalar [6 8]
                       [date date date (num) (num 1000.0) (int-arg 0) (int-arg 0) (bool-arg true)]
                       accrint*)

   "COUPDAYBS"  (scalar [3 4] coupon-args (coupon-fn :a))
   "COUPDAYS"   (scalar [3 4] coupon-args (coupon-fn :e))
   "COUPDAYSNC" (scalar [3 4] coupon-args (coupon-fn :dsc))
   "COUPNCD"    (scalar [3 4] coupon-args (coupon-fn :ncd))
   "COUPPCD"    (scalar [3 4] coupon-args (coupon-fn :pcd))
   "COUPNUM"    (scalar [3 4] coupon-args (coupon-fn :n))
   "PRICE"      (scalar [6 7] [date date (num) (num) (num) (int-arg 0) (int-arg 0)] price*)
   "YIELD"      (scalar [6 7] [date date (num) (num) (num) (int-arg 0) (int-arg 0)] yield*)
   "DURATION"   (scalar [5 6] [date date (num) (num) (int-arg 0) (int-arg 0)] duration*)
   "MDURATION"  (scalar [5 6] [date date (num) (num) (int-arg 0) (int-arg 0)] mduration*)})
