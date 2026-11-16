(ns sutra.capps.ts-ops
  (:require [clojure.string :as str]
            [sutra.kit.coerce :refer [->kw]]))

(defn- sqr [x] (* x x))

(defn- fold
  [how vs]
  (let [vs (filter number? vs)]
    (when (seq vs)
      (case (->kw how)
        :min   (reduce min vs)
        :max   (reduce max vs)
        :sum   (reduce + vs)
        :count (count vs)
        :first (first vs)
        :last  (last vs)
        (/ (reduce + vs) (count vs))))))

(defn stats
  [points [lo hi]]
  (let [in? (fn [t] (and (or (nil? lo) (>= t lo)) (or (nil? hi) (<= t hi))))
        vs  (keep (fn [[t v]] (when (and (number? v) (in? t)) v)) points)]
    (when (seq vs)
      {:min (reduce min vs) :max (reduce max vs) :mean (/ (reduce + vs) (count vs))})))

(defn- trailing
  [points window-ms]
  (let [pts (vec points)]
    (loop [i 0 lo 0 out []]
      (if (= i (count pts))
        out
        (let [[t _] (pts i)
              lo    (loop [l lo] (if (< (first (pts l)) (- t window-ms)) (recur (inc l)) l))]
          (recur (inc i) lo (conj out [t (mapv second (subvec pts lo (inc i)))])))))))

(defn rolling
  [points {:keys [window how]}]
  (mapv (fn [[t vs]] [t (fold how vs)]) (trailing points window)))

(defn bollinger
  [points {:keys [window k]}]
  (let [k (or k 2)
        w (trailing points window)
        one (fn [[t vs]]
              (let [vs (filterv number? vs)
                    n  (count vs)
                    m  (when (pos? n) (/ (reduce + vs) n))
                    sd (when m (Math/sqrt (/ (reduce + (map #(sqr (- % m)) vs)) n)))]
                [t m sd]))
        rows (mapv one w)]
    {:mid   (mapv (fn [[t m _]] [t m]) rows)
     :upper (mapv (fn [[t m s]] [t (when m (+ m (* k s)))]) rows)
     :lower (mapv (fn [[t m s]] [t (when m (- m (* k s)))]) rows)}))

(defn cumulative
  [points {:keys [how period]}]
  (let [slot (if period #(quot % period) (constantly 0))]
    (first
     (reduce (fn [[out seen sl] [t v]]
               (let [s   (slot t)
                     acc (if (= s sl) (conj seen v) [v])]
                 [(conj out [t (fold (or how :sum) acc)]) acc s]))
             [[] [] nil]
             points))))

(defn derivative
  [points _]
  (mapv (fn [[t1 v1] [t2 v2]]
          [t2 (when (and (number? v1) (number? v2) (not= t1 t2))
                (/ (- v2 v1) (/ (- t2 t1) 1000.0)))])
        points (rest points)))

(defn integral
  [points _]
  (first
   (reduce (fn [[out acc [pt pv]] [t v]]
             (let [acc (if (and pt (number? pv) (number? v))
                         (+ acc (* (/ (+ pv v) 2) (/ (- t pt) 1000.0)))
                         acc)]
               [(conj out [t acc]) acc [t v]]))
           [[] 0 nil]
           points)))

(defn periodic
  [points {:keys [period how]}]
  (->> (group-by #(quot (first %) period) points)
       (sort-by key)
       (mapv (fn [[slot ps]] [(* slot period) (fold how (map second ps))]))))

(defn shift
  [points {:keys [by]}]
  (mapv (fn [[t v]] [(+ t by) v]) points))

(defn value-at
  [points t]
  (let [[a b] (reduce (fn [[lo _ :as acc] p]
                        (cond (<= (first p) t) [p nil]
                              (nil? (second acc)) [lo p]
                              :else (reduced acc)))
                      [nil nil] points)]
    (cond
      (and a (= (first a) t)) (second a)
      (and a b (number? (second a)) (number? (second b)))
      (+ (second a) (* (- (second b) (second a)) (/ (- t (first a)) (- (first b) (first a)))))
      :else nil)))

(defn sample
  [points {:keys [every]}]
  (when (seq points)
    (let [lo (first (first points)) hi (first (last points))]
      (mapv (fn [t] [t (value-at points t)]) (range lo (inc hi) every)))))

(defn- union-times [many] (sort (distinct (mapcat #(map first %) many))))

(defn combine
  [many {:keys [how]}]
  (let [merged (apply merge-with into (map (fn [ps] (into {} (map (fn [[t v]] [t [v]])) ps)) many))]
    (mapv (fn [t] [t (fold (or how :mean) (get merged t))]) (sort (keys merged)))))

(defn linear
  [many {:keys [weights]}]
  (mapv (fn [t]
          [t (let [vs (map #(value-at % t) many)]
               (when (every? number? vs)
                 (reduce + (map * vs (or (seq weights) (repeat 1))))))])
        (union-times many)))

(defn filter-points
  [points {:keys [from until op value keep?]}]
  (let [hit (fn [[t v]]
              (and (or (nil? from) (>= t from))
                   (or (nil? until) (< t until))
                   (or (nil? op)
                       (and (number? v)
                            (case (->kw op)
                              :gt (> v value) :gte (>= v value)
                              :lt (< v value) :lte (<= v value)
                              :eq (== v value) :ne (not= v value)
                              true)))))]
    (filterv (if (false? keep?) (complement hit) hit) points)))

(defn- biquad
  [[b0 b1 b2 a1 a2] points]
  (first
   (reduce (fn [[out x1 x2 y1 y2] [t x]]
             (if (number? x)
               (let [y (- (+ (* b0 x) (* b1 x1) (* b2 x2)) (* a1 y1) (* a2 y2))]
                 [(conj out [t y]) x x1 y y1])
               [(conj out [t nil]) x1 x2 y1 y2]))
           [[] 0.0 0.0 0.0 0.0]
           points)))

(defn- low-pass
  [fc fs alpha beta gain]
  (let [w  (* 2 fs (Math/tan (/ (* Math/PI fc) fs)))
        s  (* w alpha)
        o  (* w beta)
        k  (* 2 fs)
        g0 (+ (* s s) (* o o))
        a0 (+ (* k k) (* 2 s k) g0)
        n  (/ (* g0 gain) a0)]
    [n (* 2 n) n
     (/ (- (* 2 g0) (* 2 k k)) a0)
     (/ (+ (* k k) (- (* 2 s k)) g0) a0)]))

(defn- family-poles
  [family ripple-db]
  (let [e (Math/sqrt (dec (Math/pow 10 (/ (or ripple-db 1) 10))))
        c (/ (Math/log (+ (/ 1 e) (Math/sqrt (inc (/ 1 (* e e)))))) 2)
        sh (/ (- (Math/exp c) (Math/exp (- c))) 2)
        ch (/ (+ (Math/exp c) (Math/exp (- c))) 2)
        pa (* sh (Math/sqrt 0.5))
        pb (* ch (Math/sqrt 0.5))]
    (case (->kw family)
      :chebyshev [pa pb (/ (+ (* pa pa) (* pb pb)) (Math/sqrt (inc (* e e))))]
      :inverse-chebyshev (let [m (+ (* pa pa) (* pb pb))] [(/ pa m) (/ pb m) 1])
      [(Math/sqrt 0.5) (Math/sqrt 0.5) 1])))

(defn dsp
  [points {:keys [family cutoff ripple-db]}]
  (if (< (count points) 2)
    (vec points)
    (let [span (- (first (last points)) (first (first points)))
          fs   (/ (dec (count points)) (max 0.001 (/ span 1000.0)))
          fc   (min (or cutoff (/ fs 8)) (* 0.45 fs))
          [alpha beta gain] (family-poles family ripple-db)]
      (biquad (low-pass fc fs alpha beta gain) points))))

(defn- tokens
  [s]
  (re-seq #"\d+\.?\d*|[A-Za-z_][A-Za-z0-9_]*|[-+*/()^]" s))

(declare parse-sum)

(defn- parse-atom
  [[t & more]]
  (cond
    (nil? t)              (throw (ex-info "Formel endet zu früh" {}))
    (= t "(")             (let [[e r] (parse-sum more)] [e (rest r)])
    (= t "-")             (let [[e r] (parse-atom more)] [[:neg e] r])
    (re-matches #"\d.*" t) [(parse-double t) more]
    (= (first more) "(")  (let [[e r] (parse-sum (rest more))] [[:call (->kw t) e] (rest r)])
    :else                 [[:var (->kw t)] more]))

(defn- parse-pow
  [ts]
  (let [[a r] (parse-atom ts)]
    (if (= (first r) "^")
      (let [[b r2] (parse-pow (rest r))] [[:pow a b] r2])
      [a r])))

(defn- parse-chain
  [next-level ops ts]
  (let [[a r] (next-level ts)]
    (loop [a a r r]
      (if-let [op (get ops (first r))]
        (let [[b r2] (next-level (rest r))] (recur [op a b] r2))
        [a r]))))

(defn- parse-product [ts] (parse-chain parse-pow {"*" :mul "/" :div} ts))
(defn- parse-sum [ts] (parse-chain parse-product {"+" :add "-" :sub} ts))

(defn parse-formula
  [s]
  (let [[e r] (parse-sum (tokens (str s)))]
    (when (seq r) (throw (ex-info (str "Formel: unerwartet " (first r)) {})))
    e))

(defn- eval-formula
  [e env]
  (cond
    (number? e) e
    :else
    (let [[op a b] e
          x (when-not (= op :var) (when (not= op :call) (eval-formula a env)))
          y (when (#{:add :sub :mul :div :pow} op) (eval-formula b env))]
      (case op
        :var  (get env a)
        :neg  (when x (- x))
        :call (when-let [v (eval-formula b env)]
                (case a
                  :abs (Math/abs (double v)) :sqrt (Math/sqrt v) :log (Math/log v)
                  :exp (Math/exp v) nil))
        (when (and (number? x) (number? y))
          (case op
            :add (+ x y) :sub (- x y) :mul (* x y)
            :div (when-not (zero? y) (/ x y))
            :pow (Math/pow x y)))))))

(defn formula
  [named {:keys [expr]}]
  (let [tree (parse-formula expr)]
    (mapv (fn [t]
            [t (eval-formula tree (into {} (map (fn [[k ps]] [(->kw k) (value-at ps t)])) named))])
          (union-times (vals named)))))

(defn search-events
  [points {:keys [op value]}]
  (let [hit? (fn [[_ v]] (and (number? v) (case (->kw op)
                                            :gt (> v value) :gte (>= v value)
                                            :lt (< v value) :lte (<= v value)
                                            false)))]
    (->> (partition-by hit? points)
         (filter (comp hit? first))
         (mapv (fn [ps] {:start (first (first ps)) :end (first (last ps))})))))

(defn event-statistics
  [points events {:keys [how]}]
  (mapv (fn [{:keys [start end]}]
          [start (fold how (keep (fn [[t v]] (when (<= start t end) v)) points))])
        events))

(defn count-in-view
  [events [lo hi]]
  (count (filter (fn [{:keys [start end]}]
                   (and (or (nil? hi) (<= start hi)) (or (nil? lo) (>= end lo))))
                 events)))

(def plot-types
  [:bollinger :combine :cumulative :derivative :dsp :event-statistics :filter :formula :integral
   :linear :periodic :rolling :sample :shift])

(def event-set-types [:linked :search])

(defn derive-plot
  [{:keys [plot-type params]} inputs events]
  (let [one (first inputs)]
    (case (->kw plot-type)
      :bollinger        (bollinger one params)
      :combine          (combine inputs params)
      :cumulative       (cumulative one params)
      :derivative       (derivative one params)
      :dsp              (dsp one params)
      :event-statistics (event-statistics one events params)
      :filter           (filter-points one params)
      :formula          (formula (zipmap (map #(str/lower-case (str (char (+ 97 %))))
                                              (range))
                                         inputs)
                                 params)
      :integral         (integral one params)
      :linear           (linear inputs params)
      :periodic         (periodic one params)
      :rolling          (rolling one params)
      :sample           (sample one params)
      :shift            (shift one params)
      nil)))
