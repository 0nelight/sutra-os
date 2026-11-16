(ns blueprint-cljs.table.layout
  "Sizes along one axis of the table: prefix offsets and index lookup by pixel. Pure.")

(defn sizes
  "A vector of `n` sizes: `given` (may hold nils or be short) over `default`, clamped."
  [n given default min-size max-size]
  (let [given (vec given)]
    (mapv (fn [i]
            (-> (or (get given i) default) (max min-size) (min max-size)))
          (range n))))

(defn offsets
  "Prefix sums as a JS array of length n+1: `(aget o i)` is where index i starts."
  [sizes]
  (let [n (count sizes)
        o (js/Float64Array. (inc n))]
    (loop [i 0 acc 0]
      (aset o i acc)
      (when (< i n)
        (recur (inc i) (+ acc (nth sizes i)))))
    o))

(defn total [o] (aget o (dec (.-length o))))

(defn n-of [o] (dec (.-length o)))

(defn start [o i] (aget o i))

(defn size [o i] (- (aget o (inc i)) (aget o i)))

(defn index-at
  "The index whose span holds pixel `px`, clamped to [0, n-1]; nil when n is 0."
  [o px]
  (let [n (n-of o)]
    (when (pos? n)
      (loop [lo 0 hi (dec n)]
        (if (>= lo hi)
          lo
          (let [mid (unsigned-bit-shift-right (+ lo hi 1) 1)]
            (if (<= (aget o mid) px)
              (recur mid hi)
              (recur lo (dec mid)))))))))

(defn boundary-at
  "The insertion boundary (0..n) nearest to pixel `px`."
  [o px]
  (let [n (n-of o)]
    (if (zero? n)
      0
      (let [i (index-at o px)]
        (if (> (- px (start o i)) (/ (size o i) 2)) (inc i) i)))))

(defn visible-range
  "[first last] indices in [lo-idx, hi-idx) intersecting pixels [from, to), plus `overscan`;
   nil when empty."
  [o lo-idx hi-idx from to overscan]
  (when (< lo-idx hi-idx)
    (let [a (max lo-idx (- (or (index-at o from) 0) overscan))
          b (min (dec hi-idx) (+ (or (index-at o (max from (dec to))) 0) overscan))]
      (when (<= a b) [a b]))))

(defn base26
  "Blueprint's default column names: 0 → A, 25 → Z, 26 → AA."
  [i]
  (loop [n (inc i) s ""]
    (if (pos? n)
      (let [r (mod (dec n) 26)]
        (recur (quot (- n r 1) 26) (str (char (+ 65 r)) s)))
      s)))
