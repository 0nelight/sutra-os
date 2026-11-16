(ns sutra.calx.fn.lookup
  (:require [clojure.string :as str]
            [sutra.calx.value :as v :refer [error?]]
            [sutra.calx.eval :as ev]
            [sutra.calx.address :as addr]
            [sutra.calx.fn.util :as u]))

(set! *warn-on-reflection* true)

(defn- spec
  [[lo hi] impl & {:as opts}]
  (let [ctx? (or (= :refs (:args opts)) (:ctx? opts))
        n (when hi (cond-> hi ctx? inc))]
    (assoc opts
           :arity [lo hi]
           :impl (if n
                   (fn [& args] (apply impl (concat args (repeat (- n (count args)) v/omitted))))
                   impl))))

(defn- first-error [xs] (some #(when (error? %) %) xs))

(defn- dims
  [x]
  (cond (v/ref? x) [(inc (- (:r2 x) (:r1 x))) (inc (- (:c2 x) (:c1 x)))]
        (v/array? x) [(v/rows x) (v/cols x)]
        :else [1 1]))

(defn- column [a j] (mapv #(nth % j) a))

(defn- vector-1d
  [a]
  (cond (= 1 (v/rows a)) (first a)
        (= 1 (v/cols a)) (mapv first a)))

(defn- ->col [xs] (mapv vector xs))
(defn- ->row [xs] [(vec xs)])

(defn- sub-array
  [a r1 c1 r2 c2]
  (mapv #(subvec % c1 (inc c2)) (subvec a r1 (inc r2))))

(defn- pad-row [row width fill]
  (into (vec row) (repeat (- width (count row)) fill)))

(defn- non-empty
  [a]
  (if (and (seq a) (seq (first a))) (vec a) v/ERR-CALC))

(defn- fill-value
  [pad]
  (if (u/present? pad) (v/scalarize pad) v/ERR-NA))

(defn- deref* [ctx x] (ev/deref-value ctx x))

(defn- scalar-result
  [ctx x]
  (let [x (v/scalarize (deref* ctx x))]
    (if (v/array? x) (ffirst x) x)))

(defn- kind [x] (cond (number? x) :num (string? x) :text (boolean? x) :bool))

(defn- exact-pred
  [x wildcards?]
  (let [k (kind x)]
    (cond
      (and wildcards? (string? x) (re-find #"[*?~]" x))
      (let [re (u/wildcard->regex x)] #(and (string? %) (boolean (re-matches re %))))
      k #(and (= k (kind %)) (zero? (v/compare-values % x)))
      :else (constantly false))))

(defn- lower-bound
  [n pred]
  (loop [lo 0 hi (long n)]
    (if (< lo hi)
      (let [m (quot (+ lo hi) 2)]
        (if (pred m) (recur lo m) (recur (inc m) hi)))
      lo)))

(defn- linear-find [xs x match reverse?]
  (let [order (if reverse? (range (dec (count xs)) -1 -1) (range (count xs)))
        exact? (exact-pred x (= match 2))]
    (or (first (filter #(exact? (xs %)) order))
        (when-let [want (case (long match) -1 neg? 1 pos? nil)]
          (let [k (kind x)
                cands (filter #(and k (= k (kind (xs %))) (want (v/compare-values (xs %) x)))
                              order)]
            (when (seq cands)
              (reduce (fn [best i] (if (want (v/compare-values (xs best) (xs i))) i best))
                      cands)))))))

(defn- binary-find
  [xs x match asc?]
  (when-let [k (kind x)]
    (let [ps (filterv #(= k (kind (xs %))) (range (count xs)))
          n (count ps)
          cmp (fn [j] (let [c (v/compare-values (xs (ps j)) x)] (if asc? c (- c))))
          first-ge (lower-bound n #(>= (cmp %) 0))
          last-le (dec (lower-bound n #(pos? (cmp %))))
          pick (fn [j] (when (< -1 j n) (ps j)))]
      (case (long match)
        (0 2) (when (and (< first-ge n) (zero? (cmp first-ge))) (ps first-ge))
        -1 (pick (if asc? last-le first-ge))
        1 (pick (if asc? first-ge last-le))))))

(defn- find-index
  [xs x match search]
  (case (long search)
    (2 -2) (binary-find xs x match (= search 2))
    (linear-find xs x match (= search -1))))

(def ^:private approx-asc  [-1 2])
(def ^:private approx-desc [1 -2])
(def ^:private exact-wild  [2 1])

(defn- find-with [xs x [match search]] (find-index xs x match search))

(defn- table-lookup
  [x table idx approx]
  (u/check [n (u/int* idx) approx? (u/bool approx true)]
    (cond (< n 1) v/ERR-VALUE
          (> n (v/cols table)) v/ERR-REF
          :else (if-let [i (find-with (column table 0) x (if approx? approx-asc exact-wild))]
                  (get-in table [i (dec n)])
                  v/ERR-NA))))

(defn- vlookup [x table idx approx] (table-lookup x (u/->2d table) idx approx))
(defn- hlookup [x table idx approx] (table-lookup x (v/transpose (u/->2d table)) idx approx))

(defn- lookup
  [x lookup-arg result-arg]
  (let [a (u/->2d lookup-arg)
        [keys results] (cond
                         (u/present? result-arg) [(vector-1d a) (vector-1d (u/->2d result-arg))]
                         (> (v/cols a) (v/rows a)) [(first a) (peek a)]
                         :else [(column a 0) (mapv peek a)])
        i (when (and keys results) (find-with keys x approx-asc))]
    (if (and i (< i (count results))) (results i) v/ERR-NA)))

(defn- match-fn [x arr type]
  (u/check [t (u/int* type 1)]
    (if-let [xs (vector-1d (u/->2d arr))]
      (if-let [i (find-with xs x (cond (pos? t) approx-asc (neg? t) approx-desc :else exact-wild))]
        (double (inc i))
        v/ERR-NA)
      v/ERR-NA)))

(defn- x-modes
  [match search]
  (u/check [m (u/int* match 0) s (u/int* search 1)]
    (if (and (#{0 -1 1 2} m) (#{1 -1 2 -2} s)) [m s] v/ERR-VALUE)))

(defn- xmatch [x arr match search]
  (u/check [modes (x-modes match search)]
    (if-let [xs (vector-1d (u/->2d arr))]
      (if-let [i (find-with xs x modes)] (double (inc i)) v/ERR-NA)
      v/ERR-VALUE)))

(defn- slice-at
  [ret vertical? i]
  (cond
    (v/ref? ret) (if vertical?
                   (let [r (+ (:r1 ret) i)] (assoc ret :r1 r :r2 r))
                   (let [c (+ (:c1 ret) i)] (assoc ret :c1 c :c2 c)))
    (v/array? ret) (v/scalarize (if vertical? [(nth ret i)] (mapv #(vector (nth % i)) ret)))
    :else ret))

(defn- xlookup [ctx x lookup-arg ret if-not-found match search]
  (let [x (deref* ctx x)
        la (deref* ctx lookup-arg)
        ret (if (v/ref? ret) ret (deref* ctx ret))
        modes (x-modes (deref* ctx match) (deref* ctx search))
        [lr lc] (dims lookup-arg)
        [rr rc] (dims ret)
        vertical? (and (= lc 1) (= rr lr))
        horizontal? (and (= lr 1) (= rc lc))]
    (cond
      (first-error [x la ret modes]) (first-error [x la ret modes])
      (not (or vertical? horizontal?)) v/ERR-VALUE
      :else
      (let [a (u/->2d la)
            xs (if vertical? (column a 0) (first a))
            not-found (if (u/present? if-not-found) (deref* ctx if-not-found) v/ERR-NA)
            find1 (fn [x] (if-let [i (find-with xs x modes)] (slice-at ret vertical? i) not-found))]
        (if (v/array? x)
          (v/amap2 #(if (error? %) % (scalar-result ctx (find1 %))) x)
          (find1 x))))))

(defn- index-1
  [src row col col-given?]
  (u/check [r (u/int* row 0) c (u/int* col 0)]
    (let [[nr nc] (dims src)
          [r c] (if (and (= 1 nr) (not col-given?)) [0 r] [r c])]
      (cond
        (or (neg? r) (neg? c)) v/ERR-VALUE
        (or (> r nr) (> c nc)) v/ERR-REF
        :else
        (let [[r1 r2] (if (zero? r) [0 (dec nr)] [(dec r) (dec r)])
              [c1 c2] (if (zero? c) [0 (dec nc)] [(dec c) (dec c)])]
          (if (v/ref? src)
            (v/->Ref (:sheet src) (+ (:r1 src) r1) (+ (:c1 src) c1) (+ (:r1 src) r2)
                     (+ (:c1 src) c2))
            (v/scalarize (sub-array (u/->2d src) r1 c1 r2 c2))))))))

(defn- index-fn [ctx src row col area]
  (let [src (if (v/ref? src) src (deref* ctx src))
        row (deref* ctx row)
        col (deref* ctx col)
        area (u/int* (deref* ctx area) 1)
        col-given? (u/present? col)]
    (cond
      (first-error [src row col area]) (first-error [src row col area])
      (not= area 1) v/ERR-REF
      (or (v/array? row) (v/array? col))
      (ev/broadcast (fn [r c] (scalar-result ctx (index-1 src r c col-given?))) [row col])
      :else (index-1 src row col col-given?))))

(defn- in-sheet? [r1 c1 r2 c2]
  (and (<= 0 r1 r2 (dec addr/max-rows)) (<= 0 c1 c2 (dec addr/max-cols))))

(defn- span
  [start n]
  (if (pos? n) [start (+ start n -1)] [(+ start n 1) start]))

(defn- offset [ctx ref rows cols height width]
  (if-not (v/ref? ref)
    (if (error? ref) ref v/ERR-VALUE)
    (let [[nr nc] (dims ref)]
      (u/check [dr (u/int* (deref* ctx rows))
                dc (u/int* (deref* ctx cols))
                h (u/int* (deref* ctx height) nr)
                w (u/int* (deref* ctx width) nc)]
        (let [[r1 r2] (span (+ (:r1 ref) dr) h)
              [c1 c2] (span (+ (:c1 ref) dc) w)]
          (if (and (not (zero? h)) (not (zero? w)) (in-sheet? r1 c1 r2 c2))
            (v/->Ref (:sheet ref) r1 c1 r2 c2)
            v/ERR-REF))))))

(def ^:private r1c1-cell-re #"(?i)R(\[-?\d+\]|\d+)?C(\[-?\d+\]|\d+)?")
(def ^:private r1c1-row-re #"(?i)R(\[-?\d+\]|\d+)?")
(def ^:private r1c1-col-re #"(?i)C(\[-?\d+\]|\d+)?")

(defn- r1c1-coord
  [tok base]
  (cond (nil? tok) base
        (str/starts-with? tok "[") (+ base (parse-long (subs tok 1 (dec (count tok)))))
        :else (dec (parse-long tok))))

(defn- r1c1-part [s row col]
  (if-let [[_ r c] (re-matches r1c1-cell-re s)]
    (let [r (r1c1-coord r row) c (r1c1-coord c col)] [r c r c])
    (if-let [[_ r] (re-matches r1c1-row-re s)]
      (let [r (r1c1-coord r row)] [r 0 r (dec addr/max-cols)])
      (when-let [[_ c] (re-matches r1c1-col-re s)]
        (let [c (r1c1-coord c col)] [0 c (dec addr/max-rows) c])))))

(defn- parse-r1c1
  [s row col]
  (let [[a b] (str/split s #":" 2)
        pa (r1c1-part a row col)
        pb (if b (r1c1-part b row col) pa)]
    (when (and pa pb)
      (let [[r1 c1 r2 c2] (map #(%1 %2 %3) [min min max max] pa pb)
            [r1 r2] [(min r1 r2) (max r1 r2)]
            [c1 c2] [(min c1 c2) (max c1 c2)]]
        (when (in-sheet? r1 c1 r2 c2) {:r1 r1 :c1 c1 :r2 r2 :c2 c2})))))

(defn- named-ref
  [ctx s]
  (when-let [names (and (re-matches #"[A-Za-z_\\][A-Za-z0-9_.]*" s) (:names ctx))]
    (when-let [ast (names (str/upper-case s) (:sheet ctx))]
      (let [r (ev/evaluate (dissoc ctx :env) ast)]
        (when (v/ref? r) r)))))

(defn- indirect [ctx text a1]
  (u/check [s (u/text (deref* ctx text)) a1? (u/bool (deref* ctx a1) true)]
    (let [[sheet-name area] (addr/split-sheet s)
          sheet (if sheet-name (some-> (:sheet-id ctx) (apply [sheet-name])) (:sheet ctx))
          box (when (and sheet (seq area))
                (if a1? (addr/parse-area area) (parse-r1c1 area (:row ctx) (:col ctx))))]
      (cond
        box (v/->Ref sheet (:r1 box) (:c1 box) (:r2 box) (:c2 box))
        (and (nil? sheet-name) a1?) (or (named-ref ctx s) v/ERR-REF)
        :else v/ERR-REF))))

(defn- position-fn
  [k1 k2 host ->vec limit]
  (fn [ctx ref]
    (cond
      (v/omitted? ref) (double (inc (host ctx)))
      (v/ref? ref) (let [ref (if (= (k2 ref) (dec limit)) (ev/clip-ref ctx ref) ref)
                         a (k1 ref) b (k2 ref)]
                     (if (= a b)
                       (double (inc a))
                       (->vec (map #(double (inc %)) (range a (inc b))))))
      (error? ref) ref
      :else v/ERR-VALUE)))

(defn- size-fn [f]
  (fn [ctx x]
    (let [x (if (v/ref? x) x (deref* ctx x))]
      (if (error? x) x (double (f (dims x)))))))

(defn- address [row col abs a1 sheet]
  (u/check [r (u/int* row) c (u/int* col) abs (u/int* abs 1) a1? (u/bool a1 true) sheet
            (u/text sheet "")]
    (if (and (<= 1 r addr/max-rows) (<= 1 c addr/max-cols) (<= 1 abs 4))
      (let [row-abs? (#{1 2} abs)
            col-abs? (#{1 3} abs)
            cell (if a1?
                   (str (when col-abs? "$") (addr/col->letters (dec c)) (when row-abs? "$") r)
                   (str "R" (if row-abs? r (str "[" r "]")) "C" (if col-abs? c (str "[" c "]"))))]
        (str (when (seq sheet) (str (addr/quote-sheet-name sheet) "!")) cell))
      v/ERR-VALUE)))

(defn- areas [_ctx x]
  (cond (v/ref? x) 1.0
        (error? x) x
        :else v/ERR-VALUE))

(defn- select-by
  [rows flags if-empty]
  (let [bs (map v/to-bool flags)]
    (or (first-error bs)
        (let [kept (keep (fn [[row b]] (when b row)) (map vector rows bs))]
          (cond (seq kept) (vec kept)
                (u/present? if-empty) if-empty
                :else v/ERR-CALC)))))

(defn- filter-fn [arr include if-empty]
  (or (first-error [arr include])
      (let [a (u/->2d arr)
            inc (u/->2d include)
            [nr nc] (dims a)
            [ir ic] (dims inc)]
        (cond
          (and (= ir nr) (= ic 1)) (select-by a (column inc 0) if-empty)
          (and (= ic nc) (= ir 1)) (let [r (select-by (v/transpose a) (first inc) if-empty)]
                                     (if (v/array? r) (v/transpose r) r))
          :else v/ERR-VALUE))))

(defn- compare-by
  [keys]
  (fn [a b]
    (or (some (fn [[j o]]
                (let [c (* o (v/compare-values (nth a j) (nth b j)))]
                  (when-not (zero? c) c)))
              keys)
        0)))

(defn- numbers [x] (map #(u/int* %) (u/flat x)))

(defn- sort-fn [arr idx order by-col]
  (u/check [by-col? (u/bool by-col false)]
    (let [a (cond-> (u/->2d arr) by-col? v/transpose)
          idxs (if (u/present? idx) (numbers idx) [1])
          ords (if (u/present? order) (numbers order) [1])]
      (or (first-error (concat idxs ords))
          (if (and (every? #(<= 1 % (v/cols a)) idxs)
                   (every? #{1 -1} ords)
                   (or (= 1 (count ords)) (= (count ords) (count idxs))))
            (let [ks (map (fn [i o] [(dec i) o]) idxs
                          (if (= 1 (count ords)) (repeat (first ords)) ords))]
              (cond-> (vec (sort (compare-by ks) a)) by-col? v/transpose))
            v/ERR-VALUE)))))

(defn- sortby-fn [arr & pairs]
  (let [a (u/->2d arr)
        [nr nc] (dims a)
        sort-key (fn [[by ord]]
                   (let [b (u/->2d by) o (u/int* ord 1)
                         d (dims b)]
                     (cond (error? o) o
                           (not (#{1 -1} o)) v/ERR-VALUE
                           (= d [nr 1]) {:axis :rows :xs (column b 0) :order o}
                           (= d [1 nc]) {:axis :cols :xs (first b) :order o}
                           :else v/ERR-VALUE)))
        ks (map sort-key (partition 2 2 [v/omitted] pairs))]
    (or (first-error (cons arr ks))
        (if (apply = (map :axis ks))
          (let [rows? (= :rows (:axis (first ks)))
                cmp (compare-by (map-indexed (fn [j k] [j (:order k)]) ks))
                key-rows (apply map vector (map :xs ks))
                perm (map first (sort-by second cmp (map-indexed vector key-rows)))]
            (if rows?
              (mapv a perm)
              (mapv (fn [row] (mapv row perm)) a)))
          v/ERR-VALUE))))

(defn- value-key
  [x]
  (cond (string? x) [:text (str/lower-case x)]
        (number? x) (double x)
        :else x))

(defn- unique-fn [arr by-col once]
  (u/check [by-col? (u/bool by-col false) once? (u/bool once false)]
    (let [a (cond-> (u/->2d arr) by-col? v/transpose)
          ks (mapv #(mapv value-key %) a)
          freq (frequencies ks)
          rows (->> (map vector ks a)
                    (reduce (fn [[seen out] [k row]]
                              (if (seen k) [seen out] [(conj seen k) (conj out [k row])]))
                            [#{} []])
                    second
                    (filter (fn [[k _]] (or (not once?) (= 1 (freq k)))))
                    (mapv second))]
      (if (seq rows) (cond-> rows by-col? v/transpose) v/ERR-CALC))))

(defn- array-size
  [nr nc]
  (cond (or (neg? nr) (neg? nc) (> nr addr/max-rows) (> nc addr/max-cols)) v/ERR-VALUE
        (or (zero? nr) (zero? nc)) v/ERR-CALC))

(defn- sequence-fn [rows cols start step]
  (u/check [nr (u/int* rows 1) nc (u/int* cols 1) s (u/num start 1) d (u/num step 1)]
    (or (array-size nr nc)
        (v/make-array2 nr nc (fn [r c] (v/normalize-number (+ s (* d (+ (* r nc) c)))))))))

(defn- randarray [ctx rows cols lo hi integer]
  (u/check [nr (u/int* rows 1) nc (u/int* cols 1) lo (u/num lo 0) hi (u/num hi 1) int?
            (u/bool integer false)]
    (let [whole? #(== % (Math/floor %))
          rand (:rand ctx)]
      (cond
        (array-size nr nc) (array-size nr nc)
        (> lo hi) v/ERR-VALUE
        (and int? (not (and (whole? lo) (whole? hi)))) v/ERR-VALUE
        :else (v/make-array2 nr nc (fn [_ _]
                                     (if int?
                                       (+ lo (Math/floor (* (rand) (inc (- hi lo)))))
                                       (+ lo (* (rand) (- hi lo))))))))))

(defn- vstack [& arrays]
  (let [as (map #(or (u/->2d %) [[nil]]) arrays)
        width (apply max (map v/cols as))]
    (vec (for [a as row a] (pad-row row width v/ERR-NA)))))

(defn- hstack [& arrays]
  (v/transpose (apply vstack (map #(v/transpose (or (u/->2d %) [[nil]])) arrays))))

(defn- slicer
  [f]
  (fn [arr rows cols]
    (u/check [r (if (u/present? rows) (u/int* rows) ::all)
              c (if (u/present? cols) (u/int* cols) ::all)]
      (non-empty (cond->> (u/->2d arr)
                   (not= r ::all) (f r)
                   (not= c ::all) (mapv #(vec (f c %))))))))

(defn- take-n [n xs] (vec (if (neg? n) (take-last (- n) xs) (take n xs))))
(defn- drop-n [n xs] (vec (if (neg? n) (drop-last (- n) xs) (drop n xs))))

(defn- choose-from
  [a idx-args]
  (let [n (count a)
        ns (mapcat numbers idx-args)]
    (or (first-error ns)
        (let [pos (map #(if (neg? %) (+ n %) (dec %)) ns)]
          (if (and (seq pos) (every? #(< -1 % n) pos) (not-any? zero? ns))
            (mapv a pos)
            v/ERR-VALUE)))))

(defn- chooserows [arr & idx] (choose-from (u/->2d arr) idx))
(defn- choosecols [arr & idx]
  (let [r (choose-from (v/transpose (u/->2d arr)) idx)]
    (if (v/array? r) (v/transpose r) r)))

(defn- flatten-fn
  [shape]
  (fn [arr ignore by-col]
    (u/check [ig (u/int* ignore 0) by-col? (u/bool by-col false)]
      (if (<= 0 ig 3)
        (let [skip? #(or (and (odd? ig) (nil? %)) (and (>= ig 2) (error? %)))
              xs (remove skip? (apply concat (cond-> (u/->2d arr) by-col? v/transpose)))]
          (if (seq xs) (shape xs) v/ERR-CALC))
        v/ERR-VALUE))))

(defn- wraprows [vec-arg n pad]
  (let [xs (vector-1d (u/->2d vec-arg))]
    (u/check [n (u/int* n)]
      (cond (nil? xs) v/ERR-VALUE
            (< n 1) v/ERR-NUM
            :else (let [fill (fill-value pad)]
                    (mapv #(pad-row % n fill) (partition-all n xs)))))))

(defn- wrapcols [vec-arg n pad]
  (let [r (wraprows vec-arg n pad)]
    (if (v/array? r) (v/transpose r) r)))

(defn- expand [arr rows cols pad]
  (let [a (u/->2d arr)
        [nr nc] (dims a)]
    (u/check [r (u/int* rows nr) c (u/int* cols nc)]
      (if (or (< r nr) (< c nc))
        v/ERR-VALUE
        (let [fill (fill-value pad)]
          (into (mapv #(pad-row % c fill) a) (repeat (- r nr) (vec (repeat c fill)))))))))

(defn- trim-bounds
  [hits n mode]
  [(if (#{1 3} mode) (first hits) 0)
   (if (#{2 3} mode) (last hits) (dec n))])

(defn- trimrange [ctx x rows cols]
  (u/check [tr (u/int* (deref* ctx rows) 3) tc (u/int* (deref* ctx cols) 3)]
    (let [ref? (v/ref? x)
          base (if ref? (ev/clip-ref ctx x) (deref* ctx x))
          a (u/arg->2d ctx base)
          used? (fn [xs] (some some? xs))
          row-hits (keep-indexed #(when (used? %2) %1) a)
          col-hits (keep-indexed #(when (used? %2) %1) (v/transpose a))]
      (cond
        (error? base) base
        (not (and (<= 0 tr 3) (<= 0 tc 3))) v/ERR-VALUE
        (empty? row-hits) v/ERR-CALC
        :else
        (let [[nr nc] (dims (if ref? x a))
              [r1 r2] (trim-bounds row-hits nr tr)
              [c1 c2] (trim-bounds col-hits nc tc)]
          (if ref?
            (v/->Ref (:sheet x) (+ (:r1 x) r1) (+ (:c1 x) c1) (+ (:r1 x) r2) (+ (:c1 x) c2))
            (sub-array a r1 c1 r2 c2)))))))

(defn- aggregator
  [ctx node]
  (let [finish #(let [x (v/scalarize (deref* ctx %))] (if (v/array? x) v/ERR-CALC x))
        fname (when (= :name (:kind node)) (str/upper-case (:name node)))]
    (if (and fname (not (contains? (:env ctx) fname)) (get (:fns ctx) fname))
      (fn [arr] (finish (ev/call-function ctx fname [{:kind :lit :value arr}])))
      (let [f (ev/eval-arg ctx node)]
        (when (v/lambda? f) (fn [arr] (finish (ev/call-lambda ctx f [arr]))))))))

(defn- compare-rows [a b]
  (or (some #(when-not (zero? %) %) (map v/compare-values a b)) 0))

(defn- groups
  [key-rows]
  (->> key-rows
       (reduce-kv (fn [acc i row]
                    (let [k (mapv value-key row)]
                      (if (get acc k)
                        (update-in acc [k :idx] conj i)
                        (assoc acc k {:label row :idx [i] :order (count acc)}))))
                  {})
       vals
       (sort-by :order)))

(defn- aggregate [agg vals idx j]
  (agg (mapv (fn [i] [(get-in vals [i j])]) idx)))

(defn- split-headers
  [mode arrays]
  (if (#{1 3} mode)
    [(map first arrays) (map #(vec (rest %)) arrays)]
    [nil arrays]))

(defn- auto-headers [vals]
  (if (and (string? (ffirst vals)) (> (count vals) 1) (number? (get-in vals [1 0]))) 3 0))

(defn- apply-filter [flt n idx]
  (if (u/present? flt)
    (let [fs (u/flat flt)
          fs (if (> (count fs) n) (drop (- (count fs) n) fs) fs)
          bs (vec (map v/to-bool fs))]
      (or (first-error bs) (filterv #(get bs % false) idx)))
    idx))

(defn- order-groups
  [rows n]
  (let [j (dec (Math/abs (long n)))
        cmp (fn [a b] (let [c (if (zero? n)
                                0
                                (v/compare-values (nth (apply concat a) j nil)
                                                  (nth (apply concat b) j nil)))]
                        (if (neg? n) (- c) c)))]
    (if (zero? n) rows (sort cmp rows))))

(defn- lazy-args [ctx nodes]
  (map #(if (nil? %) v/omitted (ev/eval-arg ctx %)) nodes))

(defn- groupby [ctx rf-node val-node fn-node & opt-nodes]
  (let [[rf vals hdr depth sort-order
         flt] (lazy-args ctx (concat [rf-node val-node] (take 5 (concat opt-nodes (repeat nil)))))
        agg (aggregator ctx fn-node)]
    (or (first-error [rf vals hdr depth sort-order flt])
        (let [rf (u/->2d rf) vals (u/->2d vals)]
          (u/check [mode (u/int* hdr (auto-headers vals))
                    depth (u/int* depth 1)
                    sort-n (u/int* sort-order 1)]
            (cond
              (nil? agg) v/ERR-VALUE
              (not= (count rf) (count vals)) v/ERR-VALUE
              :else
              (let [[hdrs [rf vals]] (split-headers mode [rf vals])
                    idx (apply-filter flt (count rf) (vec (range (count rf))))]
                (if (error? idx)
                  idx
                  (let [nv (v/cols vals)
                        lw (v/cols rf)
                        aggs (fn [idx] (mapv #(aggregate agg vals idx %) (range nv)))
                        gs (groups (mapv rf idx))
                        body (->> gs
                                  (map (fn [{:keys [label] gidx :idx}]
                                         [label (aggs (mapv idx gidx))]))
                                  (#(order-groups % sort-n))
                                  (map #(vec (apply concat %))))
                        total (vec (concat ["Total"] (repeat (dec lw) nil) (aggs idx)))
                        header (when (= 3 mode) [(vec (concat (first hdrs) (second hdrs)))])
                        rows (cond (zero? depth) body
                                   (pos? depth) (concat body [total])
                                   :else (cons total body))]
                    (non-empty (vec (concat header rows))))))))))))

(defn- pivotby [ctx rf-node cf-node val-node fn-node & opt-nodes]
  (let [[rf cf vals hdr rdepth rsort cdepth csort flt]
        (lazy-args ctx (concat [rf-node cf-node val-node] (take 6 (concat opt-nodes (repeat nil)))))
        agg (aggregator ctx fn-node)]
    (or (first-error [rf cf vals hdr rdepth rsort cdepth csort flt])
        (let [rf (u/->2d rf) cf (u/->2d cf) vals (u/->2d vals)]
          (u/check [mode (u/int* hdr (auto-headers vals))
                    rdepth (u/int* rdepth 1) cdepth (u/int* cdepth 1)
                    rsort (u/int* rsort 1) csort (u/int* csort 1)]
            (cond
              (nil? agg) v/ERR-VALUE
              (not (= (count rf) (count cf) (count vals))) v/ERR-VALUE
              :else
              (let [[_ [rf cf vals]] (split-headers mode [rf cf vals])
                    idx (apply-filter flt (count rf) (vec (range (count rf))))]
                (if (error? idx)
                  idx
                  (let [sorted (fn [gs n]
                                 (cond->> (sort-by :label compare-rows gs) (neg? n) reverse))
                        rgs (sorted (groups (mapv rf idx)) rsort)
                        cgs (sorted (groups (mapv cf idx)) csort)
                        ->set (fn [g] (set (map idx (:idx g))))
                        cell (fn [rows cols]
                               (let [is (filterv (every-pred rows cols) idx)]
                                 (when (seq is) (aggregate agg vals is 0))))
                        all (constantly true)
                        col-sets (cond-> (mapv ->set cgs) (pos? cdepth) (conj all))
                        lw (v/cols rf)
                        header-rows (for [k (range (v/cols cf))]
                                      (vec (concat (repeat lw nil)
                                                   (map #(nth (:label %) k) cgs)
                                                   (when (pos? cdepth)
                                                     [(when (zero? k) "Total")]))))
                        line (fn [label rows] (vec (concat label (map #(cell rows %) col-sets))))
                        body (map #(line (:label %) (->set %)) rgs)
                        total (line (cons "Total" (repeat (dec lw) nil)) all)]
                    (vec (concat header-rows
                                 (cond (zero? rdepth) body
                                       (pos? rdepth) (concat body [total])
                                       :else (cons total body)))))))))))))

(def functions
  {
   "VLOOKUP" (spec [3 4] vlookup :lift #{0})
   "HLOOKUP" (spec [3 4] hlookup :lift #{0})
   "LOOKUP"  (spec [2 3] lookup :lift #{0})
   "MATCH"   (spec [2 3] match-fn :lift #{0})
   "XMATCH"  (spec [2 4] xmatch :lift #{0})
   "XLOOKUP" (spec [3 6] xlookup :args :refs)

   "INDEX"    (spec [2 4] index-fn :args :refs)
   "OFFSET"   (spec [3 5] offset :args :refs :volatile true)
   "INDIRECT" (spec [1 2] indirect :args :refs :volatile true)
   "ROW"      (spec [0 1] (position-fn :r1 :r2 :row ->col addr/max-rows) :args :refs)
   "COLUMN"   (spec [0 1] (position-fn :c1 :c2 :col ->row addr/max-cols) :args :refs)
   "ROWS"     (spec [1 1] (size-fn first) :args :refs)
   "COLUMNS"  (spec [1 1] (size-fn second) :args :refs)
   "AREAS"    (spec [1 1] areas :args :refs)
   "ADDRESS"  (spec [2 5] address :lift :all)
   "TRANSPOSE" (spec [1 1] (comp v/transpose u/->2d))

   "FILTER"   (spec [2 3] filter-fn :errors :pass)
   "SORT"     (spec [1 4] sort-fn)
   "SORTBY"   (spec [2 nil] sortby-fn)
   "UNIQUE"   (spec [1 3] unique-fn)
   "SEQUENCE" (spec [1 4] sequence-fn)
   "RANDARRAY" (spec [0 5] randarray :ctx? true :volatile true)
   "HSTACK"   (spec [1 nil] hstack :errors :pass)
   "VSTACK"   (spec [1 nil] vstack :errors :pass)
   "TAKE"     (spec [2 3] (slicer take-n))
   "DROP"     (spec [2 3] (slicer drop-n))
   "CHOOSEROWS" (spec [2 nil] chooserows)
   "CHOOSECOLS" (spec [2 nil] choosecols)
   "TOCOL"    (spec [1 3] (flatten-fn ->col))
   "TOROW"    (spec [1 3] (flatten-fn ->row))
   "WRAPROWS" (spec [2 3] wraprows :errors :pass)
   "WRAPCOLS" (spec [2 3] wrapcols :errors :pass)
   "EXPAND"   (spec [2 4] expand :errors :pass)
   "TRIMRANGE" (spec [1 3] trimrange :args :refs)
   "GROUPBY"  {:arity [3 8] :args :lazy :impl groupby}
   "PIVOTBY"  {:arity [4 11] :args :lazy :impl pivotby}})
