(ns sutra.capps.plan-faces)

(def ^:private eps 0.005)

(defn- k [[x y]] [(Math/round (* 100.0 x)) (Math/round (* 100.0 y))])

(defn- pt [[kx ky]] [(/ kx 100.0) (/ ky 100.0)])

(defn- sub [[ax ay] [bx by]] [(- ax bx) (- ay by)])

(defn- cross [[ax ay] [bx by]] (- (* ax by) (* ay bx)))

(defn- dot [[ax ay] [bx by]] (+ (* ax bx) (* ay by)))

(defn- len [v] (Math/sqrt (dot v v)))

(defn- param-on
  [a b p]
  (let [ab (sub b a) l (len ab)]
    (when (pos? l)
      (let [t (/ (dot (sub p a) ab) (* l l))]
        (when (and (<= (- (/ eps l)) t (+ 1 (/ eps l)))
                   (<= (/ (Math/abs (double (cross ab (sub p a)))) l) eps))
          t)))))

(defn- crossing
  [[a b] [c d]]
  (let [r (sub b a) s (sub d c) den (cross r s)]
    (when (> (Math/abs (double den)) 1e-12)
      (let [t (/ (cross (sub c a) s) den)
            u (/ (cross (sub c a) r) den)]
        (when (and (< 0 t 1) (< 0 u 1))
          [(+ (first a) (* t (first r))) (+ (second a) (* t (second r)))])))))

(defn- pieces
  [segs]
  (let [ends (distinct (mapcat identity segs))
        xs   (for [[i s] (map-indexed vector segs) [j t] (map-indexed vector segs)
                   :when (< i j) :let [p (crossing s t)] :when p] p)
        pts  (concat ends xs)]
    (into #{}
          (comp (mapcat (fn [[a b]]
                          (let [ts (->> pts (keep #(param-on a b %)) (cons 0) (cons 1)
                                        distinct sort)
                                at #(let [d (sub b a)]
                                      (k [(+ (first a) (* % (first d)))
                                          (+ (second a) (* % (second d)))]))]
                            (map (fn [t u] [(at t) (at u)]) ts (rest ts)))))
                (remove (fn [[u v]] (= u v)))
                (map (fn [[u v]] (if (neg? (compare u v)) [u v] [v u]))))
          segs)))

(defn- adjacency [edges]
  (reduce (fn [m [u v]] (-> m (update u (fnil conj #{}) v) (update v (fnil conj #{}) u)))
          {} edges))

(defn- pruned
  [adj]
  (loop [adj adj]
    (if-let [u (some (fn [[u vs]] (when (< (count vs) 2) u)) adj)]
      (recur (reduce (fn [m v]
                       (let [m (update m v disj u)] (if (empty? (get m v)) (dissoc m v) m)))
                     (dissoc adj u) (get adj u)))
      adj)))

(defn- angle [u v] (let [[dx dy] (sub v u)] (Math/atan2 dy dx)))

(defn- next-edge
  [adj u v]
  (let [back (angle v u)
        turn #(let [a (mod (- (angle v %) back) (* 2 Math/PI))] (if (< a 1e-9) 0 a))]
    (apply max-key turn (get adj v))))

(defn- signed-area [ring]
  (/ (reduce + (map (fn [[x1 y1] [x2 y2]] (- (* x1 y2) (* x2 y1))) ring (rest ring))) 2.0))

(defn- straightened
  [ring]
  (let [open (vec (butlast ring))
        n    (count open)
        keep (keep-indexed (fn [i p]
                             (let [a (nth open (mod (dec i) n)) b (nth open (mod (inc i) n))]
                               (when (> (Math/abs (double (cross (sub p a) (sub b p)))) 1e-9) p)))
                           open)]
    (conj (vec keep) (first keep))))

(defn- traced-faces [adj]
  (loop [todo (for [[u vs] adj v vs] [u v]) seen #{} faces []]
    (if-let [[u v :as h] (first todo)]
      (if (seen h)
        (recur (rest todo) seen faces)
        (let [walk (loop [a u b v acc [u] seen seen]
                     (if (and (= b u) (< 1 (count acc)))
                       [(conj acc u) (conj seen [a b])]
                       (let [c (next-edge adj a b)]
                         (recur b c (conj acc b) (conj seen [a b])))))
              [ks seen] walk
              ring (mapv pt ks)]
          (recur (rest todo) (conj seen h)
                 (if (> (signed-area ring) 1e-6) (conj faces ring) faces))))
      faces)))

(defn point-in-ring?
  [[x y] ring]
  (->> (map vector ring (rest ring))
       (filter (fn [[[_ y1] [_ y2]]] (not= (> y1 y) (> y2 y))))
       (filter (fn [[[x1 y1] [x2 y2]]] (< x (+ x1 (/ (* (- y y1) (- x2 x1)) (- y2 y1))))))
       count odd?))

(defn- inner-point
  [ring]
  (let [[a b] (apply max-key (fn [[a b]] (len (sub b a))) (map vector ring (rest ring)))
        [dx dy] (sub b a)
        l       (len [dx dy])
        [mx my] [(/ (+ (first a) (first b)) 2) (/ (+ (second a) (second b)) 2)]]
    [(- mx (* 0.02 (/ dy l))) (+ my (* 0.02 (/ dx l)))]))

(defn- edges-of [ring] (map vector ring (rest ring)))

(defn- closed-ring
  [ring]
  (let [r (vec ring)] (if (= (first r) (peek r)) r (conj r (first r)))))

(defn faces
  [outline rooms walls]
  (let [outline (closed-ring outline)
        rooms   (update-vals rooms closed-ring)
        segs    (concat (edges-of outline) (mapcat edges-of (vals rooms)) walls)
        fs      (->> (pieces segs) adjacency pruned traced-faces
                     (map straightened)
                     (filter #(point-in-ring? (inner-point %) outline)))
        owners  (fn [f] (let [p (inner-point f)]
                          (vec (keep (fn [[rk r]] (when (point-in-ring? p r) rk)) rooms))))
        out     (for [f fs :let [os (owners f)]]
                  {:ring f :area (signed-area f) :room (first os) :owners os})
        per     (frequencies (keep :room out))]
    {:faces    (mapv #(dissoc % :owners) out)
     :problems (vec (concat
                     (for [{:keys [owners]} out :when (next owners)] [:overlap owners])
                     (for [[rk n] per :when (< 1 n)] [:cut rk])
                     (for [rk (keys rooms) :when (not (contains? per rk))] [:outside rk])))}))

(defn ring-edges
  [rings]
  (mapcat (fn [r] (edges-of (closed-ring r))) rings))

(defn- cm [[x y]] [(/ (Math/round (* 100.0 x)) 100.0) (/ (Math/round (* 100.0 y)) 100.0)])

(defn- closest-on
  [[a b] p]
  (let [ab (sub b a) l2 (dot ab ab)]
    (if (zero? l2)
      a
      (let [t (max 0.0 (min 1.0 (/ (dot (sub p a) ab) l2)))]
        [(+ (first a) (* t (first ab))) (+ (second a) (* t (second ab)))]))))

(defn onto-wall
  [segs p tol]
  (let [dist   #(len (sub % p))
        corner (->> segs (mapcat identity) (filter #(<= (dist %) tol)) (sort-by dist) first)]
    (if corner
      (cm corner)
      (some->> segs (map #(closest-on % p)) (filter #(<= (dist %) tol)) (sort-by dist) first cm))))

(defn across
  [segs a ang]
  (let [r [(Math/cos ang) (Math/sin ang)]
        hits (keep (fn [[c d]]
                     (let [s (sub d c) den (cross r s)]
                       (when (> (Math/abs (double den)) 1e-12)
                         (let [t (/ (cross (sub c a) s) den)
                               u (/ (cross (sub c a) r) den)]
                           (when (and (> t 0.01) (<= -1e-9 u (+ 1 1e-9))) t)))))
                   segs)]
    (when (seq hits)
      (let [t (apply min hits)]
        (cm [(+ (first a) (* t (first r))) (+ (second a) (* t (second r)))])))))
