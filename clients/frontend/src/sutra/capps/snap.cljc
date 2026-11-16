(ns sutra.capps.snap)

(defn angle-at [c p] (Math/atan2 (- (second p) (second c)) (- (first p) (first c))))

(def snap-tolerance
  (/ Math/PI 30))

(def quarter (/ Math/PI 2))

(defn- edge-angles
  [coords]
  (for [[a b] (map vector coords (rest coords)) :when (not= a b)] (angle-at a b)))

(defn snap-directions
  [draft refs]
  (concat (map #(* % (/ Math/PI 4)) (range 8))
          (for [a (mapcat edge-angles (cons draft refs)) k (range 4)] (+ a (* k quarter)))))

(defn angle-gap
  [a b]
  (let [full (* 2 Math/PI)
        d    (mod (- a b) full)]
    (min d (- full d))))

(defn- cm [x] (/ (Math/round (* x 100)) 100.0))

(defn along
  [a ang len]
  [(cm (+ (first a) (* len (Math/cos ang)))) (cm (+ (second a) (* len (Math/sin ang))))])

(defn snap-point
  [draft p dirs]
  (let [a (peek (vec draft))]
    (if-not a
      p
      (let [ang  (angle-at a p)
            len  (Math/hypot (- (first p) (first a)) (- (second p) (second a)))
            best (when (and (pos? len) (seq dirs)) (apply min-key #(angle-gap ang %) dirs))]
        (if (and best (<= (angle-gap ang best) snap-tolerance))
          (along a best (cm len))
          p)))))

(defn edge-angle
  [draft dir]
  (let [last-ang (when (next draft) (apply angle-at (take-last 2 draft)))]
    (case dir
      :right 0
      :up    quarter
      :left  Math/PI
      :down  (- quarter)
      :straight     last-ang
      :left-square  (some-> last-ang (+ quarter))
      :right-square (some-> last-ang (- quarter))
      nil)))
