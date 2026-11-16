(ns sutra.kit.size)

(def klassen
  [[:kompakt 700] [:mittel 1199] [:breit 1799] [:weit nil]])

(defn klasse
  [breite]
  (some (fn [[k bis]] (when (or (nil? bis) (<= breite bis)) k)) klassen))

(defn ab
  [k]
  (let [ks (mapv first klassen)]
    (subvec ks 0 (min (count ks) (inc (count (take-while #(not= % k) ks)))))))

(defn reicht?
  [k mindestens]
  (boolean (some #{mindestens} (ab k))))
