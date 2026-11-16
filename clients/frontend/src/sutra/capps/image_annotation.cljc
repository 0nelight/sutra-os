(ns sutra.capps.image-annotation
  (:require [clojure.string :as str]))

(def max-annotations 1000)

(defn- abs* [x] (if (neg? x) (- x) x))

(def default-color "#2d72d2")

(defn parse-box
  [s]
  (let [ns (map parse-double (str/split (str/trim (str s)) #"\s*,\s*"))]
    (when (and (= 4 (count ns)) (every? some? ns))
      (let [[a b c d] ns]
        {:x1 (min a c) :y1 (min b d) :x2 (max a c) :y2 (max b d)}))))

(defn box-text
  [{:keys [x1 y1 x2 y2]}]
  (str (long x1) ", " (long y1) ", " (long x2) ", " (long y2)))

(defn- field
  [row k]
  (when k
    (if (contains? row k) (get row k) (get (:values row) k))))

(defn shapes
  [{:keys [rows box-prop color-prop active]}]
  (let [active-id (:id (if (sequential? active) (first active) active))]
    (into []
          (comp (keep (fn [row]
                        (when-let [box (parse-box (field row box-prop))]
                          (let [c (str (field row color-prop))]
                            {:row     row
                             :box     box
                             :color   (if (re-matches #"#[0-9A-Fa-f]{3,8}" c) c default-color)
                             :active? (and (some? active-id) (= active-id (:id row)))}))))
                (take max-annotations))
          rows)))

(defn clamp-point
  [[x y] {:keys [w h]}]
  [(max 0 (min x w)) (max 0 (min y h))])

(def ^:private min-pixels 3)

(defn draw
  [state [kind point]]
  (case kind
    :start  {:from point :to point}
    :move   (if (:from state) (assoc state :to point) state)
    :finish (when-let [[x0 y0] (:from state)]
              (let [[x1 y1] (or point (:to state))]
                (when (and (>= (abs* (- x1 x0)) min-pixels) (>= (abs* (- y1 y0)) min-pixels))
                  {:done (box-text {:x1 (min x0 x1) :y1 (min y0 y1)
                                    :x2 (max x0 x1) :y2 (max y0 y1)})})))
    state))

(defn drawing-box
  [{:keys [from to]}]
  (when (and from to)
    (let [[x0 y0] from
          [x1 y1] to]
      {:x (min x0 x1) :y (min y0 y1) :width (abs* (- x1 x0)) :height (abs* (- y1 y0))})))
