(ns sutra.capps.media-view
  (:require [clojure.string :as str]
            [sutra.kit.coerce :refer [->kw]]))

(defn field
  [row k]
  (let [k (->kw k)]
    (if (contains? row k) (get row k) (get (:values row) k))))

(defn object-value
  [{:keys [object prop]}]
  (when (and object prop)
    (let [row (if (map? object) object (first object))]
      (when (map? row) (field row prop)))))

(defn source
  [cfg]
  (let [x (or (:src cfg) (object-value cfg))]
    (if (sequential? x) (first x) x)))

(defn clamp-seek
  [t duration]
  (when (number? t)
    (let [t (max 0 t)]
      (if (and (number? duration) (pos? duration)) (min t duration) t))))

(def ^:private max-frames 200)

(defn frame-times
  [{:keys [every times]} duration]
  (cond
    (sequential? times)
    (into [] (comp (filter number?) (filter #(>= % 0)) (take max-frames)) times)

    (and (number? every) (pos? every) (number? duration) (pos? duration))
    (into [] (take max-frames) (range 0 duration every))

    :else []))

(def ^:private scene-thresholds {:less 60 :standard 35 :more 18})

(defn- distance
  [a b]
  (if (and (seq a) (= (count a) (count b)))
    (/ (reduce + (map (fn [x y] (let [d (- x y)] (if (neg? d) (- d) d))) a b)) (count a))
    0))

(defn scene-times
  [samples sensitivity]
  (let [limit (get scene-thresholds (->kw sensitivity) (:standard scene-thresholds))]
    (first
     (reduce (fn [[acc prev] [t sig]]
               [(if (or (nil? prev) (> (distance prev sig) limit)) (conj acc t) acc) sig])
             [[] nil]
             samples))))

(defn seconds-text
  [t]
  (let [t (long t)
        s (mod t 60)]
    (str (quot t 60) ":" (when (< s 10) "0") s)))

(defn segments
  [{:keys [rows text start end speaker]}]
  (->> rows
       (keep (fn [row]
               (let [b (field row start)]
                 (when (number? b)
                   {:text    (str (field row text))
                    :start   b
                    :end     (let [e (field row end)] (if (number? e) e b))
                    :speaker (when speaker (some-> (field row speaker) str))
                    :row     row}))))
       (sort-by :start)
       vec))

(defn active-index
  [segs ms]
  (first (keep-indexed (fn [i {:keys [start end]}] (when (and (<= start ms) (< ms end)) i)) segs)))

(defn speakers
  [segs]
  (into [] (comp (keep :speaker) (distinct)) segs))

(defn speaker-bars
  [segs duration-ms]
  (let [total (max 1 (or duration-ms 0) (reduce max 0 (map :end segs)))
        pct   #(* 100.0 (/ % total))]
    (mapv (fn [{:keys [start end speaker]}]
            {:speaker speaker :left (pct start) :width (pct (- end start)) :start start})
          (remove #(str/blank? (:speaker %)) segs))))
