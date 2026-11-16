(ns sutra.builds.ledger.stack-map
  (:require [sutra.foundation.encoding :as enc]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def ^:private b64-idx
  (into {} (map-indexed (fn [i c] [c i])
                        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")))

(defn- vlq
  [s]
  (loop [cs (seq s), shift 0, acc 0, out []]
    (if-let [c (first cs)]
      (let [d     (b64-idx c 0)
            cont? (pos? (bit-and d 32))
            acc   (+ acc (bit-shift-left (bit-and d 31) shift))]
        (if cont?
          (recur (rest cs) (+ shift 5) acc out)
          (recur (rest cs) 0 0
                 (conj out (if (odd? acc) (- (unsigned-bit-shift-right acc 1))
                               (unsigned-bit-shift-right acc 1))))))
      out)))

(defn- resolve-inner
  [mp ll lc]
  (let [sources (get mp "sources"), names (get mp "names")
        lines   (str/split (str (get mp "mappings")) #";" -1)]
    (when (< ll (count lines))
      (loop [li 0, si 0, sl 0, sc 0, ni 0]
        (let [[si2 sl2 sc2 ni2 _ segs]
              (reduce
               (fn [[si sl sc ni gcol segs] seg]
                 (if (str/blank? seg)
                   [si sl sc ni gcol segs]
                   (let [d    (vlq seg)
                         gcol (+ gcol (nth d 0 0))
                         has4 (>= (count d) 4)
                         si   (if has4 (+ si (nth d 1)) si)
                         sl   (if has4 (+ sl (nth d 2)) sl)
                         sc   (if has4 (+ sc (nth d 3)) sc)
                         ni   (if (>= (count d) 5) (+ ni (nth d 4)) ni)]
                     [si sl sc ni gcol (conj segs [gcol si sl sc ni])])))
               [si sl sc ni 0 []]
               (str/split (nth lines li) #"," -1))]
          (if (= li ll)
            (let [cands (filter (fn [x] (<= (nth x 0) lc)) segs)
                  x     (if (seq cands) (last cands) (first segs))]
              (when x
                (str (nth sources (nth x 1) "?") ":" (inc (nth x 2)) ":" (inc (nth x 3))
                     (when-let [nm (nth names (nth x 4) nil)] (str "  (" nm ")")))))
            (recur (inc li) (long si2) (long sl2) (long sc2) (long ni2))))))))

(defn- find-section
  [secs gl gc]
  (reduce (fn [chosen s]
            (let [off (get s "offset")]
              (if (<= (compare [(get off "line") (get off "column")] [gl gc]) 0)
                s chosen)))
          nil secs))

(def ^:private map-dirs
  ["resources/public/js" "/app/resources/public/js" "/usr/share/nginx/html/js"])

(defn- read-map
  [bundle]
  (some (fn [src] (some-> src slurp enc/json-or-nil))
        (cons (io/resource (str "public/js/" bundle ".map"))
              (for [d map-dirs
                    :let [f (io/file d (str bundle ".map"))]
                    :when (.exists f)]
                f))))

(def ^:private smap (memoize read-map))

(defn- resolve-frame [bundle line1 col1]
  (when-let [m (smap bundle)]
    (let [gl (dec line1), gc (dec col1)
          s  (find-section (get m "sections") gl gc)]
      (when s
        (let [off (get s "offset")
              ll  (- gl (get off "line"))
              lc  (- gc (if (zero? ll) (get off "column") 0))]
          (resolve-inner (get s "map") ll lc))))))

(def frame-re
  #"/js/([A-Za-z][\w-]*(?:\.[0-9A-F]+)?\.js):(\d+):(\d+)")

(defn map-stack
  [stack]
  (let [frames (re-seq frame-re (str stack))]
    (if (empty? frames)
      (str stack)
      (->> frames
           (map (fn [[whole bundle l c]]
                  (str "  " whole "  ->  "
                       (or (try (resolve-frame bundle (Long/parseLong l) (Long/parseLong c))
                                (catch Exception e (str "map-error: " (.getMessage e))))
                           "?"))))
           (str/join "\n")))))
