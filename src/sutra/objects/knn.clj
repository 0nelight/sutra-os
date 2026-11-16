(ns sutra.objects.knn
  (:require [clojure.string :as str]
            [sutra.objects.ports :as ports]
            [sutra.ref.value :as rv]))

(set! *warn-on-reflection* true)

(defn embed
  [store schema model text]
  (when-let [f (ports/llm-embed store)]
    (f store {:model model :text text :caller "objects-knn" :branch (:branch store)
              :schema schema})))

(def module
  {:name :knn
   :wants #{:llm/relay-embed}})

(defn- ->doubles [v] (mapv double v))

(defn- dot [a b] (reduce + 0.0 (map * a b)))
(defn- norm [a] (Math/sqrt (dot a a)))

(defn score
  [metric q v]
  (case metric
    :dot (dot q v)
    :l2  (- (Math/sqrt (reduce + 0.0 (map (fn [x y] (let [d (- x y)] (* d d))) q v))))
    (let [qn (norm q) vn (norm v)]
      (if (or (zero? qn) (zero? vn)) 0.0 (/ (dot q v) (* qn vn))))))

(defn nearest
  [query-vec objects {:keys [property k metric]}]
  (let [metric (or metric :cosine)
        k      (or k 10)
        q      (->doubles query-vec)
        dim    (count q)]
    (->> objects
         (keep (fn [o]
                 (let [v (rv/value o property)]
                   (when (and (sequential? v) (= dim (count v)))
                     (assoc o :score (score metric q (->doubles v)))))))
         (sort-by :score >)
         (take k)
         vec)))

(defn nearest-paged
  [query-vec fetch {:keys [k page] :or {page 500} :as opts}]
  (let [k (or k 10)]
    (loop [off 0 best []]
      (let [rows (fetch off page)
            best (->> (concat best (nearest query-vec rows (assoc opts :k k)))
                      (sort-by :score >)
                      (take k)
                      vec)]
        (if (< (count rows) page) best (recur (+ off (long page)) best))))))

(defn parse-query-vector
  [v]
  (cond
    (sequential? v) (when (every? number? v) (mapv double v))
    (string? v)     (let [s (str/trim v)
                          s (if (and (str/starts-with? s "[") (str/ends-with? s "]"))
                              (subs s 1 (dec (count s))) s)
                          parts (remove str/blank? (map str/trim (str/split s #",")))]
                      (try (when (seq parts) (mapv #(Double/parseDouble %) parts))
                           (catch NumberFormatException _ nil)))
    :else nil))
