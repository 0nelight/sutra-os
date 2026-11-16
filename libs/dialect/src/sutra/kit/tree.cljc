(ns sutra.kit.tree
  (:require [clojure.walk :as walk]))

(def ^:dynamic *nodes-memo*
  nil)

(defn- nodes* [x] (filterv map? (tree-seq coll? seq x)))

(defn nodes
  [x]
  (if-let [m *nodes-memo*]
    (let [k #?(:clj (System/identityHashCode x) :cljs (hash x))]
      (or (some (fn [[x' ns]] (when (identical? x x') ns)) (get @m k))
          (let [ns (nodes* x)] (swap! m update k conj [x ns]) ns)))
    (filter map? (tree-seq coll? seq x))))

(defn reach
  [succ seeds]
  (loop [order [] seen #{} todo (vec seeds) i 0]
    (if (< i (count todo))
      (let [x (nth todo i)]
        (if (contains? seen x)
          (recur order seen todo (inc i))
          (recur (conj order x) (conj seen x) (into todo (succ x)) (inc i))))
      order)))

(defn canonical
  ([x] (canonical {} x))
  ([{:keys [order leaf] :or {order str leaf identity}} x]
   (walk/postwalk (fn [v]
                    (if (map? v)
                      (into (sorted-map-by #(compare (order %1) (order %2))) v)
                      (leaf v)))
                  x)))
