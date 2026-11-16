(ns sutra.kit.cache
  (:import (java.util Collections LinkedHashMap Map)))

(set! *warn-on-reflection* true)

(defn lru
  ^Map [n]
  (Collections/synchronizedMap
   (proxy [LinkedHashMap] [16 0.75 true]
     (removeEldestEntry [_]
       (> (.size ^LinkedHashMap this) (long n))))))

(defn through
  [^Map c k f]
  (let [v (.getOrDefault c k ::miss)]
    (if (identical? ::miss v)
      (let [v (f)]
        (.putIfAbsent c k v)
        (.getOrDefault c k v))
      v)))

(defn once
  [f]
  (let [held (atom {})]
    (fn [k]
      (let [d (delay (f k))]
        (force (get (swap! held #(cond-> % (not (contains? % k)) (assoc k d))) k))))))

(defn clear! [^Map c]
  (.clear c)
  nil)
