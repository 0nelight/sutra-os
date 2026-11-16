(ns sutra.foundation.boundaries
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def resource "sutra/boundaries.edn")

(defn register
  []
  (edn/read-string (slurp (io/resource resource))))

(defn boundaries
  [reg]
  (sort-by (comp :n val) (:boundaries reg)))

(defn owner-of-url
  [reg url]
  (some->> (for [[id row] (:boundaries reg)
                 p (get-in row [:covers :url-prefixes])
                 :when (str/starts-with? url p)]
             [(count p) id])
           seq
           (apply max-key first)
           second))

(defn endpoints-by-boundary
  [reg endpoints]
  (reduce-kv (fn [acc id {:keys [url]}] (update acc (owner-of-url reg url) (fnil conj #{}) id))
             {}
             endpoints))

(defn claimed-capabilities
  [reg]
  (for [[id row] (:boundaries reg)
        c (get-in row [:covers :capabilities])]
    [c id]))

(defn claimed-protocols
  [reg]
  (for [[id row] (:boundaries reg)
        p (get-in row [:covers :protocols])]
    [p id]))

(defn protocol-files
  [reg]
  (into (sorted-set) (map (comp first first)) (claimed-protocols reg)))

(defn protocols-in
  [s]
  (set (map second (re-seq #"\(defprotocol\s+([\w\-]+)" s))))

(defn named-files
  [reg]
  (for [[id row] (:boundaries reg)
        f (get-in row [:spec :source])]
    [id f]))
