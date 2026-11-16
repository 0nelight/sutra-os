(ns ent-lsp.spec
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn load-spec
  ([] (load-spec (io/resource "ent_lsp/spec.edn")))
  ([src] (edn/read-string (slurp src))))

(defn match?
  [pattern path]
  (cond
    (empty? pattern) (empty? path)
    (= :** (first pattern)) (or (match? (rest pattern) path)
                                (and (seq path) (match? pattern (rest path))))
    (empty? path) false
    (or (= :* (first pattern)) (= (first pattern) (first path))) (match? (rest pattern) (rest path))
    :else false))

(defn kinds
  [k]
  (if (set? k) k #{k}))

(defn fill
  [template vars]
  (let [missing (volatile! false)
        out     (str/replace template #"\{([a-z-]+)\}"
                             (fn [[_ k]]
                               (if-some [v (get vars (keyword k))]
                                 (if (keyword? v) (name v) (str v))
                                 (do (vreset! missing true) ""))))]
    (when-not @missing out)))
