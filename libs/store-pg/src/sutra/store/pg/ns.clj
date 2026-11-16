(ns sutra.store.pg.ns
  (:require [sutra.store.pg.ident :as ident]))

(set! *warn-on-reflection* true)

(defn main? [ns] (= "main" (str ns)))

(defn read-schema [ns]
  (if (main? ns) "main" (ident/fit (str "br_" (ident/sanitize ns)))))

(defn own-schema [ns]
  (let [s (read-schema ns)] (if (= "main" s) "main" (ident/fit (str "bo_" (subs s 3))))))
