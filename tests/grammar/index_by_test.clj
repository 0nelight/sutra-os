(ns grammar.index-by-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

(def ^:private roots
  ["src" "libs/core/src" "libs/dialect/src" "libs/kernel/src" "libs/world/src" "libs/commits/src"
   "libs/service/src" "plugins"])

(def ^:private by-hand #"\(juxt\s+(?:\S+|\([^()]*\))\s+identity\)")

(defn offenders []
  (into (sorted-set)
        (for [r roots
              ^java.io.File f (file-seq (io/file r))
              :let  [p (.getPath f)]
              :when (and (re-find #"\.cljc?$" (.getName f))
                         (not= p "libs/kernel/src/sutra/kit/coll.cljc")
                         (re-find by-hand (slurp f)))]
          p)))

(deftest indexes-go-through-index-by
  (is (= #{} (offenders))))
