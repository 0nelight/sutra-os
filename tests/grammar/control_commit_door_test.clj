(ns grammar.control-commit-door-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(def ^:private roots ["src" "plugins"])

(def ^:private store-dir (str (io/file "src" "sutra" "store") java.io.File/separator))

(def ^:private by-hand #"\(ctl/(?:plan!|plan-as!|served)[\s)]")

(defn offenders
  []
  (into (sorted-map)
        (for [r     roots
              ^java.io.File f (file-seq (io/file r))
              :let  [p (.getPath f)]
              :when (and (re-find #"\.cljc?$" (.getName f))
                         (not (str/starts-with? p store-dir)))
              :let  [n (count (re-seq by-hand (slurp f)))]
              :when (pos? n)]
          [p n])))

(deftest modules-write-through-commit
  (is (= {} (offenders))
      "write with (ctl/commit! store [[:upsert t rows] …]) instead of ctl/plan! / served"))
