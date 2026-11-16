(ns ^:unit grammar.anomaly-markers-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(def ^:private roots
  ["src" "libs/core/src" "libs/dialect/src" "libs/kernel/src" "libs/world/src" "libs/commits/src"
   "libs/service/src" "libs/lib/src"
   "plugins/dot/src" "plugins/llm/src" "plugins/packages/src" "plugins/sso/src"
   "libs/store/src" "libs/store-pg/src"])

(defn- sources []
  (for [r roots
        :let [d (io/file r)]
        :when (.isDirectory d)
        ^java.io.File f (file-seq d)
        :when (re-find #"\.clj[cs]?$" (.getName f))]
    [(str/replace (.getPath f) "\\" "/") (slurp f)]))

(defn- count-of [re s] (count (re-seq re s)))

(def ^:private reads #"\((?::client-error\??|:http-status|:not-found\?) ")
(def ^:private writes #":(?:client-error\?? true|not-found\? true|http-status \d{3})")

(deftest no-file-reads-a-marker
  (doseq [[p s] (sources)]
    (is (zero? (count-of reads s)) (str p " reads a marker — ask anomaly/category"))))

(deftest no-file-writes-a-marker
  (doseq [[p s] (sources)]
    (is (zero? (count-of writes s))
        (str p " writes a marker — name a category (anomaly/raise!, a refusal code, a :ref/kind)"))))
