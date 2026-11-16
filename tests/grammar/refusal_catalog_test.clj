(ns grammar.refusal-catalog-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.kit.anomaly :as anomaly]))

(def ^:private en (edn/read-string (slurp "libs/core/resources/i18n/refusal.en.edn")))
(def ^:private de (edn/read-string (slurp "libs/core/resources/i18n/refusal.de.edn")))
(def ^:private category (edn/read-string (slurp "libs/core/resources/refusal/category.edn")))

(def ^:private code-re #":(refusal\.[a-z0-9-]+/[a-z0-9-]+)")

(defn- named-in
  [roots]
  (apply merge-with into
         {}
         (for [r     roots
               ^java.io.File f (file-seq (io/file r))
               :when (and (.isFile f)
                          (re-find #"\.(clj[cs]?|edn|bb|py|js)$" (.getName f))
                          (not (str/includes? (.getPath f) "node_modules")))
               [_ c] (re-seq code-re (slurp f))]
           {(keyword c) #{(.getPath f)}})))

(def ^:private raising-roots
  ["src" "libs" "plugins" "clients"])

(deftest every-code-stands-in-all-three
  (testing "an English text, a German text and a category"
    (is (= [] (vec (sort (remove #(and (de %) (category %)) (keys en))))))
    (is (= [] (vec (sort (remove en (keys de))))))
    (is (= [] (vec (sort (remove en (keys category)))))))
  (testing "a category is one sutra.kit.anomaly answers with"
    (is (= {} (into (sorted-map) (remove (comp anomaly/statuses val)) category)))))

(deftest every-raised-code-is-the-catalogs
  (is (= [] (vec (sort (for [[c ps] (named-in raising-roots) :when (not (en c))]
                         [c (first (sort ps))]))))))

(deftest no-code-outlives-its-last-use
  (let [named (named-in (conj raising-roots "tests"))]
    (is (= [] (vec (sort (remove named (keys en))))))))

(deftest no-code-is-built-at-run-time
  (is (= [] (vec (for [r     ["src" "libs/core/src" "libs/dialect/src" "libs/kernel/src"
                              "libs/world/src"
                              "libs/commits/src" "plugins"]
                       ^java.io.File f (file-seq (io/file r))
                       :when (re-find #"\.clj[cs]?$" (.getName f))
                       :when (re-find #"\(keyword\s+\"refusal\." (slurp f))]
                   (.getPath f))))))
