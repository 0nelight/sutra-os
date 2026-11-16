(ns grammar.dialect-refusals-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(def ^:private roots ["libs/dialect/src" "libs/kernel/src"])

(def ^:private call #"\(anomaly/coded!?\s")

(def ^:private literal
  #"\(anomaly/coded!?\s+(:refusal\.[a-z0-9-]+/[a-z0-9-]+)\s+:([a-z-]+)\s+\"((?:[^\"\\]|\\.)*)\"")

(defn- source-files []
  (for [r roots
        f (file-seq (io/file r))
        :when (re-find #"\.cljc?$" (.getName ^java.io.File f))]
    f))

(defn- unescape [s] (edn/read-string (str "\"" s "\"")))

(defn calls
  []
  (let [per (for [f (source-files)
                  :let [t  (slurp f)
                        ms (re-seq literal t)
                        n  (count (re-seq call t))]
                  :when (pos? n)]
              {:file (str f) :calls n
               :found (for [[_ code cat tmpl] ms]
                        {:file (str f) :code (keyword (subs code 1)) :category (keyword cat)
                         :template (unescape tmpl)})})]
    {:found   (vec (mapcat :found per))
     :opaque  (into {} (for [{:keys [file calls found]} per
                             :when (not= calls (count found))]
                         [file (- calls (count found))]))}))

(def ^:private en (edn/read-string (slurp (io/resource "i18n/refusal.en.edn"))))
(def ^:private de (edn/read-string (slurp (io/resource "i18n/refusal.de.edn"))))
(def ^:private categories (edn/read-string (slurp (io/resource "refusal/category.edn"))))

(deftest every-dialect-refusal-is-the-catalogs
  (let [{:keys [found opaque]} (calls)]
    (testing "coded and coded! name their code, category and template as literals"
      (is (= {} opaque)))
    (testing "the catalog words every code in English and German"
      (is (= [] (vec (for [{:keys [code file]} found
                           :when (not (and (contains? en code) (contains? de code)))]
                       [file code])))))
    (testing "the category raised is the catalog's"
      (is (= [] (vec (for [{:keys [code category file]} found
                           :when (not= category (get categories code))]
                       [file code category (get categories code)])))))
    (testing "the English template is the catalog's English text"
      (is (= [] (vec (for [{:keys [code template file]} found
                           :when (not= template (get en code))]
                       [file code template (get en code)])))))))
