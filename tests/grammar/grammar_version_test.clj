(ns ^:unit grammar.grammar-version-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [sutra.parser.ent :as ent]
            [sutra.parser.grammar-schema :as gs]))

(defn- preset [ext] (edn/read-string (slurp (str "resources/presets/grammar." ext))))

(defn- with-version [ext v] (pr-str (assoc (preset ext) :sutra/grammar v)))

(defn- refusal [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-message e))))

(deftest a-text-may-name-its-grammar
  (testing "a schema text"
    (let [r (ent/read-text nil "t.ent" (with-version "ent" ent/grammar-version))]
      (is (map? (:schema r)))
      (is (not (contains? r :sutra/grammar)) "the version is not part of what a text holds")))
  (testing "each lean kind"
    (doseq [[ext kind] [["enta" :app] ["entp" :pipeline] ["entt" :doc-template]
                        ["entv" :view]]]
      (is (map? (:schema (ent/read-text kind (str "t." ext) (with-version ext "1"))))
          ext))))

(deftest a-newer-major-is-refused
  (is (re-find #"newer" (str (refusal #(ent/read-text nil "t.ent" (with-version "ent" "2.0"))))))
  (is (re-find #"newer"
               (str (refusal #(ent/read-text :pipeline "t.entp" (with-version "entp" "9"))))))
  (testing "a later minor of the same major is read"
    (is (map? (ent/read-text nil "t.ent" (with-version "ent" "1.99"))))))

(deftest a-version-is-a-version-string
  (is (re-find #"version string"
               (str (refusal #(ent/validate! (assoc (preset "ent") :sutra/grammar 1) :t))))))

(defn- errors-at-key [kind data]
  (filter #(some #{:sutra/grammar} (:in %)) (:errors (m/explain (ent/grammar-of kind) data))))

(deftest the-grammar-names-the-key
  (doseq [[ext kind] gs/languages]
    (is (empty? (errors-at-key kind (assoc (preset ext) :sutra/grammar "1.0"))) ext)
    (is (seq (errors-at-key kind (assoc (preset ext) :sutra/grammar "x"))) ext)))

(deftest the-published-schemas-are-current
  (doseq [ext (keys gs/languages)]
    (is (= (gs/render ext) (slurp (gs/file-of ext)))
        (str (gs/file-of ext) " is stale — run (grammar.grammar-version-test/-main)"))))

(defn -main [& _]
  (doseq [ext (keys gs/languages)
          :let [f (io/file (gs/file-of ext))]]
    (io/make-parents f)
    (spit f (gs/render ext))
    (println "wrote" (str f))))
