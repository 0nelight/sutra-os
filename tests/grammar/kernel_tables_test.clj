(ns ^:unit grammar.kernel-tables-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [sutra.catalog.problems :as problems]))

(deftest the-kernel-gates-are-declares
  (let [gates (edn/read-string (slurp (io/resource "gates.edn")))]
    (is (= gates
           (mapv #(select-keys % [:id :rule :relational? :interface-gate?]) @#'problems/gates)))))
