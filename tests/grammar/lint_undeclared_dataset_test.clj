(ns ^:unit grammar.lint-undeclared-dataset-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.parser.lint :as lint]
            [sutra.parser.spec :as spec]))

(def ^:private ds "ri.sutra.main.dataset.11111111-1111-4111-8111-111111111111")

(defn- schema [input upload]
  {:datasets     {ds {:dataset-name :da :kind :standalone}}
   :transforms   {"ri.t" {:transform-name :t :language :python :code ""
                          :inputs {:a {:kind :media-set :dataset input}}
                          :output {:kind :dataset :dataset ds}}}
   :applications {"ri.a" {:application-name :a :layout {:upload {:dataset upload}}}}})

(defn- findings [s]
  (filter #(= :names-undeclared-dataset (:rule %)) (lint/run-lint s (spec/vocabulary))))

(deftest an-input-or-upload-naming-no-dataset-is-flagged
  (let [f (findings (schema :weg :auch-weg))]
    (is (= #{"t" "a"} (set (map :entity f))))
    (is (every? #(= :warn (:severity %)) f))
    (is (re-find #"declared nowhere" (:message (first f))))))

(deftest a-rid-or-a-name-of-a-declared-dataset-passes
  (is (empty? (findings (schema ds :da))))
  (is (empty? (findings (schema :da ds)))))
