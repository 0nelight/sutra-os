(ns ^:unit grammar.reference-rules-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.parser.ent]
            [sutra.parser.resolve :as res]))

(def ^:private edit-rule
  {:actions {:x {:rules [{:op :update-of-interface :interface :nur-auf-prod}]}}})

(deftest resolution-is-judged-under-the-world-emptiness-always
  (testing "a named interface the file relies on prod for: a partial lint says nothing"
    (is (empty? (res/findings edit-rule))))
  (testing "…and the authoritative door, told prod does not have it either, refuses"
    (is (= 1 (count (res/findings edit-rule {:interface-names #{:other}})))))
  (testing "…and told prod has it, accepts"
    (is (empty? (res/findings edit-rule {:interface-names #{:nur-auf-prod}}))))
  (testing "an EMPTY required slot is refused in every world"
    (let [s {:actions {:x {:rules [{:op :update-of-interface}]}}}]
      (is (re-find #"names no :interface" (:message (first (res/findings s)))))
      (is (re-find #"names no :interface" (:message (first (res/findings s {}))))))))

(deftest optional-slots-name-nothing-when-empty
  (is (empty? (res/findings {:agents {:a {:agent-name :helfer :tools [{:kind :action}]}}
                             :llm-models {"m" {}}})))
  (testing "but a required one does — the grammar requires a retrieval's type"
    (is (re-find #"names no :retrieval object-type"
                 (:message (first (res/findings {:agents {:a {:agent-name :helfer
                                                              :retrieval {:kind :semantic-search}}}
                                                 :objects {}})))))))
