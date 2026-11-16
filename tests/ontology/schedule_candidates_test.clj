(ns ^:unit ontology.schedule-candidates-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.lineage.graph :as lineage]))

(def ^:private edges
  [{:name "pull"      :inputs [{:kind :datasource :name "imap"}]
    :outputs [{:kind :dataset :name "raw_messages"}
              {:kind :dataset :name "raw_attachments"}]}
   {:name "map-email" :inputs [{:kind :dataset :name "raw_messages"}]
    :outputs [{:kind :object-type :name "email"}]}
   {:name "classify"  :inputs [{:kind :object-type :name "email"}
                               {:kind :language-model :name "grok"}]
    :outputs [{:kind :object-type :name "lead"}]}])

(deftest watch-candidates-test
  (let [watch-candidates lineage/watch-candidates]
    (testing "a data node's parents are the inputs of the transform(s) that produce it"
      (is (= [{:kind :dataset :name "raw_messages"}]
             (watch-candidates edges "email"))
          "email ← map-email ← raw_messages")
      (is (= [{:kind :object-type :name "email"}]
             (watch-candidates edges "lead"))
          "lead ← classify ← email; the language-model input is dropped"))
    (testing "a source-fed terminal has no watchable parent (⇒ use a cadence)"
      (is (= [] (watch-candidates edges "raw_messages"))
          "raw_messages ← pull ← ds:imap; a datasource root is not watchable")
      (is (= [] (watch-candidates edges "raw_attachments"))
          "the SECOND output of the same multi-output producer resolves the same way —
           keyed on a singular :output this answered 'no producer at all', which is
           the right answer here for the wrong reason and the wrong one the moment
           such a producer has a watchable input"))
    (testing "naming a transform directly yields its own watchable inputs"
      (is (= [{:kind :dataset :name "raw_messages"}]
             (watch-candidates edges "map-email"))))
    (testing "an unknown target yields no candidates"
      (is (= [] (watch-candidates edges "nope"))))
    (testing "EVERY output of a multi-output producer finds it"
      (let [multi [{:name "pull" :inputs [{:kind :dataset :name "staging"}]
                    :outputs [{:kind :dataset :name "a"} {:kind :dataset :name "b"}]}]]
        (is (= [{:kind :dataset :name "staging"}] (watch-candidates multi "a")))
        (is (= [{:kind :dataset :name "staging"}] (watch-candidates multi "b"))
            "the second output must not report as having no producer")))))
