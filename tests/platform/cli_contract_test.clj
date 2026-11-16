(ns ^:unit platform.cli-contract-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.cli.contract :as contract]))

(deftest unknown-verb-suggests-the-nearest
  (testing "a 1-edit typo of a real verb comes back as a suggestion"
    (let [r (contract/run nil [:describ {}])]
      (is (contract/blocked? r))
      (is (= :unknown-verb (:code (first (:errors r)))))
      (is (re-find #"describe" (:fix (first (:errors r)))))
      (is (some #{{:verb :describe}} (:next r))
          ":next offers the corrected verb as an invocable command")))
  (testing "a genuinely unknown word gets no misleading guess"
    (let [r (contract/run nil [:zzqqxx {}])]
      (is (contract/blocked? r))
      (is (empty? (:next r)))
      (is (nil? (:fix (first (:errors r))))))))

(deftest missing-required-args-become-next-slots
  (let [r (contract/run nil [:grant {}])]
    (is (contract/blocked? r))
    (is (= #{:missing-arg} (set (map :code (:errors r)))))
    (is (= #{:project :to} (set (map :need (:next r))))
        ":next names each missing required slot to fill")))

(deftest command-accepts-vector-or-map-form
  (testing "the {:verb :args} tool-call form is the same value as [:verb args]"
    (is (= (:errors (contract/run nil [:grant {}]))
           (:errors (contract/run nil {:verb :grant :args {}})))
        "a keypress and an LLM tool-call run through the same interpreter")))
