(ns ^:unit grammar.omit-clears-test
  (:require [sutra.ref.install :as ri]
            [clojure.test :refer [deftest is testing]]))

(defn- merged
  ([section old value] (merged section old value {}))
  ([section old value extra]
   (:value (ri/merge-existing {section {:x old}}
                              (merge {:op :set :path [section :x] :value value} extra)))))

(deftest a-source-never-revives-its-cipher-pointer
  (testing "cleared even when the caller restates it — the cipher is minted in-platform"
    (is (nil? (:secret-ref (merged :sources {:secret-ref "alt"} {:type :postgres}))))
    (is (nil? (:secret-ref (merged :sources {:secret-ref "alt"} {:secret-ref "neu"}))))))

(deftest a-flipped-sync-loses-its-key-with-its-mode
  (is (nil? (:primary-key (merged :transforms
                                  {:primary-key [:name] :output {:kind :dataset}}
                                  {:language :pg-sql}))))
  (testing "a restated key survives, and an object-type output is not a sync"
    (is (= [:nr] (:primary-key (merged :transforms
                                       {:primary-key [:name] :output {:kind :dataset}}
                                       {:language :pg-sql :primary-key [:nr]}))))
    (is (= [:name] (:primary-key (merged :transforms
                                         {:primary-key [:name] :output {:kind :object-type}}
                                         {:language :pg-sql}))))))
