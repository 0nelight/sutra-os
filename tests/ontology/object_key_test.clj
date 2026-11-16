(ns ^:unit ontology.object-key-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.store.lib :as lib]
            [sutra.types.world-rules :as rules]))

(def ^:private schema
  {:object-types {:kunde   {:object-type-name :kunde :match-keys [:kunde-id] :plural "Kunden"}
                  :person  {:object-type-name :person :match-keys [:email :telefon]
                            :resolve-entity true
                            :plural "Personen"}
                  :notiz   {:object-type-name :notiz :plural "Notizen"}
                  :auftrag {:object-type-name :auftrag :match-keys [:nr] :plural "Aufträge"}}
   :transforms {:shape-auftrag {:transform-name :shape-auftrag :primary-key :nr
                                :output {:kind :object-type :object-type :auftrag}}}})

(deftest the-key-of-a-type
  (is (= {:properties [:nr] :source :primary-key} (rules/object-key schema :auftrag))
      "a minted type is keyed by its producer's primary key")
  (is (= {:properties [:kunde-id] :source :match-keys} (rules/object-key schema :kunde))
      "a type that exists only through actions by its :match-keys")
  (is (nil? (rules/object-key schema :person))
      "entity resolution merges ON its match keys — they are not one key")
  (is (nil? (rules/object-key schema :notiz)) "no key: the object id is the key"))

(defn- create [type id values]
  {:op :create-object :type type :payload {:id id :values values}})

(deftest collisions-are-judged-over-the-whole-submission
  (let [holders (fn [_ _ tuples] (for [t tuples :when (= ["k-1"] t)] {:id "live-1" :key t}))]
    (testing "a key a live object holds"
      (is (= [{:key ["k-1"] :taken-by "live-1" :object-type :kunde :properties [:kunde-id]}]
             (lib/key-collisions schema [(create :kunde "n-1" {:kunde-id "k-1"})] holders))))
    (testing "an object the same submission deletes first gives its key up"
      (is (empty? (lib/key-collisions schema
                                      [{:op :delete-object :type :kunde :payload {:id "live-1"}}
                                       (create :kunde "n-1" {:kunde-id "k-1"})]
                                      holders))))
    (testing "two creates of one key in one submission"
      (is (= [:same-submission]
             (map :taken-by (lib/key-collisions schema [(create :kunde "a" {:kunde-id "k-2"})
                                                        (create :kunde "b" {:kunde-id "k-2"})]
                                                holders)))))
    (testing "keyless types and effect events pass"
      (is (empty? (lib/key-collisions schema [(create :notiz "a" {}) (create :notiz "b" {})
                                              {:op :create :target {:type :kunde}}]
                                      holders))))))
