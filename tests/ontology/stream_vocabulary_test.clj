(ns ^:unit ontology.stream-vocabulary-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.types.stream :as strm]))

(def ^:private cdc
  (strm/normalize {:partitions 4
                   :primary-key {:columns [:key] :deletion-column :gone}}))

(deftest normalize-folds-the-cdc-key-into-the-partition-keys
  (testing "a dedup column is a partition key too, or its updates have no order"
    (is (= [:key] (:partition-keys cdc)))
    (is (= [:key :other] (:partition-keys
                          (strm/normalize {:partition-keys [:key :other]
                                           :primary-key {:columns [:key]}})))
        "already-named partition keys keep their position and are not duplicated"))

  (testing "partitions are clamped to what a stream can actually carry"
    (is (= 16 (:partitions (strm/normalize {:partitions 99}))))
    (is (= 1 (:partitions (strm/normalize {:partitions 0})))))

  (testing "an absent :stream stays absent — nil is not an empty stream"
    (is (nil? (strm/normalize nil)))))

(deftest partition-assignment-keeps-a-key-together
  (testing "the same key always lands on the same partition"
    (is (apply = (map #(strm/partition-for cdc {:key "abc"} %) (range 20)))))

  (testing "different keys do spread over the partitions"
    (is (< 1 (count (into #{} (map #(strm/partition-for cdc {:key (str %)} 0)) (range 50))))))

  (testing "an unkeyed stream goes round-robin, because nothing promises order"
    (let [plain (strm/normalize {:partitions 4})]
      (is (= [0 1 2 3 0] (mapv #(strm/partition-for plain {:v %} %) (range 5))))))

  (testing "a single-partition stream is partition 0 whatever the key"
    (is (zero? (strm/partition-for (strm/normalize {:partitions 1}) {:key "z"} 7)))))

(deftest current-view-resolves-the-changelog
  (let [records [{:key "k1" :v "first"  :gone false}
                 {:key "k2" :v "second" :gone false}
                 {:key "k1" :v "third"  :gone true}
                 {:key "k2" :v nil      :gone false}]]
    (testing "one row per key, latest wins, retractions dropped"
      (is (= [{:key "k2" :v nil :gone false}] (strm/current-view cdc records))))

    (testing "the latest record wins WHOLE — a null is a value, not a gap to fill"
      (is (nil? (:v (first (strm/current-view cdc records))))))

    (testing "without a primary key there is nothing to resolve, so nothing is dropped"
      (is (= 4 (count (strm/current-view (strm/normalize {}) records)))))))

(deftest record-problems-name-the-column
  (let [cols [:key :v :gone]]
    (testing "an undeclared column is refused"
      (is (seq (strm/record-problems cdc cols {:key "k" :nope 1}))))
    (testing "a null primary key is refused — it cannot be resolved into a view"
      (is (seq (strm/record-problems cdc cols {:key nil :v 1}))))
    (testing "a declared record is admitted"
      (is (empty? (strm/record-problems cdc cols {:key "k" :v 1 :gone false}))))
    (testing "with no declared columns the schema cannot judge a column"
      (is (empty? (strm/record-problems (strm/normalize {}) [] {:anything 1}))))))

(deftest deletion-is-only-meaningful-with-a-key
  (is (true? (strm/deleted? cdc {:key "k" :gone true})))
  (is (true? (strm/deleted? cdc {:key "k" :gone "yes"})))
  (is (false? (strm/deleted? cdc {:key "k" :gone "false"})))
  (is (false? (strm/deleted? cdc {:key "k"})))
  (is (false? (strm/deleted? (strm/normalize {}) {:gone true}))
      "a stream with no primary key has nothing for a deletion to be about"))
