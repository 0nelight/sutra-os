(ns sutra.streams.conformance
  (:require [clojure.test :refer [is testing]]
            [sutra.streams.api :as s]))

(set! *warn-on-reflection* true)

(def ^:private a {:branch "main" :stream "h_conf_a"})

(defn- at [ms] (java.util.Date. (long ms)))

(defn- entries [p & ns]
  (mapv (fn [n] {:partition p :key (str "k" n) :at (at (* 1000 n)) :record {:n n}}) ns))

(defn check-append-read [make]
  (testing "append and read"
    (let [streams (make)]
      (is (= {} (s/ends streams a)) "an empty stream ends at nothing")
      (is (= [] (s/read streams a)))
      (is (= [{:partition 0 :offset 0} {:partition 0 :offset 1} {:partition 1 :offset 0}]
             (s/append! streams a (into (entries 0 1 2) (entries 1 3))))
          "offsets per partition, dense, in entry order")
      (is (= [{:partition 0 :offset 2}] (s/append! streams a (entries 0 4))))
      (is (= {0 3 1 1} (s/ends streams a)))
      (testing "a read is ordered by partition, then offset, and carries what was appended"
        (is (= [[0 0] [0 1] [0 2] [1 0]] (mapv (juxt :partition :offset) (s/read streams a))))
        (is (= {:partition 0 :offset 0 :key "k1" :at (at 1000) :record {:n 1}}
               (first (s/read streams a)))))
      (testing ":from is a cursor, :partition and :limit narrow"
        (is (= [[0 2]] (mapv (juxt :partition :offset) (s/read streams a {:from {0 2 1 1}}))))
        (is (= [[1 0]] (mapv (juxt :partition :offset) (s/read streams a {:partition 1}))))
        (is (= 2 (count (s/read streams a {:limit 2})))))
      (testing "reading to ends misses nothing"
        (is (= [] (s/read streams a {:from (s/ends streams a)}))))
      (testing "a stream belongs to its branch"
        (is (= [] (s/read streams (assoc a :branch "other"))))))))

(defn check-trim-reset [make]
  (testing "trim, reset, drop"
    (let [streams (make)]
      (s/append! streams a (into (entries 0 1 2 3) (entries 1 4 5)))
      (is (= {:depth {0 3 1 2} :last-at (at 5000)} (s/stats streams a)))
      (testing "trim drops below the cursor and older than :before only"
        (is (= 1 (s/trim! streams a {:below {0 2 1 2} :before (at 1500)})))
        (is (= [[0 1] [0 2] [1 0] [1 1]] (mapv (juxt :partition :offset) (s/read streams a))))
        (is (= 2 (s/trim! streams a {:below {1 2}})))
        (is (= [[0 1] [0 2]] (mapv (juxt :partition :offset) (s/read streams a)))))
      (testing "offsets are never reused"
        (s/reset! streams a)
        (is (= [] (s/read streams a)))
        (is (= {0 3 1 2} (s/ends streams a)))
        (is (= [{:partition 0 :offset 3}] (s/append! streams a (entries 0 6)))))
      (testing "drop forgets the stream"
        (s/drop! streams a)
        (is (= {} (s/ends streams a)))
        (is (= {:depth {} :last-at nil} (s/stats streams a)))))))

(defn check-pure-helpers []
  (testing "advance and floor"
    (is (= {0 3 1 1} (s/advance {0 1} [{:partition 0 :offset 2} {:partition 1 :offset 0}])))
    (is (= {0 1 1 0} (s/floor [{0 3 1 2} {0 1}])))))

(defn check-all [make]
  (check-append-read make)
  (check-trim-reset make)
  (check-pure-helpers))
