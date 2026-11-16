(ns ^:api ontology.flaw-rejected-skipped-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]))

(use-fixtures :once pod/with-mem-pod)

(defn- col [n t] {:attribute-name n :attribute-type t})

(def ^:private schema
  {:schema
   {:projects {:flawrs {}}
    :datasets
    {:rs_readings
     {:project :flawrs
      :kind    :standalone
      :columns [(col :sensor_id :string) (col :label :string)]
      :stream  {:partitions 1
                :primary-key {:columns [:sensor_id]}}}}
    :objects
    {:rsreading {:project    :flawrs
                 :symbol     "🌡"
                 :attributes [[:sensor-id :string] [:label :string]]
                 :backing    {:dataset :rs_readings :primary-key :sensor-id}
                 :plural "Rsreadings"}}}})

(defn- body [r] (:body r))
(defn- stream [] (body (api/get! :stream-by {:name "rs_readings"})))

(defn- index-offset []
  (reduce + 0 (vals (get-in (stream) [:cursors :index :offsets]))))

(deftest a-refused-stream-record-goes-to-a-dead-letter-set
  (testing "setup"
    (is (ok? (api/post! :install schema)))
    (is (ok? (api/post! :staging))))
  (testing "one good record and one the restrictions refuse (empty string)"
    (let [r (api/post! :stream-records {:name "rs_readings"}
                       {:records [{:sensor_id "good" :label "ok"}
                                  {:sensor_id "bad" :label ""}]})]
      (is (ok? r) (pr-str (body r))))
    (let [r (api/post! :stream-archive {:name "rs_readings"} {})]
      (is (ok? r) (pr-str (body r)))
      (is (= 1 (get-in (body r) [:indexed :types 0 :rejected]))
          "the indexer refused exactly the empty-string record")))
  (testing "the cursor moved past both records"
    (is (= 2 (index-offset))))
  (testing "the refused record is kept in a dead-letter set, not dropped"
    (let [s (stream)
          dead (or (:dead-letter s) (:rejected s) (get-in s [:cursors :index :dead-letter]))]
      (is (some #(= "bad" (or (:sensor_id %) (get-in % [:record :sensor_id]))) dead)
          (str "no dead-letter entry for the refused record; stream status keys: "
               (pr-str (sort (keys s))))))))
