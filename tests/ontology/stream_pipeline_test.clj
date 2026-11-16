(ns ^:api ontology.stream-pipeline-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]))

(use-fixtures :once pod/with-mem-pod)

(defn- col [n t] {:attribute-name n :attribute-type t})

(def ^:private schema
  {:schema
   {:projects {:streams {}}
    :datasets
    {:sensor_readings
     {:project :streams
      :kind    :standalone
      :columns [(col :sensor_id :string)
                (col :temperature :double)
                (col :retired :boolean)]
      :stream  {:partitions       2
                :primary-key      {:columns [:sensor_id] :deletion-column :retired}
                :archive-every-ms 1000}}}
    :objects
    {:reading {:project    :streams
               :symbol     "🌡"
               :attributes [[:sensor-id :string] [:temperature :double]]
               :backing    {:dataset :sensor_readings :primary-key :sensor-id}
               :plural "Readings"}}}})

(defn- body [r] (:body r))

(defn- readings []
  (:rows (body (api/list-objects :reading))))

(defn- reading-temps []
  (into {}
        (map (fn [r] [(get-in r [:values :sensor-id]) (get-in r [:values :temperature])]))
        (readings)))

(deftest stream-end-to-end
  (testing "a dataset that declares :stream installs"
    (is (ok? (api/post! :install schema)) "install stages")
    (is (ok? (api/post! :staging)) "commit"))

  (testing "it shows up as a stream, with its CDC key folded into the partition keys"
    (let [s (body (api/get! :stream-by {:name (name :sensor_readings)}))]
      (is (= 2 (:partitions s)))
      (is (true? (:cdc? s)))
      (is (= [:sensor_id] (get-in s [:spec :partition-keys]))
          "the primary key's columns are partition keys too — otherwise updates to
           one key could arrive on two partitions and have no order to resolve")
      (is (= 0 (:buffered s)) "nothing published yet")))

  (testing "a batch carrying an undeclared column is refused whole"
    (let [r (api/post! :stream-records {:name (name :sensor_readings)}
                       {:records (vec [{:sensor_id "s1" :temperature 20.0}
{:sensor_id "s2" :nonsense 1}])})]
      (is (= 400 (:status r)) "the batch is rejected, not partially accepted")
      (is (zero? (:buffered (body (api/get! :stream-by {:name (name :sensor_readings)}))))
          "and nothing from it landed")))

  (testing "publishing answers with the offset each record took"
    (let [offs (:offsets (body (api/post! :stream-records {:name (name :sensor_readings)}
                                          {:records
                                             (vec [{:sensor_id "s1" :temperature 20.0
                                                    :retired false}
{:sensor_id "s2" :temperature 30.0 :retired false}
{:sensor_id "s1" :temperature 21.5 :retired false}])})))
          [a b c] offs]
      (is (= 3 (count offs)))
      (is (= (:partition a) (:partition c))
          "both s1 records took the same partition — a keyed record is not spread")
      (is (= [0 1] [(:offset a) (:offset c)])
          "and consecutive offsets within it, which is the order guarantee")
      (is (zero? (:offset b)) "s2's partition starts at 0 of its own"))
    (let [offsets (:end-offsets (body (api/get! :stream-offsets {:name (name :sensor_readings)})))]
      (is (= 3 (reduce + 0 (vals offsets)))
          "the end offsets account for every published record"))
    (is (some? (:last-record-at (body (api/get! :stream-by {:name (name :sensor_readings)}))))
        "the stream says when its newest record arrived"))

  (testing "records read back by offset, oldest first"
    (let [s1 (->> (range 2)
                  (mapcat (fn [part]
                            (:records (body (api/stream-records
                                             :sensor_readings
                                             {:partition part :start-offset 0 :limit 10})))))
                  (filter #(= "s1" (get-in % [:record :sensor_id])))
                  (sort-by :offset))]
      (is (= [20.0 21.5] (mapv #(get-in % [:record :temperature]) s1))
          "the two updates to s1 come back in publication order")))

  (testing "the current view resolves the changelog before anything is archived"
    (let [v (body (api/get! :stream-view {:name (name :sensor_readings) :limit nil}))]
      (is (= 3 (:hot v)) "all three records are still in the buffer")
      (is (= 0 (:cold v)) "none archived yet")
      (is (= #{"s1" "s2"} (into #{} (map :sensor_id) (:rows v)))
          "one row per key")
      (is (= 21.5 (some #(when (= "s1" (:sensor_id %)) (:temperature %)) (:rows v)))
          "and the most recent record for s1 wins")))

  (testing "a flush indexes the Ontology and archives the current view"
    (is (ok? (api/post! :stream-archive {:name (name :sensor_readings)} {})))
    (is (= {"s1" 21.5 "s2" 30.0} (reading-temps))
        "the object index holds the resolved view, not the changelog")
    (let [v (body (api/get! :stream-view {:name (name :sensor_readings) :limit nil}))]
      (is (= 2 (:cold v)) "two rows archived — one per key")))

  (testing "a record whose deletion column is true retracts its key"
    (is (ok? (api/post! :stream-records {:name (name :sensor_readings)}
                        {:records (vec [{:sensor_id "s2" :temperature 30.0 :retired true}])})))
    (is (ok? (api/post! :stream-archive {:name (name :sensor_readings)} {})))
    (is (= {"s1" 21.5} (reading-temps)) "s2 is gone from the object index")
    (is (= #{"s1"}
           (into #{} (map :sensor_id)
                 (:rows (body (api/get! :stream-view {:name (name :sensor_readings) :limit nil})))))
        "and from the current view"))

  (testing "reset clears an ingest stream: its buffer and its archive"
    (is (ok? (api/post! :stream-records {:name (name :sensor_readings)}
                        {:records (vec [{:sensor_id "s3" :temperature 9.0 :retired false}])})))
    (is (ok? (api/post! :stream-reset {:name (name :sensor_readings)} (or nil {}))))
    (let [s (body (api/get! :stream-by {:name (name :sensor_readings)}))]
      (is (= 0 (:buffered s)) "the buffer is empty")
      (is (= 0 (:archived-rows s)) "and so is cold storage"))))
