(ns ^:unit ontology.stream-index-decision-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.builds.run.stream-index :as stream-index]))

(def ^:private T "ri.sutra.main.object-type.00000000-0000-0000-0000-00000000abcd")

(def ^:private schema
  {:object-types
   {T {:object-type-name :reading
      :attribute-list [{:attribute-name :sensor-id :attribute-type :string}
                       {:attribute-name :label :attribute-type :string}]}}})

(def ^:private stream
  {:stream-name "readings" :spec {:primary-key {:deletion-column :gone}}})

(def ^:private backing {:primary-key :sensor-id})

(defn- entry [offset record] {:partition 0 :offset offset :record record})

(defn- decide
  ([entries] (decide backing entries))
  ([backing entries] (stream-index/index-decision schema stream T backing entries)))

(deftest without-a-primary-key-nothing-is-indexed
  (let [{:keys [ops rejects result]} (decide {} [(entry 0 {:sensor_id "a"})])]
    (is (empty? ops))
    (is (empty? rejects))
    (is (re-find #"no primary key" (:error result)))))

(deftest a-refused-record-becomes-a-dead-letter-the-rest-is-indexed
  (let [{:keys [ops rejects result]} (decide [(entry 0 {:sensor_id "good" :label "ok"})
                                              (entry 1 {:sensor_id "bad" :label ""})])]
    (is (= [:create-object] (map :op ops)))
    (is (= ["pk=\"bad\""] rejects))
    (is (= {:indexed 1 :retired 0 :rejected 1}
           (select-keys result [:indexed :retired :rejected])))
    (is (= [1] (map :offset (:dead-letters result))))
    (is (string? (:reason (first (:dead-letters result)))))))

(deftest the-last-record-of-a-key-wins
  (let [{:keys [ops]} (decide [(entry 0 {:sensor_id "a" :label "old"})
                               (entry 1 {:sensor_id "a" :label "new"})])]
    (is (= ["new"] (map #(get-in % [:payload :values :label]) ops)))))

(deftest a-deletion-retires-and-is-not-judged
  (let [{:keys [ops result]} (decide [(entry 0 {:sensor_id "a" :label "" :gone true})])]
    (is (= [:delete-object] (map :op ops)))
    (is (= {:indexed 0 :retired 1 :rejected 0}
           (select-keys result [:indexed :retired :rejected])))))

(deftest a-fault-of-the-type-refuses-the-whole-batch
  (let [schema (assoc-in schema [:object-types T :attribute-list 0 :attribute-type] :double)
        {:keys [ops result]} (stream-index/index-decision schema stream T backing
                                                [(entry 0 {:sensor_id 1.5 :label "x"})])]
    (is (empty? ops))
    (is (= 0 (:indexed result)))
    (is (some? (:error result)))
    (is (= [0] (map :offset (:dead-letters result))))))

(deftest a-sync-cursor-stands-at-the-highest-row-it-read
  (let [sync {:cursor :row_ver}]
    (is (= 7 (stream-index/pulled-cursor sync 3 [{:row_ver 7} {:row_ver 5}])))
    (is (= 3 (stream-index/pulled-cursor sync 3 [])))
    (is (= 3 (stream-index/pulled-cursor sync 3 [{:id 1}])))))

(deftest a-shape-answers-one-row-for-each-record
  (is (= [{:partition 0 :offset 1 :record {:nummer "43"}}]
         (stream-index/shape-entries :f [(entry 1 {:number "043"})] [{:nummer "43"}])))
  (is (thrown? clojure.lang.ExceptionInfo
               (stream-index/shape-entries :f [(entry 1 {:number "043"})] []))))

(deftest a-sync-with-every-ms-waits-for-its-interval
  (let [stream {:spec {:sync {:every-ms 60000}}}]
    (is (stream-index/sync-due? stream nil 1000))
    (is (not (stream-index/sync-due? stream 1000 60999)))
    (is (stream-index/sync-due? stream 1000 61000)))
  (is (stream-index/sync-due? {:spec {:sync {}}} 1000 1001)))
