(ns ^:unit ontology.streaming-parity-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.builds.run.stream-index :as stream-index]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.streaming :as streaming]
            [sutra.types.authority :as authority]))

(def ^:private T "ri.sutra.main.object-type.00000000-0000-0000-0000-0000000000a1")
(def ^:private U "ri.sutra.main.object-type.00000000-0000-0000-0000-0000000000a2")

(def ^:private schema
  {:datasets {"ri.ds.calls" {:dataset-name :calls :stream {:partitions 1}}
              "ri.ds.plain" {:dataset-name :plain}}
   :functions {"ri.fn.clean" {:function-name :clean}}
   :object-types
   {T {:object-type-name :call
       :attribute-list [{:attribute-name :call-id :attribute-type :string}
                        {:attribute-name :seen :attribute-type :timestamp}
                        {:attribute-name :note :attribute-type :string}]
       :backing {:dataset :calls :primary-key :call-id}}
    U {:object-type-name :thing
       :attribute-list [{:attribute-name :id :attribute-type :string}]
       :backing {:dataset :plain :primary-key :id}}}
   :actions {"ri.a.note" {:action-name :note-call
                          :rules [{:op :update :object-type T}]}
             "ri.a.ring" {:action-name :ring
                          :rules [{:op :invoke-webhook :webhook :ring}]}}})

(deftest a-stream-backs-a-type-exactly-once-by-default
  (is (= {T {:stream "ri.ds.calls" :direct? false :consistency :exactly-once
             :source-timestamp nil}}
         (streaming/stream-backed schema))))

(deftest a-streaming-type-takes-no-edits-a-direct-one-does
  (is (= [{:action :note-call :object-type :call}] (streaming/edit-violations schema))
      "an action that edits the stream-backed type; a webhook action is no edit")
  (is (empty? (streaming/edit-violations
               (assoc-in schema [:object-types T :backing :direct] true)))))

(deftest a-streaming-type-has-one-datasource
  (let [s (assoc schema :transforms
                 {"ri.t.more" {:transform-name :more
                               :outputs [{:kind :object-type :object-type T
                                          :role :supplement}]}})]
    (is (= [{:transform :more :object-type :call}]
           (streaming/multi-datasource-violations s)))))

(deftest backing-settings-hold-only-where-they-mean-something
  (let [with (fn [ot b] (update-in schema [:object-types ot :backing] merge b))
        reasons #(map :reason (streaming/setting-violations %))]
    (is (empty? (reasons (with T {:direct true :source-timestamp :seen
                                  :consistency :at-least-once}))))
    (is (re-find #"only a stream-backed" (first (reasons (with U {:direct true})))))
    (is (re-find #"none of" (first (reasons (with T {:consistency :twice})))))
    (is (re-find #"set :direct true" (first (reasons (with T {:source-timestamp :seen})))))
    (is (re-find #"no timestamp property"
                 (first (reasons (with T {:direct true :source-timestamp :note})))))))

(deftest a-streaming-transform-needs-a-stream-and-a-function
  (let [derive #(assoc schema :datasets
                       (assoc (:datasets schema) "ri.ds.clean"
                              {:dataset-name :clean :stream {:derive %}}))]
    (is (empty? (streaming/derive-violations (derive {:from :calls :function :clean}))))
    (is (re-find #"names no stream"
                 (:reason (first (streaming/derive-violations
                                  (derive {:from :plain :function :clean}))))))
    (is (re-find #"names no function"
                 (:reason (first (streaming/derive-violations
                                  (derive {:from :calls :function :nope}))))))))

(def ^:private stream {:stream-name "calls" :spec {}})

(defn- entry [offset record] {:partition 0 :offset offset :record record})

(defn- at [mm ss] (format "2026-11-11T10:%02d:%02dZ" mm ss))

(deftest a-direct-write-older-than-its-object-is-dropped
  (let [backing {:primary-key :call-id}
        decide  (fn [entries stored]
                  (stream-index/index-decision schema stream T backing entries
                                               {:ts-attr :seen :stored stored}))
        call    (fn [o ts] (entry o {:call_id "a" :seen ts}))
        id      (-> (decide [(call 0 (at 0 0))] {}) :ops first :payload :id)
        seen    #(map (fn [op] (get-in op [:payload :values :seen])) (:ops %))]
    (testing "the latest source timestamp of a key wins inside a batch, not the last record"
      (is (= [(at 3 20)] (seen (decide [(call 0 (at 3 20)) (call 1 (at 2 30))] {})))))
    (testing "a record older than the stored object is dropped and counted"
      (let [{:keys [ops result]} (decide [(call 2 (at 0 10))] {id (at 1 40)})]
        (is (empty? ops))
        (is (= 1 (:out-of-order result)))))
    (is (= [(at 2 0)] (seen (decide [(call 3 (at 2 0))] {id (at 1 40)}))))))

(deftest an-exactly-once-mark-skips-what-was-indexed
  (let [mark (stream-index/advanced-mark nil [(entry 4 {}) (entry 7 {})])]
    (is (= {0 7} mark))
    (is (= [8] (map :offset (stream-index/unmarked mark [(entry 6 {}) (entry 7 {})
                                                          (entry 8 {})]))))
    (is (= [0] (map :offset (stream-index/unmarked nil [(entry 0 {})]))))))

(def ^:private lineage
  {:organizations {"ri.o.acme" {:organization-name :acme :members ["acme-staff"]}
                   "ri.o.lief" {:organization-name :lief :members ["lief-staff"]}}
   :spaces   {"ri.s.acme"   {:space-name :privat :organization :acme}
              "ri.s.shared" {:space-name :gemeinsam :organization :acme
                             :shared-with [:lief]}}
   :projects {"ri.p.priv"  {:project-name :priv :space "ri.s.acme" :markings [:pii]}
              "ri.p.share" {:project-name :share :space "ri.s.shared"}}
   :markings {:pii {}}
   :datasets {"ri.ds.raw"  {:dataset-name :raw :project "ri.p.priv" :stream {}}
              "ri.ds.copy" {:dataset-name :copy :project "ri.p.share"
                            :stream {:derive {:from :raw :function :clean}}}
              "ri.ds.free" {:dataset-name :free :project "ri.p.share"
                            :stream {:derive {:from :raw :function :clean
                                              :stop-requiring [:acme]
                                              :stop-propagating [:pii]}}}}})

(deftest a-streaming-transform-carries-and-lifts-like-a-transform
  (let [ben {:who "ben" :memberships [["ben" "lief-staff"]] :clears []}
        db  (authority/database (authority/schema-facts lineage)
                                (authority/role-facts grants/standard-roles)
                                (authority/subject-facts ben))
        blocked (authority/blocked db "ben")]
    (is (contains? blocked "ri.ds.copy")
        "a stream derived from acme's private one stays acme's and stays marked")
    (is (not (contains? blocked "ri.ds.free"))
        "its :derive stops requiring acme and propagating pii: lief reads it")))
