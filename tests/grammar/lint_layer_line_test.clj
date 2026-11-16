(ns ^:unit grammar.lint-layer-line-test
  (:require [sutra.parser.spec :as spec]
            [clojure.test :refer [deftest testing is]]
            [sutra.catalog.transforms :as transforms]
            [sutra.parser.lint :as lint]))

(def ^:private schema
  {:object-types
   {"rid.stunde"   {:object-type-name :stundenbuchung
                  :attribute-list [{:attribute-name :buchung-id :attribute-type :string}]
                    :plural "Stundenbuchungen"}
    "rid.arbeit" {:object-type-name :arbeitszeit
                  :attribute-list [{:attribute-name :az-id :attribute-type :string}]
                  :plural "Arbeitszeiten"}
    "rid.amos"   {:object-type-name :amos-bestellung
                  :attribute-list [{:attribute-name :bestell-id :attribute-type :string}
                                   {:attribute-name :resolved-bestellung-id
                                    :attribute-type :string}]
                  :plural "Amos bestellungen"}
    "rid.import" {:object-type-name :teile-import
                  :attribute-list [{:attribute-name :import-id :attribute-type :string}]
                  :plural "Teile importe"}
    "rid.satz"   {:object-type-name :teilebuchung
                  :attribute-list [{:attribute-name :buchung-id :attribute-type :string}
                                   {:attribute-name :import-id :attribute-type :string}]
                  :plural "Teilebuchungen"}}
   :datasets {"rid.ds.raw" {:dataset-name :zeitbuchung_raw}
              "rid.ds.mat" {:dataset-name :zeitbuchung_mat
                            :kind :materialization
                            :object-type "rid.stunde"}}
   :transforms
   {    "rid.t.agg"  {:transform-name :arbeitszeit-rows
                  :inputs {:zeit {:kind :object-type :object-type "rid.stunde"}}
                  :output {:kind :object-type :object-type "rid.arbeit"}}
    "rid.t.shape" {:transform-name :shape-stundenbuchung
                   :inputs {:raw {:kind :dataset :dataset "rid.ds.raw"}}
                   :output {:kind :object-type :object-type "rid.stunde"}}
    "rid.t.supp" {:transform-name :resolve-amos-bestellung
                  :inputs {:amos {:kind :object-type :object-type "rid.amos"}}
                  :output {:kind :object-type :object-type "rid.amos" :role :supplement}}
    "rid.t.self" {:transform-name :resolve-satz
                  :inputs {:satz {:kind :object-type :object-type "rid.satz"}}
                  :output {:kind :object-type :object-type "rid.satz"}}
    "rid.t.sync-bad" {:transform-name :stundenbuchung-rows
                      :input  {:kind :datasource :datasource :amos-db}
                      :source {:table "Stundenbuchung"}
                      :output {:kind :object-type :object-type "rid.stunde"}}
    "rid.t.sync-ok"  {:transform-name :zeitbuchung_raw-sync
                      :input  {:kind :datasource :datasource :amos-db}
                      :source {:table "Stundenbuchung"}
                      :output {:kind :dataset :dataset "rid.ds.raw"}}
    "rid.t.imp"  {:transform-name :map-imports
                  :inputs {:up {:kind :dataset :dataset "rid.ds.raw"}}
                  :output {:kind :object-type :object-type "rid.import"}
                  :primary-key :import-id}
    "rid.t.satz" {:transform-name :teilebuchungen-parsen
                  :inputs {:up {:kind :dataset :dataset "rid.ds.raw"}}
                  :output {:kind :object-type :object-type "rid.satz"}
                  :primary-key :buchung-id}
    "rid.t.mat"  {:transform-name :zeitbuchung_mat-materialize
                  :input  {:kind :object-type :object-type "rid.stunde"}
                  :output {:kind :dataset :dataset "rid.ds.mat"}}
    "rid.t.fk"   {:transform-name :zeit-arbeit-conn-rows
                  :input  {:kind :object-type :object-type "rid.stunde"}
                  :fk-attribute :az-id
                  :output {:kind :link-type :link-type "rid.c.za"}}}})

(defn- of [rule]
  (->> (lint/run-lint schema (spec/vocabulary)) (filter #(= rule (:rule %))) (map :entity) set))

(deftest ontology-as-pipeline-intermediate
  (let [hits (of :ontology-as-pipeline-intermediate)]
    (testing "an object-type → object-type transform is dataset-layer work"
      (is (contains? hits "arbeitszeit-rows")))
    (testing "the shaping hop is the CORRECT shape and must stay silent"
      (is (not (contains? hits "shape-stundenbuchung"))
          "dataset in → object-type out is exactly what the rule asks for"))
    (testing "a §7b supplement is a declared backing, not a pipeline hop"
      (is (not (contains? hits "resolve-amos-bestellung"))))))

(deftest transform-writes-own-input
  (let [hits (of :transform-writes-own-input)]
    (testing "reading and writing the SAME type is the pipeline mutating in place"
      (is (contains? hits "resolve-satz")))
    (testing "unless declared :role :supplement — prod's four all are"
      (is (not (contains? hits "resolve-amos-bestellung"))))))

(deftest etl-column-as-property
  (let [hits (of :etl-column-as-property)]
    (testing "resolution scratch and ETL batch refs are flagged"
      (is (contains? hits "amos-bestellung.resolved-bestellung-id"))
      (is (contains? hits "teilebuchung.import-id")))
    (testing "a type's own PRIMARY KEY is identity, not scratch"
      (is (not (contains? hits "teile-import.import-id"))
          "teile-import is KEYED by import-id — hiding its PK is nonsense"))
    (testing "an ordinary business column is untouched"
      (is (not (contains? hits "stundenbuchung.buchung-id"))))))

(deftest source-syncs-into-ontology
  (let [hits (of :source-syncs-into-ontology)]
    (testing "a datasource bound straight to an object type is the violation"
      (is (contains? hits "stundenbuchung-rows")))
    (testing "the same source bound to a DATASET is the fix, not a finding"
      (is (not (contains? hits "zeitbuchung_raw-sync"))
          "outputting a dataset is exactly what the rule asks for"))
    (testing "a shaping hop reading that dataset is untouched"
      (is (not (contains? hits "shape-stundenbuchung"))))))

(def ^:private need-schema
  {:object-types {"rid.a" {:object-type-name :edited-type}
                  "rid.b" {:object-type-name :plain-type}}
   :actions {:setzen {:rules [{:op :modify :object-type :edited-type}]}}
   :datasets {"rid.m" {:dataset-name :plain_mat :kind :materialization :object-type "rid.b"}
              "rid.e" {:dataset-name :edited_mat :kind :materialization :object-type "rid.a"}
              "rid.o" {:dataset-name :plain-table :kind :object-backing}}
   :transforms {"t.p" {:transform-name :shape-plain
                       :output {:kind :object-type :object-type "rid.b"}}
                "t.r" {:transform-name :liest-tabelle
                       :inputs {:x {:kind :dataset :dataset "rid.o"}}
                       :output {:kind :dataset :dataset "rid.ziel"}}}})

(defn- rule-hits [schema rule]
  (->> (lint/run-lint schema (spec/vocabulary)) (filter #(= rule (:rule %)))))

(deftest a-materialization-is-for-when-it-is-needed
  (testing "one over a type no action edits is named, one over an edited type is not"
    (is (= #{"plain_mat"}
           (set (map :entity (rule-hits need-schema :materialization-without-edits))))))
  (testing "a read of a type's object table has no build order"
    (is (= #{"liest-tabelle"}
           (set (map :entity (rule-hits need-schema :transform-reads-object-table)))))))

(deftest a-dataset-read-builds-after-what-writes-it
  (let [schema {:datasets {"ds.p" {:dataset-name :shape-p}}
                :object-types {"ot.p" {:object-type-name :p}}
                :transforms {"t.p" {:transform-name :shape-p
                                    :outputs [{:kind :object-type :object-type "ot.p"
                                               :dataset "ds.p"}]}
                             "t.c" {:transform-name :c
                                    :inputs {:x {:kind :dataset :dataset "ds.p"}}}}}]
    (is (contains? (set (transforms/full-deps schema (get-in schema [:transforms "t.c"])))
                   "t.p"))))

(deftest a-cycle-is-what-runs-in-a-circle
  (testing "what builds after a cycle is not part of it"
    (is (= #{:a :b}
           (transforms/cycle-core {:a #{:b} :b #{:a} :danach #{:a} :spaeter #{:danach}}
                                  #{:a :b :danach :spaeter}))))
  (testing "a node between two cycles keeps them one"
    (is (= #{:a :b :m :c :d}
           (transforms/cycle-core {:a #{:b} :b #{:a} :m #{:a} :c #{:d :m} :d #{:c}}
                                  #{:a :b :m :c :d})))))

(deftest an-action-editing-through-its-function-counts-as-editing
  (let [schema {:object-types {"rid.a" {:object-type-name :edited-type}}
                :datasets {:a_mat {:dataset-name :a_mat :kind :materialization
                                   :object-type "rid.a"}}
                :actions {:buchen {:rules [{:op :apply-edit-function :function :f}]
                                   :edits [{:object-type "rid.a"}]}}}]
    (is (empty? (rule-hits schema :materialization-without-edits)))))
