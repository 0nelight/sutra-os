(ns ^:unit ontology.authored-backing-test
  (:require [sutra.parser.spec :as spec]
            [clojure.test :refer [deftest is testing]]
            [sutra.authoring.install.live :as live]
            [sutra.catalog.retirement :as retirement]
            [support.in-project :refer [in-project]]
            [sutra.catalog.kernel :as kernel]
            [sutra.authoring.install.migration :as migration]
            [sutra.authoring.install.world :as onto-install]
            [sutra.builds.plan :as plan]
            [sutra.lineage.graph :as lineage]
            [sutra.parser.lint :as lint]
            [sutra.types.rid :as rid]))

(defn- ix [W] (live/world-index W))

(def ^:private schema
  {:object-types {:buchungssatz {:object-type-name :buchungssatz
                                 :backing {:dataset :satz-ds :primary-key :satz-id
                                           :column-map {:satz-id :SATZ_ID}}
                                 :plural "Buchungssätze"}
                  :upload-typ   {:object-type-name :upload-typ
                                 :backing {:dataset :upload-ds :primary-key :nr}
                                 :plural "Upload-Typen"}
                  :messung      {:object-type-name :messung
                                 :backing {:dataset :stream-ds :primary-key :id}
                                 :plural "Messungen"}
                  :direkt       {:object-type-name :direkt :plural "Direkte"}}
   :datasets     {:satz-ds   {:dataset-name :satz-ds}
                  :upload-ds {:dataset-name :upload-ds :kind :standalone}
                  :stream-ds {:dataset-name :stream-ds :kind :standalone
                              :stream {:partitions 1}}}
   :transforms
   {:fill-satz {:transform-name :fill-satz :language :pg-sql
                :inputs {:mro {:kind :datasource :datasource :mro}}
                :output {:kind :dataset :dataset :satz-ds}}
    :mk-direkt {:transform-name :mk-direkt
                :input  {:kind :datasource :datasource :mro}
                :source {:table "DIREKT"}
                :output {:kind :object-type :object-type :direkt :primary-key :nr}}}})

(deftest a-backed-type-is-planned-from-its-dataset
  (let [objects (:objects (plan/derive-plan-from-schema schema))]
    (testing "the entry reads the landed dataset with the backing's key and column map"
      (is (= {:from {:dataset :satz-ds} :role :primary
              :primary-key :satz-id :column-map {:satz-id :SATZ_ID}}
             (:buchungssatz objects))))
    (testing "a dataset nobody's transform fills (an upload) backs a type as well"
      (is (= {:dataset :upload-ds} (get-in objects [:upload-typ :from]))))
    (testing "a stream's dataset stays with the stream indexer"
      (is (not (contains? objects :messung))))
    (testing "a transform-produced type is planned as before"
      (is (= {:datasource :mro :table "DIREKT"} (get-in objects [:direkt :from]))))))

(deftest a-backed-type-is-a-build-target
  (testing "naming the type builds the dataset's producer"
    (let [t (plan/resolve-build-targets schema [:buchungssatz])]
      (is (= #{:fill-satz} (:transforms t)))
      (is (contains? (:object-types t) :buchungssatz))
      (is (empty? (:unresolved t)))))
  (testing "naming the producer or the dataset builds the type it backs"
    (is (contains? (:object-types (plan/resolve-build-targets schema [:fill-satz]))
                   :buchungssatz))
    (is (contains? (:object-types (plan/resolve-build-targets schema [:satz-ds]))
                   :buchungssatz))
    (is (contains? (:object-types (plan/resolve-build-targets schema [:upload-ds]))
                   :upload-typ)))
  (testing "the scoped plan keeps the type's entry and runs the producer"
    (let [{{only-ds :datasets} :node-scope :keys [import]}
          (plan/scoped-import-plan schema (plan/derive-plan-from-schema schema)
                                       [:buchungssatz])]
      (is (contains? (:objects import) :buchungssatz))
      (is (= #{:fill-satz} only-ds)))))

(deftest lineage-draws-the-backing-edge
  (testing "the type names its dataset"
    (let [g (lineage/graph schema :buchungssatz)]
      (is (= "satz-ds" (:backed-by g)))))
  (testing "the dataset names the type it backs, beside its producer"
    (let [g (lineage/graph schema :satz-ds)]
      (is (= ["buchungssatz"] (:backs g)))
      (is (= ["fill-satz"] (:produced-by g)))))
  (testing "related lists the edge from both ends"
    (is (some #(= {:kind "dataset" :name "satz-ds" :via "backed-by"} %)
              (:items (lineage/related schema :object-type :buchungssatz))))))

(defn- backing-findings [schema]
  (filterv #(= :backing-without-producer (:rule %)) (lint/run-lint schema (spec/vocabulary))))

(deftest the-lint-asks-who-fills-the-dataset
  (testing "a transform writing the dataset is its producer — no warning"
    (is (empty? (backing-findings (update schema :object-types select-keys [:buchungssatz])))))
  (testing "an upload or a stream fills its dataset from outside"
    (is (empty? (backing-findings (update schema :object-types select-keys
                                          [:upload-typ :messung])))))
  (testing "a dataset nothing fills is named"
    (let [fs (backing-findings (assoc-in schema [:object-types :waise]
                                         {:object-type-name :waise
                                          :backing {:dataset :leer-ds :primary-key :id}
                                          :plural "Waisen"}))]
      (is (= ["waise"] (mapv :entity fs)))
      (is (re-find #"nothing fills that dataset" (:message (first fs)))))))

(def ^:private ot-rid "ri.sutra.main.object-type.a3db06cd-7da8-4cf3-a5f8-52f0bdac7ed4")

(defn- prepared
  [value whole?]
  (:value (kernel/prepare {:object-types {ot-rid {:object-type-name :artikel :id ot-rid
                                   :backing {:dataset "ri.ds.x" :primary-key :nr}
                                                  :plural "Artikel"}}}
                          (cond-> {:op :set :path [:object-types ot-rid] :value value}
whole? (assoc :whole-declaration? true)))))

(deftest leaving-the-backing-out-unbinds-the-type
  (testing "a whole declaration without :backing retracts it"
    (is (not (contains? (prepared {:object-type-name :artikel :id ot-rid} true) :backing))))
  (testing "a partial update leaves it standing"
    (is (= {:dataset "ri.ds.x" :primary-key :nr}
           (:backing (prepared {:object-type-name :artikel :id ot-rid} false)))))
  (testing "a whole declaration that states it keeps it"
    (is (= {:dataset "ri.ds.y"}
           (:backing (prepared {:object-type-name :artikel :id ot-rid
                                :backing {:dataset "ri.ds.y"}} true))))))

(def ^:private prepped
  (:ok (onto-install/prepare-schema
        (in-project :t
                    {:datasets     {:satz_ds {:dataset-name :satz_ds :kind :standalone :columns []}}
                     :object-types
                       {:buchung {:attribute-list [{:attribute-name :nr :attribute-type :string}]
                                  :plural "Buchungen"
                                  :backing {:dataset :satz_ds :primary-key :nr}}}})
        {})))

(deftest a-cascading-dataset-delete-unbinds-the-backing
  (let [ds (rid/resolve-name->rid (rid/name->rid-index prepped) #{:datasets} :satz_ds)
        ot (rid/resolve-name->rid (rid/name->rid-index prepped) #{:object-types} :buchung)]
    (testing "the backing is a hard reference: a plain delete is refused"
      (is (= [:object-types] (mapv :in (retirement/deletion-blockers prepped :datasets :satz_ds false (ix prepped))))))
    (testing "--cascade unbinds it, and the delete goes through"
      (is (= #{ot} (retirement/backing-referrers prepped :datasets :satz_ds (ix prepped))))
      (let [casc (retirement/cascade-associated-links prepped :datasets :satz_ds (ix prepped))]
        (is (not (contains? (get-in casc [:object-types ot]) :backing)))
        (is (contains? (:datasets casc) ds) "the dataset itself is the delete's own step")
        (is (empty? (retirement/deletion-blockers casc :datasets :satz_ds false (ix casc))))))))

(defn- typ
  ([] (typ nil))
  ([backing]
   {:object-types {:konto (cond-> {:object-type-name :konto
                                   :attribute-list [{:attribute-name :nr :attribute-type :string}]}
                            backing (assoc :backing backing))}
    :transforms   {:parse-konten {:transform-name :parse-konten
                                  :input  {:kind :datasource :datasource :fibu}
                                  :output {:kind :object-type :object-type :konto :primary-key :nr}}
                   :fill-konten  {:transform-name :fill-konten
                                  :input  {:kind :datasource :datasource :fibu}
                                  :output {:kind :dataset :dataset :konten_raw}}}}))

(defn- replaced [after live edits]
  (filterv #(= :backing-replaced (:rule %)) (migration/migration-findings after live edits)))

(deftest adding-a-backing-to-a-populated-type-is-a-confirmed-breaking-change
  (let [after (typ {:dataset :konten_raw :primary-key :nr})]
    (testing "an empty type rebinds freely"
      (is (empty? (replaced after (typ) {:rows {:konto 0}}))))
    (testing "a type holding objects is refused, and told how many and what to confirm"
      (let [[f :as fs] (replaced after (typ) {:rows {:konto 79}})]
        (is (= 1 (count fs)))
        (is (= :error (:severity f)))
        (is (re-find #"PERMANENTLY REPLACED" (:message f)))
        (is (re-find #"79 object" (:message f)))
        (is (re-find #":replaces :parse-konten" (:message f)))))
    (testing "edits alone are enough to refuse"
      (is (= [:error] (mapv :severity (replaced after (typ) {:types #{:konto}})))))
    (testing ":replaces naming the old binding turns the refusal into a note"
      (is (= [:warn] (mapv :severity
                           (replaced (typ {:dataset :konten_raw :primary-key :nr
                                           :replaces :parse-konten})
                                     (typ) {:rows {:konto 79}})))))
    (testing ":replaces naming something else confirms nothing"
      (is (= [:error] (mapv :severity
                            (replaced (typ {:dataset :konten_raw :primary-key :nr
                                            :replaces :anderes})
                                      (typ) {:rows {:konto 79}})))))))
