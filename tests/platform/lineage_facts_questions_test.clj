(ns ^:unit platform.lineage-facts-questions-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.cli.verbs.explore :as explore]
            [sutra.lineage.facts :as facts]
            [sutra.types.kinds :as kinds]))

(def ^:private schema
  {:datasets     {"ri.d.1" {:dataset-name :adresse_raw :columns [{:attribute-name :adr_id}]}
                  "ri.d.2" {:dataset-name :vereint}
                  "ri.d.3" {:dataset-name :fremd}}
   :object-types {"ri.o.1" {:object-type-name :adresse
                            :attribute-list [{:attribute-name :adr-id}]}
                  "ri.o.2" {:object-type-name :person :dataset "ri.d.2"
                            :backing {:dataset "ri.d.2"}}}
   :link-types   {"ri.l.1" {:link-type-name :person-adresse}}
   :transforms
   {"ri.t.1" {:transform-name :adresse-sync
              :input {:kind :datasource :datasource :erpoem}
              :source {:query "SELECT a.X FROM dbo.ADR_STAMM a LEFT JOIN [dbo].[ADR_ART] b ON 1=1"}
              :output {:kind :dataset :dataset "ri.d.1"}}
    "ri.t.2" {:transform-name :shape :language :pg-sql :query "SELECT * FROM j"
              :column-map {:adr-id "ADR_ID"}
              :inputs {:j {:kind :dataset :dataset "ri.d.1"}}
              :output {:kind :object-type :object-type "ri.o.1"}}
    "ri.t.3" {:transform-name :vereinen :language :pg-sql :query "SELECT * FROM j"
              :inputs {:j {:kind :object-type :object-type "ri.o.1"}}
              :output {:kind :dataset :dataset "ri.d.2"}}
    "ri.t.4" {:transform-name :link-person-adresse
              :inputs {:source {:kind :object-type :object-type "ri.o.2"}}
              :output {:kind :link-type :link-type "ri.l.1" :fk-attribute :adr-id}}}})

(defn- answers [status]
  {:status (delay status) :now 0})

(defn- facts [q] (facts/facts schema (answers {}) q))

(deftest naming-one-direction-leaves-the-other-at-zero
  (let [nodes #(set (keys (:nodes (facts (assoc % :node "dataset/adresse_raw")))))]
    (is (every? (nodes {}) ["transform/adresse-sync" "transform/shape"])
        "no depth at all: one step either way")
    (is (not (contains? (nodes {:up "all"}) "transform/shape")) "--up brings no consumer")
    (is (not (contains? (nodes {:down "1"}) "transform/adresse-sync")) "--down brings no producer")
    (is (contains? (nodes {:up "all" :down "1"}) "transform/shape"))))

(deftest a-path-is-the-way-data-takes
  (testing "downstream, in the order it flows"
    (let [f (facts {:node "dataset/adresse_raw" :to "dataset/vereint"})]
      (is (= {:found? true :direction "downstream"
              :nodes ["dataset/adresse_raw" "transform/shape" "object-type/adresse"
                      "transform/vereinen" "dataset/vereint"]}
             (select-keys (:path f) [:found? :direction :nodes])))
      (is (= 4 (count (:edges f))) "only the steps of the way")))
  (testing "asked against the flow it says so"
    (is (= "upstream"
           (get-in (facts {:node "dataset/vereint" :to "dataset/adresse_raw"})
                   [:path :direction]))))
  (testing "no way is an answer, not an error"
    (is (false? (get-in (facts {:node "dataset/fremd" :to "dataset/vereint"}) [:path :found?]))))
  (testing "it asks about one node and one other"
    (is (thrown? Exception (facts {:node "dataset/fremd,dataset/vereint"
                                   :to "dataset/adresse_raw"})))))

(deftest a-sync-names-the-tables-it-reads
  (is (= ["dbo.ADR_ART" "dbo.ADR_STAMM"]
         (facts/source-tables (get-in schema [:transforms "ri.t.1"]))))
  (is (= ["T"] (facts/source-tables {:source {:table "T"}})))
  (is (= [] (facts/source-tables {:language :python}))))

(deftest a-column-is-one-name-across-the-layers
  (let [f (facts {:column "adr_id"})]
    (is (= ["dataset/adresse_raw" "object-type/adresse" "transform/shape"] (:anchors f))
        "snake and kebab are one name, and a :column-map joins them")
    (is (= facts/column-note (:column-note f)) "it says what kind of answer this is")))

(deftest what-is-joined-when-read-was-not-forgotten
  (let [es (facts/edges schema nil)
        fx (facts/node-facts schema (facts/nodes schema es) es
                             (facts/fact-ctx schema es {})
                             {"transform/link-person-adresse" {:build-status "never"}})]
    (is (= "read-time" (get-in fx ["transform/link-person-adresse" :freshness])))
    (is (= "read-time" (get-in fx ["transform/link-person-adresse" :build-status])))
    (is (false? (get-in fx ["transform/link-person-adresse" :out-of-date?])))))

(deftest a-type-and-the-dataset-behind-it-share-their-build
  (let [es (facts/edges schema nil)
        fx (facts/node-facts schema (facts/nodes schema es) es
                             (facts/fact-ctx schema es {})
                             {"object-type/person" {:build-status "never"}
                              "dataset/vereint"    {:build-status "succeeded" :rows 427}})]
    (is (= "succeeded" (get-in fx ["object-type/person" :build-status])))
    (is (= 427 (get-in fx ["object-type/person" :rows])))
    (is (= "dataset/vereint" (get-in fx ["object-type/person" :build-via])))))

(deftest a-cut-does-not-read-as-writing-nothing
  (let [lines #(vec (#'explore/edge-lines %))
        one   {:transforms [{:transform "t"}]
               :edges [{:from "dataset/a" :to "transform/t" :rel "input"}]}]
    (is (= ["  d:a ─[t]→ …2 outside scope"]
           (lines (assoc one :beyond [{:transform "transform/t" :in 0 :out 2}]))))
    (is (= ["  d:a ─[t]→ ∅"] (lines one)) "a transform without outputs stays ∅"))
  (is (= [{:transform "transform/shape" :in 1 :out 0}]
         (:beyond (facts {:node "object-type/adresse" :up "1"})))))

(deftest an-io-kind-names-its-lineage-node
  (is (= :dataset (kinds/lineage-node :media-set)))
  (is (= :source (kinds/lineage-node :datasource)))
  (is (= :object-type (kinds/lineage-node :object-type)))
  (is (= :export (kinds/lineage-node :export))))
