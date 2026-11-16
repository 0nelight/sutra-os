(ns ^:unit platform.dataset-ownership-gate-test
  (:require [sutra.parser.spec :as spec]
            [clojure.test :refer [deftest is testing]]
            [sutra.parser.lint :as lint]
            [sutra.catalog.system-datasets :as sysds]
            [sutra.types.membership :as member]
            [sutra.authoring.install :as install]))

(defn- unowned [world]
  (set (keep #(when (= :resource-without-project (:rule %)) [(:entity-class %) (:entity %)])
             (lint/gate-violations world (spec/vocabulary)))))

(def ^:private world
  {:projects {"ri.pr.w" {:project-name :werft}}
   :datasets {"ri.ds.zb" {:dataset-name :zeitbuchung_raw :kind :standalone
                          :columns [{:attribute-name :id :attribute-type :string}]}}
   :sources  {"ri.src.crm" {:source-name :crm :type :postgres
                            :jdbc-url "jdbc:postgresql://h:5432/db"}}})

(deftest what-names-no-project-and-gets-none-is-refused
  (is (= #{[:dataset :zeitbuchung_raw] [:source :crm]} (unowned world)))
  (testing "a declared :project owns it"
    (is (empty? (unowned (-> world
                             (assoc-in [:datasets "ri.ds.zb" :project] "ri.pr.w")
                             (assoc-in [:sources "ri.src.crm" :project] "ri.pr.w"))))))
  (testing "an owner the ownership table derives owns it: an action its edited type's"
    (is (not (contains? (unowned (-> world
                                     (assoc :object-types
                                            {"ri.ot.a"
                                               {:object-type-name :auftrag :project "ri.pr.w"
                                                :plural "Aufträge"}})
                                     (assoc :actions
                                            {:auftrag-anlegen
                                               {:rules [{:op :create :object-type "ri.ot.a"}]}})))
                        [:action :auftrag-anlegen]))))
  (testing "what a code repository carries is its repository's"
    (is (empty? (unowned {:projects     {"ri.pr.w" {:project-name :werft}}
                          :repositories {"ri.repo.e" {:repository-name :etl :project "ri.pr.w"}}
                          :transforms
                            {"ri.tx.t" {:transform-name :t :repository "ri.repo.e"}}})))))

(deftest the-platforms-own-datasets-are-its-by-construction
  (is (= member/platform-datasets
         (into #{} (map (comp keyword name :dataset-name val)) sysds/system-datasets))
      "the ownership table names exactly the datasets the platform writes")
  (is (empty? (unowned (sysds/ensure-system-datasets {})))))

(deftest a-minted-sync-inherits-its-datasets-project
  (let [plan (install/plan-schema
              {:projects {:werft {}}
               :datasets {:zeitbuchung_raw
                          {:project :werft
                           :external-source {:datasource :mro :table "ZV"}}}}
              {} {})
        sync (->> plan
                  (filter #(and (= :transform (:kind %))
                                (= :zeitbuchung_raw-sync (:entity %))))
                  first)]
    (is (some? sync) "der Sync wird gemuenzt")
    (is (= :werft (:project (:args sync)))
        "ohne das faellt der Sync aus dem gemessenen Projektgraph")))
