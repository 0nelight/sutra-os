(ns ^:unit grammar.rebacked-producer-gate-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.authoring.install :as install]
            [sutra.authoring.install.plan :as install-plan]
            [sutra.parser.project-export :as pe]))

(def ^:private repo "ri.sutra.main.repository.materialwirtschaft")

(defn- live [backing]
  {:repositories {repo {:repository-name :materialwirtschaft}}
   :datasets     {"ri.ds.1" {:dataset-name :rechnung_erfasst :kind :standalone}}
   :object-types {"ri.o.1" (cond-> {:object-type-name :arbeitsauftrag
                                    :attribute-list [{:attribute-name :nr :attribute-type :string}]}
                             backing (assoc :backing backing))}
   :transforms   {"ri.t.1" {:transform-name :shape-arbeitsauftrag :repository repo :language :pg-sql
                            :query "select 1"
                            :output {:kind :object-type :object-type "ri.o.1" :primary-key :nr}}}})

(def ^:private code-opts
  {:strict? true :strict-sections pe/repo-sections :strict-repository repo})

(defn- rules [live-schema]
  (let [tree {}]
    (set (map :rule (install-plan/plan-problems tree live-schema
                                           (install/plan-schema tree live-schema code-opts))))))

(deftest the-code-repo-may-drop-a-producer-the-ontology-replaced
  (testing "the type is backed by its own dataset now: the old producer goes freely"
    (is (not (contains? (rules (live {:dataset "ri.ds.1" :primary-key :nr})) :producer-removed))))
  (testing "without a backing the gate still speaks"
    (is (contains? (rules (live nil)) :producer-removed))))

(deftest the-ontology-re-backs-while-the-code-repo-still-carries-the-producer
  (let [tree {:projects {:materialwirtschaft {}}
              :datasets {:rechnung_erfasst {:project :materialwirtschaft :kind :standalone
                                            :columns
                                              [{:attribute-name :nr :attribute-type :string}]}}
              :objects  {:arbeitsauftrag {:project :materialwirtschaft :attributes [[:nr :string]]
                                    :backing {:dataset :rechnung_erfasst :primary-key :nr
                                              :replaces :shape-arbeitsauftrag}
                                          :plural "Arbeitsaufträge"}}}
        opts {:strict? true :strict-sections (pe/carried-sections true)}
        live (live nil)]
    (is (empty? (install-plan/plan-problems tree live (install/plan-schema tree live opts)))
        "the ontology push goes first; the producer stands until its own repository drops it")))
