(ns ^:unit platform.resource-planes-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.parser.authoring.registry :as registry]
            [sutra.parser.authoring.standard :as standard]
            [sutra.parser.project-export :as pe]))

(defn- texts [projections nm]
  (registry/texts {:projections projections} nm))

(def ^:private repo (texts standard/projections :code-repository))
(def ^:private onto (texts standard/projections :ontologie))

(deftest a-code-repository-s-planes-are-its-projection-s-texts
  (testing "what a text kind reads, or carries as a unit's sibling, is entity plane"
    (doseq [p ["etl.ent" "transforms/raw_messages.py" "transforms/x.clj" "transforms/requirements"
               "apps/quoten.enta" "apps/quoten.cljs" "exports/report.ods"
               "pipelines/ingest.entp" "doc-templates/brief.entt" "doc-templates/brief.j2"]]
      (is (= :entity (pe/plane-of repo p)) p)))
  (testing "everything else is file plane — carried, never generated, therefore never deleted"
    (doseq [p ["sutra.repo.edn" "tests/test_x.py" ".gitignore" "README.md"
               "vendor/other.ent" "types/foo.ent" "transforms/sub/x.py"
               "object-views/x.entv" "" "/etc/passwd"]]
      (is (= :file (pe/plane-of repo p)) p))))

(deftest the-ontology-s-planes-are-its-projection-s-texts
  (doseq [p ["ontology.ent" "transform.ent" "controlling.ent" "controlling.modell.ent"
             "controlling.ent.yaml" "a.objects.ent" "a.ingest.ent" "a.apps.ent"
             "object-views/x.entv" ".gitmodules"]]
    (is (= :entity (pe/plane-of onto p)) p))
  (doseq [p ["README.md" "AGENTS.md" "a/ontology.ent" "a/README.md"
             "apps/x.enta" "transforms/a.py"]]
    (is (= :file (pe/plane-of onto p)) p)))

(deftest a-text-kind-moves-a-path-onto-the-entity-plane
  (testing "nothing is built in: a kind of text a projection declares is read from
            then on, and one it drops is content again"
    (is (= :file (pe/plane-of repo "docs/a.edn")))
    (is (= :entity
           (pe/plane-of (texts (assoc-in standard/projections [:code-repository :filename :docs]
                                         {:language :edn :name ["docs/*.edn"]})
                               :code-repository)
                        "docs/a.edn")))
    (is (= :file
           (pe/plane-of (texts (update-in standard/projections [:code-repository :filename] dissoc
                                          :apps)
                               :code-repository)
                        "apps/x.enta")))))

(deftest a-projection-without-texts-reads-nothing
  (is (= {} (texts {} :ontologie)))
  (is (= :file (pe/plane-of {} "ontology.ent"))))
