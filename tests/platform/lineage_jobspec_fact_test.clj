(ns ^:unit platform.lineage-jobspec-fact-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.builds.plan.jobspec :as jobspec]
            [sutra.lineage.facts :as facts]))

(def ^:private schema
  {:datasets   {"ri.d.1" {:dataset-name :journal_raw}
                "ri.d.2" {:dataset-name :journal}}
   :transforms {"ri.t.1" {:transform-name :shape :language :pg-sql :query "SELECT * FROM j"
                          :inputs {:j {:kind :dataset :dataset "ri.d.1"}}
                          :output {:kind :dataset :dataset "ri.d.2"}}}})

(defn- ctx [js]
  (facts/fact-ctx schema (facts/edges schema nil) {:jobspecs (delay js)}))

(deftest a-dataset-names-the-branch-whose-jobspec-builds-it
  (let [ts  (:transforms schema)
        js  (:jobspecs (jobspec/build-transforms
                        [{:branch "main" :transforms ts
                          :published (jobspec/published {:transforms ts} nil true)}]
                        schema))
        fx  (:jobspec-nodes (ctx js))]
    (is (= "none" (get-in fx ["dataset/journal_raw" :jobspec]))
        "the gray icon: no JobSpec builds it")
    (is (= "main" (get-in fx ["dataset/journal" :jobspec])))
    (is (= "shape" (get-in fx ["dataset/journal" :jobspec-transform])))
    (is (= (jobspec/id schema (get ts "ri.t.1")) (get-in fx ["dataset/journal" :jobspec-id])))
    (is (= {"transform/shape" (jobspec/id schema (get ts "ri.t.1"))} (:spec-ids (ctx js)))
        "freshness compares a job's recorded JobSpec with this one")))

(deftest no-jobspec-answer-no-jobspec-facts
  (is (nil? (:jobspec-nodes (facts/fact-ctx schema (facts/edges schema nil) {})))))
