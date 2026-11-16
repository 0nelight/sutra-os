(ns ^:unit grammar.function-entry-error-test
  (:require [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [sutra.parser.ent :as ent]
            [sutra.parser.ent-errors :as ent-errors]))

(defn- lines [schema]
  (ent-errors/humanize->lines (ent/explain {:schema schema})))

(deftest a-query-function-reports-only-its-own-errors
  (is (= ["schema > functions > f > bogus — disallowed key"
          "schema > functions > f > inputs > a > kind — missing required key"]
         (lines {:functions {:f {:language :python :code-file "x.py"
                                 :inputs {:a {:dataset :x}} :bogus 1}}}))
      "no `code-file`/`inputs — disallowed key` from the edit-function form"))

(deftest an-edit-function-reports-against-the-edit-form
  (is (= ["schema > functions > f > bogus — disallowed key"]
         (lines {:functions {:f {:target-type :x :source "(x)" :bogus 1}}}))))

(deftest a-union-reports-the-branch-the-value-is
  (testing "a map under `:boolean | map` is judged as the map"
    (is (= ["schema > actions > a > action-log > object-type — should be a keyword"]
           (lines {:actions {:a {:action-log {:object-type "x"}}}}))))
  (testing "a value of neither shape names both"
    (is (= ["schema > actions > a > action-log — should be a boolean or a map"]
           (lines {:actions {:a {:action-log "x"}}})))))

(deftest no-grammar-keeps-an-undiscriminated-or
  (doseq [g '[EntFile* EntSchema* Widget* ViewFile* AppFile* PipelineFile* TemplateFile*]]
    (is (not-any? #(and (vector? %) (= :or (first %)))
                  (tree-seq coll? seq (m/form @@(or (ns-resolve 'sutra.parser.ent g)
                                                    (ns-resolve 'sutra.parser.ent-view g)))))
        (str g " — an :or reports every branch's errors at once"))))

(deftest the-output-form-is-checked-at-the-path
  (testing "two output forms"
    (is (= [(str "schema > transforms > t — output-mutex: a transform must declare EXACTLY ONE"
                 " of :output / :outputs / :output-columns — found 2 (:output, :output-columns).")]
           (lines {:transforms {:t {:language :pg-sql :query "select 1"
                                    :inputs {:a {:kind :dataset :dataset :x}}
                                    :output {:kind :dataset :dataset :y}
                                    :output-columns [[:id :string]]}}}))))
  (testing "SQL without an output"
    (is (= 1 (count (lines {:transforms {:t {:language :pg-sql :query "select 1"
                                             :inputs {:a {:kind :dataset :dataset :x}}}}})))))
  (testing "code leaves the output to its introspected declaration"
    (is (empty? (lines {:transforms {:t {:language :python :code-file "t.py"}}})))))
