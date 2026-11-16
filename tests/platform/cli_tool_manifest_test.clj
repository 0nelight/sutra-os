(ns ^:unit platform.cli-tool-manifest-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [sutra.cli.json :as json]
            [sutra.cli.spec-dump :as sd]))

(deftest tools-are-well-formed
  (doseq [{:keys [name description input_schema]} (:tools (sd/tool-manifest))]
    (testing name
      (is (string? name))
      (is (and (string? description) (seq description))
          "a tool must carry a non-empty description (its :purpose)")
      (is (= "object" (:type input_schema))
          "an Anthropic input_schema is a JSON-Schema object")
      (is (map? (:properties input_schema)))
      (is (vector? (or (:required input_schema) [])))
      (testing "required keys are all declared properties"
        (is (every? (set (map clojure.core/name (keys (:properties input_schema))))
                    (:required input_schema)))))))

(deftest tool-surface-is-generated-reproducibly
  (is (= (with-out-str (json/pprint (sd/tool-manifest)))
         (with-out-str (json/pprint (sd/tool-manifest))))))

(deftest every-json-result-routes-through-one-projecting-emit
  (let [dir     "clients/sutra-cli/src/sutra/cli/"
        src     (apply str (slurp (io/file dir "out.clj"))
                       (map slurp (.listFiles (io/file dir "verbs"))))
        all     (count (re-seq #"\(json/write-str" src))
        to-file (count (re-seq #"\(spit [^()]*\(json/write-str" src))
        emits   (- all to-file)]
    (is (= 1 emits)
        (str "expected exactly one (json/write-str …) that EMITS — result-json — but found "
             emits " of " all " call site(s) (" to-file " written to a file);"
             " a raw emit bypasses --fields"))))

(deftest a-method-names-only-the-endpoints-of-its-own-body
  (let [eps (#'sd/endpoint-map)]
    (testing "the last method of a file gets nothing from the head of the next"
      (is (= ["api/edit-history"] (:edit-history eps)))
      (is (= [":run-jobs"] (:jobs eps))))
    (testing "a helper of the same file it calls counts, a neighbour's does not"
      (is (some #{":feedback"} (:feedback eps)))
      (is (not-any? #{":graph-open"} (:widget-set-init eps))))
    (testing "a docstring or message naming /api/… calls nothing; spacing does not matter"
      (is (not-any? #{"api/schedules"} (:llm-next eps)))
      (is (not-any? #{"api/actions"} (:action eps)))
      (is (= [":restore-point"] (:restore-point eps))))
    (testing "a byte call to a written-out path names that path"
      (is (= ["/api/blob"] (:blob eps))))))
