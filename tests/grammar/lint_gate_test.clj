(ns ^:unit grammar.lint-gate-test
  (:require [sutra.parser.spec :as spec]
            [clojure.test :refer [deftest testing is]]
            [sutra.parser.lint :as lint]
            [sutra.catalog.kernel :as kernel]))

(def ^:private ot-rid "ri.sutra.main.object-type.11111111-1111-4111-8111-111111111111")
(def ^:private t-rid  "ri.sutra.main.transform.22222222-2222-4222-8222-222222222222")

(def ^:private source-syncs-schema
  {:object-types
   {ot-rid {:id ot-rid :object-type-name :halter :primary-key :pk :title :pk
            :attribute-list [{:attribute-name :pk :attribute-type :string}]
            :match-keys [:pk]
            :plural "Halter"}}
   :link-types {}
   :transforms
   {t-rid {:id t-rid :transform-name :halter-rows
           :input {:kind :datasource :datasource :amos}
           :source {:table "halter"}
           :primary-key :pk
           :column-map {:pk :pk}
           :output {:kind :object-type :object-type ot-rid}}}})

(deftest a-gated-rule-refuses-the-schema
  (let [as-gated (fn [rule-id f]
                   (with-redefs [lint/gated-rules #{rule-id}]
                     (f)))]
    (testing "the schema is structurally sound — the gate is the only thing at issue"
      (with-redefs [lint/gated-rules #{}]
        (is (nil? (kernel/reasons source-syncs-schema))
            "no structural or relational fault; without the gate it is valid")))

    (testing "with the rule gated, the same schema is REFUSED"
      (as-gated :source-syncs-into-ontology
                (fn []
                  (is (false? (boolean (kernel/accept? source-syncs-schema)))
                      "valid-schema? — the commit gate — says no"))))

    (testing "and the refusal is legible: the failed check plus the findings"
      (as-gated :source-syncs-into-ontology
                (fn []
                  (let [why (kernel/reasons source-syncs-schema)]
                    (is (some #{:lint-gate-clean?} (:relational-checks-failed why))
                        "names the failed check")
                    (let [f (first (:lint-gate-violations why))]
                      (is (= :source-syncs-into-ontology (:rule f)) "itemizes the rule")
                      (is (= "halter-rows" (:entity f)) "and the offending entity, by name")
                      (is (re-find #":datasets" (str (:message f)))
                          "and the message says what to author instead"))))))

    (testing "an unrelated gated rule leaves the schema alone (no blanket refusal)"
      (as-gated :cryptic-name
                (fn []
                  (is (empty? (lint/gate-violations source-syncs-schema (spec/vocabulary)))
                      "the gate reports only its own rules"))))))

(deftest gating-is-per-rule-not-per-severity
  (testing "`gated-rules` is derived from the registry, so flagging a rule is the whole change"
    (is (= lint/gated-rules
           (into #{} (keep #(when (:gate? %) (:id %))) lint/rules))
        "nothing re-states the set")
    (is (every? #(= :error (:severity %))
                (filter (comp lint/gated-rules :id) lint/rules))
        "a gated rule is at least :error — a gate is never softer than its report"))

  (testing "NOT every :error rule is gated, and that is deliberate"
    (let [errors (into #{} (comp (filter #(= :error (:severity %))) (map :id)) lint/rules)]
      (is (contains? errors :app-source) "the measured counter-example is still :error")
      (is (not (contains? lint/gated-rules :app-source))
          "and it is NOT gated — severity is not the criterion"))))

(deftest layer-line-rule-is-gated
  (testing "the rule detects a datasource synced straight into an object type"
    (let [f (->> (lint/run-lint source-syncs-schema (spec/vocabulary))
                 (filter #(= :source-syncs-into-ontology (:rule %)))
                 first)]
      (is (some? f) "the finding is produced")
      (is (= :error (:severity f)) "at :error")))
  (testing "and it is enforced"
    (is (contains? lint/gated-rules :source-syncs-into-ontology) "gated")
    (is (seq (lint/gate-violations source-syncs-schema (spec/vocabulary)))
        "so the schema is refused by the commit gate")
    (is (false? (boolean (kernel/accept? source-syncs-schema)))
        "end to end: valid-schema? says no")))

(def ^:private log-rid "ri.sutra.main.object-type.33333333-3333-4333-8333-333333333333")
(def ^:private p-rid   "ri.sutra.main.project.44444444-4444-4444-8444-444444444444")

(defn- without-project-messages [schema]
  (map :message
       ((:scan (first (filter #(= :resource-without-project (:id %)) lint/rules))) schema)))

(def ^:private generated-log-schema
  {:projects {p-rid {:id p-rid :project-name :beschaffung}}
   :actions  {:rechnung-entfallen {:rules [{:op :update :object-type ot-rid}]}}
   :object-types
   {log-rid {:id log-rid :object-type-name :rechnung-entfallen-log
             :attribute-list [{:attribute-name :log-id :attribute-type :string}]}}})

(deftest a-generated-log-type-is-refused-in-its-action-s-name
  (testing "nobody wrote the log type: it follows from :action-log, and its place
            follows its action's"
    (let [msgs (filter #(re-find #"rechnung-entfallen-log" %)
                       (without-project-messages generated-log-schema))]
      (is (= 1 (count msgs)))
      (is (re-find #"submission log of action :rechnung-entfallen" (first msgs)))
      (is (re-find #"place the ACTION" (first msgs)))))
  (testing "and once the action has a place, the log has one too — the installed
            form derives it like the authored one"
    (is (empty? (filter #(re-find #"rechnung-entfallen" %)
                        (without-project-messages
                         (assoc-in generated-log-schema [:actions :rechnung-entfallen :project]
                                   p-rid)))))))
