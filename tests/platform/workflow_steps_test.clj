(ns ^:unit platform.workflow-steps-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.cli.parse :as parse]
            [sutra.cli.table :as table]
            [sutra.cli.tree :as tree]
            [sutra.kit.refusal :as refusal]
            [sutra.parser.authoring.workflow :as workflow]))

(defn- cli-verb?
  [verb]
  (let [[head sub :as toks] (str/split verb #" ")]
    (boolean
     (or (tree/match toks)
         (some #{head} parse/verbs)
         (and (contains? table/process-words head) (nil? sub))))))

(deftest every-step-names-a-verb-the-cli-has
  (let [{:keys [steps ways]} workflow/standard]
    (doseq [[way ks] ways
            k ks]
      (is (contains? steps k) (str way " names the unknown step " k)))
    (doseq [[k {:keys [verb line]}] steps]
      (is (string? line) (str k " has no :line"))
      (when verb (is (cli-verb? verb) (str k " names `" verb "`, which the CLI does not have"))))))

(deftest every-way-renders-filled
  (doseq [way (keys (:ways workflow/standard))]
    (is (not-any? #(re-find #"\{\w+\}" %) (workflow/lines way {:branch "b" :trunk "main"}))
        (str way " leaves a placeholder"))))

(deftest the-git-refusals-render-their-steps
  (testing "a refusal names the CLI way, not a retired tool"
    (let [s (refusal/message :refusal.git/no-open-proposal
                             {:branch "b"
                              :steps  (workflow/sentence :open-proposal
                                                         {:branch "b" :trunk "main"})})]
      (is (str/includes? s "propose --source b --target main"))
      (is (not (str/includes? s "change start")))
      (is (not (str/includes? s "⟦"))))))
