(ns ^:unit platform.llm-usage-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.types.llm-cost :as llm-cost]))

(defn- call [caller & {:keys [error pt ct model curr]
                       :or {pt 100 ct 20 model "grok-4"}}]
  (cond-> {:caller caller :model model
           :prompt-tokens pt :completion-tokens ct
           :cost (when-not error 0.001) :currency (or curr "USD")}
    error (assoc :error error)))

(deftest a-refused-call-is-neither-a-success-nor-a-failure
  (is (llm-cost/rate-limited? {:error "daily LLM budget reached (1.0) — call refused"}))
  (is
   (llm-cost/rate-limited? {:error
                              "language-model call returned HTTP 429 — {\"code\":\"rate_limit\"}"}))
  (is (llm-cost/rate-limited? {:error "Too Many Requests"}) "case-insensitive")
  (testing "a real failure is NOT a refusal"
    (is (not (llm-cost/rate-limited?
              {:error "language-model call returned HTTP 403 — used all available credits"})))
    (is (not (llm-cost/rate-limited? {:error nil})))
    (is (not (llm-cost/rate-limited? {})))))

(deftest the-three-outcomes-partition-the-attempts-exactly
  (let [rows [(call "parse") (call "parse") (call "parse")
              (call "parse" :error "daily LLM budget reached (1.0) — call refused")
              (call "parse" :error "language-model call returned HTTP 403 — no credits")]
        u (get (llm-cost/usage-rollup :caller rows) "parse")]
    (is (= 5 (:attempted u)))
    (is (= 3 (:successful u)))
    (is (= 1 (:rate-limited u)))
    (is (= 1 (:failed u)))
    (is (= (:attempted u) (+ (:successful u) (:rate-limited u) (:failed u))))))

(deftest tokens-count-successful-calls-only
  (let [rows [(call "parse" :pt 100 :ct 20)
              (call "parse" :pt 999 :ct 999 :error "daily LLM budget reached — call refused")
              (call "parse" :pt 555 :ct 555 :error "HTTP 502 provider blew up")]
        u (get (llm-cost/usage-rollup :caller rows) "parse")]
    (is (= 100 (:prompt-tokens u)))
    (is (= 20 (:completion-tokens u)))
    (is (= 120 (:tokens u)))))

(deftest an-entirely-unpriced-group-reports-nil-cost-never-zero
  (let [rows [(assoc (call "parse") :cost nil)
              (assoc (call "parse") :cost nil)]
        u (get (llm-cost/usage-rollup :caller rows) "parse")]
    (is (nil? (:cost u)))
    (is (= 2 (:successful u)))))

(deftest a-row-whose-key-cannot-be-resolved-is-DROPPED-not-bucketed
  (let [rows [(call "parse") (call "a-caller-no-transform-owns")]
        u (llm-cost/usage-rollup #(when (= "parse" (:caller %)) "transform/parse") rows)]
    (is (= #{"transform/parse"} (set (keys u))))
    (is (= 1 (:attempted (get u "transform/parse"))))))

(deftest every-model-a-group-touched-is-named-including-on-refused-calls
  (let [rows [(call "parse" :model "grok-4")
              (call "parse" :model "grok-4")
              (call "parse" :model "grok-3" :error "daily LLM budget reached")]
        u (get (llm-cost/usage-rollup :caller rows) "parse")]
    (is (= ["grok-3" "grok-4"] (:models u)))))
