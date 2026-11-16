(ns ^:unit platform.llm-cost-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.types.llm-cost :as llm-cost]))

(def LEDGER
  [{:model "grok-4.3" :rate-in "3.0"  :rate-out "15.0" :currency "USD"
    :effective-from "2026-07-01T00:00:00Z"}
   {:model "grok-4.3" :rate-in "5.0"  :rate-out "25.0" :currency "USD"
    :effective-from "2026-07-15T00:00:00Z"}
   {:model "grok-fast" :rate-in "0.2" :rate-out "0.5"  :currency "USD"
    :effective-from "2026-07-10T00:00:00Z"}])

(deftest rate-at-is-effective-dated
  (testing "picks the latest rate whose effective_from ≤ ts"
    (is (= 3.0 (:in (llm-cost/rate-at LEDGER "grok-4.3" "2026-07-04T12:00:00Z"))) "pre-bump window")
    (is (= 5.0 (:in (llm-cost/rate-at LEDGER "grok-4.3" "2026-07-20T00:00:00Z")))
        "post-bump window")
    (is (= 5.0 (:in (llm-cost/rate-at LEDGER "grok-4.3" "2026-07-15T00:00:00Z")))
        "boundary is inclusive"))
  (testing "a ts before any row for the model is UNPRICED (nil), never free"
    (is (nil? (llm-cost/rate-at LEDGER "grok-4.3" "2026-06-30T00:00:00Z")))
    (is (nil? (llm-cost/rate-at LEDGER "unknown-model" "2026-07-20T00:00:00Z")))))

(deftest cost-is-tokens-times-rate-per-million
  (is (== 0.006477 (llm-cost/call-cost {:in 3.0 :out 15.0} 1874 57)) "(1874·3 + 57·15)/1e6")
  (is (nil? (llm-cost/call-cost nil 100 100)) "unknown rate ⇒ unpriced (nil), not 0")
  (is (nil? (llm-cost/call-cost {:in 3.0} 100 100)) "half a rate is still unpriced"))

(deftest price-calls-view
  (let [calls [{:model "grok-4.3" :ts "2026-07-04T00:00:00Z" :prompt-tokens "1000000"
                :completion-tokens "0"}
               {:model "grok-4.3" :ts "2026-07-20T00:00:00Z" :prompt-tokens "1000000"
                :completion-tokens "0"}
               {:model "grok-4.3" :ts "2026-06-01T00:00:00Z" :prompt-tokens "1000000"
                :completion-tokens "0"}]
        [a b unpriced] (llm-cost/price-calls LEDGER calls)]
    (is (== 3.0 (:cost a)) "1M input at the July-1 rate")
    (is (== 5.0 (:cost b)) "same call, post-bump rate")
    (is (nil? (:cost unpriced)) "predates every ledger row ⇒ unpriced")))

(deftest backdated-correction-reprices-history
  (let [call    {:model "grok-4.3" :ts "2026-06-01T00:00:00Z" :prompt-tokens "1000000"
                 :completion-tokens "0"}
        before  (llm-cost/price-call LEDGER call)
        fixed   (llm-cost/price-call (conj LEDGER {:model "grok-4.3" :rate-in "2.0" :rate-out "10.0"
                                            :currency "USD" :effective-from "2026-01-01T00:00:00Z"})
                              call)]
    (is (nil? (:cost before)) "unpriced before the correction")
    (is (== 2.0 (:cost fixed)) "the backdated rate now covers the call")))

(deftest day-window-is-inclusive-and-open-ended
  (is (= "2026-07-23" (llm-cost/iso-day "2026-07-23T09:17:38.250Z")))
  (is (true?  (llm-cost/within-day-window? "2026-07-23" "2026-07-23" "2026-07-23T09:00:00Z"))
      "same-day inclusive")
  (is (false? (llm-cost/within-day-window? "2026-07-23" "2026-07-23" "2026-07-24T00:00:00Z"))
      "next day excluded")
  (is (true?  (llm-cost/within-day-window? nil "2026-07-23" "2026-01-01T00:00:00Z"))
      "open lower bound")
  (is (true?  (llm-cost/within-day-window? "2026-07-01" nil "2026-12-31T00:00:00Z"))
      "open upper bound")
  (is (true?  (llm-cost/within-day-window? nil nil "2026-07-23T09:00:00Z"))
      "both open ⇒ everything"))

(deftest resource-resolution-collapses-rid-and-name
  (let [m {"09e0-uuid" :classify-events}]
    (is (= "classify-events" (llm-cost/resolve-resource m "ri.sutra.main.transform.09e0-uuid")))
    (is (= "classify-events" (llm-cost/resolve-resource m "classify-events")))
    (is (= "ri.sutra.main.transform.unknown-uuid"
           (llm-cost/resolve-resource m "ri.sutra.main.transform.unknown-uuid"))
        "unknown uuid passes through")))

(deftest rollup-never-prices-an-unpriced-group-as-zero
  (let [priced [{:resource "parse" :model "grok" :cost 0.006 :prompt-tokens "100"
                 :completion-tokens "10"}
                {:resource "parse" :model "grok" :cost nil   :prompt-tokens "200"
                 :completion-tokens "20"}
                {:resource "classify" :model "grok" :cost nil :prompt-tokens "50"
                 :completion-tokens "5"}]
        g (into {} (map (juxt :resource identity) (llm-cost/rollup :resource :resource priced)))]
    (testing "an all-unpriced group is nil cost (never 0.0), and flags its unpriced count"
      (is (nil? (:cost (g "classify"))))
      (is (= 1 (:calls (g "classify"))))
      (is (= 1 (:unpriced (g "classify")))))
    (testing "a partly-priced group sums ONLY its priced rows, flags the rest"
      (is (== 0.006 (:cost (g "parse"))))
      (is (= 2 (:calls (g "parse"))))
      (is (= 1 (:unpriced (g "parse"))))
      (is (= 330 (:tokens (g "parse"))) "tokens counted for all rows, priced or not"))))
