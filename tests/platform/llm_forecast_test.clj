(ns ^:unit platform.llm-forecast-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.types.llm-cost :as llm-cost]
            [sutra.types.llm-forecast :as fc]))

(def RATE {:in 1.25 :out 2.50 :currency "USD"})

(def HISTORY
  [{:caller "parse-positions" :model "grok-4.20" :calls 162 :in-tokens 473557 :out-tokens 153090}
   {:caller "ri.sutra.main.transform.48cd044c-7609-4e37-8d3c-d001fb92620d"
    :model "grok-4-fast" :calls 70 :in-tokens 202161 :out-tokens 8615}])

(deftest history-joins-both-spellings-of-one-caller
  (testing "by name"
    (is (= 162
           (:calls (fc/sample (fc/history-for HISTORY "ri.sutra.main.transform.aaa"
                                              "parse-positions")
                              "grok-4.20")))))
  (testing "by rid — the half `llm cost` used to render as a bare rid"
    (is (= 70 (:calls (fc/sample (fc/history-for
                                  HISTORY
                                  "ri.sutra.main.transform.48cd044c-7609-4e37-8d3c-d001fb92620d"
                                  "grok-email-attached-extract")
                                 "grok-4-fast")))))
  (testing "underscore and hyphen are one transform"
    (is (= "parse-positions" (llm-cost/caller-key "parse_positions")))
    (is (= (llm-cost/caller-key "PARSE-POSITIONS") (llm-cost/caller-key "parse_positions"))))
  (testing "a caller nobody claims yields no sample — which is UNCALIBRATED, not empty history"
    (is (nil? (fc/sample (fc/history-for HISTORY "ri.sutra.main.transform.zzz" "brand-new") "m")))))

(deftest fan-out-is-measured-not-assumed
  (let [s  (fc/sample (fc/history-for HISTORY nil "parse-positions") "grok-4.20")
        pr (fc/per-row s 40)]
    (is (= :measured (:source pr)))
    (is (== 4.05 (:calls pr)))
    (testing "and the projection scales by ROWS, so 10 new rows cost a quarter of 40"
      (let [a (fc/project 40 pr RATE)
            b (fc/project 10 pr RATE)]
        (is (== (:cost a) (* 4 (:cost b))))
        (is (= 162 (:calls a)) "the calls it projects are the calls it measured"))))
  (testing "no input rows in the window ⇒ tokens per CALL, 1 call/row assumed, and it SAYS so"
    (let [pr (fc/per-row (fc/sample HISTORY "grok-4.20") 0)]
      (is (= :per-call (:source pr)))
      (is (== 1.0 (:calls pr)))))
  (testing "--calls-per-row overrides the measurement and scales the tokens with it"
    (let [s  (fc/sample (fc/history-for HISTORY nil "parse-positions") "grok-4.20")
          pr (fc/per-row s 40 4.0)]
      (is (= :override (:source pr)))
      (is (== 4.0 (:calls pr)))
      (is (== (* 4.0 (/ 473557.0 162)) (:in-tokens pr))))))

(deftest an-unmeasurable-transform-is-unknown-never-free
  (let [none (fc/project 500 (fc/per-row nil nil) RATE)]
    (is (nil? (:cost none)))
    (is (= :uncalibrated (:unknown none))))
  (testing "a calibrated transform on an UNREGISTERED model is unpriced, also not free"
    (let [pr (fc/per-row (fc/sample HISTORY "grok-4.20") 40)
          p  (fc/project 40 pr nil)]
      (is (nil? (:cost p)))
      (is (= :unregistered-model (:unknown p)))))
  (testing "zero pending rows IS a real answer — a build that reads nothing spends nothing"
    (let [p (fc/project 0 (fc/per-row (fc/sample HISTORY "grok-4.20") 40) RATE)]
      (is (== 0.0 (:cost p)))
      (is (nil? (:unknown p))))))

(deftest the-total-admits-when-it-is-a-lower-bound
  (let [entries [{:transform "a" :cost 1.0 :currency "USD" :calls 10 :in-tokens 100 :out-tokens 10}
                 {:transform "b" :cost nil :unknown :uncalibrated}]
        t (fc/roll-up entries)]
    (is (== 1.0 (:cost t)) "sums the priced ones")
    (is (true? (:lower-bound? t)))
    (is (= [{:transform "b" :reason :uncalibrated}] (:unknown t))))
  (testing "nothing priced ⇒ nil, never 0.0"
    (is (nil? (:cost (fc/roll-up [{:transform "b" :unknown :uncalibrated}]))))))

(deftest the-budget-verdict-names-where-the-cap-bites
  (let [entries [{:transform "cheap" :cost 0.10 :rows 10}
                 {:transform "parse-positions" :cost 0.97 :rows 40}]
        v (fc/budget-verdict {:budget 1.0 :spent 0.41} 1.07 entries)]
    (is (false? (:fits? v)))
    (is (< (Math/abs (- 0.48 (:shortfall v))) 1e-9))
    (testing "the cut is the transform the remaining budget runs out INSIDE, and the row"
      (is (= "parse-positions" (:transform (:cut v))))
      (is (= 20 (:covered (:cut v))))
      (is (= 40 (:rows (:cut v))))))
  (testing "a budget ≤ 0 is UNLIMITED — the same rule the guard itself applies"
    (is (true? (:fits? (fc/budget-verdict {:budget 0 :spent 99.0} 5.0 [])))))
  (testing "an unpriced estimate cannot say it fits — nil, not true"
    (is (nil? (:fits? (fc/budget-verdict {:budget 1.0 :spent 0.0} nil []))))))

(deftest the-drain-tier-counts-what-is-LEFT-not-what-is-read
  (let [pc {:source :measured-call :in-tokens 1713.0 :out-tokens 103.0 :sample {:calls 56}}
        p  (fc/project-drain 225 20 pc RATE)]
    (is (= 225 (:pending p)))
    (is (= 20  (:calls-next-run p)) "one build is bounded by the declared cap")
    (is (= 12  (:runs p))           "225 ÷ 20, rounded up — the drain takes twelve builds")
    (testing "and the two costs are kept apart: what one run costs vs the whole backlog"
      (is (< (:cost-next-run p) (:cost p)))
      (is (< (Math/abs (- (/ (:cost p) 225) (/ (:cost-next-run p) 20))) 1e-9)
          "same price per call, different multiplier"))
    (testing "no cap declared ⇒ one run drains it"
      (let [u (fc/project-drain 225 nil pc RATE)]
        (is (= 1 (:runs u)))
        (is (= 225 (:calls-next-run u)))
        (is (== (:cost u) (:cost-next-run u))))))
  (testing "an empty backlog is 0.0 and zero runs — a real answer, not unknown"
    (let [p (fc/project-drain 0 20 {:source :measured-call :in-tokens 1.0 :out-tokens 1.0
                                    :sample {:calls 1}} RATE)]
      (is (== 0.0 (:cost p)))
      (is (= 0 (:runs p)))
      (is (nil? (:unknown p)))))
  (testing "no history ⇒ the backlog is known but its price is not — unknown, never free"
    (let [p (fc/project-drain 225 20 (fc/per-call nil) RATE)]
      (is (= 225 (:pending p)))
      (is (nil? (:cost p)))
      (is (= :uncalibrated (:unknown p))))))

(deftest the-roll-up-keeps-next-run-and-backlog-apart
  (let [entries [{:transform "classify" :pending 1029 :runs 18 :calls 1029 :calls-next-run 60
                  :cost 3.65 :cost-next-run 0.213 :currency "USD" :pending-basis :done-key}
                 {:transform "extract"  :pending 225  :runs 12 :calls 225  :calls-next-run 20
                  :cost 1.37 :cost-next-run 0.122 :currency "USD" :pending-basis :done-key}]
        t (fc/roll-up entries)]
    (is (< (Math/abs (- 5.02 (:cost t))) 1e-9)          "backlog = the sum of both drains")
    (is (< (Math/abs (- 0.335 (:cost-next-run t))) 1e-9) "next run = the capped slice only")
    (is (= 18 (:runs t)) "the slowest transform sets how many builds the drain takes")
    (is (= 80 (:calls-next-run t)))
    (is (empty? (:delta-based t)) "both are done-key based, so nothing is flagged as delta"))
  (testing "a delta-based entry is NAMED, so a mixed total is not read as one kind of number"
    (is (= ["guessy"] (:delta-based (fc/roll-up [{:transform "guessy" :pending-basis :delta
                                                  :cost 1.0 :currency "USD"}]))))))

(def PC {:source :measured-call :in-tokens 100.0 :out-tokens 40.0 :sample {:calls 70}})

(deftest a-measurement-outranks-the-ceiling-but-never-erases-it
  (let [p (fc/project-drain 1641 20 PC RATE 225)]
    (is (= 225 (:pending p))     "the projection is made with what the gate actually passed")
    (is (true? (:measured? p)))
    (is (= 1641 (:ceiling p))    "…and the derived bound is kept, because the gap IS the report")
    (is (= 12 (:runs p))         "225 ÷ 20 — twelve builds, not the 83 the ceiling implies")
    (is (= 83 (:ceiling-runs p)))
    (is (< (:cost p) (:ceiling-cost p)))
    (is (= 20 (:calls-next-run p)) "the cap still bounds one run"))
  (testing "no measurement ⇒ the ceiling IS the number, and nothing claims otherwise"
    (let [p (fc/project-drain 1641 20 PC RATE nil)]
      (is (= 1641 (:pending p)))
      (is (nil? (:measured? p)))
      (is (nil? (:ceiling p)))))
  (testing "a measurement is clamped to the ceiling — work already done cannot still be pending"
    (is (= 40 (:pending (fc/project-drain 40 20 PC RATE 225)))))
  (testing "measured ZERO is a real answer, not a missing one"
    (let [p (fc/project-drain 900 20 PC RATE 0)]
      (is (= 0 (:pending p)))
      (is (== 0.0 (:cost p)))
      (is (= 900 (:ceiling p)) "…and it still says what it was measured against"))))

(deftest the-roll-up-separates-measured-from-merely-bounded
  (let [t (fc/roll-up
           [{:transform "content-extract" :pending 225 :cost 1.37 :cost-next-run 0.12
             :ceiling 1641 :ceiling-cost 10.01 :measured? true :currency "USD"
             :pending-basis :done-key}
            {:transform "classify" :pending 1060 :cost 3.76 :cost-next-run 0.21
             :currency "USD" :pending-basis :prompt-key}])]
    (is (= ["content-extract"] (:measured t)))
    (is (= ["classify"] (:ceiling-only t))
        "a prompt-key entry that has never been measured is a BOUND, and is named as one")
    (is (empty? (:delta-based t))
        "prompt-key answers the backlog question just as done-key does — only :delta does not")
    (is (< (Math/abs (- 5.13 (:cost t))) 1e-9))
    (is (< (Math/abs (- 13.77 (:ceiling-cost t))) 1e-9)
        "the bound totals too: measured where measured, derived where not")))
