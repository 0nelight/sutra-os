(ns ^:unit platform.llm-budget-counter-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [sutra.llm.budget :as budget]
            [sutra.catalog.llm-models :as llm-models]
            [sutra.types.llm-cost :as llm-cost]
            [sutra.serve.server]
            [support.mem :as mem]
            [sutra.llm.http :as llm]
            [sutra.catalog.lookup]))

(deftest an-unlimited-budget-is-a-non-positive-one
  (is (= 1.0 budget/default-budget-eur)
      "the shipped default is 1.00 — the number the operator relies on"))

(deftest a-reservation-fits-only-beside-what-is-spent-and-held
  (let [d {:day "2026-10-13" :spent 0.5 :reserved {"a" {:eur 0.3 :until 100}}}]
    (is (some? (budget/reserving d 1.0 "b" 0.2 50 150)) "0.5 + 0.3 + 0.2 fits 1.0")
    (is (nil? (budget/reserving d 1.0 "b" 0.3 50 150)) "0.5 + 0.3 + 0.3 does not")
    (testing "a lapsed reservation no longer holds its share"
      (is (= {"b" {:eur 0.3 :until 250 :pool "interactive"}}
             (:reserved (budget/reserving d 1.0 "b" 0.3 200 250)))))))

(deftest batch-calls-leave-the-interactive-reserve-and-keep-to-their-quota
  (let [lim {:budget 10.0 :reserve-pct 20.0 :quotas {"finanzen" 3.0}}
        d   {:spent 2.5 :spent-by {"batch:finanzen" 2.5}
             :reserved {"x" {:eur 0.4 :until 100 :pool "batch:finanzen"}}}]
    (testing "a project's quota counts its spend and its calls in flight"
      (is (= :project (:limit (budget/refusal d lim "batch:finanzen" 0.2 50))))
      (is (:waitable? (budget/refusal d lim "batch:finanzen" 0.2 50))
          "it fits once the call in flight settles — worth waiting for")
      (is (nil? (budget/refusal d lim "batch:finanzen" 0.2 200)) "the reservation lapsed"))
    (testing "batch together may not reach into the reserve"
      (let [d {:spent 7.9 :spent-by {"batch:mail" 7.9}}]
        (is (= :batch (:limit (budget/refusal d lim "batch:mail" 0.2 0))))
        (is (not (:waitable? (budget/refusal d lim "batch:mail" 0.2 0))))
        (is (nil? (budget/refusal d lim "interactive" 2.0 0)) "people may use the reserve")))
    (testing "the ceiling holds for everyone"
      (is (= :instance (:limit (budget/refusal {:spent 9.9} lim "interactive" 0.2 0)))))
    (testing "settling charges the pool the call reserved for"
      (is (= {"batch:finanzen" 2.6}
             (:spent-by (budget/settling d "x" 0.1)))))))

(deftest a-call-is-charged-to-its-transforms-project
  (let [schema {:projects   {"ri.p" {:project-name :finanzen}}
                :transforms {"ri.t" {:transform-name :mail-lesen :project "ri.p"}}}]
    (with-redefs [sutra.catalog.lookup/rid-of (fn [_ _ k]
                                              (when (#{:mail-lesen "mail-lesen"} k) "ri.t"))]
      (is (= "batch:finanzen" (budget/pool-of schema "mail-lesen")))
      (is (= "batch:finanzen" (budget/pool-of schema "ri.t")))
      (is (= "interactive" (budget/pool-of schema "agent:analyst")))
      (is (= "interactive" (budget/pool-of schema nil))))))

(deftest settling-charges-the-cost-and-frees-the-reservation-in-one-value
  (let [d (budget/settling {:reserved {"a" {:eur 0.4 :until 9}}} "a" 0.25)]
    (is (= 0.25 (:spent d)))
    (is (empty? (:reserved d))))
  (is (nil? (budget/releasing {:reserved {}} "a")) "releasing what is not held writes nothing"))

(deftest the-spend-is-the-days-and-survives-the-process
  (let [t      (atom (java.util.Date. 0))
        store     (mem/store #(deref t))
        model  "test-model"
        schema {:llm-models {model {:in 1000000.0 :out 1000000.0 :currency "USD"}}}]
    (with-redefs [budget/budget-eur (constantly 10.0)]
      (is (true? (budget/reserve! store "r1" 3.0 60000)))
      (budget/settle! store schema model 1 1 "r1")
      (is (= 2.0 (budget/spent-today store)) "the settled cost is written, not held in memory")
      (is (true? (budget/reserve! store "r2" 8.0 60000)) "2.0 spent + 8.0 fits 10.0")
      (is (false? (budget/reserve! store "r3" 0.1 60000)))
      (testing "the next day starts empty"
        (reset! t (java.util.Date. (* 1000 60 60 24)))
        (is (= 0.0 (budget/spent-today store)))
        (is (true? (budget/reserve! store "r4" 9.0 60000)))))))

(deftest the-guards-day-and-the-money-views-day-are-the-same-day
  (is (= "UTC" budget/day-zone)
      "named, not implied — a zone that is not written down is one that drifts")
  (is (= (budget/today {:clock #(java.util.Date.)})
         (llm-cost/iso-day (str (java.time.Instant/now))))
      "the guard's day IS the day `llm cost` buckets an ISO-8601 `ts` into"))

(deftest the-providers-own-reason-reaches-the-caller-not-only-the-audit
  (let [f llm/provider-error-text
        body "{\"code\":\"permission-denied\",\"error\":\"…used all available credits…\"}"]
    (testing "message and body are joined for BOTH sinks, and the body comes back separately"
      (let [[full raw] (f "egress agent refused: …returned HTTP 403" body)]
        (is (= body raw) "…so a caller can parse the provider's code without scraping our prose")
        (is (re-find #"HTTP 403" full))
        (is (re-find #"permission-denied" full)
            "the REASON, not just the status — this is the whole fix")))

    (testing "no body ⇒ the message alone, and no empty :provider_error field"
      (is (= ["boom" nil] (f "boom" nil)))
      (is (= ["boom" nil] (f "boom" ""))
          "an empty body is absent, not a body — else the response grows a blank field"))

    (testing "an untrusted-length body is bounded, because this string is copied
              into a job-log row and a python exception message"
      (let [[full raw] (f "boom" (apply str (repeat 5000 \x)))]
        (is (= 2000 (count raw)))
        (is (< (count full) 2100))))))

(deftest a-provider-keeps-to-its-own-daily-budget
  (let [lim {:budget 0.0 :source-budgets {"anthropic" 10.0 "xai" 1.0}}
        d   {:spent-by-source {"xai" 0.95 "anthropic" 2.0}}]
    (is (= :source (:limit (budget/refusal d lim "batch:finanzen" "xai" 0.1 0))))
    (is (= "xai" (:source (budget/refusal d lim "batch:finanzen" "xai" 0.1 0))))
    (is (nil? (budget/refusal d lim "batch:finanzen" "anthropic" 0.1 0))
        "another provider is not held by it")
    (is (nil? (budget/refusal d lim "interactive" "openai" 5.0 0))
        "a provider without a budget is unlimited here")
    (testing "settling charges the provider the call reserved for"
      (let [r (budget/reserving d lim "batch:finanzen" "anthropic" "q" 1.0 0 100)]
        (is (= 2.5 (get-in (budget/settling r "q" 0.5) [:spent-by-source "anthropic"])))))))

(deftest cached-prompt-tokens-cost-their-own-share
  (let [schema {:llm-models {"m" {:in 4.0 :out 20.0} "c" {:in 4.0 :out 20.0 :cached-in 0.2}}}
        near?  #(< (Math/abs (- (double %1) (double %2))) 1e-9)]
    (is (near? 4.0 (llm-models/cost schema "m" 1000000 0))
        "without a breakdown every prompt token is fresh")
    (is (near? 0.81 (llm-models/cost schema "m" 1000000 0 {:cached 900000 :written 50000}))
        "50k fresh at 4, 900k read at a tenth, 50k written at a quarter more")
    (is (near? 0.63 (llm-models/cost schema "c" 1000000 0 {:cached 900000 :written 50000}))
        "a rate's own :cached-in prices the reads")))

(deftest an-account-out-of-credits-is-told-apart-from-one-failed-call
  (let [xai (str "egress agent default refused: language-model call returned HTTP 403 — "
                 "{\"code\":\"permission-denied\",\"error\":\"Your team … has either used all"
                 " available credits or reached its monthly spending limit.\"}")]
    (is (budget/account-exhausted? xai))
    (is (budget/account-exhausted? "language-model call returned HTTP 402 — payment required"))
    (is (budget/account-exhausted?
         "HTTP 429 — {\"error\":{\"code\":\"insufficient_quota\",\"message\":\"…\"}}"))
    (is (not (budget/account-exhausted? "HTTP 429 — rate limit reached, retry in 2s")))
    (is (not (budget/account-exhausted? "HTTP 403 — invalid api key")))
    (is (not (budget/account-exhausted? "HTTP 500 — credits service unavailable")))))

(deftest an-exhausted-account-holds-its-source-until-the-pause-ends
  (let [lim {:budget 0.0 :exhausted {"xai" {:since 0 :until 100 :detail "credits"}}}]
    (is (= :account (:limit (budget/refusal {} lim "batch:mail" "xai" 0.1 50))))
    (is (not (:waitable? (budget/refusal {} lim "batch:mail" "xai" 0.1 50)))
        "no call in flight frees it — the caller waits for a later run, not a second")
    (is (nil? (budget/refusal {} lim "batch:mail" "anthropic" 0.1 50)))
    (is (nil? (budget/refusal {} lim "batch:mail" "xai" 0.1 100))
        "after the pause one call goes out to see whether the account takes calls again")))

(deftest the-gateway-stops-calling-an-exhausted-account-and-resumes-on-an-answer
  (let [t      (atom (java.util.Date. 0))
        store  (mem/store #(deref t))
        schema {:llm-models {"m" {:in 1.0 :out 1.0 :currency "USD"}}}
        calls  (atom 0)
        out    (atom :exhausted)
        call   (fn [_ _ _]
                 (swap! calls inc)
                 (if (= :exhausted @out)
                   (throw (ex-info "language-model call returned HTTP 403"
                                   {:status 403
                                    :body   "{\"error\":\"used all available credits\"}"}))
                   {:latency-ms 1
                    :body (str "{\"choices\":[],"
                               "\"usage\":{\"prompt_tokens\":1,\"completion_tokens\":1}}")}))
        chat!  #(llm/relay-chat! store {:source "xai" :model "m" :schema schema :call call
                                        :request {:messages [{:role "user" :content "x"}]}})]
    (with-redefs [llm-models/registered? (constantly true)
                  llm/source-spec        (constantly {:type :xai})]
      (testing "the provider's 'no credits' answers the call as a budget wait, not a failure"
        (let [{:keys [status error]} (chat!)]
          (is (= 429 status))
          (is (str/starts-with? error "llm budget wait: provider xai's account takes no calls"))
          (is (= 1 @calls))))
      (testing "the next call stays home"
        (is (= 429 (:status (chat!))))
        (is (= 1 @calls) "no second call to an account that pays for none"))
      (testing "after the pause one call tries again, and an answer lifts the hold"
        (reset! out :ok)
        (reset! t (java.util.Date. (inc budget/exhausted-pause-ms)))
        (is (= 200 (:status (chat!))))
        (is (empty? (budget/exhausted-sources store)))))))
