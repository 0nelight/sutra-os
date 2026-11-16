(ns grammar.finding-shape-test
  (:require [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [sutra.kit.anomaly :as anomaly]
            [sutra.parser.lint :as lint]
            [sutra.types.finding :as finding]))

(deftest the-constructor-builds-the-one-shape
  (let [f (finding/finding :sql-refused :error :transform :t {:message "nope"})]
    (is (= {:rule :sql-refused :severity :error :entity-class :transform :entity :t
            :message "nope"}
           f))
    (is (m/validate finding/Finding f)))
  (testing "no entry judged: no :entity-class, no :entity"
    (is (= {:rule :identity-remint :severity :warn}
           (finding/finding :identity-remint :warn nil nil)))))

(deftest a-severity-outside-the-enum-is-a-fault
  (doseq [s [:warning :fatal nil "error"]]
    (let [e (try (finding/finding :r s :actions :a) nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (some? e) (pr-str s))
      (is (= :fault (anomaly/category (ex-data e))) (pr-str s))))
  (is (not (m/validate finding/Finding {:rule :r :severity :warning}))))

(deftest no-lint-rule-declares-a-severity-a-reader-cannot-rank
  (doseq [{:keys [id severity]} lint/rules]
    (is (contains? finding/severities severity) (str id " " severity))))

(deftest blocking-findings-become-one-refusal
  (let [warn  (finding/finding :w :warn :actions :a {:message "only a hint"})
        err-b (finding/finding :b :error :actions :b {:message "second"})
        err-a (finding/finding :a :error :actions :a {:message "first"})]
    (testing "nothing blocks: no refusal"
      (is (nil? (finding/findings->refusal [warn])))
      (is (nil? (finding/findings->refusal []))))
    (let [r (finding/findings->refusal [warn err-b err-a] {:data {:sutra/gate :plan-problems}})
          d (ex-data r)]
      (is (= :incorrect (anomaly/category d)))
      (is (= :refusal.finding/blocking (:refusal/code d)))
      (is (= :plan-problems (:sutra/gate d)) "the caller's data rides along")
      (is (= [err-a err-b] (:problems d)) "only the blocking ones, worst first")
      (is (= "refused by 2 blocking finding(s), the first: first" (ex-message r))))))

(deftest where-producers-meet-a-severity-is-ranked
  (is (= :error (:severity (finding/ranked {:rule :r} :error))) "none named: the default")
  (is (= :info (:severity (finding/ranked {:rule :r :severity :info} :error)))
      "its own severity wins")
  (is (thrown? clojure.lang.ExceptionInfo
               (finding/ranked {:rule :r :severity :warning} :error))))
