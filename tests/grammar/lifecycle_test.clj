(ns ^:unit grammar.lifecycle-test
  (:require [sutra.kit.anomaly :as anomaly]
            [clojure.test :refer [deftest is testing]]
            [sutra.kit.lifecycle :as lc]
            [sutra.access.oauth :as oauth]
            [sutra.access.sessions :as sessions]))

(def ^:private tables [sessions/lifecycle oauth/client-lifecycle])

(defn- refused-code [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (:refusal/code (ex-data e)))))

(deftest every-edge-is-taken-and-every-other-pair-refused
  (doseq [{:keys [name states edges] :as t} tables
          event (keys edges)
          from  (cons nil states)
          :let  [guard (get-in edges [event :guard])]
          :when (nil? guard)]
    (testing (str name " " event " from " from)
      (if (lc/allowed? t event from)
        (is (keyword? (lc/decide t "id" event from {} nil)))
        (is (= :refusal.lifecycle/transition-refused
               (refused-code #(lc/decide t "id" event from {} nil))))))))

(deftest an-unknown-event-is-refused
  (is (= :refusal.lifecycle/transition-refused
         (refused-code #(lc/decide sessions/lifecycle "t" :reopen :ended nil nil)))))

(deftest a-guard-refuses-with-its-reason
  (let [e (try (lc/decide oauth/client-lifecycle "c" :issue :active {:secret-hash nil}
                          {:secret "x"})
               (catch clojure.lang.ExceptionInfo e e))]
    (is (= 409 (anomaly/status (ex-data e))))
    (is (re-find #"not the client's current one" (ex-message e)))))

(deftest a-table-that-leaves-a-final-state-is-refused-at-definition
  (is (thrown? clojure.lang.ExceptionInfo
               (lc/table {:name :t :states #{:a :b} :final #{:b}
                          :edges {:back {:from #{:b} :to :a}}})))
  (is (thrown? clojure.lang.ExceptionInfo
               (lc/table {:name :t :states #{:a} :final #{}
                          :edges {:go {:from #{:a} :to :nowhere}}}))))
