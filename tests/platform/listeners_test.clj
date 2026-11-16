(ns ^:unit platform.listeners-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.schedules.listener-events :as listener-events]
            [sutra.schedules.listeners :as listeners]
            [sutra.schedules.triggers :as triggers]))

(def ^:private leaf {:kind :listener :of :mailbox :folders ["shared/rechnung"]})

(deftest trigger-leaf
  (testing "a :listener leaf is evaluatable, alone or beside a cadence"
    (is (nil? (triggers/assert-evaluatable! leaf)))
    (is (nil? (triggers/assert-evaluatable!
               {:kind :any-of :triggers [{:kind :cadence :cron "*/15 * * * *"} leaf]}))))
  (testing "the listeners the schedules hold, distinct"
    (is (= [{:source :mailbox :folders ["shared/rechnung"]}]
           (triggers/listeners [{:trigger leaf}
                                {:trigger {:kind :any-of :triggers [leaf]}}
                                {:trigger {:kind :parent-updated :of :raw_messages}}]))))
  (testing "only an :imap source can be listened to"
    (let [schema {:sources {"r1" {:source-name :mailbox :type :imap}
                            "r2" {:source-name :erp :type :jdbc}}}]
      (is (nil? (triggers/assert-listenable! schema leaf)))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"only an :imap"
                            (triggers/assert-listenable! schema (assoc leaf :of :erp))))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"which is no source"
                            (triggers/assert-listenable! schema (assoc leaf :of :nope))))))
  (testing "an arrival moves the listener signature"
    (let [before (listener-events/signature :test-source)]
      (listener-events/heard! :test-source)
      (is (not= before (listener-events/signature :test-source))))))

(deftest windows
  (testing "a change waits gap-ms before the next window; a quiet one reopens at once"
    (let [e (listeners/settle {:inflight true} {:changed true :state {"INBOX" {:uidnext 2}}} 0)]
      (is (= listeners/gap-ms (:next-at e)))
      (is (= {"INBOX" {:uidnext 2}} (:state e)))
      (is (not (:inflight e))))
    (is (= 7 (:next-at (listeners/settle {} {:changed false :state {}} 7)))))
  (testing "a failed window backs off, doubling, capped, and keeps the folder state"
    (let [fail #(listeners/settle % {:error "unreachable"} 0)
          e1   (fail {:state {"INBOX" {:uidnext 2}}})
          e2   (fail e1)
          e9   (nth (iterate fail e1) 10)]
      (is (= listeners/backoff-ms (:next-at e1)))
      (is (= (* 2 listeners/backoff-ms) (:next-at e2)))
      (is (= listeners/backoff-max-ms (:next-at e9)))
      (is (= {"INBOX" {:uidnext 2}} (:state e9)))
      (is (= 0 (:failures (listeners/settle e9 {:changed false :state {}} 1))))))
  (testing "due: not while a window is open, and not before next-at"
    (is (listeners/due? nil 0))
    (is (not (listeners/due? {:inflight true} 0)))
    (is (not (listeners/due? {:next-at 10} 5)))
    (is (listeners/due? {:next-at 10} 10))))
