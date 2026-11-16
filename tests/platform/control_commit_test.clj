(ns ^:unit platform.control-commit-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.store.control :as ctl]
            [sutra.store.served :as served]
            [support.mem :as mem]))

(defn- snooze [id reason]
  {:check-id id :branch "main" :until (java.util.Date. 0) :reason reason :snoozed-by "u"})

(defn- k [id] {:check-id id :branch "main"})

(deftest every-op-lowers-onto-its-planner
  (let [store (mem/store)]
    (testing "upsert and insert"
      (ctl/commit! store [[:upsert :health-snoozes [(snooze "a" "r1")]]
                          [:insert :health-snoozes [(snooze "b" "r2")]]])
      (is (= ["r1" "r2"] (mapv #(:reason (ctl/get store :health-snoozes (k %))) ["a" "b"]))))
    (testing "update with a fn of the row read in the commit, and with a map"
      (ctl/commit! store [[:update :health-snoozes (k "a") #(assoc % :reason (str (:reason %) "+"))]
                          [:update :health-snoozes (k "b") {:reason "m"}]])
      (is (= "r1+" (:reason (ctl/get store :health-snoozes (k "a")))))
      (is (= "m" (:reason (ctl/get store :health-snoozes (k "b"))))))
    (testing "swap, a [:plan t f] op and a bare planner, results in order"
      (let [[sw pl bare]
            (ctl/commit! store [[:swap :health-snoozes (k "a") #(assoc % :reason "s")]
                                [:plan :health-snoozes
                                 (fn [s _] (ctl/deleting s :health-snoozes (k "b")))]
                                (fn [_ _] {:result :bare})])]
        (is (= "s" (:reason sw)))
        (is (= 1 pl))
        (is (= :bare bare))
        (is (nil? (ctl/get store :health-snoozes (k "b"))))))
    (testing "delete"
      (ctl/commit! store [[:delete :health-snoozes (k "a")]])
      (is (nil? (ctl/get store :health-snoozes (k "a")))))))

(deftest the-lane-follows-from-the-tables
  (is (nil? (ctl/ops-lane [[:upsert :health-snoozes []] (fn [_ _] {})])))
  (is (= :auth (ctl/ops-lane [[:plan :sessions identity] [:delete :oauth-clients {}]])))
  (let [lanes (atom [])]
    (with-redefs [served/on-lane (fn [db l] (swap! lanes conj l) db)]
      (ctl/commit! (mem/store) [[:delete :sessions {:token "none"}]])
      (ctl/commit! (mem/store) [[:delete :health-snoozes (k "none")]]))
    (is (= [:auth] @lanes) "a laned table serves the commit on its lane, others stay put"))
  (testing "two lanes in one commit are refused, before anything is written"
    (let [e (try (ctl/commit! (mem/store) [[:upsert :health-snoozes [(snooze "x" "r")]]
                                           [:delete :sessions {:token "t"}]])
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (= :refusal.store/mixed-lanes (:refusal/code (ex-data e))))
      (is (re-find #"different lanes" (ex-message e))))))
