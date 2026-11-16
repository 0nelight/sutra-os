(ns ^:unit platform.health-watch-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.health.watch :as w]))

(def ^:private now 1000000000)

(defn- at [ms] (java.util.Date. (long ms)))

(def ^:private watches
  [{:username "anna" :level "critical"} {:username "bob" :level "all"}])

(deftest who-hears-of-a-failure
  (let [crit {:expects {:severity :critical}}
        mod  {:expects {:severity :moderate}}
        fail {:status "failed"}
        pass {:status "passed"}]
    (testing "a check that turns failed tells the watchers its severity reaches"
      (is (= ["anna" "bob"] (w/to-tell crit pass fail watches nil now)))
      (is (= ["bob"] (w/to-tell mod pass fail watches nil now)))
      (is (= ["anna" "bob"] (w/to-tell crit nil fail watches nil now)) "its first run too"))
    (testing "a check failing again tells nobody, nor a passing one"
      (is (empty? (w/to-tell crit fail fail watches nil now)))
      (is (empty? (w/to-tell crit fail pass watches nil now))))
    (testing "a snooze silences it until it ends"
      (is (empty? (w/to-tell crit pass fail watches {:until (at (inc now))} now)))
      (is (= 2 (count (w/to-tell crit pass fail watches {:until (at (dec now))} now)))))))

(deftest digest-periods
  (is (= 900000 (w/every-ms "15m")))
  (is (= 86400000 (w/every-ms "daily")))
  (is (= 604800000 (w/every-ms "weekly")))
  (is (nil? (w/every-ms "sometimes")))
  (is (w/due? {:every-ms 60000 :last-sent-at nil} now))
  (is (not (w/due? {:every-ms 60000 :last-sent-at (at (- now 59999))} now)))
  (is (w/due? {:every-ms 60000 :last-sent-at (at (- now 60000))} now)))

(deftest a-digest-counts-the-group
  (let [g      {:name "Kern" :check-ids ["c1" "c2"]}
        checks [{:id "c1" :name "Zeilen" :latest {:status "failed" :detail "leer"}}
                {:id "c2" :name "Frische" :latest {:status "passed"}}
                {:id "c3" :name "Fremd" :latest {:status "failed"}}]]
    (testing "failing checks are listed, a snoozed one marked"
      (let [d (w/digest g checks {"c1" {}} true)]
        (is (= "Check-Gruppe Kern: 1 von 2 fehlgeschlagen" (:heading d)))
        (is (str/includes? (:message d) "Zeilen (snoozed) — leer"))
        (is (not (str/includes? (:message d) "Fremd")))))
    (testing "only-failing sends nothing while all pass"
      (is (nil? (w/digest g (assoc-in checks [0 :latest :status] "passed") {} true)))
      (is (some? (w/digest g (assoc-in checks [0 :latest :status] "passed") {} false))))))

(deftest the-list-shows-my-watch-and-the-snooze
  (let [rows [{:id "c1"} {:id "c2"}]
        out  (w/annotate rows [{:check-id "c1" :username "anna" :level "all"}
                               {:check-id "c2" :username "bob" :level "all"}]
                         [{:check-id "c2" :until (at (inc now)) :reason "Umbau"}]
                         "anna" now)]
    (is (= "all" (:watching (first out))))
    (is (nil? (:watching (second out))))
    (is (= "Umbau" (get-in (second out) [:snoozed :reason])))))
