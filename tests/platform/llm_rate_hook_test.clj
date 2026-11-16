(ns ^:api platform.llm-rate-hook-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.wire.api :as api]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-pod)

(defn- rate-rid []
  (->> (:datasets (:body (api/get! :datasets)))
       (some (fn [[rid d]] (when (= :llm_model_rates (:dataset-name d)) rid)))))

(defn- ledger-rows-for [model]
  (->> (-> (api/get! :dataset-preview {:id (rate-rid)}) :body :rows)
       (map :values)
       (filter #(= model (:model %)))))

(defn- dep? [row]
  (contains? #{1 true "1" "true"} (:deprecated row)))

(defn- reg!
  ([id in out] (reg! id in out nil))
  ([id in out deprecated]
   (api/mutate :create-llm-model
               (cond-> {:name id :in in :out out :currency "USD"}
                 (some? deprecated) (assoc :deprecated deprecated))
               {:stage? true})
   (api/post! :staging)))

(deftest hook-dedups-against-latest-ledger-rate
  (testing "a fresh registration seeds one ledger row (ledger-absent ⇒ write — the
            SAME code path that self-heals a registered model with no ledger row)"
    (reg! "hook-a" 1.0 2.0)
    (is (= 1 (count (ledger-rows-for "hook-a"))))
    (let [r (first (ledger-rows-for "hook-a"))]
      (is (== 1.0 (double (:rate_in r))))
      (is (== 2.0 (double (:rate_out r))))
      (is (some? (:effective_from r)) "dated at commit time")))

  (testing "a second, different model gets its own row (hook fires on later commits)"
    (reg! "hook-b" 3.0 4.0)
    (is (= 1 (count (ledger-rows-for "hook-b")))))

  (testing "re-registering the SAME rate writes NOTHING (idempotent vs ledger latest)"
    (reg! "hook-a" 1.0 2.0)
    (reg! "hook-a" 1.0 2.0)
    (is (= 1 (count (ledger-rows-for "hook-a"))) "still one row"))

  (testing "a CHANGED rate appends a new dated row"
    (reg! "hook-a" 1.5 2.0)
    (is (= 2 (count (ledger-rows-for "hook-a"))) "now two rows")
    (is (= #{1.0 1.5} (set (map (comp double :rate_in) (ledger-rows-for "hook-a"))))))

  (testing "DEPRECATION is a tracked, dated ledger event (a new row, reason=deprecate)"
    (reg! "hook-b" 3.0 4.0 true)
    (let [rs (ledger-rows-for "hook-b")]
      (is (= 2 (count rs)) "deprecation appended a row despite unchanged rate")
      (let [dep (first (filter dep? rs))]
        (is (some? dep) "a row carries deprecated=true")
        (is (= "deprecate" (:reason dep)))
        (is (some? (:effective_from dep)) "with the timestamp of WHEN it was deprecated"))))

  (testing "re-asserting the SAME deprecated status writes nothing (idempotent)"
    (reg! "hook-b" 3.0 4.0 true)
    (is (= 2 (count (ledger-rows-for "hook-b"))) "still two rows"))

  (testing "RESTORE (un-deprecate) is also a dated event"
    (reg! "hook-b" 3.0 4.0 false)
    (let [rs (ledger-rows-for "hook-b")]
      (is (= 3 (count rs)))
      (is (= "restore" (:reason (last (sort-by #(str (:effective_from %)) rs))))))))
