(ns datasets.landing-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.datasets.landing :as landing]))

(def ^:private before [{:id "1" :n 1 :m "a"}])

(defn- tx [kind & rows] {:kind kind :pk "id" :rows (vec rows)})

(deftest the-profile-cuts-the-landing
  (is (= [:merge :diff] (landing/residual (landing/offer {}))))
  (is (= [:diff] (landing/residual (landing/offer {:landing #{:merge}}))))
  (is (= [] (landing/residual (landing/offer {:landing #{:merge :diff}})))))

(deftest the-core-reads-only-for-what-it-does
  (let [whole {:whole-row? true}]
    (is (landing/reads-before? {:merge false :diff false} whole))
    (is (landing/reads-before? {:merge true :diff false} {}))
    (is (landing/reads-before? {:merge false :diff true} {}))
    (is (not (landing/reads-before? {:merge false :diff true} whole)))
    (is (not (landing/reads-before? {:merge true :diff true} {})))))

(deftest what-the-driver-is-handed
  (testing "a driver without :merge gets whole rows, one per key, the last"
    (is (= [{:id "1" :n 3 :m "a"}]
           (:rows (:tx (landing/pushed (tx :update {:id "1" :n 2} {"id" "1" "n" 3})
                                       {:merge false :diff true} before true))))))
  (testing "a driver with :merge gets the write as it was asked"
    (let [t (tx :update {:id "1" :n 2})]
      (is (= t (:tx (landing/pushed t {:merge true :diff false} before true))))))
  (testing "a driver without :diff never gets a write that changes nothing"
    (is (= {:rows 1} (landing/pushed (tx :update {:id "1" :n 1})
                                     {:merge true :diff false} before true))))
  (testing "a first write is a write, even of nothing"
    (is (:tx (landing/pushed (tx :snapshot) {:merge false :diff false} [] false)))))

(deftest one-step-list-for-every-driver
  (testing "a snapshot retracts what it inherits and refreshes in one merge"
    (is (= [:retract-inherited :ensure-columns :merge]
           (mapv first (landing/plan {:replace? true :groups [[nil [{:id "1"}]]]})))))
  (testing "an update forks and merges"
    (is (= [:fork :ensure-columns :merge]
           (mapv first (landing/plan {:fork? true :groups [[nil [{:id "1"}]]]}))))))
