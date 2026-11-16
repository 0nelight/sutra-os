(ns ^:unit ontology.retention-policy-plan-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.catalog.nodes :as nodes]
            [sutra.types.retention :as ret]))

(def ^:private day (* 24 60 60 1000))
(def ^:private now (* 400 day))

(defn- txn [v kind age] {:version v :kind kind :at (- now (* age day))})

(def ^:private ds {:rid "ri.d" :name "d"})

(deftest a-frequent-build-keeps-its-rollback-window
  (let [hourly (mapv #(txn (inc %) :snapshot (/ (- 20 %) 24)) (range 20))]
    (is (nil? (:horizon (ret/plan ret/recommended ds hourly now)))
        "twenty views of today are younger than 30 days, however many there are"))
  (let [old (mapv #(txn (inc %) :snapshot (- 60 %)) (range 20))]
    (is (= 10 (:deleted (ret/plan ret/recommended ds old now)))
        "outside the last ten views and older than 30 days")))

(deftest a-rare-build-keeps-its-last-views-however-old
  (let [rare (mapv #(txn (inc %) :snapshot (- 300 (* 30 %))) (range 5))]
    (is (nil? (:horizon (ret/plan ret/recommended ds rare now))))))

(deftest a-modify-chain-sheds-its-past-but-no-recommended-policy-drops-rows
  (let [chain [(txn 1 :snapshot 90) (txn 2 :update 60) (txn 3 :update 1)]
        p     (ret/plan ret/recommended ds chain now)]
    (is (= 3 (:horizon p)))
    (is (nil? (:expire-through p)))))

(def ^:private custom
  [{:name "keep-90" :datasets :all :selectors [[:older-than-days 90]] :allow-latest-view? true}])

(deftest a-custom-policy-expires-current-rows-after-the-grace
  (testing "selected only just now: the horizon moves, the rows stay for the grace period"
    (let [p (ret/plan custom ds [(txn 1 :snapshot 91) (txn 2 :append 1)] now)]
      (is (= 2 (:horizon p)))
      (is (nil? (:expire-through p)))))
  (testing "selected a grace period ago: its rows go"
    (let [p (ret/plan custom ds [(txn 1 :snapshot 120) (txn 2 :append 98) (txn 3 :append 1)] now)]
      (is (= 2 (:expire-through p)))))
  (testing "a policy without the flag never does"
    (let [p (ret/plan [(assoc (first custom) :allow-latest-view? false)]
                      ds [(txn 1 :snapshot 120) (txn 2 :append 98) (txn 3 :append 1)] now)]
      (is (nil? (:expire-through p))))))

(deftest rolling-back-a-node-takes-the-jobs-other-outputs-along
  (let [sch {:datasets   {"ri.a" {:dataset-name :a} "ri.b" {:dataset-name :b}
                          "ri.c" {:dataset-name :c}}
             :transforms {"ri.t" {:transform-name :t
                                  :outputs [{:kind :dataset :dataset "ri.a"}
                                            {:kind :dataset :dataset "ri.b"}]}}}]
    (is (= ["ri.a" "ri.b"] (nodes/node->datasets sch :transform :t)))
    (is (= ["ri.b" "ri.a"] (nodes/node->datasets sch :dataset :b)))
    (is (= ["ri.c"] (nodes/node->datasets sch :dataset :c)) "an upload stands alone")
    (is (nil? (nodes/node->datasets sch :object-type :x)))))
