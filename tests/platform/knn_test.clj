(ns ^:unit platform.knn-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.objects.knn :as knn]))

(def ^:private objs
  [{:id "a" :values {:v [1.0 0.0 0.0]}}
   {:id "b" :values {:v [0.0 1.0 0.0]}}
   {:id "c" :values {:v [0.9 0.1 0.0]}}
   {:id "d" :values {:v [0.0 0.0 1.0]}}
   {:id "none" :values {}}
   {:id "wrong-dim" :values {:v [1.0 0.0]}}])

(deftest nearest-cosine
  (testing "cosine ranks the closest direction first and drops bad rows"
    (let [r (knn/nearest [1.0 0.0 0.0] objs {:property :v :metric :cosine :k 3})]
      (is (= ["a" "c" "b"] (map :id r)) "a (exact) > c (near) > b (orthogonal)")
      (is (= 3 (count r)) "k honored; missing/wrong-dim rows excluded")
      (is (every? :score r) "each result carries a similarity score")
      (is (apply >= (map :score r)) "scores are descending (nearest first)"))))

(deftest nearest-metrics
  (testing ":dot rewards magnitude+direction, :l2 rewards proximity"
    (is (= "a" (:id (first (knn/nearest [1.0 0.0 0.0] objs {:property :v :metric :dot :k 1})))))
    (is (= "a" (:id (first (knn/nearest [1.0 0.0 0.0] objs {:property :v :metric :l2 :k 1})))))
    (is (= "d" (:id (first (knn/nearest [0.0 0.0 2.0] objs {:property :v :metric :l2 :k 1})))))))

(deftest parse-query-vector
  (testing "accepts EDN vectors, comma strings, and bracketed strings"
    (is (= [1.0 0.0 0.5] (knn/parse-query-vector [1 0 0.5])))
    (is (= [1.0 0.0 0.5] (knn/parse-query-vector "1,0,0.5")))
    (is (= [1.0 0.0 0.5] (knn/parse-query-vector "[1, 0, 0.5]")))
    (is (nil? (knn/parse-query-vector "not,a,vector")))
    (is (nil? (knn/parse-query-vector nil)))))
