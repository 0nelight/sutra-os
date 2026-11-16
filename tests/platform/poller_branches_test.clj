(ns ^:unit platform.poller-branches-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.automation.runs :as autos]
            [sutra.store.seat :as seat]
            [sutra.schedules.poller :as poller]))

(deftest every-due-item-fires-in-its-own-branch
  (let [fired (atom [])
        home  "main"
        tick  (poller/each-due-across
               "t"
               (fn [_] [{:branch "main" :id 1} {:branch "try-x" :id 2} {:branch "try-x" :id 3}])
               (fn [store branch item] (swap! fired conj [(:branch store) branch (:id item)]))
               :id)]
    (with-redefs [seat/seated-branch (constantly home)]
      (tick {:branch "main"}))
    (is (= #{["main" home 1] ["try-x" "try-x" 2] ["try-x" "try-x" 3]} (set @fired))
        "every branch fires under its name")))

(deftest one-failing-item-does-not-stop-the-others
  (let [fired (atom [])
        tick  (poller/each-due-across
               "t"
               (fn [_] [{:branch "a" :id 1} {:branch "b" :id 2}])
               (fn [_ _ item]
                 (if (= 1 (:id item)) (throw (ex-info "boom" {})) (swap! fired conj (:id item))))
               :id)]
    (with-redefs [seat/seated-branch (constantly "main")]
      (tick {:branch "main"}))
    (is (= [2] @fired))))

(deftest ownership-is-judged-against-the-nearest-ancestor-that-has-the-row
  (let [r      {:id "a" :name "n" :on-type "t" :effect-spec "{}" :every-ms 1 :paused false}
        parent {"feat" "main" "feat2" "feat"}
        rows   {["main" "a"] r
                ["feat" "a"] (assoc r :effect-spec "{:x 1}")}
        own    #(set (map :branch (autos/own-rows % parent (fn [b id] (get rows [b id])))))]
    (is (= #{"main" "feat"}
           (own [(assoc r :branch "main") (assoc r :branch "feat" :effect-spec "{:x 1}")
           (assoc r :branch "feat2" :effect-spec "{:x 1}")]))
        "feat changed it on a fork of main and owns it; feat2 only restates feat's version")
    (is (= #{"feat2"} (own [(assoc r :branch "feat2")]))
        "feat2 went back to main's version, which differs from its parent's: its own change")))
