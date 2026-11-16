(ns sutra-ui.apps.branching.events-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [sutra-ui.api :as api]
            [sutra-ui.apps.branching.events]
            [sutra-ui.state.governance :as gov]
            [sutra-ui.state.machine-testing :refer [run step]]
            [sutra-ui.state.shell :as shell]))

(defn- asked [[_ fx]] (keep (fn [[k a]] (when (= :call k) a)) fx))

(defn- calls [r] (map :call (asked r)))

(deftest reloading-asks-for-branches-and-proposals-by-status
  (let [[_ fx :as r] (step {:status "all"} [:branching/reload])]
    (is (= [[api/get! :branches {:closed true}]] (calls r)))
    (is (some #{[:invoke [gov/refresh-proposals! {:status "all"}]]} fx)))
  (is (= [[api/get! :branches {:closed false}]] (calls (step {} [:branching/reload])))))

(deftest opening-a-branch-drops-the-last-one-and-loads-its-parts
  (let [[s :as r] (step {:changes {:x 1} :roles {:y 1}} [:branching/open "b"])]
    (is (= "b" (:open s)))
    (is (nil? (:changes s)))
    (is (= [[api/get! :branch-changes {:name "b"}] [api/get! :branch-roles {:name "b"}]]
           (calls r)))))

(deftest closing-asks-first-then-posts-and-goes-back
  (let [[_ [[k {:keys [ok]}]]] (step {} [:branching/archive "b"])]
    (is (= :confirm k))
    (let [[{:keys [call ok]}] (asked (step {} ok))]
      (is (= [api/post! :branch-status {:name "b"} {:closed true}] call))
      (is (nil? (:open (run {:open "b"} (conj ok {}))))))))

(deftest an-owner-is-added-trimmed-and-the-draft-cleared
  (let [[s fx] (step {:owner-draft " anna "} [:branching.role/add "b"])]
    (is (= "" (:owner-draft s)))
    (is (= [[:dispatch [:branching.role/set "b" {:principal "anna"}]]] fx))))

(deftest retention-is-asked-per-space
  (is (= [[api/get! :branch-policy {:name "space/s1"}]]
         (calls (step {:space "s1"} [:branching.policy/load]))))
  (testing "saving posts only the two limits"
    (is (= [[api/post! :branch-policy {:name "*"} {:inactive-days 3}]]
           (calls (step {:space "*" :policy {:inactive-days 3 :branch "*"}}
                        [:branching.policy/save]))))))

(deftest a-failure-is-shown-an-abort-is-not
  (let [[{:keys [err]}] (asked (step {} [:branching.role/set "b" {}]))]
    (is (= [[:invoke [shell/fail! "roles" "nein"]]]
           (remove nil? (second (step {} (conj err "nein"))))))
    (is (empty? (remove nil? (second (step {} (conj err nil))))))))
