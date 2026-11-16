(ns sutra-ui.builder.events-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [sutra-ui.api :as api]
            [sutra-ui.builder.events :as ev]
            [sutra-ui.feature :as feature]
            [sutra-ui.state.branches :as branches]
            [sutra-ui.state.governance :as gov]
            [sutra-ui.state.machine-testing :refer [step]]
            [sutra-ui.state.ontology :as ontology]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.state.sync :as sync]))

(defn- call-of [fx] (some (fn [[k a]] (when (= :call k) a)) fx))

(defn- answered
  [fx v]
  (let [[[_ ev]] (second (step {} (conj (:ok (call-of fx)) v)))]
    (second (step {} ev))))

(deftest the-palette-searches-objects-only-for-two-letters-and-after-a-pause
  (let [[_ fx] (step {} [:builder.cmdk/query "a"])]
    (is (= [[:invoke [shell/set-cmdk-query! "a"]] [:cancel :cmdk-search]
            [:invoke [shell/set-cmdk-objects! []]]]
           fx)))
  (let [[_ fx] (step {} [:builder.cmdk/query "ab"])]
    (is (= [:after {:key :cmdk-search :ms ev/search-ms :event [:builder.cmdk/search "ab"]}]
           (second fx))))
  (let [[_ fx] (step {} [:builder.cmdk/search "ab"])]
    (is (= [api/get! :instances-search {:q "ab" :type nil}] (:call (call-of fx))))
    (is (= [[:dispatch [:rf/invoke shell/set-cmdk-objects! []]]]
           (second (step {} (conj (:err (call-of fx)) "weg")))))
    (is (= [[:invoke [shell/set-cmdk-objects! []]]]
           (second (step {} [:rf/invoke shell/set-cmdk-objects! []]))))))

(deftest a-proposal-forks-replays-each-staged-action-commits-and-proposes
  (let [actions [{:action :a1 :args {:x 1}} {:action :a2 :args {}}]
        [_ fx]  (step {} [:builder.proposal/make "main" "feature" "" actions])
        p       {:parent "main" :pname "feature" :title "" :actions actions}]
    (is (= [branches/fork-branch! {:parent "main" :name "feature"}] (:call (call-of fx))))
    (is (= [[:builder.proposal/forked p]] (:then (second (:ok (call-of fx))))))
    (let [fx (answered fx nil)]
      (is (= [sync/checkout-branch! "feature"] (:call (call-of fx))))
      (loop [fx (answered fx nil) seen []]
        (let [{:keys [call]} (call-of fx)]
          (if (= ontology/mutate! (first call))
            (recur (answered fx nil) (conj seen (vec (rest call))))
            (do (is (= [[:a1 {:x 1}] [:a2 {}]] seen))
                (is (= [sync/commit-staged!] call))
                (is (= [gov/propose-change! {:source "feature" :target "main"
                                             :title "Changes from feature"}]
                       (:call (call-of (answered fx nil))))))))))))

(deftest a-made-proposal-opens-a-landed-one-goes-to-main
  (is (= [:invoke [feature/command! :open-proposal 7]]
         (second (second (step {} [:builder.proposal/made {:id 7}])))))
  (is (= [] (second (step {} [:builder.proposal/made nil]))))
  (is (= [[:invoke [sync/checkout-branch! "main"]]]
         (second (step {} [:builder.propose/done nil {:auto-landed true :id 3}]))))
  (testing "a merge goes to main only when it answered"
    (let [[s fx] (step {:taskbar {:merging? true :build "affected"}} [:builder.merge/go 5])]
      (is (false? (get-in s [:taskbar :merging?])))
      (is (= [gov/merge-branch-proposal! 5 :affected] (:call (call-of fx)))))
    (is (= [] (second (step {} [:builder.merge/done nil]))))))

(deftest saving-as-needs-a-name
  (is (= [{:save-as {:name "  "}} []] (step {:save-as {:name "  "}} [:builder.save-as/submit "a"])))
  (let [[s fx] (step {:save-as {:name " Plan " :folder-rid "f"}} [:builder.save-as/submit "a"])]
    (is (nil? (:save-as s)))
    (is (= {:name "Plan" :area "a" :folder-rid "f"} (second (second (first fx)))))))
