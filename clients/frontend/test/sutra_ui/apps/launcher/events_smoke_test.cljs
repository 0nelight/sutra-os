(ns sutra-ui.apps.launcher.events-smoke-test
  (:require [cljs.test :refer [deftest is]]
            [sutra-ui.api :as api]
            [sutra-ui.apps.launcher.events]
            [sutra-ui.state.machine-testing :refer [run step]]
            [sutra-ui.routing :as routing]
            [sutra-ui.state.portal :as pstate]
            [sutra-ui.state.shell :as shell]))

(deftest arriving-sets-the-version-before-asking-and-opens-the-named-app-after
  (let [[_ [v [_ c]]] (step {} [:launcher/enter {:app-version "dev" :open-app-slug "a"}])]
    (is (= [:invoke [pstate/set-consumer-app-version! "dev"]] v))
    (is (= [pstate/refresh-consumer-apps!] (:call c)))
    (is (= [[:dispatch [:rf/invoke routing/apply-open-app! "a"]]]
           (second (step {} (conj (:ok c) nil))))))
  (let [[_ [_ [_ c]]] (step {} [:launcher/enter {}])]
    (is (= [] (second (step {} (conj (:ok c) nil)))))))

(deftest only-the-access-tab-loads-the-snapshot
  (let [[s [[k c]]] (step {} [:launcher/tab :access])]
    (is (= :access (:tab s)))
    (is (= :call k))
    (is (= [api/get! :access] (:call c)))
    (is (= {:a 1} (:access (run s (conj (:ok c) {:a 1})))))
    (is (= [:invoke [shell/fail! "access" "weg"]]
           (first (second (step s (conj (:err c) "weg")))))))
  (is (= [] (second (step {} [:launcher/tab :apps])))))
