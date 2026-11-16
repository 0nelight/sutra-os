(ns sutra-ui.apps.lineage.events-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [sutra-ui.api :as api]
            [sutra-ui.apps.lineage.events :as ev]
            [sutra-ui.apps.lineage.model :as lineage-model]
            [sutra-ui.state.machine-testing :refer [run step]]
            [sutra-ui.state.sync :as sync]))

(defn- asked [[_ fx]] (keep (fn [[k a]] (when (= :call k) a)) fx))

(defn- calls [r] (map :call (asked r)))

(deftest a-build-is-planned-before-it-runs
  (let [[s :as r] (step {:builds {:strategy :selected}} [:lineage.builds/estimate {:only ["a"]}])]
    (is (= :loading (get-in s [:builds :preview])))
    (is (= [[api/build-estimate {:only ["a"]}]] (calls r))))
  (testing "another strategy drops the plan"
    (is (nil? (get-in (first (step {:builds {:preview {}}} [:lineage.builds/strategy :all]))
                      [:builds :preview]))))
  (testing "a run without targets builds everything"
    (is (= [[api/post! :import-run]] (calls (step {} [:lineage.builds/run nil])))))
  (let [[s :as r] (step {:builds {:preview {}}} [:lineage.builds/run {:only ["a"]}])
        [{:keys [ok]}] (asked r)]
    (is (true? (get-in s [:builds :busy?])))
    (is (= {:busy? false :preview nil} (:builds (run s (conj ok {})))))
    (is (some #{[:invoke [sync/refresh-snapshot!]]} (second (step s (conj ok {})))))))

(deftest schedules-load-on-entering-and-forget-on-leaving
  (let [[s :as r] (step {} [:lineage.schedules/enter])
        [{:keys [call ok]}] (asked r)]
    (is (= ev/blank-schedules (:schedules s)))
    (is (= [api/get! :schedules] call))
    (is (= [{:id 1}] (get-in (run s (conj ok {:schedules [{:id 1}]})) [:schedules :items]))))
  (is (nil? (:schedules (first (step {:schedules {}} [:lineage.schedules/leave]))))))

(deftest a-schedule-is-added-with-what-was-typed
  (let [db {:schedules {:name "nacht" :cron "0 2 * * *"}}
        r  (step db [:lineage.schedules/add "a,b"])
        [{:keys [ok]}] (asked r)]
    (is (= [[api/post! :schedules {:name "nacht" :cron "0 2 * * *" :target "a,b"}]] (calls r)))
    (is (= "" (get-in (run db (conj ok {})) [:schedules :name])))))

(deftest a-delete-is-asked-first
  (let [[_ [[k {:keys [ok]}]]] (step {} [:lineage.schedules/delete 7 "nacht"])]
    (is (= :confirm k))
    (is (= [[api/delete! :schedule-of {:id 7}]] (calls (step {} ok)))))
  (let [[_ [[k {:keys [ok]}]]] (step {} [:lineage.group/delete {:id "g" :name "G"}])]
    (is (= :confirm k))
    (is (= [[:invoke [lineage-model/delete-group! "g"]]] (second (step {} ok))))))

(deftest an-empty-group-name-changes-nothing
  (is (= [] (second (step {} [:lineage.group/renamed "g" ""]))))
  (is (= 1 (count (second (step {} [:lineage.group/renamed "g" "Neu"]))))))
