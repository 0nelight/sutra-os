(ns sutra-ui.apps.admin-settings.events-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [sutra.kit.remote :as rd]
            [sutra-ui.api :as api]
            [sutra-ui.apps.admin-settings.events :as ev]
            [sutra-ui.platform :as platform]
            [sutra-ui.state.machine-testing :refer [run step]]))

(defn- asked [[_ fx]] (keep (fn [[k a]] (when (= :call k) a)) fx))

(defn- calls [r] (map :call (asked r)))

(deftest a-tab-leaves-the-one-session
  (is (= {:tab :budget} (first (step {:sitzung "s1"} [:admin/tab :budget])))))

(deftest the-platform-draft-is-saved-trimmed-and-cleared
  (let [[s :as r] (step {:platform {:draft {:title " Sutra "}}} [:admin.platform/save])
        [{:keys [call ok err]}] (asked r)
        after #(select-keys (:platform (run s %)) [:draft :busy :saved :error])]
    (is (true? (get-in s [:platform :busy])))
    (is (= [platform/save! {:title "Sutra"}] call))
    (is (= {:draft {} :busy false :saved true :error nil} (after (conj ok {}))))
    (testing "a failure says why, an abort only settles"
      (is (= {:draft {:title " Sutra "} :busy false :saved false :error "nein"}
             (after (conj err "nein"))))
      (is (= {:draft {:title " Sutra "} :busy false :saved false :error nil}
             (after (conj err nil)))))))

(deftest overrides-move-and-drop
  (let [ovs [{:url "/a"} {:url "/b"}]]
    (is (= [{:url "/b"} {:url "/a"}] (ev/edit-overrides ovs [:down 0])))
    (is (= ovs (ev/edit-overrides ovs [:up 0])))
    (is (= [{:url "/b"}] (ev/edit-overrides ovs [:remove 0])))
    (is (= {:groups [] :url ""} (last (ev/edit-overrides ovs [:add nil]))))))

(deftest a-budget-limit-is-sent-as-a-number
  (let [[_ [[_ e]]] (step {:budget {:draft {:eur "1,5" :quotas {"p" "2"}}}}
                          [:admin.budget/save-limits])]
    (is (= [:admin.budget/save {:eur 1.5}] e)))
  (let [[_ [[_ e]]] (step {:budget {:draft {:quotas {"p" "2"}}}}
                          [:admin.budget/save-quota :project "p"])]
    (is (= [:admin.budget/save {:project "p" :quota 2}] e)))
  (is (= [[api/post! :llm-budget {:source "s" :quota nil}]]
         (calls (step {} [:admin.budget/save {:source "s" :quota nil}])))))

(deftest a-failed-reload-keeps-what-is-shown
  (let [failed (fn [data]
                 (let [db {:egress {:data data}}
                       [{:keys [err]}] (asked (step db [:admin.egress/load]))]
                   (get-in (run db (conj err "weg")) [:egress :data])))]
    (is (= {:policies []} (failed {:policies []})))
    (is (rd/failed? (failed nil)))))

(deftest a-checked-session-is-merged-in-place
  (let [[s] (step {:feedback {:sitzungen [{:id "a"} {:id "b"}]}}
                  [:admin.feedback/checked "b" {:erledigt true}])]
    (is (= [{:id "a"} {:id "b" :erledigt true}] (get-in s [:feedback :sitzungen])))))
