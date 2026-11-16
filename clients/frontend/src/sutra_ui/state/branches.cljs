(ns sutra-ui.state.branches
  (:require [sutra.kit.remote :as rd]
            [sutra.viz.lineage-branch :as lb]
            [sutra-ui.api :as api]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.shell :as shell]))

(defonce
  branches (db/cursor [:branches :branches] rd/loading))

(defn branch-list [] (rd/value-or @branches []))

(defonce
  branch-changes (db/cursor [:branches :branch-changes] rd/absent))

(defn- as-branches [resp] (vec (:branches resp)))

(defn- adopt-current! [selected resp]
  (shell/edit! assoc :current-branch (or selected (:current resp))))

(db/reg :sutra.branches/refresh
        (fn [_ [_ selected reply]]
          {:fx [(db/load [:branches :branches] [api/list-branches]
                         {:pick as-branches :report "branches"
                          :then [[:sutra.branches/loaded selected]]}
                         reply)]}))

(db/reg :sutra.branches/loaded
        (fn [_ [_ selected resp]]
          {:fx [[:invoke [adopt-current! selected resp]]
                [:dispatch [:sutra.branches/changes (or selected (:current resp))]]]}))

(defn refresh-branches!
  []
  (db/promise! [:sutra.branches/refresh (api/selected-page-branch)]))

(defn- branch-call!
  [c what]
  (let [selected (api/selected-page-branch)]
    (db/promise! [:rf/request {:ask c :report what :handover? true
                               :then [(fn [_ reply] [:sutra.branches/refresh selected reply])]}])))

(defn fork-branch!
  [{:keys [parent name]}]
  (branch-call! [api/post! :branches {:parent parent :name name}] "fork"))

(defn drop-branch!
  [name]
  (branch-call! [api/delete! :branch {:name name}] "drop"))

(db/reg :sutra.branches/changes
        (fn [db [_ cur]]
          (let [db (assoc-in db [:branches :changes-of] cur)]
            (if (or (nil? cur) (= "main" cur))
              {:db (assoc-in db [:branches :branch-changes] rd/absent)}
              {:db (update-in db [:branches :branch-changes]
                              #(if (= cur (:branch %)) % rd/loading))
               :fx [(db/load [:branches :branch-changes] [api/get! :branch-changes {:name cur}]
                             {:pick rd/of-answer :report "branch changes"
                              :current [[:branches :changes-of] cur]})]}))))

(defn refresh-branch-changes! []
  (db/send! [:sutra.branches/changes (:current-branch @shell/ui-state)]))

(defn branch-changes-view
  []
  (lb/changes-view (:current-branch @shell/ui-state) @branch-changes))

(defn branch-modified-refs
  []
  (lb/view-modified-refs (branch-changes-view)))
