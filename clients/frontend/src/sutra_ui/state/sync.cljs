(ns sutra-ui.state.sync
  (:require [clojure.string :as str]
            [sutra-ui.state.requests :as rq]
            [sutra.viz.lineage-branch :as lb]
            [sutra-ui.api :as api]
            [sutra-ui.feature :as feature]
            [sutra-ui.state.branches :as branches]
            [sutra-ui.state.canvas :as canvas]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.instances :as instances]
            [sutra-ui.state.atlas :as kompass]
            [sutra-ui.state.lineage]
            [sutra-ui.state.lineage-status :as lineage-status]
            [sutra-ui.state.panel :as panel]
            [sutra-ui.state.principals]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.state.staging :as staging]))

(defonce snapshot
  (db/cursor [:sync :snapshot] {:branch "main" :counts {:objects 0 :links 0}}))

(defn- integrity-warning
  [{:keys [object-types link-types unresolved-backings]}]
  (let [names (concat object-types link-types)
        msgs  (cond-> []
                (seq names)
                (conj
                 (str "Some types are missing their backing table and were skipped: "
                      (str/join ", " names)
                      ". The rest of the ontology loaded normally; committing an install repairs "
                      "them."))
                (seq unresolved-backings)
                (conj
                 (str "Some producers could not be resolved and were left out of the import plan: "
                      (str/join ", " (for [u unresolved-backings]
                                       (str (:name u) " (" (:error u) ")")))
                      ".")))]
    (when (seq msgs) (str/join " " msgs))))

(defn- snapshot-query
  [db]
  (let [v (get-in db [:sync :schema-version])]
    (cond-> {}
      (and v (seq (get-in db [:catalog :canonical-schema :object-types]))) (assoc :since v))))

(db/reg :sutra.sync/snapshot
        (fn [db [_ reply]]
          (rq/request db {:ask  [api/get! :snapshot (snapshot-query db)]
                          :then [[:sutra.sync/snapshot-read]]
                          :else [[:sutra.sync/snapshot-failed]] :report "snapshot"}
                      reply)))

(db/reg :sutra.sync/snapshot-read
        (fn [db [_ snap]]
          (let [changed? (not (:schema-unchanged snap))]
            {:db (-> (cond-> (update db :sync assoc
                                     :snapshot snap :schema-version (:schema-version snap))
                       changed? (-> (update :catalog catalog/adopt (:schema snap)
                                            (:schema-derived snap))
                                    (update :data data/adopt-snapshot snap)))
                     (update-in [:catalog :schema-version] inc))
             :fx [(when changed? [:invoke [shell/warn! (integrity-warning (:integrity snap))]])
                  data/refresh-object-counts
                  [:dispatch [:sutra.data/refresh-runs]]
                  [:dispatch [:sutra.lineage/refresh]]
                  [:dispatch [:sutra.principals/refresh]]]})))

(db/reg :sutra.sync/snapshot-failed
        (fn [db [_ sentence]]
          {:db (-> db
                   (update :catalog catalog/failed sentence)
                   (update :data data/snapshot-failed sentence))}))

(defn refresh-snapshot!
  []
  (db/promise! [:sutra.sync/snapshot]))

(defn chain-refreshes!
  [fs]
  (reduce (fn [p f] (.then p (fn [_] (f)))) (js/Promise.resolve nil) fs))

(defn refresh-after-staging-change! []
  (chain-refreshes!
   [refresh-snapshot! staging/refresh-staged! instances/refresh-instance-state!]))

(defn- reread!
  ([reply fs] (reread! reply fs ::last))
  ([reply fs v]
   (cond-> (chain-refreshes! fs)
     reply (.then #((:resolve reply) (if (= ::last v) % v))))))

(defn- send!
  [req]
  (db/promise! [:rf/request (assoc req :reject? true)]))

(defn commit-staged!
  []
  (shell/with-loading
    (send! {:ask [api/post! :staging]
            :fx  [[:invoke [reread! nil [refresh-snapshot! staging/refresh-staged!
                                         instances/refresh-instance-state!
                                         branches/refresh-branch-changes!]]]]})))

(db/reg :sutra.sync/unstaged
        (fn [_ [_ reply]]
          {:fx [[:invoke [reread! reply [refresh-snapshot! staging/refresh-staged!
                                         instances/refresh-instance-state!]]]]}))

(defn- unstage!
  [c]
  (send! {:ask c :handover? true :then [(fn [_ reply] [:sutra.sync/unstaged reply])]}))

(defn discard-staged!
  [seq]
  (unstage! [api/delete! :staged-by-seq {:seq seq}]))

(defn clear-staged!
  []
  (unstage! [api/delete! :staging]))

(defn run-import! []
  (shell/with-loading
    (send! {:ask [api/post! :import-run]
            :fx  [[:invoke [data/refresh-import-status!]]
                  [:dispatch [:sutra.data/refresh-runs]]
                  [:invoke [instances/refresh-instance-state!]]]})))

(defn- drop-view-of-other-branch! []
  (when-not (lb/keeps-view? (:mode @canvas/current-view))
    (canvas/close-view!)))

(defn- reread-canvas! []
  (when (lb/keeps-view? (shell/canvas-mode))
    (when @canvas/current-view (lineage-status/refresh-lineage-status!))
    (kompass/refresh-kompass-nodes!)))

(def ^:private branch-reloads
  [#(refresh-snapshot!) #(branches/refresh-branches!) #(catalog/refresh-views!)
   #(staging/refresh-staged!)
   drop-view-of-other-branch! #(instances/refresh-instance-state!) reread-canvas!
   panel/reload-branch-panels!])

(defn- reload-branch-data! []
  (doseq [reload! branch-reloads] (reload!))
  (when-let [reload! (:on-branch-change (feature/app (shell/current-app)))]
    (reload!)))

(defonce ^:private scoped-branch (atom nil))

(defn checkout-branch!
  [name]
  (api/set-page-branch! name)
  (reset! scoped-branch (api/effective-page-branch))
  (-> (js/Promise.resolve name)
      (.then (fn [_] (reload-branch-data!)))
      (.catch (partial shell/fail! "checkout"))))

(add-watch shell/ui-state ::branch-scope
           (fn [_ _ old new]
             (when (not= (:nav old) (:nav new))
               (let [b (api/effective-page-branch)]
                 (when (not= b @scoped-branch)
                   (reset! scoped-branch b)
                   (reload-branch-data!))))))
