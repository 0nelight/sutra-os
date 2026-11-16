(ns sutra-ui.state.shell
  (:require [reagent.core :as r]
            [sutra-ui.feature :as feature]
            [sutra-ui.api :as api]
            [sutra-ui.state.machine :as machine]))

(declare ui-state send!)

(defn error-sentence
  [e]
  (or (not-empty (str (get-in (ex-data e) [:body :error])))
      (.-message e)
      (str e)))

(defn fail!
  ([e] (fail! nil e))
  ([prefix e]
   (when-not (api/aborted-error? e)
     (let [msg (if (string? e) e (error-sentence e))]
       (send! [::error (if prefix (str prefix ": " msg) msg)])))
   nil))

(defonce ui-state
  (r/atom {:panel            :none
           :connect-modal    nil
           :detail-modal     nil
           :show-fields?     true
           :ctrl-held?       false
           :color-mode       :resource-kind
           :writeback-modal  nil
           :loading?         false
           :error            nil
           :decisions        []
           :compiled-info    nil
           :function-source  ""
           :preset-loaded?   false
           :bottom-panel     nil
           :selected-project nil}))

(defonce shell (machine/frame ui-state {:name :shell}))

(defn send! [event] (machine/dispatch event {:frame shell}))

(defn reg
  [id f]
  (machine/reg-event id (fn [{:keys [db]} [_ & args]] {:db (apply f db args)})))

(reg ::error (fn [db msg] (assoc db :error msg)))

(reg ::edit (fn [db f & args] (apply f db args)))

(defn edit!
  [f & args]
  (send! (into [::edit f] args)))

(reg ::enter (fn [db nav params] (feature/arrive db nav params)))

(reg ::loading (fn [db on? error] (assoc db :loading? on? :error error)))

(defn current-nav
  []
  (feature/current-nav @ui-state))

(defn current-app
  []
  (feature/app-of (current-nav)))

(defn canvas-mode
  []
  (feature/nav->mode (current-nav)))

(defn slice
  [app-id]
  (r/cursor ui-state (feature/slice-path app-id)))

(defn enter!
  ([nav] (enter! nav {}))
  ([nav params] (send! [::enter nav params])))

(defn go!
  ([nav] (go! nav {}))
  ([nav params]
   (enter! nav params)
   (when-let [f (:on-enter (feature/screen nav))] (f params))))

(defn instance-graph-live?
  []
  (or (= :instances (canvas-mode)) (= :object-workspace (current-nav))))

(reg ::show-panel (fn [db k] (assoc db :panel k)))
(defn show-panel!
  [k]
  (send! [::show-panel k]))

(reg ::open-staging (fn [db] (assoc db :staging-modal? true)))
(defn open-staging!
  []
  (send! [::open-staging]))

(defn with-loading [p]
  (send! [::loading true nil])
  (-> p
      (.then  (fn [r] (send! [::loading false nil]) r))
      (.catch (fn [e]
                (send! [::loading false (str (.-message e))])
                (throw e)))))

(defn open-in-new-tab!
  [target]
  (when-let [w (.open js/window "" "_blank")]
    (set! (.-opener w) nil)
    (-> (js/Promise.resolve target)
        (.then (fn [href] (if href (set! (.. w -location -href) href) (.close w))))
        (.catch (fn [e]
                  (.close w)
                  (fail! (str "Konnte nicht öffnen: " (or (.-message e) e))))))))

(defn open-object!
  ([type-kw id] (open-object! type-kw id nil))
  ([type-kw id opts] (feature/command! :open-object type-kw id opts)))

(reg ::adopt-route (fn [db f route] (f db route)))
(defn adopt-route!
  [f route]
  (send! [::adopt-route f route]))

(reg ::warn (fn [db msg] (assoc db :warning msg)))
(defn warn!
  [msg]
  (send! [::warn msg]))

(reg ::dismiss-status (fn [db kind msg] (if (= msg (get db kind)) (assoc db kind nil) db)))
(defn dismiss-status!
  [kind msg]
  (send! [::dismiss-status kind msg]))

(reg ::set-ctrl-held (fn [db held?] (assoc db :ctrl-held? held?)))
(defn set-ctrl-held! [held?] (send! [::set-ctrl-held held?]))

(reg ::open-connect-modal (fn [db] (assoc db :connect-modal true)))
(defn open-connect-modal! [] (send! [::open-connect-modal]))
(reg ::close-connect-modal (fn [db] (assoc db :connect-modal nil)))
(defn close-connect-modal! [] (send! [::close-connect-modal]))

(reg ::open-edit-node-modal (fn [db target] (assoc db :edit-node-modal target)))
(defn open-edit-node-modal! [target] (send! [::open-edit-node-modal target]))
(reg ::close-edit-node-modal (fn [db] (assoc db :edit-node-modal nil)))
(defn close-edit-node-modal! [] (send! [::close-edit-node-modal]))

(reg ::open-writeback-modal (fn [db target] (assoc db :writeback-modal target)))
(defn open-writeback-modal! [target] (send! [::open-writeback-modal target]))
(reg ::close-writeback-modal (fn [db] (assoc db :writeback-modal nil)))
(defn close-writeback-modal! [] (send! [::close-writeback-modal]))

(reg ::open-detail-modal (fn [db kind id] (assoc db :detail-modal {:kind kind :id id})))
(defn open-detail-modal! [kind id] (send! [::open-detail-modal kind id]))
(reg ::close-detail-modal (fn [db] (assoc db :detail-modal nil)))
(defn close-detail-modal! [] (send! [::close-detail-modal]))

(reg ::close-staging (fn [db] (assoc db :staging-modal? false)))
(defn close-staging! [] (send! [::close-staging]))

(reg ::set-lens-drawer (fn [db open?] (assoc db :lens-drawer? open?)))
(defn set-lens-drawer! [open?] (send! [::set-lens-drawer open?]))
(reg ::set-color-popover (fn [db open?] (assoc db :color-popover? open?)))
(defn set-color-popover! [open?] (send! [::set-color-popover open?]))
(reg ::set-mobile-nav (fn [db open?] (assoc db :mobile-nav? open?)))
(defn set-mobile-nav! [open?] (send! [::set-mobile-nav open?]))
(reg ::set-branch-panel (fn [db open?] (assoc db :branch-panel? open?)))
(defn set-branch-panel! [open?] (send! [::set-branch-panel open?]))
(reg ::toggle-branch-panel (fn [db] (update db :branch-panel? not)))
(defn toggle-branch-panel! [] (send! [::toggle-branch-panel]))

(reg ::open-cmdk (fn [db] (assoc db :cmdk-open? true :cmdk-query "")))
(defn open-cmdk! [] (send! [::open-cmdk]))
(reg ::close-cmdk (fn [db] (assoc db :cmdk-open? false :cmdk-query "" :cmdk-objects [])))
(defn close-cmdk! [] (send! [::close-cmdk]))
(reg ::toggle-cmdk (fn [db] (-> db (update :cmdk-open? not) (assoc :cmdk-query ""))))
(defn toggle-cmdk! [] (send! [::toggle-cmdk]))
(reg ::set-cmdk-query (fn [db q] (assoc db :cmdk-query q)))
(defn set-cmdk-query! [q] (send! [::set-cmdk-query q]))
(reg ::set-cmdk-objects (fn [db hits] (assoc db :cmdk-objects hits)))
(defn set-cmdk-objects! [hits] (send! [::set-cmdk-objects hits]))

(reg ::set-object-types-filter-group (fn [db group] (assoc db :object-types-filter-group group)))
(defn set-object-types-filter-group! [group] (send! [::set-object-types-filter-group group]))
(reg ::toggle-object-types-group-by (fn [db] (update db :object-types-group-by? not)))
(defn toggle-object-types-group-by! [] (send! [::toggle-object-types-group-by]))
