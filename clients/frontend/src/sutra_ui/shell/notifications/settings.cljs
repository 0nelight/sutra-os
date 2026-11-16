(ns sutra-ui.shell.notifications.settings
  (:require [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.ui.core :as ui]
            [sutra.i18n.core :as i18n]
            [sutra-ui.api :as api]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.requests :as rq]
            [sutra.kit.remote :as rd]
            [sutra-ui.ui.busy :as busy]))

(def ^:private path [:shared.notification-settings])

(defonce ^:private ui (db/cursor path {:open? false :view rd/loading}))

(db/reg :shared.notification-settings/open
        (fn [s _]
          {:db (assoc-in s path {:open? true :view rd/loading :automations {}})
           :fx [(db/load (conj path :view) [api/get! :notification-preferences])
                (db/load (conj path :automations) [api/get! :automations]
                         {:pick #(into {} (map (juxt (comp str :id) :name)) (:automations %))})]}))

(db/reg :shared.notification-settings/close
        (fn [s _] {:db (assoc-in s (conj path :open?) false)}))

(db/reg :shared.notification-settings/save
        (fn [s [_ settings]]
          (rq/request (assoc-in s (conj path :error) nil)
                      {:ask  [api/put! :notification-preferences {} settings]
                       :path (conj path :view) :pick rd/of-answer
                       :busy (conj path :busy?) :error (conj path :error)})))

(db/reg :shared.notification-settings/mute
        (fn [_ [_ id]]
          {:fx [[:call {:call [api/get! :notification-preferences]
                        :ok [:shared.notification-settings/muting id] :err [:sutra/ignored]}]]}))

(db/reg :shared.notification-settings/muting
        (fn [_ [_ id {:keys [settings]}]]
          {:fx [[:call {:call [api/put! :notification-preferences {}
                               (update settings :muted #(vec (distinct (conj (vec %) (str id)))))]
                        :err [:sutra/ignored]}]]}))

(defn open! [] (db/send! [:shared.notification-settings/open]))

(defn- close! [] (db/send! [:shared.notification-settings/close]))

(defn- save! [settings] (db/send! [:shared.notification-settings/save settings]))

(defn mute!
  [id]
  (db/send! [:shared.notification-settings/mute id]))

(def ^:private kind-key
  {"default" :ui/notification-kind-default "action" :ui/notification-source-action
   "automation" :ui/notification-source-automation "review" :ui/notification-source-review
   "branch" :ui/notification-source-branch "mention" :ui/notification-source-mention
   "health" :ui/notification-source-health
   "automation-triggered" :ui/notification-kind-automation-triggered
   "automation-recovered" :ui/notification-kind-automation-recovered
   "automation-errors" :ui/notification-kind-automation-errors
   "automation-info" :ui/notification-kind-automation-info})

(def ^:private way-key
  {"platform" :ui/notification-way-platform "email" :ui/notification-way-email
   "both" :ui/notification-way-both "off" :ui/notification-way-off})

(def ^:private way-order ["platform" "email" "both" "off"])

(defn- way-of
  [{:keys [settings defaults]} kind]
  (let [own (:delivery settings) dflt (:delivery defaults)
        k   (keyword kind)]
    (name (or (get own k) (get dflt k) (get own :default) (get dflt :default) "platform"))))

(defn- settings-with [{:keys [settings]} kind way]
  (assoc-in (or settings {}) [:delivery (keyword kind)] way))

(defn- kind-row [view kind busy?]
  (let [current (way-of view kind)]
    [:div {:class "notif-prefs__row"}
     [:span (if-let [k (kind-key kind)] (i18n/t k) kind)]
     [ui/segmented-control
      {:small true :disabled busy? :value current
       :on-value-change #(save! (settings-with view kind %))
       :options (for [w way-order] {:value w :label (i18n/t (way-key w))})}]]))

(defn- muted-list [{:keys [settings]} automations busy?]
  (let [muted (:muted settings)]
    [:div {:class "notif-prefs"}
     [:h5 (i18n/t :ui/notification-muted)]
     (if (empty? muted)
       [:p {:class "u-muted"} (i18n/t :ui/notification-muted-none)]
       (for [id muted]
         ^{:key id}
         [:div {:class "notif-prefs__muted"}
          [:span (or (get automations (str id)) id)]
          [ui/button
           {:disabled busy?
            :on-click #(save! (update settings :muted (fn [m] (vec (remove #{id} m)))))}
           (i18n/t :ui/notification-unmute)]]))]))

(defn host []
  (let [{:keys [open? error busy? automations] rview :view} @ui
        view (rd/value-or rview nil)
        error (or error (rd/reason rview))]
    (when open?
      [overlay/modal {:on-close #(close!)
                      :style {:width "560px"}
                      :title (i18n/t :ui/notification-settings)
                      :foot [ui/button {:auto-focus true :on-click #(close!)}
                             (i18n/t :ui/close)]
                      :foot-end? true}
       [:p {:class "u-muted"} (i18n/t :ui/notification-settings-hint)]
       (when error [:p {:class "notif-prefs__error"} error])
       (if (rd/loading? rview)
         [busy/block (i18n/t :ui/loading)]
         (when view
           [:<>
            [:div {:class "notif-prefs"}
             (for [k (distinct (cons "default" (map name (:kinds view))))]
               ^{:key k} [kind-row view k busy?])]
            [muted-list view automations busy?]]))])))
