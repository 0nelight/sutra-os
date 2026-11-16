(ns sutra-ui.shell.notifications
  (:require [reagent.core :as r]
            [sutra-ui.ui.core :as ui]
            [sutra.i18n.core :as i18n]
            [sutra.types.paths :as paths]
            [sutra-ui.api :as api]
            [sutra-ui.shell.desktop-nav :as desktop-nav]
            [sutra-ui.routing :as routing]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.shell.notifications.settings :as notification-settings]))

(def ^:private shown 30)

(defonce ^:private inbox (db/cursor [:shared.notifications :inbox] {:items [] :unread 0}))
(defonce ^:private open? (db/cursor [:shared.notifications :open?] false))
(defonce ^:private toasted (atom nil))
(defonce ^:private source (atom nil))

(defn- desktop!
  [{:keys [heading message]}]
  (when (and (not (desktop-nav/desktop?))
             (exists? js/Notification)
             (= "granted" (.-permission js/Notification))
             (.-hidden js/document))
    (js/Notification. heading #js {:body (or message "")})))

(declare toast-props)

(defn- dismiss-toast! []
  (when-let [k @toasted]
    (reset! toasted nil)
    (ui/dismiss! (ui/default-toaster) k)))

(defn- toast! [n]
  (dismiss-toast!)
  (let [k (str (:id n))]
    (reset! toasted k)
    (ui/show! (ui/default-toaster) (toast-props n) k))
  (desktop! n))

(defn- ask-desktop-permission! []
  (when (and (exists? js/Notification) (= "default" (.-permission js/Notification)))
    (.requestPermission js/Notification)))

(defn- external? [url] (boolean (re-find #"^[a-zA-Z][a-zA-Z0-9+.-]*:" (str url))))

(defn- navigate!
  [url]
  (if (external? url)
    (.open js/window url "_blank" "noopener")
    (routing/visit! url)))

(machine/reg-fx :shared.notifications/stream
                (fn [_ on?]
                  (cond
                    (and on? (not @source))
                    (let [es (js/EventSource. (paths/address :notifications-stream {})
                                              #js {:withCredentials true})]
                      (reset! source es)
                      (.addEventListener es "open" #(db/send! [:shared.notifications/load]))
                      (.addEventListener
                       es "notification"
                       #(db/send! [:shared.notifications/arrived
                                   (js->clj (js/JSON.parse (.-data %)) :keywordize-keys true)])))
                    (not on?)
                    (do (some-> ^js @source .close)
                        (reset! source nil)))))

(db/reg :shared.notifications/connect
        (fn [_ [_ on?]]
          {:fx [[:shared.notifications/stream on?]
                (when on? [:dispatch [:shared.notifications/load]])]}))

(db/reg :shared.notifications/load
        (fn [_ _]
          {:fx [[:call {:call [api/get! :notifications {:limit shown}]
                        :ok [:shared.notifications/loaded] :err [:sutra/ignored]}]]}))

(db/reg :shared.notifications/loaded
        (fn [s [_ r]]
          (when (map? r)
            {:db (assoc-in s [:shared.notifications :inbox]
                           {:items (vec (:notifications r)) :unread (or (:unread r) 0)})})))

(db/reg :shared.notifications/arrived
        (fn [s [_ n]]
          {:db (update-in s [:shared.notifications :inbox]
                          (fn [{:keys [items unread]}]
                            {:items (vec (take shown (cons n items))) :unread (inc unread)}))
           :fx [[:invoke [toast! n]]]}))

(db/reg :shared.notifications/read
        (fn [_ [_ ids]]
          {:fx [[:call {:call [api/post! :notifications-read
                               (if (= :all ids) {:all true} {:ids ids})]
                        :ok [:shared.notifications/marked ids] :err [:sutra/ignored]}]]}))

(db/reg :shared.notifications/marked
        (fn [s [_ ids r]]
          {:db (update-in s [:shared.notifications :inbox]
                          (fn [{:keys [items]}]
                            {:items (mapv #(cond-> % (or (= :all ids) (some #{(:id %)} ids))
                                             (assoc :read? true))
                                          items)
                             :unread (or (:unread r) 0)}))}))

(db/reg :shared.notifications/open
        (fn [s [_ o url]]
          {:db (assoc-in s [:shared.notifications :open?] o)
           :fx (cond o   [[:invoke [dismiss-toast!]] [:invoke [ask-desktop-permission!]]]
                     url [[:invoke [navigate! url]]])}))

(defn- read! [ids] (db/send! [:shared.notifications/read ids]))

(defn- open! [o & [url]] (db/send! [:shared.notifications/open o url]))

(defn- when-str [s]
  (when s (.toLocaleString (js/Date. s))))

(def ^:private source-key
  {"action" :ui/notification-source-action "automation" :ui/notification-source-automation
   "review" :ui/notification-source-review "branch" :ui/notification-source-branch
   "mention" :ui/notification-source-mention "health" :ui/notification-source-health})

(defn- item [{:keys [id heading message created-at read? object-type object-id source
                     link-url link-label automation-id] :as n} on-open]
  [:li {:class (str "notif__item" (when-not read? " notif__item--unread"))
        :on-click (fn []
                    (when-not read? (read! [id]))
                    (cond
                      (seq link-url) (open! false link-url)
                      (and on-open object-type object-id)
                      (do (open! false)
                          (on-open n))))}
   [:div {:class "notif__heading"} heading]
   (when (seq message) [:div {:class "notif__message"} message])
   (when (seq link-url)
     [ui/button {:class      "notif__link"
                 :minimal    true
                 :small      true
                 :intent     :primary
                 :right-icon :share
                 :on-click   (fn [e]
                               (.stopPropagation e)
                               (when-not read? (read! [id]))
                               (open! false link-url))}
      (or (not-empty link-label) (i18n/t :ui/notification-open))])
   [:div {:class "notif__time"}
    (when-let [k (source-key source)] [ui/tag {:minimal true} (i18n/t k)])
    (when-str created-at)
    (when (seq automation-id)
      [ui/tooltip {:content (i18n/t :ui/notification-mute-hint)}
       [ui/button {:class    "notif__mute"
                   :minimal  true
                   :small    true
                   :on-click (fn [e]
                               (.stopPropagation e)
                               (notification-settings/mute! automation-id))}
        (i18n/t :ui/notification-mute)]])]])

(defn- toast-props
  [{:keys [heading message link-url link-label]}]
  {:icon    :notifications
   :timeout 6000
   :message [:div {:class "notif__toast-body"
                   :on-click #(open! true)}
             [:strong heading]
             (when (seq message) [:div message])]
   :action  (when (seq link-url)
              {:text     (or (not-empty link-label) (i18n/t :ui/notification-open))
               :on-click #(open! false link-url)})
   :on-dismiss (fn [_] (reset! toasted nil))})

(defn bell
  ([] (bell nil))
  ([{:keys [on-open]}]
   (r/with-let [_ (db/send! [:shared.notifications/connect true])]
     (let [{:keys [items unread]} @inbox]
       [:div {:class "notif"}
        [ui/popover
         {:is-open            @open?
          :on-interaction     (fn [o _] (open! o))
          :placement          :bottom-end
          :popover-class-name "notif__panel"
          :content
          [:<>
           [:div {:class "notif__head"}
            [:span (i18n/t :ui/notifications)]
            (when (pos? unread)
              [ui/button {:class "notif__read-all" :minimal true :small true
                          :intent :primary :on-click #(read! :all)}
               (i18n/t :ui/notifications-read-all)])]
           (if (seq items)
             [:ul {:class "notif__list"}
              (for [n items] ^{:key (:id n)} [item n on-open])]
             [ui/empty-state {:icon :notifications
                              :small? true
                              :title (i18n/t :ui/notifications-empty)}])]}
         [ui/tooltip {:content (i18n/t :ui/notifications)}
          [ui/button {:class      "notif__bell"
                      :minimal    true
                      :icon       :notifications
                      :aria-label (i18n/t :ui/notifications)}
           (when (pos? unread)
             [ui/tag {:class "notif__badge" :intent :danger :round true}
              (if (> unread 99) "99+" (str unread))])]]]])
     (finally (db/send! [:shared.notifications/connect false])))))
