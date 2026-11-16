(ns sutra-ui.shell.user-menu
  (:require [sutra.i18n.core :as i18n]
            [sutra-ui.ui.core :as ui]
            [sutra-ui.shell.feedback.recording :as aufzeichnung]
            [sutra-ui.shell.feedback :as feedback]
            [sutra-ui.shell.instance-switcher :as instance-switcher]
            [sutra-ui.shell.notifications.settings :as notification-settings]
            [sutra-ui.shell.sessions :as sessions]
            [sutra-ui.ui.theme :as theme]))

(defn feedback-button []
  [ui/tooltip
   {:content (i18n/t (if (aufzeichnung/aktiv?) :ui/feedback-oeffnen :ui/user-menu-feedback))}
   [ui/button
    {:class         "notif__bell feedback-knopf"
     :minimal       true
     :icon          :chat
     :data-feedback true
     :aria-label    (i18n/t :ui/user-menu-feedback)
     :on-click      feedback/feedback!}
    (when (aufzeichnung/aktiv?) [:span.feedback__punkt-laeuft])]])

(defn menu [user]
  [:<>
   [:div.user-menu {:data-feedback true}
    [ui/popover
     {:placement          :bottom-end
      :popover-class-name "user-menu__panel"
      :content
      [:div {:data-feedback true}
       [ui/menu
        [ui/menu-item {:text     (i18n/t :ui/user-menu-sessions)
                       :on-click #(sessions/open!)}]
        [ui/menu-item {:text     (i18n/t :ui/notification-settings)
                       :on-click #(notification-settings/open!)}]
        [theme/menu-item]
        [instance-switcher/items]]]}
     [ui/button {:class         "shell-bar__account"
                 :minimal       true
                 :icon          :user
                 :right-icon    :caret-down
                 :aria-haspopup "menu"}
      user]]
    [sessions/host]
    [notification-settings/host]]])
