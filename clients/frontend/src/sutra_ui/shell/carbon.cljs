(ns sutra-ui.shell.carbon
  (:require [clojure.string :as str]
            [sutra-ui.ui.core :as ui]
            [sutra-ui.shell.notifications :as notifications]
            [sutra-ui.shell.placement :as placement]
            [sutra-ui.shell.user-menu :as user-menu]
            [sutra-ui.state.auth :as auth]
            [sutra.i18n.core :as i18n]))

(defn frame
  [{:keys [variant class]} & children]
  (into [:div {:class (str "shell shell--" (name variant) (when class (str " " class)))}]
        children))

(defn menu-bar
  [{:keys [variant class]} & groups]
  (into [ui/navbar {:class (str "shell-bar shell-bar--" (name variant)
                                (when class (str " " class)))}]
        groups))

(defn home
  [{:keys [on-click]} & content]
  (into [ui/navbar-heading {:class "shell-bar__home" :on-click on-click}] content))

(defn- account
  []
  (let [{:keys [status user username clearances]} @auth/auth]
    (if (#{:authed :guest} status)
      [:<>
       [user-menu/menu (or username user)]
       (when (seq clearances)
         [ui/tooltip {:content (i18n/t :ui/clearances-hint)}
          [ui/tag {:minimal true :intent :warning :icon :key}
           (str/join ", " (map name clearances))]])
       [ui/button {:on-click #(auth/logout!) :minimal true :class "shell-bar__signout"}
        (i18n/t :ui/signout)]]
      [ui/button {:on-click #(auth/show-sign-in!) :minimal true :class "shell-bar__signout"}
       (i18n/t :ui/signin)])))

(defn- tool
  [k {:keys [on-open own]}]
  (let [signed-in? (#{:authed :guest} (:status @auth/auth))]
    (case k
      :notifications (when signed-in? [notifications/bell (when on-open {:on-open on-open})])
      :feedback      (when signed-in? [user-menu/feedback-button])
      :account       [account]
      (get own k))))

(defn region
  ([variant r] (region variant r nil))
  ([variant r opts]
   (when-let [ks (seq (placement/in variant r))]
     (into [:div {:class (str "shell-tools shell-tools--" (name r))}]
           (for [k ks] ^{:key k} [tool k opts])))))

(defn back-row
  [label on-click]
  [:div {:class "shell-back"}
   [:button {:class "shell-back__link" :type "button" :aria-label (i18n/t :ui/back-step)
             :on-click on-click}
    [ui/icon {:icon :chevron-left :class "shell-back__arrow"}] label]
   [region :bare :back-row]])
