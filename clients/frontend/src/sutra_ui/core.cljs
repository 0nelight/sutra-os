(ns ^{:clj-kondo/config
      '{:linters {:unresolved-namespace {:exclude [sutra-ui.shell.builder
                                                   sutra-ui.apps.object-view.feature
                                                   sutra-ui.apps.assistant.core]}}}}
 sutra-ui.core
  (:require [sutra-ui.ui.busy :as busy]
            [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.storage :as storage]
            [sutra-ui.ui.core :as ui]
            [sutra-ui.shell.unavailable :refer [unavailable]]
            [clojure.string :as str]
            [sutra-ui.shell.keyboard-viewport :as tastatur-viewport]
            [shadow.lazy :as lazy]
            [sutra-ui.apps.ui-kit.feature :as ui-kit]
            [reagent.core :as r]
            [sutra-ui.state.portal :as pstate]
            [sutra-ui.state.auth :as auth]
            [sutra-ui.state.branches :as branches]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.instances :as instances]
            [sutra-ui.state.object-view :as object-view]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.state.staging :as staging]
            [sutra-ui.state.sync :as sync]
            [sutra-ui.feature :as feature]
            [sutra-ui.shell.home-page :as home-page]
            [sutra-ui.routing :as routing]
            [sutra-ui.shell.boundary :as boundary]
            [sutra-ui.shell.view-notes :as view-notes]
            [sutra-ui.shell.feedback.recording :as aufzeichnung]
            [sutra-ui.shell.feedback :as feedback]
            [sutra-ui.shell.consumer :as consumer]
            [sutra-ui.shell.carbon :as carbon]
            [sutra-ui.platform :as platform]
            [sutra-ui.shell.focus-style :as focus-style]
            [sutra-ui.shell.menu-keys :as menu-keys]
            [sutra-ui.ui.hotkeys :as hotkeys]
            [sutra-ui.ui.theme :as theme]
            [sutra-ui.ui.size :as groesse]
            [sutra.kit.remote :as rd]
            [sutra-ui.debug :as dbg]
            [sutra-ui.client-log :as client-log]
            [sutra-ui.render-ctx :as rctx]
            [sutra.i18n.core :as i18n]
            [reagent.dom.client :as rdomc]))

(def ^:private bp-icons
  (lazy/loadable blueprint-cljs.icons.all/names))

(def ^:private ui-kit-gallery
  (lazy/loadable blueprint-cljs.gallery.all/root))

(reset! ui-kit/load-gallery!
        (fn [on-loaded]
          (lazy/load ui-kit-gallery on-loaded
                     #(js/console.error "[ui-kit] module load failed" %))))

(def ^:private builder-shell
  (lazy/loadable sutra-ui.shell.builder/parts))

(defonce ^:private builder-view (r/atom nil))
(defonce ^:private builder-error (r/atom nil))
(defonce ^:private builder-loading (atom nil))

(defn- load-builder!
  []
  (or @builder-loading
      (let [t0 (dbg/now)]
        (reset! builder-loading
                (lazy/load builder-shell
                           (fn [parts]
                             (dbg/measure! :boot/module t0 "builder")
                             (reset! builder-view parts)
                             (routing/reapply!)
                             parts)
                           (fn [err]
                             (js/console.error "[builder-shell] module load failed" err)
                             (reset! builder-error err)))))))

(def ^:private object-screen
  (lazy/loadable sutra-ui.apps.object-view.feature/loaded))

(defn- load-object-screen!
  []
  (let [t0 (dbg/now)]
    (lazy/load object-screen
               (fn [_] (dbg/measure! :boot/module t0 "object")))))

(defn- object-address?
  []
  (and (re-find #"^/object/[^/]+/[^/]+/?$" (.-pathname js/location))
       (str/blank? (.-search js/location))))

(defn- main-view
  [fallback]
  (if-let [v (:view (feature/screen (shell/current-nav)))]
    [v]
    (when fallback [fallback])))

(defn- builder-frame
  [{:keys [class top-bar banner rail fallback overlays taskbar]}]
  (let [nav (shell/current-nav)]
    [carbon/frame {:variant :builder :class (when class (class))}
     (if top-bar [top-bar] [:div {:class "shell-bar shell-bar--builder"}])
     (when banner [banner])
     [:div {:class "body"}
      (when rail [rail])
      [:div {:class "main"}
       ^{:key nav}
       [boundary/error-boundary {:label (str "view:" (name (or nav :?)))}
        [main-view fallback]]
       (when overlays [overlays])]]
     (when taskbar [taskbar])]))

(defn- builder-shell-view
  []
  (r/create-class
   {:display-name "builder-shell-loader"
    :component-did-mount (fn [_] (when-not (object-address?) (load-builder!)))
    :reagent-render
    (fn []
      (cond
        @builder-error
        [:div {:class "boot-error" :role "alert"}
         [:h1 (i18n/t :ui/builder-module-failed)]
         [:p (i18n/t :ui/builder-module-failed-hint)]
         [ui/button {:intent :primary :icon :refresh :on-click #(.reload js/location)}
          (i18n/t :ui/reload)]]

        @builder-view
        [builder-frame @builder-view]

        (:view (feature/screen (shell/current-nav)))
        [builder-frame {}]

        :else
        [busy/block {:class "loading"} (i18n/t :ui/loading)]))}))

(defn- field
  [label v submit & [opts]]
  [:<>
   [:label {:class "explorer-newview__label"} label]
   [ui/input-group (merge {:value @v
                           :on-change #(reset! v (.. % -target -value))
                           :on-key-down #(when (= "Enter" (.-key %)) (submit))
                           :input-class-name "login__input"}
                          opts)]])

(defn- login-screen []
  (let [u (r/atom "") p (r/atom "") err (r/atom nil) busy (r/atom false)
        step (r/atom :credentials) code (r/atom "") new-pw (r/atom "") enrol (r/atom nil)]
    (fn []
      (let [failed  #(reset! err (or (.-message %) (i18n/t :ui/signin-failed)))
            next!   (fn [{:keys [limited]}]
                      (let [ls (set (map keyword limited))]
                        (cond
                          (ls :password) (reset! step :password)
                          (ls :mfa)      (-> (auth/mfa-enroll! @p)
                                             (.then #(do (reset! enrol %)
                                                         (reset! code "")
                                                         (reset! step :enrol))))
                          :else          (auth/finish-login!))))
            run-step! (fn [promise]
                      (reset! busy true) (reset! err nil)
                      (-> promise
                          (.catch (fn [e]
                                    (if (get-in (ex-data e) [:body :mfa-required])
                                      (do (reset! step :code) (reset! code ""))
                                      (failed e))))
                          (.finally (fn [] (reset! busy false)))))
            submit  (fn []
                      (when-not @busy
                        (case @step
                          :credentials
                          (when (seq (str/trim @u))
                            (run-step! (.then (auth/sign-in! (str/trim @u) @p nil) next!)))
                          :code
                          (run-step! (.then (auth/sign-in! (str/trim @u) @p (str/trim @code))
                                            next!))
                          :password
                          (run-step! (.then (auth/change-password! @p @new-pw)
                                       #(do (reset! p @new-pw) (next! %))))
                          :enrol
                          (run-step! (.then (auth/mfa-confirm! (str/trim @code)) next!)))))]
        [:div {:class "login"}
         [ui/card {:class "login__card" :elevation 3}
          [platform/gate-logo]
          [:div {:class "login__title"}
           (i18n/t :ui/signin-title {:platform (platform/title)})]
          (case @step
            :credentials
            [:<>
             [field (i18n/t :ui/username) u submit {:auto-focus true}]
             [field (i18n/t :ui/password) p submit {:type "password"}]]
            :code
            [field (i18n/t :ui/mfa-code) code submit
             {:auto-focus true :input-mode "numeric" :auto-complete "one-time-code"}]
            :password
            [:<>
             [ui/callout {:intent :primary} (i18n/t :ui/initial-password-change)]
             [field (i18n/t :ui/new-password) new-pw submit
              {:type "password" :auto-focus true :auto-complete "new-password"}]]
            :enrol
            [:<>
             [ui/callout {:intent :primary} (i18n/t :ui/mfa-enrol-hint)]
             [:code {:class "login__seed"} (:seed @enrol)]
             [:a {:href (:uri @enrol)} (i18n/t :ui/mfa-open-app)]
             [field (i18n/t :ui/mfa-code) code submit
              {:auto-focus true :input-mode "numeric" :auto-complete "one-time-code"}]])
          (when @err
            [ui/problem @err nil "login__error"])
          [ui/button {:on-click submit :loading @busy :intent :primary :fill true}
           (i18n/t :ui/signin)]
          [ui/button
           {:on-click (fn []
                        (when-not @busy
                          (reset! busy true) (reset! err nil)
                          (-> (auth/sso-login!)
                              (.then (fn [ok] (when-not ok
                                                (reset! err (i18n/t :ui/sso-failed-no-ticket)))))
                              (.catch (fn [_] (reset! err (i18n/t :ui/sso-failed))))
                              (.finally (fn [] (reset! busy false))))))
            :disabled @busy
            :intent   :primary
            :outlined true
            :fill     true
            :style    {:margin-top "var(--sp-3)"}}
           (i18n/t :ui/signin-windows)]
          (when (:allow-anon @auth/auth)
            [:div {:class "login__alt"}
             [:a {:href "#" :on-click (fn [e] (.preventDefault e)
                                        (auth/remember-guest!))
                  :class "login__guest-link"}
              (i18n/t :ui/continue-as-guest)]])]]))))

(defn- address [] (routing/parse-workspace-path (.-pathname js/location)))

(defn- workspace-address? [] (some? (address)))

(defn- unknown-workspace
  []
  (when-let [{:keys [workspace-slug]} (address)]
    (when (and (nil? @auth/current-workspace)
               (not (rd/loading? @pstate/workspaces))
               (not (rd/failed? @pstate/workspaces))
               (nil? (routing/workspace-of-slug workspace-slug (pstate/workspace-list))))
      workspace-slug)))

(defn- entering-workspace? []
  (when-let [{:keys [workspace-slug]} (address)]
    (and (nil? @auth/current-workspace)
         (or (rd/loading? @pstate/workspaces)
             (routing/workspace-of-slug workspace-slug (pstate/workspace-list))))))

(defn- shell []
  (case (:status @auth/auth)
    :unknown [busy/block {:class "loading"} (i18n/t :ui/loading)]
    :anon    [login-screen]
    (cond
      @auth/current-workspace
      [boundary/error-boundary {:label "workspace-shell"} [consumer/workspace-shell]]
      (entering-workspace?)
      [busy/block {:class "loading"} (i18n/t :ui/loading)]
      (unknown-workspace)
      [unavailable (i18n/t :ui/workspace-slug-unknown {:slug (unknown-workspace)})
       [:a {:href "/"} (i18n/t :ui/workspace-home)]]
      @home-page/choosing?
      [boundary/error-boundary {:label "workspace-chooser"} [home-page/chooser]]
      (= "consumer" (:role @auth/auth))
      [boundary/error-boundary {:label "consumer-shell"} [consumer/consumer-shell]]
      :else
      [boundary/error-boundary {:label "app-shell"} [builder-shell-view]])))

(def ^:private assistant-module
  (lazy/loadable {:drawer sutra-ui.apps.assistant.core/drawer
                  :open!  sutra-ui.apps.assistant.core/open!}))

(defonce ^:private assistant-drawer (r/atom nil))

(defn- assistant-entry []
  (if-let [drawer @assistant-drawer]
    [drawer]
    (when (and (= :authed (:status @auth/auth))
               (not= :assistant (:nav @shell/ui-state)))
      [ui/tooltip {:content (i18n/t :ui/assistant-open)}
       [ui/button {:class    "assistant-fab"
                   :intent   :primary
                   :on-click (fn [_]
                               (lazy/load assistant-module
                                          (fn [{:keys [drawer open!]}]
                                            (open!)
                                            (reset! assistant-drawer drawer))))}
        [:span.fab__icon "✦"] [:span.fab__label (i18n/t :ui/assistant-open)]]])))

(defn app []
  [:<> [shell] [overlay/host] [assistant-entry] [view-notes/fab] [hotkeys/root]])

(defn- mount-root []
  (rdomc/render (rdomc/create-root (.getElementById js/document "app")) [app]))

(defn- load-builder-data!
  []
  (-> (sync/refresh-snapshot!)
      (.then (fn [_] (instances/refresh-instance-state!)))
      (.then (fn [_] (routing/resolve-deep-links!))))
  (catalog/refresh-views!)
  (staging/refresh-staged!))

(defn- load-builder-data-outside-workspace!
  []
  (if-not (workspace-address?)
    (load-builder-data!)
    (add-watch auth/current-workspace ::builder-data
               (fn [k ref _ ws]
                 (when-not ws
                   (remove-watch ref k)
                   (load-builder-data!))))))

(def ^:private BUILDER-AT-LATEST-MS 3000)

(defn- once-idle! [f]
  (if (exists? js/requestIdleCallback)
    (js/requestIdleCallback f #js {:timeout 1000})
    (js/setTimeout f 0)))

(defn- after-first-object-page!
  [f]
  (let [done (atom false)
        fire (fn []
               (when (compare-and-set! done false true)
                 (remove-watch object-view/object-view-data ::first-page)
                 (js/requestAnimationFrame
                  (fn [_] (js/requestAnimationFrame (fn [_] (once-idle! f)))))))]
    (add-watch object-view/object-view-data ::first-page
               (fn [_ _ _ pages]
                 (when (some #(false? (:loading? %)) (vals pages)) (fire))))
    (js/setTimeout fire BUILDER-AT-LATEST-MS)))

(defn- load-builder-and-its-data!
  []
  (load-builder!)
  (load-builder-data-outside-workspace!))

(defn- load-app-data!
  []
  (let [object? (object-address?)
        first-module (cond
                       (workspace-address?) (do (pstate/ensure-workspaces!) nil)
                       object? (-> (js/Promise.resolve (load-object-screen!))
                                   (.catch (fn [_] (load-builder!))))
                       :else (-> (js/Promise.resolve (load-builder!))
                                 (.catch (fn [_] nil))))]
    (-> (js/Promise.all #js [(branches/refresh-branches!) first-module])
        (.then (fn [_] (routing/install! :builder)))
        (.then (fn [_]
                 (if object?
                   (after-first-object-page! load-builder-and-its-data!)
                   (load-builder-data-outside-workspace!)))))))

(defn- consumer-bootstrap!
  []
  (-> (pstate/refresh-consumer-apps!)
      (.then (fn [_] (routing/install! :consumer)))))

(defn- install-locale!
  []
  (let [pick (some-> (js/URLSearchParams. (.-search js/location)) (.get "lang"))
        kept (storage/get-item "sutra.locale")
        tag  (or pick kept)]
    (when pick
      (storage/set-item! "sutra.locale" pick))
    (rctx/install! tag)
    (i18n/set-locale! (i18n/from-tag tag))))

(defn- mount-when-ready! []
  (if-not (:open-app-slug (address))
    (mount-root)
    (-> (js/Promise.
         (fn [ready]
           (add-watch pstate/current-consumer-app ::mount
                      (fn [_ _ _ a] (when (and a (not (:loading? a))) (ready))))
           (add-watch auth/auth ::mount
                      (fn [_ _ _ au] (when (= :anon (:status au)) (ready))))
           (js/setTimeout ready 3000)))
        (.then (fn [_]
                 (remove-watch pstate/current-consumer-app ::mount)
                 (remove-watch auth/auth ::mount)
                 (mount-root))))))

(defn- init-arbeitsfenster! []
  (dbg/install-boot-trace!)
  (platform/refresh!)
  (dbg/install!)
  (client-log/install!)
  (feedback/install!)
  (aufzeichnung/install!)
  (dbg/install-click-trace!)
  (dbg/install-visibility-trace!)
  (dbg/install-state-trace! {"ui-state" shell/ui-state
                             "auth"     auth/auth})
  (home-page/install!)
  (auth/install-bootstraps! (home-page/landing-first load-app-data!)
                            (home-page/landing-first consumer-bootstrap!))
  (-> (auth/check-auth!)
      (.then (fn [_]
               (if (= :anon (:status @auth/auth))
                 (-> (auth/try-sso!)
                     (.then (fn [ok?] (when ok? (auth/check-auth!)))))
                 js/undefined)))
      (.then (fn [_]
               (dbg/session-header!
                {:user     (:user @auth/auth)
                 :branch   (some-> (:branch @sync/snapshot) name)})
               (when (#{:authed :guest} (:status @auth/auth))
                 (if (= "consumer" (:role @auth/auth))
                   (@auth/consumer-bootstrap-fn)
                   (@auth/bootstrap-fn))))))
  (mount-when-ready!)
  (js/setTimeout #(lazy/load bp-icons) 1500))

(defn ^:export init []
  (install-locale!)
  (focus-style/on!)
  (menu-keys/install!)
  (groesse/install!)
  (theme/install!)
  (tastatur-viewport/installieren!)
  (init-arbeitsfenster!))
