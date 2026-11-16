(ns sutra-ui.shell.sessions
  (:require [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.ui.busy :as busy]
            [sutra-ui.state.loading :as loading]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra-ui.api :as api]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.requests :as rq]
            [sutra.kit.remote :as rd]
            [sutra-ui.ui.core :as ui]))

(defonce ^:private ui (db/cursor [:shared.sessions] {:open? false :sessions rd/loading}))

(def ^:private load
  (db/load [:shared.sessions :sessions] [api/get! :sessions]
           {:pick (comp rd/of-answer :sessions)}))

(defn- end-failure
  [{:keys [status body sentence]}]
  (let [incident (get-in body [:data :incident])]
    (cond
      (= 503 status) (str (i18n/t :ui/sessions-end-timeout)
                          (when incident (str " (" incident ")")))
      (= 401 status) (i18n/t :ui/sessions-end-unauthenticated)
      :else (i18n/t :ui/sessions-end-failed {:msg sentence}))))

(db/reg :shared.sessions/open
        (fn [s _]
          {:db (update s :shared.sessions assoc :open? true :sessions rd/loading :error nil)
           :fx [load]}))

(db/reg :shared.sessions/close (fn [s _] {:db (assoc-in s [:shared.sessions :open?] false)}))

(db/reg :shared.sessions/end
        (fn [s [_ body busy]]
          (rq/request (assoc-in s [:shared.sessions :error] nil)
                      {:ask  [loading/or-failure api/post! :sessions-end body]
                       :busy [[:shared.sessions :busy] busy]
                       :then [[:shared.sessions/ended]]})))

(db/reg :shared.sessions/ended
        (fn [s [_ answer]]
          {:db (assoc-in s [:shared.sessions :error] (some-> (loading/failure answer) end-failure))
           :fx [load]}))

(defn open! [] (db/send! [:shared.sessions/open]))

(defn- close! [] (db/send! [:shared.sessions/close]))

(defn- end! [body busy] (db/send! [:shared.sessions/end body busy]))

(defn- client-name
  [{:keys [user-agent kind]}]
  (let [ua (str user-agent)
        browser (some (fn [[re n]] (when (re-find re ua) n))
                      [[#"Edg/" "Edge"] [#"OPR/|Opera" "Opera"] [#"Firefox/" "Firefox"]
                       [#"Chrome/" "Chrome"] [#"Safari/" "Safari"] [#"curl/" "curl"]
                       [#"babashka|bb/|java" "CLI"] [#"git/" "git"]])
        os (some (fn [[re n]] (when (re-find re ua) n))
                 [[#"iPhone|iPad" "iOS"] [#"Android" "Android"] [#"Windows" "Windows"]
                  [#"Mac OS X|Macintosh" "macOS"] [#"Linux" "Linux"]])]
    (case kind
      :clone (i18n/t :ui/sessions-kind-clone)
      :app   (i18n/t :ui/sessions-kind-app)
      (cond
        (and browser os) (str browser " · " os)
        (or browser os)  (or browser os)
        (str/blank? ua)  (i18n/t :ui/sessions-unknown-client)
        :else            (subs ua 0 (min 60 (count ua)))))))

(defn- when-str [d]
  (when d (.toLocaleString (js/Date. d) js/undefined
                           #js {:dateStyle "medium" :timeStyle "short"})))

(defn- row [{:keys [id current? ip last-seen-at created-at provider] :as s} busy]
  [:li {:class "sutra-sessions__row"
        :style {:display "flex" :gap "var(--sp-4)" :align-items "center"
                :padding "var(--sp-3) 0" :border-bottom "1px solid var(--c-border-soft)"}}
   [:div {:style {:flex 1 :min-width 0}}
    [:div {:style {:font-weight 600}}
     (client-name s)
     (when current?
       [:span {:style {:margin-left "var(--sp-3)" :font-weight 400 :color "var(--c-primary)"}}
        (i18n/t :ui/sessions-this-client)])]
    [:div {:style {:font-size "var(--fs-sm)" :color "var(--c-muted)"}}
     (str/join " · " (remove str/blank?
                             [(when ip (str "IP " ip))
                              (str (i18n/t :ui/sessions-last-seen) " " (when-str last-seen-at))
                              (str (i18n/t :ui/sessions-since) " " (when-str created-at))
                              provider]))]]
   (when-not current?
     [ui/button {:disabled (some? busy) :on-click #(end! {:id id} id)}
      (if (= busy id)
        [busy/inline {:size 12} nil]
        (i18n/t :ui/sessions-end))])])

(defn host []
  (let [{:keys [open? error busy] rsessions :sessions} @ui
        sessions (rd/value-or rsessions nil)
        error (or error (rd/reason rsessions))
        others (remove :current? sessions)]
    (when open?
      [overlay/modal
       {:on-close  #(close!)
        :size      560
        :title     (i18n/t :ui/sessions-title)
        :foot-end? true
        :foot      [:<>
                    [ui/button
                     {:intent :danger :disabled (or (some? busy) (empty? others))
                      :on-click #(end! {:others true} :others)}
                     (if (= busy :others)
                       [busy/inline {:size 12} nil]
                       (i18n/t :ui/sessions-end-others))]
                    [ui/button {:auto-focus true :on-click #(close!)} (i18n/t :ui/close)]]}
       [:p {:class "u-muted"} (i18n/t :ui/sessions-hint)]
       (when error [ui/problem error nil])
       (if (rd/loading? rsessions)
         [busy/block (i18n/t :ui/loading)]
         [:ul {:style {:list-style "none" :padding 0 :margin 0
                       :max-height "50vh" :overflow-y "auto"}}
          (for [s sessions] ^{:key (:id s)} [row s busy])])])))
