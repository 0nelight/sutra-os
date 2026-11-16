(ns sutra-ui.apphost.widgets.ai
  (:require [sutra-ui.ui.sel :as sel]
            [clojure.string :as str]
            [sutra-ui.ui.busy :as busy]
            [sutra-ui.ui.core :as ui :refer [text-area]]
            [reagent.core :as r]
            [sutra.capps.ai-chat :as chat]
            [sutra.capps.variables :refer [deref*]]
            [sutra.i18n.core :as i18n]
            [sutra-ui.api :as api]
            [sutra-ui.apphost.client :as client]
            [sutra-ui.apphost.frame :as frame]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.requests :as rq]))

(def ^:private poll-ms 1500)

(defn- zone []
  (try (.. js/Intl DateTimeFormat resolvedOptions -timeZone) (catch :default _ nil)))

(defn- state-values
  [state]
  (into {}
        (map (fn [[k {:keys [value description]}]]
               [k {:description description :value (deref* value)}]))
        state))

(defn- reasoning
  [steps]
  (when (seq steps)
    [ui/section
     {:title (i18n/t :ui/capp-ai-reasoning) :collapsible true :compact true
      :collapse-props {:default-is-open false} :class "capp-ai__reasoning"}
     [ui/section-card
      (into [:ol]
            (map-indexed (fn [i {:keys [tool answer summary]}]
                           ^{:key i} [:li (str (or tool answer summary))]))
            steps)]]))

(defn- message-view
  [{:keys [role content status] :as m} show-reasoning?]
  [:div.capp-ai__message {:class (str "is-" role)}
   (when (and show-reasoning? (= "assistant" role)) [reasoning (chat/steps-of m)])
   (if (= "running" status)
     [busy/block {:class "capp-ai__thinking"} (i18n/t :ui/capp-loading)]
     [:div.capp-ai__text (str content)])])

(defn- proposal-card
  [{:keys [id action]} decide!]
  [:div.capp-ai__proposal
   [:span (str action)]
   [ui/button {:intent :success :on-click #(decide! id :accept)}
    (i18n/t :ui/capp-ai-accept)]
   [ui/button {:intent :danger :on-click #(decide! id :reject)}
    (i18n/t :ui/capp-ai-reject)]])

(defn- live
  [db id effects]
  (when (contains? (::agent db) id) (effects (get-in db [::agent id]))))

(defn- asking
  [id ask then & {:as more}]
  (merge {:ask ask :then [then] :error [::agent id :error] :within [::agent id]} more))

(machine/reg-event ::fetch
                   (fn [{:keys [db]} [_ id answer]]
                     (live db id
                           (fn [{:keys [thread-id]}]
                             {:fx [(rq/ask (asking id [api/get! :agent-thread {:id thread-id}]
                                                   [::fetched id answer]))]}))))

(machine/reg-event ::fetched
                   (fn [{:keys [db]} [_ id answer data]]
                     (live db id
                           (fn [_]
                             {:db (assoc-in db [::agent id :data] data)
                              :fx [(when answer [:invoke [sel/wr! answer (chat/last-answer data)]])
                                   (when (chat/running? data)
                                     [:after {:key [::poll id] :ms poll-ms
                                              :event [::fetch id answer]}])]}))))

(machine/reg-event ::ask
                   (fn [{:keys [db]} [_ id body answer]]
                     (live db id
                           (fn [{:keys [thread-id]}]
                             (rq/request (assoc-in db [::agent id :error] nil)
                                         (asking id (if thread-id
                                                      [api/post! :agent-thread-ask
                                                       {:id thread-id} (dissoc body :agent)]
                                                      [api/post! :agent-threads body])
                                                 [::asked id answer]
                                                 :busy [::agent id :sending?]))))))

(machine/reg-event ::asked
                   (fn [{:keys [db]} [_ id answer resp]]
                     (live db id
                           (fn [_]
                             {:db (assoc-in db [::agent id :thread-id] (get-in resp [:thread :id]))
                              :fx [[:dispatch [::fetch id answer]]]}))))

(machine/reg-event ::decide
                   (fn [{:keys [db]} [_ id pid verdict answer]]
                     (live db id
                           (fn [{:keys [thread-id]}]
                             {:fx [(rq/ask (asking id [api/post! (if (= :accept verdict)
                                                                    :agent-thread-proposal-accept
                                                                    :agent-thread-proposal-reject)
                                                        {:id thread-id :proposal pid} {}]
                                                   [::decided id answer]))]}))))

(machine/reg-event ::decided
                   (fn [{:keys [db]} [_ id answer]]
                     (live db id (fn [_] {:fx [[:dispatch [::fetch id answer]]
                                               [:invoke [client/refresh-page!]]]}))))

(machine/reg-event ::left
                   (fn [{:keys [db]} [_ id]]
                     {:db (update db ::agent dissoc id) :fx [[:cancel [::poll id]]]}))

(defn ai-agent
  [cfg env]
  (r/with-let [f     (frame/frame-for env)
               id    (random-uuid)
               _     (frame/send! f [:capp/put [::agent id] {}])
               draft (r/atom "")
               typed (atom nil)]
    (let [{:keys [textbox title placeholder show-reasoning? class state answer]} cfg
          {:keys [data sending? error]} @(machine/subscribe [:capp/at [::agent id]] {:frame f})
          send!     (fn [text]
                      (reset! draft "")
                      (frame/send! f [::ask id
                                      (chat/question cfg (chat/input-with-state
                                                          (str/trim text) (state-values state))
                                                     (zone))
                                      answer]))
          box       (deref* textbox)
          can-send? (and (not (str/blank? @draft)) (not sending?))
          submit!   (fn []
                      (when can-send?
                        (reset! typed nil)
                        (when textbox (sel/wr! textbox nil))
                        (send! @draft)))]
      (when (and textbox (chat/auto-send? box @typed) (not sending?))
        (reset! typed box)
        (send! box))
      [:div.capp-ai (when class {:class class})
       (when title [:div.capp-table__title title])
       (into [:div.capp-ai__messages]
             (map-indexed (fn [i m]
                            ^{:key (or (:id m) i)} [message-view m show-reasoning?]))
             (:messages data))
       (into [:<>]
             (map (fn [p]
                    ^{:key (:id p)}
                    [proposal-card p #(frame/send! f [::decide id %1 %2 answer])]))
             (chat/open-proposals data))
       (when error [ui/problem error nil])
       [:div.capp-ai__composer
        [text-area
         {:value       @draft
          :placeholder (or placeholder (i18n/t :ui/capp-ai-placeholder))
          :on-change   (fn [e]
                         (let [v (.. e -target -value)]
                           (reset! draft v)
                           (when textbox
                             (reset! typed v)
                             (sel/wr! textbox v))))
          :on-key-down (fn [^js e]
                         (when (and (= "Enter" (.-key e)) (not (.-shiftKey e)))
                           (.preventDefault e)
                           (submit!)))}]
        [ui/button {:intent :primary :disabled (not can-send?) :on-click submit!}
         (i18n/t :ui/capp-ai-send)]]])
    (finally (frame/send! f [::left id]))))

(defn- out!
  [output v]
  (when (satisfies? IReset output) (sel/wr! output v)))

(defn- generated-text
  [{:keys [text]}]
  (cond
    (string? text)     [:div.capp-ai__text text]
    (sequential? text) [:div.capp-ai__text (i18n/t :ui/capp-ai-objects {:n (count text)})]
    (some? text)       [:div.capp-ai__text (str text)]))

(defn- step
  [db id ev]
  (cond-> db (get-in db [::generate id]) (update-in [::generate id] chat/generate-transition ev)))

(machine/reg-event ::step (fn [{:keys [db]} [_ id ev x]] {:db (step db id [ev x])}))

(defn- llm-ask
  [id descriptor {:keys [temperature model] :as cfg} prompt]
  (rq/ask {:ask  [client/generate! descriptor
                  (cond-> {:prompt prompt}
                    model (assoc :model model)
                    (chat/clamped-temperature temperature)
                    (assoc :temperature (chat/clamped-temperature temperature)))]
           :then [[::generated id cfg :text]] :else [[::step id :fail]]}))

(machine/reg-event ::generate
                   (fn [{:keys [db]} [_ id descriptor cfg mode prompt]]
                     {:db (step db id [:start])
                      :fx [(case mode
                             :logic
                             (rq/ask {:ask  [client/function! descriptor (:logic cfg)]
                                      :then [[::generated id cfg :rows]]
                                      :else [[::step id :fail]]})
                             :prompt-function
                             (rq/ask {:ask  [client/function! descriptor (:prompt-function cfg)]
                                      :then [[::prompted id descriptor cfg]]
                                      :else [[::step id :fail]]})
                             (llm-ask id descriptor cfg prompt))]}))

(machine/reg-event ::prompted
                   (fn [_ [_ id descriptor cfg res]]
                     {:fx [(llm-ask id descriptor cfg
                                    (chat/function-output (:rows res) (dissoc cfg :as-rows?)))]}))

(machine/reg-event ::generated
                   (fn [{:keys [db]} [_ id cfg kind res]]
                     (let [v (if (= :rows kind) (chat/function-output (:rows res) cfg) (:text res))]
                       {:db (step db id [:answer v]) :fx [[:invoke [out! (:output cfg) v]]]})))

(defn generated-content
  [{:keys [clear-on-change? title icon button-label loading-message spinner? class] :as cfg} env]
  (r/with-let [f  (frame/frame-for env)
               id (random-uuid)
               _  (frame/send! f [:capp/put [::generate id]
                                  {:status :idle :clear? clear-on-change?}])]
    (let [prompt (deref* (:prompt cfg))
          mode   (chat/generate-mode cfg)
          now    @(machine/subscribe [:capp/at [::generate id]] {:frame f})
          _      (when (not= prompt (:prompt now)) (frame/send! f [::step id :prompt prompt]))
          {:keys [status error]} now]
      [:div.capp-ai.capp-ai--generate (when class {:class class})
       (when icon [:div.capp-ai__icon icon])
       (when title [:div.capp-table__title title])
       (case status
         :loading (if spinner?
                    [busy/block {:class "capp-objectview__loading"} (i18n/t :ui/capp-loading)]
                    [:div.capp-ai__thinking
                     (or loading-message (i18n/t :ui/capp-loading))])
         :failed  [ui/problem error nil]
         :done    [generated-text now]
         nil)
       [ui/button
        {:intent   :primary
         :disabled (or (nil? mode) (= :loading status)
                       (and (= :prompt mode) (str/blank? (str prompt))))
         :on-click #(frame/send! f [::generate id (:descriptor env) cfg mode prompt])}
        (or button-label (i18n/t :ui/capp-ai-generate))]])
    (finally (frame/send! f [:capp/update [::generate] dissoc [id]]))))

(defn- agent-entry [cfg env] [ai-agent cfg env])

(def widgets
  {:ai-agent             agent-entry
   :ai-chatbot           agent-entry
   :ai-interactive       agent-entry
   :ai-generated-content (fn [cfg env] [generated-content cfg env])})
