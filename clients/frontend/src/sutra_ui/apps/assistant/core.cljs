(ns sutra-ui.apps.assistant.core
  (:require [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.ui.busy :as busy]
            [sutra-ui.storage :as storage]
            [sutra-ui.ui.core :as ui :refer [html-table tag]]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra.i18n.core :as i18n]
            [sutra-ui.apps.assistant.events :as ev]
            [sutra-ui.apps.assistant.evals :as evals]
            [sutra-ui.apps.assistant.providers :as providers]
            [sutra-ui.apps.assistant.studio :as studio]
            [sutra-ui.apphost.widgets.markdown :as md]
            [sutra-ui.state.auth :as auth]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.ui.page :as page]
            [sutra-ui.apphost.platform :as platform]))

(defonce ^:private ui
  (r/atom (ev/initial {:agent (storage/get-item ev/agent-key)
                       :model (storage/get-item ev/model-key)})))

(defonce ^:private chat (machine/frame ui {:name :assistant/chat}))

(defn- send! [event] (machine/dispatch event {:frame chat}))

(machine/reg-sub :assistant/db (fn [db _] db))

(defonce ^:private db (machine/subscribe [:assistant/db] {:frame chat}))

(defn load! [] (send! [:assistant/load]))

(defn- new-thread! [] (send! [:assistant.thread/new]))

(defn- ask! []
  (send! [:assistant/ask (try (.. js/Intl DateTimeFormat resolvedOptions -timeZone)
                    (catch :default _ nil))]))

(defn- attach-files! [^js files]
  (doseq [i (range (.-length files))]
    (send! [:assistant/attach (aget files i) (str (random-uuid))])))

(def ^:private cite-re #"\[\[([A-Za-z0-9_\-]+):([^\]\s]+)\]\]")

(defn- builder-shell? []
  (and (not @auth/current-workspace) (not= "consumer" (:role @auth/auth))))

(defn- open-object! [type id]
  (when (builder-shell?)
    (shell/open-object! (keyword type) id)))

(defn- with-cite-links
  [text by-ref]
  (str/replace text cite-re
               (fn [[_ t id]]
                 (let [title (or (:title (get by-ref (str t ":" id)))
                                 (str t " " (subs id 0 (min 8 (count id)))))]
                   (str "[" (str/replace title #"[\[\]]" "") "](#cite:" t ":" id ")")))))

(defn- table-row [l]
  (mapv str/trim (-> l str/trim (str/replace #"^\|" "") (str/replace #"\|$" "") (str/split #"\|"))))

(defn- separator? [l] (re-matches #"^\s*\|?[\s:\-|]+\|?\s*$" l))

(defn- segments
  [text]
  (loop [ls (str/split-lines text) out []]
    (if-let [l (first ls)]
      (if (str/starts-with? (str/trim l) "|")
        (let [rows (take-while #(str/starts-with? (str/trim %) "|") ls)]
          (recur (drop (count rows) ls)
                 (conj out [:table (mapv table-row (remove separator? rows))])))
        (let [txt (take-while #(not (str/starts-with? (str/trim %) "|")) ls)]
          (recur (drop (count txt) ls) (conj out [:md (str/join "\n" txt)]))))
      out)))

(defn- answer-text [text by-ref]
  (let [linked (with-cite-links text by-ref)]
    [:div {:class "assistant__answer-text"
           :on-click (fn [^js e]
                       (when-let [a (.closest (.-target e) "a")]
                         (let [href (.getAttribute a "href")]
                           (when (str/starts-with? (str href) "#cite:")
                             (.preventDefault e)
                             (let [[_ t id] (re-matches #"#cite:([^:]+):(.+)" href)]
                               (open-object! t id))))))}
     (for [[i [kind x]] (map-indexed vector (segments linked))]
       (case kind
         :md    ^{:key i} [md/markdown {:text x}]
         :table ^{:key i} [:div {:class "assistant__table-wrap"}
                           [html-table {:compact true :bordered true}
                            [:thead
                             [:tr (for [[j c] (map-indexed vector (first x))] ^{:key j} [:th c])]]
                            [:tbody
                             (for [[j row] (map-indexed vector (rest x))]
                               ^{:key j} [:tr
                                          (for [[k c] (map-indexed vector row)]
                                            ^{:key k} [:td [md/markdown {:text c}]])])]]]))]))

(defn- ref-chip [{:keys [type id title]}]
  [ui/tooltip {:content (str type " · " id)}
   [tag {:minimal true :interactive true :on-click #(open-object! type id)}
    [:span {:class "assistant__chip-type"} type] (or title id)]])

(def ^:private tool-labels
  {"list_object_types"     :ui/assistant-step-list
   "describe_object_types" :ui/assistant-step-describe
   "query_objects"         :ui/assistant-step-query
   "calculator"            :ui/assistant-step-calc})

(defn- step-label [{:keys [tool args answer]}]
  (cond
    answer (i18n/t :ui/assistant-step-answer)
    :else  (let [k (or (tool-labels tool)
                       (cond (str/starts-with? (str tool) "query_") :ui/assistant-step-query
                             (str/starts-with? (str tool) "apply_") :ui/assistant-step-action
                             :else :ui/assistant-step-tool))]
             (i18n/t k
                     {:tool tool
                      :type (or (get args "type") (some-> tool (str/replace #"^query_" "")))}))))

(defn- step-detail [{:keys [summary]}]
  (cond
    (:error summary)    [:span {:class "assistant__step-error"} (:error summary)]
    (:count summary)    (i18n/t :ui/assistant-step-rows
                                {:n (:returned summary) :total (:count summary)})
    (:groups summary)   (i18n/t :ui/assistant-step-groups {:n (:groups summary)})
    (:types summary)    (i18n/t :ui/assistant-step-types {:n (:types summary)})
    (:status summary)   (str (:status summary))
    :else               nil))

(defn- steps-view [steps]
  [:ol {:class "assistant__steps"}
   (for [[i s] (map-indexed vector steps)]
     ^{:key i}
     [:li {:class "assistant__step"}
      [:div {:class "assistant__step-head"}
       [:span (step-label s)]
       (when-let [d (step-detail s)] [:span {:class "assistant__step-meta"} " · " d])
       (when (:ms s) [:span {:class "assistant__step-meta"} " · " (:ms s) " ms"])
       (when-let [t (:tokens s)]
         [:span {:class "assistant__step-meta"} " · "
          (i18n/t :ui/assistant-tokens {:in (:in t) :out (:out t)})])]
      (when (seq (:args s))
        [:pre {:class "assistant__step-args"} (js/JSON.stringify (clj->js (:args s)) nil 2)])])])

(defn- collapsible [label & body]
  [ui/section
   {:title label :collapsible true :compact true :collapse-props {:default-is-open false}
    :class "assistant__fold"}
   [ui/section-card
    (into [:div {:class "assistant__fold-body"}] body)]])

(defn- kilo [n] (if (>= n 1000) (str (.toFixed (/ n 1000) 1) "k") (str n)))

(defn- context-bar
  [steps window]
  (when-let [peak (some->> steps (keep (comp :in :tokens)) seq (apply max))]
    (let [ratio (when (pos? (or window 0)) (/ peak window))]
      [:div {:class "assistant__ctx"}
       (when ratio
         [:div {:class (str "assistant__ctx-bar" (when (>= ratio 0.9) " is-full"))}
          [:div {:class "assistant__ctx-fill" :style {:width (str (min 100 (* 100 ratio)) "%")}}]])
       [:span {:class "assistant__step-meta"}
        (if ratio
          (i18n/t :ui/assistant-context {:used (kilo peak) :window (kilo window)})
          (i18n/t :ui/assistant-context-unknown {:used (kilo peak)}))]
       (when (and ratio (>= ratio 0.9))
         [:span {:class "assistant__ctx-warn"} (i18n/t :ui/assistant-context-full)])])))

(defn- pretty [x] (js/JSON.stringify (clj->js x) nil 2))

(defn- text-of [content]
  (cond
    (string? content) content
    (sequential? content) (str/join "\n"
                                    (map (fn [p] (or (get p "text") (str "[" (get p "type") "]")))
                                         content))
    (nil? content) ""
    :else (pretty content)))

(defn- conversation-view
  [request]
  [:div {:class "assistant__convo"}
   (for [[i m] (map-indexed vector (get request "messages"))]
     ^{:key i}
     [:div {:class (str "assistant__convo-msg assistant__convo-msg--" (get m "role"))}
      [:div {:class "assistant__convo-role"}
       (case (get m "role")
         "system" (i18n/t :ui/assistant-role-system)
         "user" (i18n/t :ui/assistant-role-user)
         "assistant" (i18n/t :ui/assistant-role-model)
         "tool" (i18n/t :ui/assistant-role-tool)
         (get m "role"))]
      (when-let [t (not-empty (text-of (get m "content")))]
        [:pre {:class "assistant__convo-text"} t])
      (for [[j tc] (map-indexed vector (get m "tool_calls"))]
        ^{:key j}
        [:pre {:class "assistant__convo-text"}
         (str "→ " (get-in tc ["function" "name"]) " " (get-in tc ["function" "arguments"]))])])])

(defn- call-view [n c]
  [ui/card {:class "assistant__call"}
   [:div {:class "assistant__call-head"}
    (i18n/t :ui/assistant-call {:n n})
    [:span {:class "assistant__step-meta"}
     (str " · " (:model c) (when (:source c) (str " · " (:source c)))
          (when (:prompt-tokens c)
            (str " · " (:prompt-tokens c) " → " (:completion-tokens c) " Token"))
          (when (:latency-ms c) (str " · " (:latency-ms c) " ms")))]]
   (when-let [e (:error c)] [:div {:class "assistant__step-error"} e])
   [collapsible (i18n/t :ui/assistant-call-readable) [conversation-view (:request c)]]
   [collapsible (i18n/t :ui/assistant-call-request)
    [:pre {:class "assistant__raw"} (pretty (:request c))]]
   (when-let [pv (:provider c)]
     [collapsible (i18n/t :ui/assistant-call-provider {:wire (:wire pv)})
      [:div {:class "assistant__step-meta"} (str "POST " (:url pv))]
      [:pre {:class "assistant__raw"} (pretty (:body pv))]])
   [collapsible (i18n/t :ui/assistant-call-response)
    [:pre {:class "assistant__raw"} (pretty (:response c))]]])

(defn- calls-view [run-id]
  (let [{:keys [loading? error] cs :calls} (get-in @db [:calls run-id])]
    (cond
      (not (or loading? error cs))
      [ui/button
       {:outlined true :class "assistant__calls-btn"
        :on-click #(send! [:assistant.calls/load (:thread-id @db) run-id])}
       (i18n/t :ui/assistant-calls-show)]
      loading? [busy/block {:class "assistant__muted"} (i18n/t :ui/loading)]
      error    [:div {:class "assistant__step-error"} error]
      (empty? cs) [:div {:class "assistant__muted"} (i18n/t :ui/assistant-calls-none)]
      :else [:div {:class "assistant__calls"}
             [:div {:class "assistant__step-meta"} (i18n/t :ui/assistant-calls-note)]
             (for [[i c] (map-indexed vector cs)] ^{:key (:request-id c)} [call-view (inc i) c])])))

(defn- proposal-card [p]
  (let [status (:status p)]
    [:div {:class (str "assistant__proposal assistant__proposal--" status)}
     [:div {:class "assistant__proposal-head"}
      [:strong (name (:action p))]
      [:span {:class "assistant__proposal-status"}
       (i18n/t (keyword "ui" (str "assistant-proposal-" status)))]]
     (when-let [why (:reason p)] [:div {:class "assistant__proposal-reason"} why])
     [:dl {:class "assistant__proposal-args"}
      (for [[k v] (or (:accepted-args p) (:args p))]
        ^{:key k} [:<> [:dt (name k)] [:dd (or (get-in p [:refs k :title]) (str v))]])]
     (when-let [e (:error p)] [:div {:class "assistant__step-error"} e])
     (when (= "open" status)
       [:div {:class "assistant__proposal-actions"}
        [ui/button {:intent :primary :on-click #(send! [:assistant/decide p :accept])}
         (i18n/t :ui/assistant-accept)]
        [ui/button {:outlined true :on-click #(send! [:assistant/decide p :reject])}
         (i18n/t :ui/assistant-reject)]])]))

(defn- model-line [{:keys [model source]}]
  (when model
    [:div {:class "assistant__model-line"} (str model (when source (str " · " source)))]))

(defn- assistant-message
  [{:keys [status content detail run-id]} proposals {:keys [question history]}]
  (let [{:keys [steps sources unverified seen error model context-window]} detail
        by-ref  (into {} (map (fn [x] [(str (:type x) ":" (:id x)) x])) (concat seen sources))
        mine    (filter #(= run-id (get-in % [:provenance :run-id])) proposals)]
    [:div {:class "assistant__msg assistant__msg--assistant"}
     (case status
       "running"
       [:div {:class "assistant__running"}
        [busy/block {:class "assistant__thinking"} (i18n/t :ui/assistant-working)]
        [model-line detail]
        (when (seq steps) [steps-view steps])]

       "failed"
       [ui/callout {:intent :danger}
        [:strong (i18n/t :ui/assistant-failed)] " " error
        (when (seq steps)
          [collapsible (i18n/t :ui/assistant-trace {:n (count steps)}) [steps-view steps]
           [calls-view run-id]])]

       [:<>
        (if (str/blank? content)
          [:div {:class "assistant__muted"} (i18n/t :ui/assistant-no-answer)]
          [answer-text content by-ref])
        (for [p mine] ^{:key (:id p)} [proposal-card p])
        (cond
          (seq sources)
          [:div {:class "assistant__sources"}
           [:span {:class "assistant__sources-label"} (i18n/t :ui/assistant-sources)]
           (for [s sources] ^{:key (str (:type s) (:id s))} [ref-chip s])]
          (seq seen)
          [collapsible (i18n/t :ui/assistant-seen {:n (count seen)})
           [:div {:class "assistant__sources"}
            (for [s seen] ^{:key (str (:type s) (:id s))} [ref-chip s])]])
        (when (seq unverified)
          [ui/callout {:intent :warning :compact true}
           (i18n/t :ui/assistant-unverified {:refs (str/join ", " unverified)})])
        [model-line detail]
        (when (and question (not (str/blank? content)))
          [evals/save-case
           {:agent (get-in @db [:data :thread :agent]) :question question :answer content
            :history history :thread (:thread-id @db) :run run-id}])
        [context-bar steps context-window]
        (when (seq steps)
          [collapsible (i18n/t :ui/assistant-trace {:n (count (remove :answer steps))})
           [steps-view steps]
           (when model [:div {:class "assistant__step-meta"} (str model " · run " run-id)])
           [calls-view run-id]])])]))

(defn- user-message [{:keys [content detail]}]
  [:div {:class "assistant__msg assistant__msg--user"}
   [:div content]
   (when-let [as (seq (:attachments detail))]
     [:div {:class "assistant__msg-atts"}
      (for [a as]
        ^{:key (:hash a)} [tag {:minimal true :icon :paperclip}
                           (or (:name a) (subs (:hash a) 0 10))])])])

(defn- messages-view [{:keys [messages proposals]}]
  [:div {:class "assistant__messages"}
   (for [[i prev m] (map vector (range) (cons nil messages) messages)]
     ^{:key (:seq m)}
     (if (= "user" (:role m))
       [user-message m]
       [assistant-message m proposals
        (when (= "user" (:role prev))
          {:question (:content prev)
           :history  (into [] (comp (filter #(= "done" (:status %)))
                                    (remove #(str/blank? (:content %)))
                                    (map #(select-keys % [:role :content])))
                           (take (max 0 (- i 1)) messages))})]))])

(defn- agent-label [a] (or (:display-name a) (:name a)))

(defn- current-agent []
  (let [{:keys [agents agent thread-id data]} @db
        nm (if thread-id (get-in data [:thread :agent]) agent)]
    (some #(when (= nm (:name %)) %) agents)))

(defn- model-picker []
  (let [{:keys [models model]} @db
        a       (current-agent)
        own     (:model a)
        chosen  (or model own)
        src     (fn [id] (some #(when (= id (:id %)) (:source %)) models))]
    [:div {:class "assistant__model"}
     [:span {:class "assistant__model-label"} (str (or (:display-name a) (:name a)) " ·")]
     (if (> (count models) 1)
       [ui/html-select
        {:class "assistant__model-select" :value (or chosen "")
         :title (i18n/t :ui/assistant-model-hint)
         :on-change #(let [v (.. % -target -value)]
                       (send! [:assistant/choose-model v own]))}
        (for [{:keys [id source default]} models]
          ^{:key id}
          [:option {:value id}
           (str id (when source (str " (" source ")"))
                (when (= id own) (str " — " (i18n/t :ui/assistant-model-agent)))
                (when (and default (not= id own)) (str " — " (i18n/t :ui/prov-is-default))))])]
       [:span {:class "assistant__model-name"}
        (str chosen (when-let [s (or (src chosen) (:source a))] (str " (" s ")")))])
     (when (and chosen (not= chosen own) (not (src chosen)))
       [:span {:class "assistant__model-warn"} (i18n/t :ui/assistant-model-no-source)])]))

(defn- composer []
  (let [{:keys [draft agents agent thread-id attachments sending? data]} @db
        busy?  (or sending? (ev/running? data))
        file-input (atom nil)]
    [:div {:class "assistant__composer"}
     (when-let [v (:version @db)]
       [:div {:class "assistant__draft"}
        (i18n/t :ui/assistant-testing-version
                {:agent agent :v (if (= "draft" v) (i18n/t :ui/evals-against-draft) (str "v" v))})
        [ui/tooltip {:content (i18n/t :ui/close)}
         [ui/button
          {:minimal true :small true :icon :cross :on-click #(send! [:assistant/clear-version])
           :aria-label (i18n/t :ui/close)}]]])
     [model-picker]
     (when (and (nil? thread-id) (> (count agents) 1))
       [ui/html-select {:class "assistant__agent-select" :value (or agent "")
                        :on-change #(send! [:assistant/choose-agent
                                            (.. % -target -value)])}
        (for [a agents] ^{:key (:name a)} [:option {:value (:name a)} (agent-label a)])])
     (when (seq attachments)
       [:div {:class "assistant__atts"}
        (for [a attachments]
          ^{:key (:key a)}
          [tag {:minimal true :icon (if (:uploading? a) :time :paperclip)
                :on-remove #(send! [:assistant.attachment/remove (:key a)])}
           (:name a)])])
     [:div {:class "assistant__input-row"}
      [ui/text-area {:class "assistant__input" :rows 2 :value draft
                     :placeholder (i18n/t :ui/assistant-placeholder)
                     :on-change #(send! [:assistant/draft (.. % -target -value)])
                     :on-key-down (fn [^js e]
                                    (when (and (= "Enter" (.-key e)) (not (.-shiftKey e)))
                                      (.preventDefault e)
                                      (ask!)))}]
      [:input {:type "file" :multiple true :style {:display "none"}
               :ref #(reset! file-input %)
               :on-change (fn [^js e] (attach-files! (.. e -target -files))
                            (set! (.. e -target -value) ""))}]
      [ui/tooltip {:content (i18n/t :ui/assistant-attach)}
       [ui/button
        {:minimal true :small true :icon :paperclip :on-click #(some-> @file-input .click)
         :aria-label (i18n/t :ui/assistant-attach)}]]
      [ui/button
       {:intent :primary :class "assistant__send"
        :disabled (or busy? (str/blank? draft) (some :uploading? attachments)) :on-click ask!}
       (if busy?
         [busy/inline {:size 12} nil]
         (i18n/t :ui/assistant-send))]]]))

(defn- thread-list []
  (let [{:keys [threads thread-id agents]} @db
        label (into {} (map (juxt :name agent-label)) agents)]
    [:<>
     [ui/button {:icon :plus :minimal true :align-text :start :on-click new-thread!}
      (i18n/t :ui/assistant-new)]
     (if (empty? threads)
       [:div {:class "assistant__muted"} (i18n/t :ui/assistant-no-threads)]
       [ui/menu
        (for [t threads]
          ^{:key (:id t)}
          [ui/menu-item
           {:text          (or (not-empty (:title t)) (i18n/t :ui/assistant-untitled))
            :active        (= thread-id (:id t))
            :on-click      #(send! [:assistant.thread/open (:id t)])
            :label-element
            [:<>
             [:span {:class "assistant__muted"} (get label (:agent t) (:agent t))]
             [ui/button
              {:minimal true :small true :icon :cross :aria-label (i18n/t :ui/assistant-delete)
               :on-click (fn [e]
                           (.stopPropagation e)
                           (send! [:assistant.thread/delete (:id t)]))}]]}])])]))

(defn- empty-view []
  (let [{:keys [agents agent]} @db
        a (some #(when (= agent (:name %)) %) agents)]
    [ui/empty-state {:icon :chat
                     :title (i18n/t :ui/assistant-ask-title
                                    {:agent (or (some->> a agent-label (str " · ")) "")})
                     :hint [:<>
                            (when-let [d (if (:builtin a)
                                           (i18n/t :ui/assistant-analyst-desc)
                                           (:description a))]
                              [:div d])
                            [:div (i18n/t :ui/assistant-ask-hint)]]}]))

(defn- no-agents-view []
  [ui/empty-state {:icon :offline
                   :title (i18n/t :ui/assistant-no-model)
                   :hint (i18n/t :ui/assistant-no-model-hint)
                   :action [ui/button {:intent   :primary
                                       :on-click #(send! [:assistant/view :providers])}
                            (i18n/t :ui/assistant-setup-models)]}])

(defn- chat-view [threads?]
  (let [{:keys [agents data error]} @db]
    [:div {:class "assistant__chat"}
     (when threads? [:div {:class "assistant__threads"} [thread-list]])
     [:div {:class "assistant__main"
            :on-drag-over (fn [^js e] (.preventDefault e))
            :on-drop (fn [^js e] (.preventDefault e) (attach-files! (.. e -dataTransfer -files)))}
      (when error [:div {:class "assistant__error"} error
                   [ui/button
                    {:minimal true :small true :icon :cross :aria-label (i18n/t :ui/close)
                     :on-click #(send! [:assistant/clear-error])}]])
      (cond
        (nil? agents)  [busy/block (i18n/t :ui/loading)]
        (empty? agents) [no-agents-view]
        data           [messages-view data]
        :else          [empty-view])
      (when (seq agents) [composer])]]))

(def ^:private sections
  [[:chat :ui/assistant-tab-chat :chat]
   [:tests :ui/assistant-tab-tests :clipboard]
   [:studio :ui/assistant-tab-studio :code]
   [:providers :ui/assistant-tab-models :cog]])

(defn- studio-opts []
  {:models (:models @db) :agents (:agents @db) :on-change load!
   :on-try #(send! [:assistant/try-draft %])})

(defn- section-body
  [threads?]
  (case (:view @db)
    :providers [providers/view {:on-change load!}]
    :tests     [evals/view {:models (:models @db) :agents (:agents @db)}]
    :studio    [studio/view (studio-opts)]
    [chat-view threads?]))

(defn- drawer-header []
  (let [{:keys [view history?]} @db]
    [page/header {:title (i18n/t :ui/assistant-title) :icon [ui/icon {:icon :chat}]}
     [ui/tabs {:id :assistant-view :selected-tab-id view
               :on-change (fn [v] (send! [:assistant/view v]))}
      (for [[k label] sections] [ui/tab {:id k :title (i18n/t label)}])]
     (when (= :chat view)
       [ui/tooltip {:content (i18n/t :ui/assistant-history)}
        [ui/button {:minimal true :small true :icon :history :active history?
                    :on-click #(send! [:assistant/toggle-history])
                    :aria-label (i18n/t :ui/assistant-history)}]])
     (when (= :chat view)
       [ui/tooltip {:content (i18n/t :ui/assistant-new)}
        [ui/button
         {:minimal true :small true :icon :plus :on-click new-thread!
          :aria-label (i18n/t :ui/assistant-new)}]])
     [ui/tooltip {:content (i18n/t :ui/close)}
      [ui/button
       {:minimal true :small true :icon :cross :on-click #(send! [:assistant/close])
        :aria-label (i18n/t :ui/close)}]]]))

(def
  panel
  (machine/entering
   chat [:assistant/load]
   (fn []
     [:div {:class "assistant"}
      [drawer-header]
      (if (= :studio (:view @db))
        [:div {:class "assistant__studio"} [studio/agent-list (studio-opts)] [section-body false]]
        [section-body (:history? @db)])])))

(defn- side []
  (let [view (:view @db)]
    [:<>
     [ui/menu
      (for [[k label icon] sections]
        ^{:key k}
        [ui/menu-item {:text (i18n/t label) :icon icon :active (= k view)
                       :on-click #(send! [:assistant/view k])}])]
     (case view
       :chat   [thread-list]
       :studio [studio/agent-list (studio-opts)]
       nil)]))

(defn- head []
  (let [view (:view @db)]
    [page/header {:title (i18n/t (or (some (fn [[k label]] (when (= k view) label)) sections)
                                     :ui/assistant-tab-chat))}]))

(page/view! :assistant/side side)
(page/view! :assistant/head head)
(page/view! :assistant/body (fn [] [section-body false]))

(def ^:private tree
  (page/werkzeug
   {:sidebar       :assistant/side
    :sidebar-label :ui/assistant-title
    :main          {:kind  :rows
                    :items [(page/view :assistant/head)
                            {:kind :rows :box {:grow? true}
                             :items [(page/view :assistant/body)]}]}}))

(defn- page-nav? [] (= :assistant (:nav @shell/ui-state)))

(defn open! [] (send! [:assistant/open]))

(defn drawer
  []
  (when (and (= :authed (:status @auth/auth)) (not (page-nav?)))
    (if (:open? @db)
      [overlay/drawer {:on-close #(send! [:assistant/close])
                       :backdrop? false
                       :close-on-backdrop? false
                       :class "assistant-drawer"
                       :header? false}
       [panel]]
      [ui/tooltip {:content (i18n/t :ui/assistant-open)}
       [ui/button {:intent :primary :large true :icon :chat :class "assistant-fab"
                   :on-click open!}
        (i18n/t :ui/assistant-open)]])))

(def page
  (machine/entering chat [:assistant/load] (fn [] [platform/module :assistant tree])))
