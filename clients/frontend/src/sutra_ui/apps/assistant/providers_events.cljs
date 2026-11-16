(ns sutra-ui.apps.assistant.providers-events
  (:require [clojure.string :as str]
            [sutra.kit.remote :as rd]
            [sutra.types.llm-registration :as llm-reg]
            [sutra-ui.api :as api]
            [sutra-ui.credentials :as creds]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.requests :as rq]))

(def presets
  [{:id :anthropic :label "Anthropic (Claude)" :name "anthropic"
    :base-url "https://api.anthropic.com/v1"
    :wire "anthropic-messages"
    :models [{:id "claude-opus-5-5" :in 4.0 :out 20.0 :window 1000000}
             {:id "claude-sonnet-5-5" :in 2.0 :out 10.0 :window 1000000}
             {:id "claude-haiku-5-5" :in 0.10 :out 0.50 :window 1000000}]}
   {:id :xai :label "xAI (Grok)" :name "xai" :base-url "https://api.x.ai/v1" :wire "openai-chat"}
   {:id :openai :label "OpenAI" :name "openai" :base-url "https://api.openai.com/v1"
    :wire "openai-chat"}
   {:id :mistral :label "Mistral" :name "mistral" :base-url "https://api.mistral.ai/v1"
    :wire "openai-chat"}
   {:id :openrouter :label "OpenRouter" :name "openrouter" :base-url "https://openrouter.ai/api/v1"
    :wire "openai-chat"}
   {:id :ollama :label (str "Ollama (" "lokal" ")") :name "ollama"
    :base-url "http://127.0.0.1:11434/v1"
    :wire "openai-chat" :local? true}
   {:id :custom :label "…" :name "" :base-url "" :wire "openai-chat"}])

(defn preset-of [id] (some #(when (= id (:id %)) %) presets))

(defn preset-for-source [{:keys [base-url]}]
  (some #(when (and (seq (:base-url %)) (str/starts-with? (str base-url) (:base-url %))) %)
        presets))

(defn local-url? [u]
  (boolean (re-find #"^https?://(localhost|127\.|10\.|192\.168\.|172\.(1[6-9]|2\d|3[01])\.|\[::1\])"
                    (str u))))

(defn- egress-of [u]
  (let [url  (js/URL. u)
        port (or (not-empty (.-port url)) (if (= "http:" (.-protocol url)) "80" "443"))]
    [(str (.-hostname url) ":" port)]))

(def initial
  {:data rd/loading :error nil :busy nil :adding nil :key-for nil :model-for nil
   :tests {} :listed {} :budget nil :cap-draft {}})

(defn- models-data [s] (rd/value-or (:data s) nil))

(defn models [s] (:models (models-data s)))

(defn sealed
  [key]
  (if (str/blank? key)
    (js/Promise.resolve nil)
    (creds/seal {:api-key (str/trim key)} {})))

(defn register-models!
  [m others]
  (let [create #(api/mutate :create-llm-model (llm-reg/registration %) {:stage? true})]
    (reduce (fn [p o] (.then p (fn [_] (create (assoc o :default false)))))
            (create m)
            others)))

(defn- provider-schema [{:keys [name base-url wire project description]} cipher]
  {:schema
   {:sources
    {(keyword name)
     (cond-> {:type               :rest
              :config             (cond-> {:base-url (str/trim base-url)}
                                    (not= "openai-chat" wire) (assoc :wire (keyword wire)))
              :external-transform true
              :access             :read-only
              :egress             (egress-of (str/trim base-url))}
       (seq project)     (assoc :project (keyword project))
       (seq description) (assoc :description description)
       cipher            (assoc :secret-cipher cipher))}}})

(defn- busy-step
  ([db c then] (busy-step db c then {}))
  ([db c then req]
   (rq/request (assoc db :error nil)
               (merge {:ask c :busy [:busy] :error [:error] :then [then]} req))))

(defn- install
  [db schema then]
  (busy-step db [api/post! :install schema] [:providers.commit/installed then]))

(defn- register [db m on-change]
  (let [others (when (:default m)
                 (filter #(and (:default %) (not= (:id %) (:id m))) (models db)))]
    (busy-step db [register-models! m others]
               [:providers.commit/installed {:set {:model-for nil} :on-change on-change}])))

(defn- model-draft [db src]
  (let [m0 (first (:models (preset-for-source src)))]
    {:id       (or (:id m0) "")
     :in       (or (:in m0) (if (:local? src) 0 ""))
     :out      (or (:out m0) (if (:local? src) 0 ""))
     :currency "USD"
     :window   (or (:window m0) "")
     :tool-calling true
     :default  (empty? (filter :default (models db)))}))

(defn- pick-model [db src mid]
  (let [k (some #(when (= mid (:id %)) %) (:models (preset-for-source src)))]
    {:db (update db :model-draft merge {:id mid} (when k (select-keys k [:in :out :window])))}))

(defn- add-draft [data]
  (assoc (preset-of :anthropic) :preset :anthropic :key ""
         :project (or (some :project (:sources data)) (:name (first (:projects data))) "")))

(machine/reg-event :providers/load
                   (fn [{:keys [db]} _]
                     (update (rq/request db {:ask  [api/get! :llm-providers]
                                             :path [:data] :rd? true})
                             :fx conj (rq/ask {:ask  [api/get! :llm-budget]
                                               :path [:budget] :error [:error]}))))

(machine/reg-event :providers/clear-error (fn [{:keys [db]} _] {:db (assoc db :error nil)}))

(machine/reg-event :providers.cap/draft
                   (fn [{:keys [db]} [_ source v]] {:db (assoc-in db [:cap-draft source] v)}))

(machine/reg-event :providers.cap/cancel
                   (fn [{:keys [db]} [_ source]] {:db (update db :cap-draft dissoc source)}))

(machine/reg-event :providers.cap/save
                   (fn [{:keys [db]} [_ source eur]]
                     (busy-step db [api/post! :llm-budget {:source source :quota eur}]
                                [:providers.cap/cancel source] {:path [:budget]})))

(machine/reg-event :providers.commit/installed
                   (fn [{:keys [db]} [_ then]]
                     (busy-step db [api/post! :staging {}] [:providers.commit/staged then])))

(machine/reg-event :providers.commit/staged
                   (fn [{:keys [db]} [_ then]]
                     {:db (merge db (:set then))
                      :fx [[:dispatch [:providers/load]] [:invoke [(:on-change then)]]]}))

(machine/reg-event :providers.add/open
                   (fn [{:keys [db]} _]
                     {:db (assoc db :adding true :add-draft (add-draft (models-data db)))}))

(machine/reg-event :providers.add/close (fn [{:keys [db]} _] {:db (assoc db :adding nil)}))

(machine/reg-event :providers.add/field
                   (fn [{:keys [db]} [_ k v]] {:db (assoc-in db [:add-draft k] v)}))

(machine/reg-event :providers.add/preset
                   (fn [{:keys [db]} [_ x]]
                     {:db (update db :add-draft merge (select-keys x [:name :base-url :wire])
                                  {:preset (:id x)})}))

(machine/reg-event :providers.add/submit
                   (fn [{:keys [db]} [_ form on-change]]
                     (busy-step db [sealed (:key form)] [:providers.add/sealed form on-change])))

(machine/reg-event :providers.add/sealed
                   (fn [{:keys [db]} [_ form on-change cipher]]
                     (install db (provider-schema form cipher)
                              {:set {:adding nil :model-for (:name form)} :on-change on-change})))

(machine/reg-event :providers.key/open
                   (fn [{:keys [db]} [_ nm]] {:db (assoc db :key-for nm :key-draft "")}))

(machine/reg-event :providers.key/close (fn [{:keys [db]} _] {:db (assoc db :key-for nil)}))

(machine/reg-event :providers.key/draft (fn [{:keys [db]} [_ v]] {:db (assoc db :key-draft v)}))

(machine/reg-event :providers.key/submit
                   (fn [{:keys [db]} [_ src on-change]]
                     (busy-step db [sealed (:key-draft db)] [:providers.key/sealed src on-change])))

(machine/reg-event :providers.key/sealed
                   (fn [{:keys [db]} [_ {:keys [name type]} on-change cipher]]
                     (install db {:schema {:sources {(keyword name)
                                                     {:type          (keyword (or type "rest"))
                                                      :secret-cipher cipher}}}}
                              {:set {:key-for nil} :on-change on-change})))

(machine/reg-event :providers.model/open
                   (fn [{:keys [db]} [_ src]]
                     {:db (assoc db :model-for (:name src) :model-draft (model-draft db src))}))

(machine/reg-event :providers.model/close (fn [{:keys [db]} _] {:db (assoc db :model-for nil)}))

(machine/reg-event :providers.model/field
                   (fn [{:keys [db]} [_ k v]] {:db (assoc-in db [:model-draft k] v)}))

(machine/reg-event :providers.model/toggle
                   (fn [{:keys [db]} [_ k]] {:db (update-in db [:model-draft k] not)}))

(machine/reg-event :providers.model/pick
                   (fn [{:keys [db]} [_ src mid]] (pick-model db src mid)))

(machine/reg-event :providers.model/register
                   (fn [{:keys [db]} [_ m on-change]] (register db m on-change)))

(machine/reg-event :providers.model/list
                   (fn [{:keys [db]} [_ source]]
                     (rq/request db {:ask  [api/get! :llm-provider-models {:source source}]
                                     :path [:listed source] :pick :models :error [:error]})))

(machine/reg-event :providers/test
                   (fn [{:keys [db]} [_ source model]]
                     (let [k [:tests [source model]]]
                       (rq/request db {:ask   [api/post! :llm-provider-test
                                               {:source source :model model}]
                                       :busy  (conj k :running?) :path k
                                       :error (conj k :error)}))))
