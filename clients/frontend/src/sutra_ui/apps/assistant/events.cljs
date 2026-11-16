(ns sutra-ui.apps.assistant.events
  (:require [clojure.string :as str]
            [sutra-ui.api :as api]
            [sutra-ui.storage :as storage]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.requests :as rq]))

(def poll-ms 1200)

(def agent-key "sutra.assistant.agent")

(def model-key "sutra.assistant.model")

(defn initial
  [stored]
  {:open? false :view :chat :agents nil :threads [] :thread-id nil :data nil :agent nil
   :draft "" :attachments [] :error nil :sending? false :history? false :calls {}
   :stored-agent (:agent stored) :stored-model (:model stored)})

(defn running? [data]
  (some #(= "running" (:status %)) (:messages data)))

(defn- upload-bytes! [buf]
  (-> (api/fetch-url (api/on-page-branch "/api/blob")
                       #js {:method "POST" :credentials "include" :body buf
                            :headers (clj->js {"accept" api/CT
                                               "content-type" "application/octet-stream"
                                               api/protocol-header (str api/protocol-version)})})
      (.then (fn [^js resp]
               (-> (.text resp)
                   (.then (fn [t]
                            (if (.-ok resp)
                              (api/decode t)
                              (throw
                               (js/Error.
                                (or (:error (try (api/decode t) (catch :default _ nil)))
                                    t)))))))))))

(defn upload-file!
  [^js file]
  (.then (.arrayBuffer file) upload-bytes!))

(defn- load [db]
  (rq/request db {:ask [api/get! :agent-threads] :then [[:assistant/loaded]] :error [:error]}))

(defn- loaded [db {:keys [agents threads models]}]
  (let [names  (set (map :name agents))
        pick   (or (some names [(:agent db) (:stored-agent db)]) (:name (first agents)))
        ids    (set (map :id models))
        stored (:stored-model db)
        own    (some #(when (= pick (:name %)) (:model %)) agents)
        want   (or (:model db) (when (not= stored own) stored))]
    {:db (assoc db :agents (vec agents) :threads (vec threads) :agent pick
                :models (vec models) :model (when (ids want) want))}))

(defn- fetch [db id]
  (rq/request db {:ask     [api/get! :agent-thread {:id id}] :path [:data]
                  :current [[:thread-id] id] :then [[:assistant.thread/fetched id]]
                  :error   [:error]}))

(defn- open-thread [db id]
  (fetch (assoc db :thread-id id :data nil :error nil :history? false) id))

(defn- fresh [db]
  (assoc db :thread-id nil :data nil :error nil :draft "" :attachments [] :history? false))

(defn- deleted [db id]
  (if (= id (:thread-id db))
    (update (load (fresh db)) :fx #(into [[:cancel :poll]] %))
    (load db)))

(defn- ask [{:keys [draft thread-id agent attachments sending? model version] :as db} zone]
  (let [input (str/trim draft)
        atts  (filterv :hash attachments)
        extra (cond-> {:input input :attachments atts :zone zone}
                model   (assoc :model model)
                version (assoc :version version))]
    (when (and (seq input) (not sending?) (or thread-id agent)
               (not-any? :uploading? attachments))
      (rq/request (assoc db :error nil)
                  {:ask  (if thread-id
                           [api/post! :agent-thread-ask {:id thread-id} extra]
                           [api/post! :agent-threads (assoc extra :agent agent)])
                   :busy [:sending?] :then [[:assistant/sent]] :error [:error]}))))

(defn- decide [db proposal verdict]
  (let [id (:thread-id db)]
    (rq/request db {:ask  [api/post! (if (= :accept verdict) :agent-thread-proposal-accept
                                         :agent-thread-proposal-reject)
                           {:id id :proposal (:id proposal)} {}]
                    :then [[:assistant.thread/fetch id]] :error [:error]})))

(defn- without-attachment [db k]
  (update db :attachments (fn [as] (filterv #(not= k (:key %)) as))))

(defn- attached [db k {:keys [hash]}]
  {:db (update db :attachments
               (fn [as] (mapv #(if (= k (:key %)) (-> % (assoc :hash hash) (dissoc :uploading?)) %)
                              as)))})

(machine/reg-event :assistant/load (fn [{:keys [db]} _] (load db)))

(machine/reg-event :assistant/loaded (fn [{:keys [db]} [_ r]] (loaded db r)))

(machine/reg-event :assistant.thread/fetch (fn [{:keys [db]} [_ id]] (fetch db id)))

(machine/reg-event :assistant.thread/fetched
                   (fn [_ [_ id data]]
                     {:fx [(if (running? data)
                             [:after {:key :poll :ms poll-ms :event [:assistant.thread/poll id]}]
                             [:dispatch [:assistant/load]])]}))

(machine/reg-event :assistant.thread/poll
                   (fn [{:keys [db]} [_ id]] (when (= id (:thread-id db)) (fetch db id))))

(machine/reg-event :assistant.thread/open (fn [{:keys [db]} [_ id]] (open-thread db id)))

(machine/reg-event :assistant.thread/new
                   (fn [{:keys [db]} _] {:db (fresh db) :fx [[:cancel :poll]]}))

(machine/reg-event :assistant.thread/delete
                   (fn [{:keys [db]} [_ id]]
                     (rq/request db {:ask   [api/delete! :agent-thread {:id id}]
                                     :then  [[:assistant.thread/deleted id]]
                                     :error [:error]})))

(machine/reg-event :assistant.thread/deleted (fn [{:keys [db]} [_ id]] (deleted db id)))

(machine/reg-event :assistant/ask (fn [{:keys [db]} [_ zone]] (ask db zone)))

(machine/reg-event :assistant/sent
                   (fn [{:keys [db]} [_ resp]]
                     (open-thread (assoc db :draft "" :attachments [])
                                  (get-in resp [:thread :id]))))

(machine/reg-event :assistant/decide
                   (fn [{:keys [db]} [_ proposal verdict]] (decide db proposal verdict)))

(machine/reg-event :assistant/attach
                   (fn [{:keys [db]} [_ file k]]
                     (rq/request (update db :attachments conj
                                         {:key k :name (.-name file) :mime (.-type file)
                                          :uploading? true})
                                 {:ask  [upload-file! file] :then [[:assistant/attached k]]
                                  :error [:error] :else [[:assistant.attachment/remove k]]})))

(machine/reg-event :assistant/attached (fn [{:keys [db]} [_ k r]] (attached db k r)))

(machine/reg-event :assistant.attachment/remove
                   (fn [{:keys [db]} [_ k]] {:db (without-attachment db k)}))

(machine/reg-event :assistant/draft (fn [{:keys [db]} [_ v]] {:db (assoc db :draft v)}))

(machine/reg-event :assistant/choose-agent
                   (fn [{:keys [db]} [_ v]]
                     {:db (assoc db :agent v :stored-agent v)
                      :fx [[:invoke [storage/set-item! agent-key v]]]}))

(machine/reg-event :assistant/choose-model
                   (fn [{:keys [db]} [_ v own]]
                     {:db (assoc db :model (when (not= v own) v) :stored-model v)
                      :fx [[:invoke [storage/set-item! model-key v]]]}))

(machine/reg-event :assistant/view (fn [{:keys [db]} [_ v]] {:db (assoc db :view v)}))

(machine/reg-event :assistant/toggle-history
                   (fn [{:keys [db]} _] {:db (update db :history? not)}))

(machine/reg-event :assistant/open (fn [{:keys [db]} _] {:db (assoc db :open? true)}))

(machine/reg-event :assistant/close (fn [{:keys [db]} _] {:db (assoc db :open? false)}))

(machine/reg-event :assistant/clear-version (fn [{:keys [db]} _] {:db (dissoc db :version)}))

(machine/reg-event :assistant/clear-error (fn [{:keys [db]} _] {:db (assoc db :error nil)}))

(machine/reg-event :assistant/try-draft
                   (fn [{:keys [db]} [_ nm]]
                     {:db (assoc (fresh db) :view :chat :agent nm :version "draft")
                      :fx [[:cancel :poll]]}))

(machine/reg-event :assistant.calls/load
                   (fn [{:keys [db]} [_ thread-id run-id]]
                     (rq/request db {:ask   [api/get! :agent-thread-calls
                                             {:id thread-id :run run-id}]
                                     :busy  [:calls run-id :loading?] :path [:calls run-id]
                                     :pick  #(hash-map :calls (vec (:calls %)))
                                     :error [:calls run-id :error]})))
