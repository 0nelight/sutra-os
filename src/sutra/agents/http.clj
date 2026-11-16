(ns sutra.agents.http
  (:require [sutra.kit.refusal :as refusal]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [sutra.llm.api :as llm]
            [sutra.catalog.values :as values]
            [sutra.catalog.llm-models :as llm-models]
            [sutra.agents.runtime :as runtime]
            [sutra.agents.threads :as threads]
            [sutra.actions.api :as actions]
            [sutra.objects.api :as objects]
            [sutra.http.responses :refer [ok]]
            [sutra.access.api :as access]
            [sutra.store.access.scope :as ctx]))

(set! *warn-on-reflection* true)

(defn- granted-agent [auth schema nm]
  (let [a (runtime/agent-of schema nm)]
    (when (and a (or (:builtin a) (access/granted? auth schema a))) a)))

(defn- card [schema a]
  (cond-> {:name (name (:agent-name a))
           :writes (if (some #(#{:action :builder} (:kind %)) (:tools a)) (or (:writes a) :propose)
                     :none)}
    (:display-name a) (assoc :display-name (:display-name a))
    (:description a)  (assoc :description (:description a))
    (:model a)        (assoc :model (:model a)
                             :source
                             (some-> (try (runtime/source-of schema a)
                                          (catch clojure.lang.ExceptionInfo _ nil))
                                     name))))

(defn- attachments-of [xs]
  (vec (for [x xs :let [h (str (or (:hash x) x))] :when (re-matches #"[0-9a-f]{64}" h)]
         (cond-> {:hash h}
           (:name x) (assoc :name (str (:name x)))
           (:mime x) (assoc :mime (str (:mime x)))))))

(defn- thread-view [store schema branch t]
  {:thread    t
   :messages  (threads/messages store (:id t))
   :proposals (actions/with-refs (objects/object-query store schema) schema
                                    (actions/list-for store branch (:id t) nil))})

(defn list-agents
  [{auth :sutra/auth schema :sutra/schema}]
  (ok {:agents (into {} (for [[_rid a] (access/granted-resources auth schema :agents)]
                          [(:agent-name a) a]))}))

(defn list-threads
  [{store :sutra/store auth :sutra/auth schema :sutra/schema branch :sutra/branch}]
  (if-not branch
    (refusal/refused :refusal.http/no-branch {})
    (ok {:agents  (let [declared (->> (access/granted-resources auth schema :agents)
                                      (map (comp #(card schema %) second))
                                      (sort-by :name))
                        names    (set (map :name declared))]
                    (into (vec declared)
                          (comp (remove #(names (name (:agent-name %))))
                                (map #(assoc (card schema %) :builtin true)))
                          (runtime/builtins schema)))
         :models  (vec (for [[k m] (sort-by (comp str key) (:llm-models schema))
                             :when (not (:deprecated m))]
                         (cond-> {:id (if (keyword? k) (subs (str k) 1) (str k))}
                           (:source m)         (assoc :source (name (:source m)))
                           (:default m)        (assoc :default true)
                           (:capabilities m)   (assoc :capabilities (mapv name (:capabilities m)))
                           (:context-window m) (assoc :context-window (:context-window m)))))
         :threads (filterv #(= branch (:branch %)) (threads/list store {}))})))

(defn- ask [store schema thread body sync?]
  (let [{:keys [input max-turns zone model version]} body]
    (if (str/blank? (str input))
      (refusal/refused :refusal.agents/a-question-needs-an-input {})
      (ok (assoc (runtime/ask! store schema thread
                               {:input (str input) :max-turns max-turns :zone zone :model model
                                :version (not-empty (str version))
                                :attachments (attachments-of (:attachments body))
                                :sync? sync?})
                 :thread thread)))))

(defn create-thread
  [{:keys [body-params] store :sutra/store auth :sutra/auth schema :sutra/schema
    branch :sutra/branch}]
  (let [store (ctx/at store branch)
        nm (:agent body-params)]
    (cond
      (not branch)                      (refusal/refused :refusal.http/no-branch {})
      (not (granted-agent auth schema nm)) (refusal/refused :refusal.agents/no-such-agent {:agent nm} {:nm nm})
      :else (ask store schema (threads/create! store {:agent nm :branch branch}) body-params
                 false))))

(defn- owned [store id f]
  (if-let [t (threads/one store id)] (f t) (refusal/refused :refusal.agents/no-thread {:id id} {:id id})))

(defn get-thread
  [{:keys [path-params] store :sutra/store schema :sutra/schema branch :sutra/branch}]
  (owned store (:id path-params) #(ok (thread-view store schema branch %))))

(defn delete-thread
  [{:keys [path-params] store :sutra/store}]
  (owned store (:id path-params) (fn [t] (threads/delete! store (:id t)) (ok {:deleted (:id t)}))))

(defn ask-thread
  [{:keys [path-params body-params] store :sutra/store auth :sutra/auth schema :sutra/schema
    branch :sutra/branch}]
  (let [store (ctx/at store branch)]
    (owned store (:id path-params)
           (fn [t]
             (if (granted-agent auth schema (:agent t))
               (ask store schema t body-params false)
               (refusal/refused :refusal.agents/no-such-agent-2 {:agent (:agent t)} {:agent (:agent t)}))))))

(defn- json-of [s]
  (when (seq (str s)) (try (json/read-str (str s)) (catch Exception _ (str s)))))

(defn- provider-view
  [schema {:keys [source model]} request]
  (let [spec (cond-> (get (values/sources->datasources schema) (some-> source keyword))
               (:provider (llm-models/rate-of schema
                                              model))
                 (assoc :wire (:provider (llm-models/rate-of schema model))))
        base (or (:base-url spec) (get-in spec [:config :base-url]))]
    (when (and spec (map? request))
      (let [[id w] (llm/wire-for llm/builtin spec base)]
        (when-not (= identity (:request w))
          {:wire (name id) :url (llm/url base (:chat-path w)) :body ((:request w) request)})))))

(defn thread-calls
  [{:keys [path-params query-params] store :sutra/store schema :sutra/schema branch :sutra/branch}]
  (owned store (:id path-params)
         (fn [t]
           (let [run (get query-params "run")
                 runs (set (keep :run-id (threads/messages store (:id t))))]
             (if-not (runs run)
               (refusal/refused :refusal.agents/no-run-in-this-thread {:run run} {:run run})
               (ok {:run run
                    :calls (vec (for [c (llm/run-calls (ctx/at store branch) schema run)
                                      :let [req (json-of (:input-json c))]]
                                  (cond-> (-> (select-keys c
                                                           [:request-id :ts :model :source
                                                            :prompt-tokens
                                                            :completion-tokens :latency-ms :error])
                                              (assoc :request req :response
                                                     (json-of (:output-json c))))
                                    (provider-view schema c req)
                                      (assoc :provider (provider-view schema c req)))))}))))))

(defn accept-proposal
  [{:keys [path-params body-params] store :sutra/store schema :sutra/schema branch :sutra/branch}]
  (let [store (ctx/at store branch)]
    (owned store (:id path-params)
           (fn [t] (ok (actions/accept! store branch schema (:id t) (:proposal path-params)
                                          (:edits body-params)))))))

(defn reject-proposal
  [{:keys [path-params] store :sutra/store branch :sutra/branch}]
  (let [store (ctx/at store branch)]
    (owned store (:id path-params)
           (fn [t] (ok (actions/reject! store branch (:id t) (:proposal path-params)))))))

(defn invoke-agent
  [{:keys [body-params path-params] store :sutra/store auth :sutra/auth schema :sutra/schema
    branch :sutra/branch}]
  (let [store (ctx/at store branch)
        nm (:name path-params)]
    (cond
      (str/blank? (str (:input body-params)))
        (refusal/refused :refusal.agents/invoke-needs-an-input-string body-params)
      (not (granted-agent auth schema
                          nm))      (refusal/refused :refusal.agents/no-such-agent {:agent nm} {:nm nm})
      :else
      (let [t (threads/create! store {:agent nm :branch branch})
            r (ask store schema t body-params true)]
        (if (= 200 (:status r))
          (let [m (some #(when (= "assistant" (:role %)) %) (rseq (threads/messages store (:id t))))
                d (:detail m)]
            (if (= "failed" (:status m))
              (refusal/refused :refusal.agents/agent-run-failed {:thread (:id t) :run-id (:run-id m)} {:error (:error d)})
              (ok (merge {:answer (:content m) :thread (:id t) :run-id (:run-id m)
                          :trace (:steps d)}
                         (select-keys d [:sources :unverified :seen :proposals :turns])))))
          r)))))

(def module
  {:name        :agents
   :contributes {:http/routes {:agents                        {:get list-agents}
                               :agent-invoke                  {:post invoke-agent}
                               :agent-threads
                                 {:get list-threads :post create-thread}
                               :agent-thread
                                 {:get get-thread :delete delete-thread}
                               :agent-thread-ask              {:post ask-thread}
                               :agent-thread-calls            {:get thread-calls}
                               :agent-thread-proposal-accept  {:post accept-proposal}
                               :agent-thread-proposal-reject  {:post reject-proposal}}
                 :blobs/roots {:agent-attachments threads/attachment-hashes}}})
