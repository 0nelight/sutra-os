(ns sutra.automation.http
  (:require [sutra.store.control :as ctl]
            [sutra.automation.activity :as activity]
            [sutra.automation.executions :as executions]
            [sutra.automation.logic-events :as logic-events]
            [sutra.automation.rules :as rules]
            [sutra.automation.runs :as autos]
            [sutra.actions.api :as actions]
            [sutra.objects.api :as objects]
            [sutra.store.access.scope :as ctx]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.http.endpoint :as ep]))

(set! *warn-on-reflection* true)

(def list-automations
  (ep/handler :automations
              {:call (fn [{:keys [store branch]}]
                       (mapv #(assoc % :subscribers (autos/subscribers store branch (:id %)))
                             (autos/list-all store branch)))
               :wrap :automations}))

(def upsert-automation
  (ep/handler :automations {:call #(activity/saved! (:store %) (:branch %) (:body %))}))

(def delete-automation
  (ep/handler :automation-of {:call #(autos/delete! (:store %) (:branch %) (:id %))}))

(def ^:private the-automation
  [#(autos/get-one (:store %) (:branch %) (:id %)) :refusal.automation/no-such-automation])

(def mute-automation
  (ep/handler :automation-mute
              {:with {:auto the-automation}
               :call (fn [{:keys [store branch id body auto user]}]
                       (activity/mute! store branch auto (not (false? (:muted body))) user)
                       (autos/get-one store branch id))}))

(defn- subscription [subscribe?]
  (ep/handler :automation-subscribe
              {:with       {:auto the-automation}
               :needs-user true
               :call       (fn [{:keys [store branch id auto user]}]
                             (activity/subscribe! store branch auto user subscribe?)
                             {:subscribers (autos/subscribers store branch id)})}))

(def subscribe-automation (subscription true))

(def unsubscribe-automation (subscription false))

(def run-automation
  (ep/handler :automation-run
              {:with {:auto the-automation}
               :call (fn [{:keys [store branch body auto]}]
                       (if (or (seq (:ids body)) (some? (:where body)))
                         (rules/execute! store branch auto
                                         (select-keys body [:ids :where :batch-size
                                                            :parallelism]))
                         (assoc (rules/run-one! store branch auto {:manual? true})
                                :ran-at (ctl/clock store))))}))

(defn- execution-row
  [x]
  (-> (dissoc x :pending :claim-id)
      (assoc :left (count (:pending x)))))

(def automation-executions
  (ep/handler :automation-executions
              {:call #(mapv execution-row
                            (executions/of-automation (:store %) (:branch %) (:id %) 20))
               :wrap :executions}))

(def cancel-execution
  (ep/handler :automation-execution-cancel
              {:call #(some-> (executions/cancel! (:store %) (:branch %) (:execution %))
                              execution-row)}))

(def automation-runs
  (ep/handler :automation-runs
              {:call #(autos/list-runs (:store %) (:branch %) (:id %) 50) :wrap :runs}))

(def automation-proposals
  (ep/handler :automation-proposals
              {:call (fn [{:keys [store branch schema id status limit offset]}]
                       {:proposals (actions/with-refs
                                     (objects/object-query (ctx/at store branch) schema) schema
                                     (actions/list-for
                                      store branch id status {:limit limit :offset offset}))
                        :counts    (actions/counts-for store branch id)})}))

(def accept-proposal
  (ep/handler :automation-proposal-accept
              {:call #(actions/accept! (ctx/at (:store %) (:branch %)) (:branch %) (:schema %)
                                         (:id %) (:proposal %)
                                         (some-> (get-in % [:body :args]) (update-keys ->kw)))}))

(def reject-proposal
  (ep/handler :automation-proposal-reject
              {:call #(actions/reject! (:store %) (:branch %) (:id %) (:proposal %))}))

(def preview-automation
  (ep/handler :automation-preview
              {:with {:auto the-automation}
               :call (fn [{:keys [store branch body auto]}]
                       (let [oid (some-> (:object-id body) str not-empty)]
                         (rules/run-one! store branch auto
                                         (cond-> {:dry? true}
                                           oid (assoc :object-id oid :manual? true)
                                           (:with-model? body)
                                           (assoc :with-model? true)))))}))

(def automation-events
  (ep/handler :automation-events
              {:call (fn [{:keys [store branch id status limit offset]}]
                       {:events
                        (logic-events/list-for store branch id status
                                               {:limit limit :offset offset})
                        :counts (logic-events/counts-for store branch id)})}))

(def module
  {:name        :automation-http
   :contributes {:http/routes
                 {:automation-events          {:get automation-events}
                  :automation-of              {:delete delete-automation}
                  :automation-preview         {:post preview-automation}
                  :automation-proposal-accept {:post accept-proposal}
                  :automation-proposal-reject {:post reject-proposal}
                  :automation-proposals       {:get automation-proposals}
                  :automation-run             {:post run-automation}
                  :automation-mute            {:post mute-automation}
                  :automation-subscribe       {:post   subscribe-automation
                                               :delete unsubscribe-automation}
                  :automation-runs            {:get automation-runs}
                  :automation-executions      {:get automation-executions}
                  :automation-execution-cancel {:post cancel-execution}
                  :automations                {:get list-automations
                                               :post upsert-automation}}}})
