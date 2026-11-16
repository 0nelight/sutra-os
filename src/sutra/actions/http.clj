(ns sutra.actions.http
  (:require [sutra.kit.refusal :as refusal]
            [sutra.kit.anomaly :as anomaly]
            [sutra.catalog.actions :as catalog-actions]
            [sutra.catalog.functions :as functions]
            [sutra.http.provenance :as provenance]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [sutra.store.access.scope :as ctx]
            [sutra.store.control :as ctl]
            [clojure.string :as str]
            [sutra.actions.apply :as action-apply]
            [sutra.actions.options :as onto-options]
            [sutra.governance.api :as governance]
            [sutra.actions.request :as request]
            [sutra.actions.staging :as stg]
            [sutra.store.edge :as edge]
            [sutra.store.world :as world]
            [sutra.actions.staged :as staged-run]
            [sutra.catalog.branch-policy :as policy]
            [sutra.actions.ports :as ports]
            [sutra.objects.api :as objects]
            [sutra.sources.api :as sources]
            [sutra.foundation.encoding :refer [coerce-uuids ->uuid]]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.actions.landing :refer [commit-branch!]]
            [sutra.types.ontologies :as ontologies]
            [sutra.http.endpoint :as ep]
            [sutra.http.responses :refer [ok]]))

(set! *warn-on-reflection* true)

(defn- told
  [result]
  (select-keys result [:action-id :commit :side-effects :suppressed-side-effects
                       :deferred-side-effects :decisions :proposals]))

(defn- at-view-home
  [{:keys [store schema branch body dry-run auth] :as a}]
  (if (and (not dry-run)
           (action-apply/instance-view-action? schema (some-> (:action body) name keyword)
                                               (:args body)))
    (let [home (or (world/instance-home (world/at store branch)) branch)]
      (assoc a :branch home :publish true :stage false
             :edge (edge/read-edge store home (grants/current-user-id auth) {:writes? true})))
    a))

(defn- creator-ids
  [auth]
  [(grants/current-user-id auth) (:username auth) (get-in auth [:subject :username])])

(defn- respond-action
  [a]
  (let [{:keys [store branch body dry-run publish stage return-edits edge auth]
         project :sutra/project http-req :req} (at-view-home a)
        action  (some-> (:action body) name keyword)
        create? (request/create-action? action)
        args    (cond->> (request/with-fresh-id action (or (:args body) {}))
                  (= :create-project action)
                  (ontologies/with-creator-space (:schema edge) (creator-ids auth)))
        stage?  (or stage (and (not publish) (not dry-run)))
        mode    (cond dry-run :dry-run stage? :stage :else :dispatch)]
    (when-not action
      (refusal/refuse! :refusal.actions/missing-action-in-body {:body body}))
    (let [prepared (if (= mode :stage) args (coerce-uuids args))
          req      {:authority (grants/request-authority store)
                    :branch    branch
                    :project   project
                    :store        (ctx/at store branch)
                    :dry-run?  (= mode :dry-run)
                    :action    action
                    :args      prepared
                    :mode      mode
                    :edge      edge}]
      (case mode
        :dry-run
        (anomaly/then (action-apply/apply-action req)
                      (fn [r]
                        (ok (cond-> {:action action :dry-run true :plan (:plan r)}
                              create? (assoc :id (:id args))))))

        :stage
        (anomaly/then
         (action-apply/apply-action
          (assoc req :provenance {:origin :action :origin-ref (name action)}))
         (fn [r]
           (ok (cond-> {:staged true :seq (:seq r) :action action}
                 create?      (assoc :id (:id args))
                 return-edits (assoc :edits (:edits r))))))

        :dispatch
        (let [branch-store   (ctx/at store branch)
              rkey  (some-> (get-in http-req [:headers "idempotency-key"]) str/trim
                            not-empty)
              where {:branch branch :request-key rkey
                     :actor  (str (or (:username (grants/actor store)) ""))}
              seen  #(when rkey
                       (:response (ctl/get branch-store :action-requests where)))]
          (if-let [prev (seen)]
            (ok (assoc prev :replayed true))
            (let [aid    (random-uuid)
                  answer (cond-> {:action action :action-id aid}
                           create? (assoc :id (:id args)))
                  schema (:schema edge)
                  guarded-store
                  (governance/write-guard branch-store edge branch nil {:data? true})
                  run    (action-apply/apply-action
                          (cond-> (assoc req :schema schema :store guarded-store
                                         :action-id aid)
                            rkey
                            (assoc :records
                                   [(ctl/entry branch-store :action-requests
                                               (assoc where
                                                      :response answer))])))
                  prev   (when (anomaly/refusal? run) (seen))]
              (cond
                prev                   (ok (assoc prev :replayed true))
                (anomaly/refusal? run) run
                :else
                (let [{:keys [result edits]} run
                      reply (cond-> (assoc answer :result (told result)
                                           :action-id (:action-id result))
                              return-edits (assoc :edits edits))]
                  (ports/announce-action! branch-store branch schema result)
                  (when rkey
                    (ctl/update! branch-store :action-requests where
                                 {:response reply}))
                  (ok reply))))))))))

(def dispatch-action
  (ep/handler :actions
              {:respond
               (fn [{:keys [body edge req] :as a}]
                 (provenance/with-sent
                   req
                   (respond-action a)
                   #(:params (catalog-actions/resolve-action
                              (:schema edge) (some-> (:action body) name keyword)))
                   (:args body)))}))

(def dispatch-action-batch
  (ep/handler :actions-batch
              {:respond
               (fn [{:keys [store schema branch body publish stage return-edits edge]
                     project :sutra/project}]
                 (let [action   (some-> (:action body) name keyword)
                       create?  (request/create-action? action)
                       requests (mapv #(request/with-fresh-id action %) (:requests body))
                       stage?   (or stage (not publish))
                       mode     (if stage? :stage :dispatch)
                       limit    (some->> action (functions/action-batch-limit schema))]
                   (cond
                     (not action)
                     (refusal/refused :refusal.actions/missing-action-in-body {:body body})

                     (empty? requests)
                     (refusal/refused :refusal.actions/missing-requests-in-body {:body body})

                     (> (count requests) limit)
                     (refusal/refused :refusal.actions/a-batch-of-holds-at {:action action :requested (count requests) :limit limit} {:action (name action) :limit limit})

                     :else
                     (let [prepared (if (= mode :stage) requests (mapv coerce-uuids requests))
                           branch-store (ctx/at store branch)
                           req      (cond-> {:authority (grants/request-authority store)
                                             :branch    branch
                                             :project   project
                                             :store     (if (= mode :dispatch)
                                                          (governance/write-guard
                                                           branch-store edge branch
                                                           nil {:data? true})
                                                          branch-store)
                                             :schema    schema
                                             :action    action
                                             :requests  prepared
                                             :mode      mode
                                             :edge      edge}
                                      (= mode :stage)
                                      (assoc :provenance
                                             {:origin :action :origin-ref (name action)}))
                           r        (action-apply/apply-action-batch req)]
                       (anomaly/then
                        r
                        (fn [r]
                          (when (= mode :dispatch)
                            (ports/announce-action! branch-store branch schema (:result r)))
                          (ok (cond-> {:action action}
                                (= mode :stage)    (assoc :staged true :seqs (:seqs r))
                                (= mode :dispatch) (assoc :result (told (:result r))
                                                          :action-id (:action-id (:result r)))
                                create?            (assoc :ids (mapv :id requests))
                                return-edits       (assoc :edits (:edits r))))))))))}))

(def action-params
  (ep/handler :action-params
              {:respond
               (fn [{:keys [body store branch schema]}]
                 (let [action (some-> (:action body) name keyword)
                       args   (coerce-uuids (or (:args body) (:parameters body) {}))
                       search (into {} (keep (fn [[k t]]
                                               (when (some? t) [(keyword (name k)) (str t)])))
                                    (:search body))]
                   (if-not action
                     (refusal/refused :refusal.actions/missing-action-in-body {:body body})
                     (ok (assoc (onto-options/evaluate-action-params! (ctx/at store branch) schema
                                                                      action args
                                                                      (not-empty search))
                                :action action)))))}))

(def list-staged
  (ep/handler :staging
              {:call (fn [{:keys [store branch]}]
                       (let [store (ctx/at store branch)]
                         {:staged (stg/list-staged-enriched store)
                          :transactions (stg/open-transactions store)}))}))

(def commit-staged
  (ep/handler :staging {:call (fn [{:keys [store branch edge]}]
                                (commit-branch! store edge branch))}))

(def preview-staged
  (ep/handler :staging-preview
              {:call (fn [{:keys [store branch edge]}]
                       (governance/commit-preview (ctx/at store branch)
                                                  (edge/staging-view edge)))}))

(def discard-staged
  (ep/handler :staged-by-seq
              {:call (fn [{:keys [store branch seq edge]}]
                       {:deleted (staged-run/discard-staged! (ctx/at store branch)
                                                             (edge/staging-view edge)
                                                             (Long/parseLong seq))})}))

(def clear-staged
  (ep/handler :staging
              {:call (fn [{:keys [store branch edge]}]
                       {:cleared (stg/clear-staged! (ctx/at store branch)
                                                    (edge/staging-view edge))})}))

(defn- proposed-values
  [store schema obj-type obj-id]
  (let [want (str obj-id)
        oid  (lookup/rid-of schema :object-types obj-type)]
    (->> (objects/objects-of-type store schema oid)
         (some (fn [o] (when (= want (str (:id o)))
                         (or (:values o) (dissoc o :id :object-type :object-type-ref))))))))

(defn- writeback-allowed!
  [auth schema obj-type op]
  (let [rid  (lookup/->type-rid schema :object-types obj-type)
        spec (get-in schema [:object-types rid])]
    (grants/authorize! auth {:op op :project (:project spec)
                             :ignore-inherited (:ignore-inherited spec) :resource rid}
                       obj-type)))

(def writeback-preview
  (ep/handler :writeback-preview
              {:respond (fn [{:keys [body store schema auth]}]
                          (let [obj-type (some-> (:object-type body) ->kw)
                                obj-id   (->uuid (:object-id body))]
                            (if (and obj-type obj-id)
                              (do (writeback-allowed! auth schema obj-type :read)
                                  (ok (sources/preview schema obj-type obj-id
                                                       {:store    store
                                                        :proposed (proposed-values store schema
                                                                                   obj-type
                                                                                   obj-id)})))
                              (refusal/refused :refusal.actions/writeback-preview-needs-object-type body))))}))

(def writeback-confirm
  (ep/handler :writeback-confirm
              {:respond (fn [{:keys [body store schema branch auth]}]
                          (let [obj-type (some-> (:object-type body) ->kw)
                                obj-id   (->uuid (:object-id body))]
                            (if (and obj-type obj-id)
                              (do (writeback-allowed! auth schema obj-type :update)
                                  (ok (sources/confirm!
                                       schema obj-type obj-id
                                       {:store       store
                                        :proposed    (proposed-values store schema obj-type obj-id)
                                        :force?      (boolean (:force? body))
                                        :seen        (some-> (:seen body) (update-keys ->kw))
                                        :effect-gate (policy/request-gate branch)})))
                              (refusal/refused :refusal.actions/writeback-confirm-needs-object-type body))))}))

(def module
  {:name        :actions
   :contributes {:http/routes
                 {:action-params              {:post action-params}
                  :actions                    {:post dispatch-action}
                  :actions-batch              {:post dispatch-action-batch}
                  :staged-by-seq              {:delete discard-staged}
                  :staging
                  {:get list-staged :post commit-staged :delete clear-staged}
                  :staging-preview            {:get preview-staged}
                  :writeback-confirm          {:post writeback-confirm}
                  :writeback-preview          {:post writeback-preview}}}})
