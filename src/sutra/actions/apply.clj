(ns sutra.actions.apply
  (:require
   [sutra.kit.anomaly :as anomaly]
   [sutra.kit.refusal :as refusal]
   [sutra.types.world-rules :as rules]
   [sutra.actions.action :as action]
   [sutra.catalog.actions :as actions]
   [sutra.store.world :as world]
   [sutra.types.entities :as ents]
   [sutra.actions.request :as request]
   [sutra.actions.staged :as staged-run]
   [sutra.foundation.contracts :as ct]
   [sutra.store.tx-vector :as tv]
   [sutra.types.rid :as rid]
   [sutra.types.schema :as sch]))

(set! *warn-on-reflection* true)

(defn- instance-held?
  [schema section args]
  (or (ents/authored-instance? section (or args {}))
      (let [k (get (ents/instance-keys (or schema {})) section)]
        (boolean
         (when-let [nm (some-> (:name args) rid/->name)]
           (and (set? k)
                (some #(= nm (rid/->name (get-in schema [section % (rid/name-field-of section)])))
                      k)))))))

(defn instance-view-action?
  [schema action args]
  (when-let [section (:section (actions/schema-action-kinds action))]
    (and (= :view (ents/section->regime section))
         (instance-held? schema section args))))

(defn refuse-instance-action!
  [store schema action argses]
  (when-let [home (world/instance-home store)]
    (when-let [section (:section (actions/schema-action-kinds action))]
      (when (some #(instance-held? schema section %) argses)
        (refusal/refuse! :refusal.actions/instance-scoped-section
                        {:reason :instance-scoped
                         :branch (:branch store) :section section :action action}
                        {:section (name section) :home home})))))

(def ^:private edit-kind->type
  {:create :add-object :update :modify-object :delete :delete-object})

(defn- edit-kinds
  [ops]
  (frequencies (map (comp tv/edit-kind :op) ops)))

(defn- action-edits
  [schema txv]
  (let [objects (filterv tv/object-write? txv)
        objs    (edit-kinds objects)
        links   (edit-kinds (filter tv/link-write? txv))]
    {:type  :edits
     :edits (vec (for [{:keys [op type payload]} objects
                       :let [pk (rules/object-primary-key schema type)]]
                   (cond-> {:type        (edit-kind->type (tv/edit-kind op))
                            :object-type (or (get-in schema [:object-types type :object-type-name])
                                             type)
                            :object-id   (:id payload)}
                     pk (assoc :primary-key (get (:values payload) pk)))))
     :added-objects-count    (objs :create 0)
     :modified-objects-count (objs :update 0)
     :deleted-objects-count  (objs :delete 0)
     :added-links-count      (links :connect 0)
     :deleted-links-count    (links :disconnect 0)}))

(defn- dispatched
  [schema r]
  (let [failed (action/side-effect-failures r)]
    (cond-> {:mode :dispatch :result r
             :edits (action-edits schema (:tx-vector r))}
      (seq failed) (assoc :summary {:side-effect-failures failed}))))

(defn- edge-request
  [{:keys [branch mode edge] :as req}]
  (if (= :dispatch mode)
    (request/action-request req)
    (let [b (str (or branch "main"))]
      (when-not (and (:exists? edge) (= b (:branch edge)))
        (refusal/refuse! :refusal.actions/branch-not-at-edge {:branch b :edge (:branch edge)}))
      (request/action-request (assoc req :branch b :schema (:schema edge))))))

(defn- dry-run-plan
  [{:keys [schema] :as req}]
  (let [idx (sch/rid-name-index schema)]
    (mapv (fn [e]
            (cond-> e
              (get-in e [:target :type])
              (update-in [:target :type] #(rid/resolve-rid->name idx %))))
          (action/plan-action! req))))

(defn apply-action
  [{:keys [mode] :as req}]
  (ct/check! ::ct/action-request req)
  (let [{:keys [schema] :as req} (edge-request req)]
    (when-not (= :dry-run mode)
      (refuse-instance-action! (:store req) schema (:action req) [(:args req)]))
    (case mode
      :dry-run
      {:mode :dry-run
       :plan (dry-run-plan req)}

      :stage
      (anomaly/then (staged-run/stage-action! req)
                    (fn [{:keys [seq tx-vector]}]
                      {:mode :stage :seq seq :edits (action-edits schema tx-vector)}))

      :dispatch
      (anomaly/then (action/run-action! req) (partial dispatched schema)))))

(defn apply-action-batch
  [{:keys [authority branch action requests mode] :as req}]
  (ct/check! ::ct/action-batch-request
             {:authority authority :branch branch :action action :requests requests})
  (let [{:keys [schema] :as req} (edge-request (update req :requests vec))]
    (when-not (= :dry-run mode)
      (refuse-instance-action! (:store req) schema action (:requests req)))
    (case mode
      :stage
      (let [staged (staged-run/stage-requests! req
                                               (mapv (fn [args] {:action action :args args})
                                                     requests))]
        {:mode  :stage
         :seqs  (mapv :seq staged)
         :edits (action-edits schema (mapcat :tx-vector staged))})

      :dispatch
      (anomaly/then (action/run-action! req) (partial dispatched schema)))))
