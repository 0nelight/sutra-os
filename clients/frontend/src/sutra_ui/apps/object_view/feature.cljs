(ns sutra-ui.apps.object-view.feature
  (:require [sutra-ui.feature :as feature]
            [sutra-ui.routing :as routing]
            [sutra-ui.state.atlas :as ks]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.apps.object-view.core :as object-view]
            [sutra-ui.apps.object-view.model :as ov-model]
            [sutra-ui.apps.object-view.matrix]))

(def loaded
  true)

(feature/contribute!
 :command :open-object
 {:run! (fn [object-type id & [{:keys [new-tab?]}]]
          (if new-tab?
            (shell/open-in-new-tab!
             (routing/href :object-view {:object-type object-type :instance-id id}))
            (do (shell/enter! :object-view {:object-type object-type :instance-id id})
                (ks/refresh! :explorations))))})

(feature/register-screen!
 {:id :object-view :app :explorer :hidden? true :segment "object"
  :embed-aliases ["objects"]
  :view object-view/full
  :params #{:object-type :instance-id :key}
  :enter (fn [slice params] (assoc slice :object-view params))
  :tail (fn [{{:keys [object-type instance-id key]} :object-view}]
          (cond
            (and object-type instance-id)
            [(routing/enc (routing/detail-id->name-str object-type))
             (routing/enc instance-id)]
            (and object-type key) :pending
            :else                 []))
  :parse-tail (fn [[t id]]
                (if (and t id)
                  {:object-type (routing/str->detail-id :object-type (routing/dec' t))
                   :instance-id (routing/dec' id)}
                  {:object-type nil :instance-id nil}))
  :parse-entry (fn [[v0 t] q]
                 (when (and (= "v0" v0) t (= 1 (count q)))
                   (let [[k v] (first q)]
                     {:object-type (routing/str->detail-id :object-type (routing/dec' t))
                      :instance-id nil
                      :key         [(keyword k) v]})))
  :on-enter (fn [{t :object-type k :key}]
              (when (and t k) (ov-model/send! [:object-view/by-key t k])))
  :output (fn [{{t :object-type id :instance-id} :object-view}]
            (when (and t id) {:kind :object :object-type t :id id}))
  :subject (fn [{{t :object-type id :instance-id} :object-view}]
             (when-let [ot (some-> t catalog/name-of name)]
               (cond-> (str "Objektansicht " ot) id (str " · " id))))})

(feature/contribute!
 :open-in :object-view
 {:accepts :object :order 0 :label-key :ui/open-in-object-view
  :href (fn [{:keys [object-type id]}]
          (routing/href :object-view {:object-type object-type :instance-id id}))})
