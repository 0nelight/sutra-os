(ns sutra-ui.graph-editor.events
  (:require [sutra-ui.ui.overlay :as overlay]
            [sutra.i18n.core :as i18n]
            [sutra-ui.object.card :as oc]
            [sutra-ui.state.canvas :as canvas]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.graph :as graph]
            [sutra-ui.state.instances :as instances]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.requests :as rq]))

(machine/reg-fx :shared.graph/prompt
                (fn [{f :frame} {:keys [ask ok]}]
                  (let [reply (machine/answering f)]
                    (.then (overlay/prompt! ask) #(when % (reply (conj ok %)))))))

(machine/reg-sub :shared.graph/arranging? (fn [s _] (boolean (get-in s [:shared.graph :busy]))))

(defn arranging? [] @(machine/subscribe [:shared.graph/arranging?] {:frame db/sutra}))

(defn- fit-view! []
  (when-let [^js inst @instances/rf-instance]
    (.fitView inst #js {:padding 0.2 :duration 400})))

(db/reg :shared.graph/lay-out
        (fn [s [_ {:keys [layout busy?] :as job}]]
          (when-not (and busy? (get-in s [:shared.graph :busy]))
            (update (rq/request s {:ask    layout
                                   :busy   (when busy? [:shared.graph :busy])
                                   :then   [[:shared.graph/laid-out job]]
                                   :else   [[:rf/invoke js/console.error "[layout] failed"]]
                                   :report (when busy?
                                             #(i18n/t :ui/ge-layout-failed {:message %}))})
                    :fx #(into [(when busy? [:invoke [graph/record-graph-state!]])] %)))))

(db/reg :shared.graph/laid-out
        (fn [_ [_ {:keys [place mode]} id->xy]]
          (when-let [placed (place id->xy)]
            {:fx [[:invoke [(canvas/positions-apply-fn mode) placed]]
                  [:after {:key :shared.graph/fit :ms 260 :event [:shared.graph/fit]}]]})))

(db/reg :shared.graph/fit (fn [_ _] {:fx [[:invoke [fit-view!]]]}))

(db/reg :shared.graph/confirm
        (fn [_ [_ ask & run]] {:fx [[:confirm {:ask ask :ok (into [:shared.graph/run] run)}]]}))

(db/reg :shared.graph/run (fn [_ [_ & run]] {:fx [[:invoke (vec run)]]}))

(defn delete-type-ask [type-kw]
  {:title   (i18n/t :ui/confirm-delete-type-title {:name (oc/type-label type-kw)})
   :body    (i18n/t :ui/confirm-delete-type-body)
   :danger? true})

(def ^:private group-palette
  ["#2d72d2" "#238551" "#c87619" "#cd4246" "#9d3f9d" "#00a396"])

(db/reg :shared.graph/new-color-group
        (fn [_ [_ title n ids]]
          {:fx [[:shared.graph/prompt
                 {:ask {:title   title
                        :label   (i18n/t :ui/name)
                        :default (i18n/t :ui/ge-new-color-default)}
                  :ok  [:shared.graph/color-group-named
                        (nth group-palette (mod n (count group-palette))) ids]}]]}))

(db/reg :shared.graph/color-group-named
        (fn [_ [_ color ids nm]]
          (when (seq nm) {:fx [[:invoke [graph/create-color-group-for-nodes! nm color ids]]]})))
