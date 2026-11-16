(ns sutra-ui.apphost.widgets.input
  (:require [reagent.core :as r]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.capps.variables :as vars :refer [deref*]]
            [sutra.types.set-query :as sq]
            [sutra-ui.ui.inputs :as inputs :refer [select-input combo-input]]
            [sutra-ui.apphost.client :as client]
            [sutra-ui.apphost.frame :as frame]
            [sutra-ui.apphost.events :refer [on-click-fn]]
            [sutra-ui.apphost.scope-handles :refer [resolve-ref]]
            [sutra-ui.apphost.widgets.filter :as fw]
            [sutra-ui.apphost.widgets.action :refer [deref-vals]]
            [sutra-ui.apphost.page-ctx :refer [ev-ctx]]))

(defn- choice-changed [on-change ctx]
  (let [f (on-click-fn on-change ctx)]
    (fn [v {:keys [on-select]}]
      (let [row {:value v}]
        (when f (f row))
        (when-let [g (on-click-fn on-select ctx)] (g row))))))

(defn- select-input-widget
  [{:keys [sel items placeholder class display on-change] :as cfg} {:keys [app]}]
  (if (#{:radio :segmented} (->kw display))
    [inputs/choice-input sel
     (cond-> (select-keys cfg [:item-value :item-label :sort :sort-by :display :layout :columns
                               :selection-required? :disabled? :class])
       true                 (assoc :items items
                                   :on-change (choice-changed on-change (ev-ctx app)))
       (contains? cfg :value) (assoc :value (:value cfg)))]
    [select-input sel
     (merge (select-keys cfg [:item-value :item-label :sort :sort-by :strict? :disabled? :value
                              :clearable?])
            {:items items :placeholder placeholder :class class
             :on-change (when on-change (choice-changed on-change (ev-ctx app)))})]))

(defn- with-found
  [items found]
  (let [have (into #{} (map :id) items)]
    (into (vec items) (remove #(have (:id %))) found)))

(defn- searching-combo [_cfg _scope _app]
  (let [found (r/atom {})
        timer (atom nil)]
    (fn [{:keys [sel items placeholder class limit item-search item-label item-match]
          sort-prop :sort-by :as cfg}
         {:keys [handles derived data*]} {desc :descriptor f :frame}]
      (let [cut     (vars/cut-of items)
            obj     (:object desc)
            ask     (fn [term nm]
                      (let [vs (into {} (map (fn [h] [h (deref* (resolve-ref
                                                                 {:val h} handles derived
                                                                 (some-> data* deref)))]))
                                     (sq/holes (sq/set-of derived nm)))]
                        (frame/request!
                         f
                         (assoc
                          (frame/kept #(swap! found assoc nm (vec (:rows %))))
                          :ask
                          [client/search!
                           desc (cond-> {:var nm :term term :vars vs
                                         :fields (cond (sequential? item-search) item-search
                                                       item-search [item-search])
                                         :show   (not-empty
                                                  (cond-> (cond (sequential? item-label)
                                                                (vec item-label)
                                                                item-label [item-label]
                                                                :else [])
                                                    sort-prop  (conj sort-prop)
                                                    item-match (conj item-match)))}
                                  obj (assoc :object {:type (or (:object-type-ref obj)
                                                                (:object-type obj))
                                                      :id (str (:id obj))}))]))))
            search! (when (seq cut)
                      (fn [term]
                        (some-> @timer js/clearTimeout)
                        (reset! timer (js/setTimeout #(run! (partial ask term) cut) 200))))]
        [combo-input sel
         (merge (select-keys cfg [:item-value :item-label :item-search :item-match :sort :sort-by
                                  :strict?])
                {:items (with-found items (mapcat val @found)) :placeholder placeholder
                 :class class :limit limit :on-search search!})]))))

(deftype ActionField [store commit!]
  IDeref (-deref [_] @store)
  IReset (-reset! [_ v]
           (when (not= v @store)
             (reset! store v)
             (when (some? v) (commit! v)))
           v))

(def ^:private typed-commit-ms 900)

(defn- action-field
  [_cfg _input _ctx]
  (let [store (r/atom nil)
        seen  (atom ::keiner)
        timer (atom nil)]
    (fn [{:keys [value action params value-param typed?]} input
         {:keys [descriptor refresh] f :frame}]
      (let [shown (deref* value)
            werte (deref-vals params)
            run   (fn [v]
                    (frame/request!
                     f
                     {:ask    [client/submit-strict! descriptor (assoc werte (->kw value-param) v)
                               {:action action}]
                      :then   [[:rf/invoke (fn [d] (when (and refresh d) (refresh d)))]]
                      :else   [[:rf/invoke (fn [_] (reset! store shown))]]
                      :report true}))]
        (when (not= shown @seen)
          (reset! seen shown)
          (reset! store shown))
        (conj input (->ActionField
                     store
                     (if typed?
                       (fn [v]
                         (some-> @timer js/clearTimeout)
                         (reset! timer (js/setTimeout #(run v) typed-commit-ms)))
                       run)))))))

(defn- combo-field [cfg scope app sel]
  [searching-combo (assoc cfg :sel sel) scope app])

(defn- combo-input-widget [cfg {:keys [app scope]}]
  (if (:action cfg)
    [action-field cfg [combo-field cfg scope app] app]
    [searching-combo cfg scope app]))

(defn- number-field [cfg sel]
  [fw/number-input (assoc cfg :sel sel)])

(defn- number-input-widget [{:keys [sel action] :as cfg} {:keys [app]}]
  (if action
    [action-field (assoc cfg :typed? true) [number-field cfg] app]
    [number-field cfg sel]))

(def widgets
  {:select-input (fw/labelled select-input-widget)
   :combo-input  (fw/labelled combo-input-widget)
   :number-input number-input-widget})
