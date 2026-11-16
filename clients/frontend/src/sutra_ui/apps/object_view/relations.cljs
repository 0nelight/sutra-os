(ns sutra-ui.apps.object-view.relations
  (:require [sutra-ui.ui.core :as ui]
            [sutra.i18n.core :as i18n]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.instances :as instances]
            [sutra-ui.apps.object-view.model :as ov-model]
            [sutra-ui.object.card :as oc]
            [sutra-ui.ui.page :as page]
            [sutra-ui.apps.object-view.actions :as actions]
            [sutra-ui.apps.object-view.registry :as reg]))

(defn- conn-rows
  [instance-id]
  (let [id-str (str instance-id)
        edges  (instances/edges-touching instance-id)]
    (sort-by (comp :label :side)
             (concat
              (for [c edges :when (= id-str (str (:from-id c)))]
                (assoc c :side (catalog/link-side (catalog/name-of (:link-type-ref c)) :to)
                       :other-id (:to-id c)))
              (for [c edges :when (= id-str (str (:to-id c)))]
                (assoc c :side (catalog/link-side (catalog/name-of (:link-type-ref c)) :from)
                       :other-id (:from-id c)))))))

(defn link-groups
  [instance-id]
  (->> (conn-rows instance-id)
       (group-by (comp :api-name :side))
       (map (fn [[_ rows]]
              (let [side (:side (first rows))]
                {:side     side
                 :far-type (some-> (:far side) catalog/name-of)
                 :peers    (vec (for [{:keys [other-id]} rows
                                      :let [peer (instances/object-by-id other-id)]]
                                  {:id other-id :row peer :type (:object-type peer)}))})))
       (sort-by (comp :label :side))
       vec))

(defn links-failure
  [id]
  (when-let [why (get @instances/object-links-failed (str id))]
    [ui/tooltip {:content why}
     [:div {:class "u-muted"} (i18n/t :ui/ov-links-unavailable)]]))

(defn- peer-type [group peer]
  (or (:type peer) (:far-type group)))

(defn root [{:keys [object-type id]}]
  (let [id (or id (ov-model/focused-instance-id))]
    (cond
      (nil? id)
      [:div {:class "explorer-attributes__empty"}
       (i18n/t :ui/property-list-no-object)]
      :else
      (let [_      (instances/ensure-object-links! id object-type)
            groups (link-groups id)]
        (if (empty? groups)
          (or (links-failure id)
              [:div {:class "explorer-attributes__empty"} (i18n/t :ui/ov-links-none)])
          (into [:div {:class "objectview-links"}]
                (for [{:keys [side peers] :as g} groups]
                  ^{:key (str (:api-name side))}
                  [:section {:class "objectview-links__group"}
                   [:h4 {:class "objectview-links__head" :title (name (:api-name side))}
                    (:label side) [:span {:class "objectview-links__count"} (count peers)]]
                   (into [:div {:class "objectview-links__cards"}]
                         (for [{pid :id row :row :as p} peers
                               :let [t (peer-type g p)]]
                           ^{:key (str pid)}
                           [:div {:class "objectview-links__entry"}
                            [oc/object-card t pid {:row row :compact? true}]
                            (when row
                              [actions/link-actions-menu object-type id
                               (assoc row :object-type t)])]))])))))))

(page/view! :relations root)

(defmethod reg/config-form :relations [_ _ _]
  [:div {:class "explorer-config__none"}
   (i18n/t :ui/ov-links-config-none)])

(defmethod reg/default-config :relations [_] {})
