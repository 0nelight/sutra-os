(ns sutra-ui.apps.object-view.new-view-modal
  (:require [sutra-ui.ui.overlay :as overlay]
            [sutra.capps.catalog :as cat]
            [sutra.capps.layout :as lay]
            [sutra.i18n.core :as i18n]
            [sutra-ui.object.card :as oc]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.apps.object-view.model :as ov-model]
            [sutra-ui.apps.object-view.registry :as reg]
            [sutra-ui.apps.object-view.matrix]
            [sutra-ui.apps.object-view.floorplan]
            [sutra-ui.ui.core :as ui]))

(defn- config-for [view-type object-type]
  (cond-> (reg/default-config view-type) object-type (assoc :owner-type object-type)))

(defn open!
  ([]                 (open! nil nil))
  ([object-type]      (open! object-type nil))
  ([object-type view]
   (let [vt (-> (reg/user-creatable-types) first :view-type)]
     (ov-model/send! [:object-view.new-view/open object-type view vt
                      (config-for vt object-type)]))))

(defn append-item
  [view item]
  (cond
    (nil? view)
    {:kind :rows :items [item]}

    (and (= :rows (keyword (name (:kind view)))) (vector? (:items view)))
    (update view :items conj item)

    :else
    {:kind :rows :items [view item]}))

(defn- section-layout
  [{:keys [view view-type config name] object-type :type}]
  (let [seed (or view
                 (some-> (catalog/object-view-app object-type :full) :layout
                         lay/lower-view-body))
        leaf (-> (select-keys (or config {}) (map :key (:params (cat/widget-spec view-type))))
                 (assoc :widget view-type :title name))]
    (append-item seed leaf)))

(defn- put [k] #(ov-model/send! [:object-view.new-view/set k %]))

(defn root []
  (when-let [{supplied :object-type object-type :type vt :view-type cfg :config nm :name
              :as modal} (:new-view-modal @ov-model/ui)]
    (let [schema-ots (sort (keys (:object-types @catalog/schema)))
          close!     #(ov-model/send! [:object-view.layout/close :new-view-modal])]
      [overlay/form-dialog
       {:on-close close! :class "ov-dialog--new-view" :title (i18n/t :ui/ov-add-section-title)
        :ok-label (i18n/t :ui/ov-add-section-create)
        :ok-disabled? (or (empty? nm) (nil? object-type))
        :on-ok #(ov-model/send! [:object-view.layout/save object-type :full
                                 (section-layout modal) :new-view-modal])}
       [:<>
        [:div {:class "explorer-newview__form"}
         [:label {:class "explorer-newview__label"} (i18n/t :ui/ov-object-type)]
         (if supplied
           [:div {:class "explorer-newview__locked-type"}
            [:b (oc/type-label supplied)]
            [:span {:class "explorer-newview__locked-note"}
             (i18n/t :ui/ov-add-section-scope {:types (oc/type-label supplied true)})]]
           [ui/html-select {:value (or (some-> object-type name) "")
                            :on-change (fn [e]
                                         (let [v (.. e -target -value)]
                                           (ov-model/send! [:object-view.new-view/type
                                                            (when (seq v)
                                                              (keyword v))])))}
            [:option {:value ""} "—"]
            (for [k (sort-by oc/type-label schema-ots)]
              ^{:key k}
              [:option {:value (name k)} (oc/type-label k)])])

         [:label {:class "explorer-newview__label"} (i18n/t :ui/ov-add-section-kind)]
         [ui/html-select
          {:value (name vt)
           :on-change (fn [e]
                        (let [v (keyword (.. e -target -value))]
                          (ov-model/send! [:object-view.new-view/kind v
                                           (config-for v object-type)])))}
          (for [{:keys [view-type label]} (reg/user-creatable-types)]
            ^{:key view-type}
            [:option {:value (name view-type)} (i18n/t label)])]

         [:label {:class "explorer-newview__label"} (i18n/t :ui/ov-add-section-name)]
         [ui/input-group {:value nm
                          :on-change #((put :name) (.. % -target -value))
                          :placeholder (i18n/t :ui/ov-add-section-name-example)
                          :fill true}]]

        [:div {:class "explorer-newview__config"}
         [:div {:class "explorer-newview__config-hint"}
          (i18n/t :ui/ov-add-section-config)]
         (if object-type
           [reg/config-form vt cfg (put :config)]
           [:div {:class "explorer-newview__config-pick"}
            (i18n/t :ui/ov-add-section-pick-type)])]]])))
