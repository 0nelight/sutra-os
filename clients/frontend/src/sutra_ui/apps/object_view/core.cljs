(ns sutra-ui.apps.object-view.core
  (:require [sutra-ui.ui.busy :as busy]
            [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.ui.core :as ui]
            [sutra-ui.ui.display :as display]
            [sutra.kit.coerce :refer [->kw]]
            [reagent.core :as r]
            [sutra.capps.format :refer [v]]
            [sutra.capps.layout :as lay]
            [sutra.types.schema-reach :as reach]
            [sutra.capps.sidebar :as sb]
            [sutra.capps.standard-view :as sv]
            [sutra.kit.decisions :as decisions]
            [sutra.capps.variables :as vars]
            [sutra-ui.render-ctx :as rctx]
            [sutra.i18n.core :as i18n]
            [sutra-ui.state.portal :as pstate]
            [sutra-ui.state.canvas :as canvas]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.instances :as instances]
            [sutra-ui.state.object-view :as object-view]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.apps.object-view.model :as ov-model]
            [sutra.capps.leaf-props :as lp]
            [sutra-ui.object.card :as oc]
            [sutra-ui.object.open-in :as open-in]
            [sutra-ui.ui.size :as groesse]
            [sutra-ui.object.page :as object-page]
            [sutra-ui.apps.object-view.new-view-modal :as new-view-modal]
            [sutra-ui.apps.object-view.registry :as reg]
            [sutra-ui.apps.object-view.actions :as actions]
            [sutra-ui.apps.object-view.relations :as relations]
            [sutra-ui.apphost.client :as sdk-client]
            [sutra-ui.apphost.registry :as capps]
            [sutra-ui.apphost.view-state :as view-state]
            [sutra.types.rid :as rid]
            [sutra-ui.apps.object-view.documents]
            [sutra-ui.apps.object-view.matrix]
            [sutra-ui.apps.object-view.floorplan]))

(defn sv-texts []
  {:properties (i18n/t :ui/sv-properties)
   :actions    (i18n/t :ui/sv-actions)
   :no-action  (i18n/t :ui/sv-no-action)})

(defn defined-view
  [object-type form-factor]
  (if-let [app (catalog/object-view-app object-type form-factor)]
    (cond
      (:layout app)                    (lay/lower-view-body (:layout app))
      (reach/elsewhere? app :layout)   ::pending)
    (when-not (seq (:object-types @catalog/canonical-schema)) ::pending)))

(defn pending? [view] (= ::pending view))

(defn top-items
  [view]
  (cond
    (nil? view)                                   []
    (and (= :rows (->kw (:kind view)))
         (vector? (:items view)))                 (:items view)
    :else                                         [view]))

(defn with-items
  [view items]
  (if (and (= :rows (->kw (:kind view))) (vector? (:items view)))
    (assoc view :items (vec items))
    {:kind :rows :items (vec items)}))

(defn item-label
  [item]
  (or (not-empty (str (:title item)))
      (some-> (:widget item) ->kw reg/label-of)
      (some-> (:kind item) name)
      "?"))

(defn- open-configure! [object-type view]
  (ov-model/send! [:object-view.layout/configure object-type view (top-items view)]))

(defn explore-in-graph!
  [object-type row]
  (instances/clear-instance-filter!)
  (canvas/seed-add! object-type row)
  (canvas/expand-around! (str (:id row)) object-type)
  (shell/enter! :instance-canvas))

(defn- author-menu
  [object-type view & [{:keys [instance]}]]
  (let [plural (oc/type-label object-type true)
        opens  (when instance
                 (seq (open-in/menu-items {:kind :object :object-type object-type
                                           :id (str (:id instance))})))]
    [ui/popover
     {:placement :bottom-end
      :class "explorer-action__menu"
      :content
      [ui/menu
       (when instance
         [ui/menu-item {:text     (i18n/t :ui/ov-explore-graph)
                        :on-click #(explore-in-graph! object-type instance)}])
       (when opens
         (into [:<> [ui/menu-divider {:title (i18n/t :ui/open-in-menu)}]] opens))
       [ui/menu-divider {:title (i18n/t :ui/ov-layout-scope {:types plural})}]
       (when view
         [ui/menu-item {:text     (i18n/t :ui/ov-layout-edit)
                        :on-click #(open-configure! object-type view)}])
       (when view
         [ui/menu-item {:text     (i18n/t :ui/ov-layout-add)
                        :on-click #(new-view-modal/open! object-type view)}])]}
     [ui/tooltip
      {:content (i18n/t :ui/ov-layout-menu-hint {:types plural})}
      [ui/button {:icon :more :outlined true :small true}]]]))

(defn- dashboard-header [object-type instance facts view]
  (let [sub    (sv/subtitle-text instance (lp/type-entry facts :property-display object-type)
                                 (:subtitle (:standard-view facts)) (rctx/current))
        phone? (groesse/kompakt?)]
    [:div {:class "explorer-dashboard__header"}
     [oc/object-header (if (:standard-views facts)
                         (oc/descriptor-meta facts object-type)
                         (oc/type-meta object-type))
      instance {:subtitle sub}]
     [:div {:class "u-flex-1"}]
     [actions/object-actions-menu object-type instance
      {:placed     (sv/placed-actions view)
       :on-applied #(object-view/refresh-object-view-data! object-type (:id instance))}]
     (when (and instance (not phone?))
       [ui/tooltip {:content (i18n/t :ui/ov-explore-graph-hint)}
        [ui/button
         {:outlined true :small true :intent :success
          :on-click #(explore-in-graph! object-type instance)}
         [:span {:class "u-label-long"} (i18n/t :ui/ov-explore-graph)]
         [:span {:class "u-label-short"} "⬈"]]])
     (when (and instance (not phone?))
       [open-in/menu {:kind :object :object-type object-type :id (str (:id instance))}])
     [author-menu object-type view (when phone? {:instance instance})]]))

(defn- configure-modal []
  (when-let [{ot :object-type view :view draft :draft} (:configure-dashboard @ov-model/ui)]
    (let [send! ov-model/send!
          close! #(send! [:object-view.layout/close :configure-dashboard])]
      [overlay/form-dialog
       {:on-close close! :class "ov-dialog--layout" :title (i18n/t :ui/ov-layout-edit-title)
        :ok-label (i18n/t :ui/save)
        :on-ok #(send! [:object-view.layout/save ot :full (with-items view draft)
                        :configure-dashboard])}
       [:<>
        [:div {:class "explorer-configure__subtitle"}
         (i18n/t :ui/ov-layout-scope {:types (oc/type-label ot true)})]
        (if (empty? draft)
          [:div {:class "explorer-configure__empty"}
           (i18n/t :ui/ov-layout-empty)]
          (into [:div {:class "explorer-configure__list"}]
                (for [[idx item] (map-indexed vector draft)
                      :let [last? (= idx (dec (count draft)))
                            label (item-label item)]]
                  ^{:key idx}
                  [:div
                   {:class (cond-> "explorer-configure__row"
                             (:hidden? item) (str " explorer-configure__row--hidden"))}
                   [ui/button {:icon :chevron-up :outlined true :small true :disabled (zero? idx)
                               :on-click #(send! [:object-view.layout/move idx (dec idx)])}]
                   [ui/button {:icon :chevron-down :outlined true :small true :disabled last?
                               :on-click #(send! [:object-view.layout/move idx (inc idx)])}]
                   [ui/checkbox
                    {:class "explorer-configure__visible u-m-0"
                     :checked (not (:hidden? item))
                     :label (i18n/t :ui/ov-layout-visible)
                     :on-change #(send! [:object-view.layout/toggle-hidden idx])}]
                   [:span {:class "explorer-configure__slot-label"
                           :title (name (or (some-> (:widget item) ->kw)
                                            (some-> (:kind item) ->kw)
                                            :item))}
                    label]
                   [ui/tooltip
                    {:content (i18n/t :ui/ov-layout-remove-hint)}
                    [ui/button
                     {:outlined true :small true :intent :danger
                      :on-click #(send! [:object-view.layout/remove idx label])}
                     (str "× " (i18n/t :ui/ov-layout-remove))]]])))]])))

(def ^:private panel-peers 5)

(defn- panel-links [object-type instance-id]
  (instances/ensure-object-links! instance-id object-type)
  (let [groups (relations/link-groups instance-id)]
    (if (empty? groups)
      (or (relations/links-failure instance-id)
          [:div {:class "objectview-panel__links"} (i18n/t :ui/ov-links-none)])
      (into [:div {:class "objectview-panel__linkgroups"}]
            (for [{:keys [side far-type peers]} groups
                  :let [more (- (count peers) panel-peers)]]
              ^{:key (str (:api-name side))}
              [:div {:class "objectview-panel__linkgroup"}
               [:div {:class "objectview-panel__linkhead" :title (name (:api-name side))}
                (:label side) [:span {:class "objectview-links__count"} (count peers)]]
               (into [:div {:class "objectview-panel__peers"}]
                     (for [{pid :id row :row t :type} (take panel-peers peers)]
                       ^{:key (str pid)}
                       [oc/object-link (or t far-type) pid {:row row}]))
               (when (pos? more)
                 [:div {:class "u-muted"} (i18n/t :ui/ov-links-more {:n more})])])))))

(declare panel* live-of live-sets live-watch)

(defn- configured-panel
  [object-type instance layout]
  (let [m (oc/type-meta object-type)
        k (:type m)]
    (capps/frame {}
                 ^{:key [object-type (:id instance) (hash layout)]}
                 [capps/render {:layout           {:widget      :object-view
                                                   :object      {:from k}
                                                   :form-factor :panel}
                                :object-views     {k {:panel layout}}
                                :property-display {k (:display m)}
                                :object-titles    {k (:title-key m)}
                                :around-fn        object-page/around-read!
                                :object           instance
                                :live             (:live (catalog/object-view-app object-type
                                                                                  :panel))
                                :reload           (fn []
                                                    (.then (object-view/read-object-view
                                                            object-type (:id instance))
                                                           #(when-let [o (:object %)]
                                                              {k [(assoc o
                                                                         :object-type-ref
                                                                         k)]})))}
                  {k [(assoc instance :object-type-ref k)]}])))

(defn panel
  [object-type instance]
  (let [layout (when instance (defined-view object-type :panel))]
    (cond
      (pending? layout)
      [busy/block (i18n/t :ui/ov-loading)]

      layout
      [:div {:class "objectview-panel"}
       (when-let [live (live-of (catalog/object-view-app object-type :panel))]
         (when-let [sets (live-sets object-type (:id instance) instance live)]
           ^{:key [object-type live sets]} [live-watch live sets]))
       [configured-panel object-type instance layout]
       [ui/button
        {:intent :success :small true :on-click #(shell/open-object! object-type (:id instance))}
        (i18n/t :ui/ov-open-full)]]

      :else
      (panel* object-type instance))))

(defn- panel*
  [object-type instance]
  (if (nil? instance)
    [ui/empty-state {:icon :selection :title (i18n/t :ui/ov-panel-empty)}]
    (let [m       (oc/type-meta object-type)
          entries (remove #(= (:prop %) (->kw (:title-key m)))
                          (sv/property-entries instance (:display m) {} (rctx/current)))]
      [:div {:class "objectview-panel"}
       [oc/object-header m instance]
       [display/property-list (sort-by (comp not :prominent?) entries) {}]
       [:div {:class "objectview-panel__section"} (i18n/t :ui/ov-linked-objects)]
       [panel-links object-type (:id instance)]
       [ui/button
        {:intent :success :small true :on-click #(shell/open-object! object-type (:id instance))}
        (i18n/t :ui/ov-open-full)]])))

(defn set-panel
  [object-type rows]
  (let [m    (oc/type-meta object-type)
        k    (:type m)
        view (or (defined-view object-type :panel-set)
                 (sv/set-node (:display m)
                              {:title-key (:title-key m)
                               :texts     {:charts (i18n/t :ui/ov-set-charts)
                                           :list   (i18n/t :ui/ov-set-list)}}))
        rows (mapv #(assoc % :object-type-ref k) rows)]
    [:div {:class "objectview-panel"}
     [:div {:class "objectview-panel__section"}
      (str (count rows) " " (oc/type-label object-type (not= 1 (count rows))))]
     (if (pending? view)
       [busy/block (i18n/t :ui/ov-loading)]
       (capps/frame {}
                    ^{:key [object-type (hash view) (count rows)]}
                    [capps/render {:layout           view
                                   :property-display {k (:display m)}
                                   :object-titles    {k (:title-key m)}}
                     {k rows vars/view-objects rows}]))]))

(defn- sidebar-node
  [sidebar object-type instance]
  (when (and (seq sidebar) instance)
    (sb/node sidebar
             {:id        (some-> (:id instance) str)
              :type-name (name object-type)
              :values    (merge (v instance) instance)}
             {:reachable? pstate/app-reachable?
              :texts      {:locked (i18n/t :ui/ov-sidebar-locked)}})))

(defn- live-sets
  [object-type id instance {:keys [types object-sets]}]
  (let [this (fn [ref]
               (if (and (map? ref) (contains? ref :this))
                 (let [p (keyword (name (:this ref)))]
                   (if (= :id p) (str id) (get (v instance) p (get instance p))))
                 ref))]
    (cond
      (seq object-sets) nil
      (seq types)       []
      :else             [(vars/watched-set object-type {:where [[:id := {:this :id}]]} this)])))

(defn- live-watch
  [live sets]
  (r/with-let [stop (sdk-client/watch!
                     nil
                     {:types       (map name (:types live))
                      :object-sets sets
                      :min-seconds (:min-seconds live)
                      :keep-paused? true
                      :on-change   (fn [_] (sdk-client/refresh-page!))})]
    nil
    (finally (stop))))

(defn- live-of
  [app]
  (let [l (:live app)] (if (true? l) {} (when (map? l) l))))

(defn- object-page
  [object-type id entry]
  (let [facts (:facts entry)
        app   (get-in facts [:object-page :app])
        live  (live-of app)
        base  (:view (decisions/decide
                      :view/object-page object-type facts
                      (lp/type-entry facts :property-display object-type)
                      {:actions? false :texts (sv-texts)}))
        view  (some-> base (sb/with-sidebar (sidebar-node (:sidebar app) object-type
                                                          (:instance entry))))]
    [:div {:class "explorer-workspace objectview-page"}
     (when-let [sets (when live (live-sets object-type id (:instance entry) live))]
       ^{:key [object-type live sets]} [live-watch live sets])
     [dashboard-header object-type (:instance entry) facts base]
     [:div {:class "explorer-workspace__body"}
      (cond
        (nil? (:data entry))
        [busy/block (i18n/t :ui/ov-loading)]

        (nil? view)
        [:div {:class "explorer-pane__error"} (i18n/t :ui/ov-no-view)]

        :else
        (capps/frame {}
                     ^{:key [object-type id (hash view)]}
                     [capps/render (-> (object-page/page-descriptor
                                        @catalog/canonical-schema object-type id entry app view)
                                       (assoc :preview-rows 4))
                      (:data entry)]))]]))

(defn full
  []
  (let [opened (atom nil)]
    (fn []
      (let [ot0      (get-in @ov-model/ui [:object-view :object-type])
            ot       (some-> ot0 catalog/name-of)
            resolved (when (and ot (not (rid/rid? ot))) ot)
            id       (some-> (get-in @ov-model/ui [:object-view :instance-id]) str)
            entry    (when (and resolved id) (get @object-view/object-view-data [resolved id]))
            subject  (sv/subject {:asked ot0 :resolved resolved :catalog @catalog/rid->name
                                  :id id :entry entry})]
        (when (and resolved id (not= @opened [resolved id]))
          (reset! opened [resolved id])
          (view-state/module! [::object resolved id])
          (when-not (object-view/object-view-fresh? resolved id)
            (object-view/refresh-object-view-data! resolved id)))
        [:div {:class "objectview-root"}
         (case (:show subject)
           :none
           [ui/empty-state {:icon :selection :title (i18n/t :ui/ov-none-selected)}]

           :loading
           [busy/block (i18n/t :ui/ov-loading)]

           :unknown-type
           [:div {:class "explorer-pane__error"}
            (i18n/t :ui/ov-unknown-type {:type (str ot0)})]

           :failed
           [:div {:class "explorer-pane__error"}
            (i18n/t :ui/ov-load-failed {:error (:error subject)})]

           :not-found
           [:div {:class "explorer-pane__error"}
            (i18n/t :ui/ov-not-found {:type (oc/type-label resolved)})]

           :page
           [object-page resolved id entry])
         [new-view-modal/root]
         [configure-modal]
         [actions/launcher-modal]]))))
