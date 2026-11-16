(ns sutra-ui.builder.bottom-panel.object-type
  (:require [sutra-ui.ui.remote :as remote]
            [sutra-ui.ui.core :as ui]
            [cljs.pprint]
            [clojure.string :as str]
            [sutra.capps.table :as tbl]
            [sutra.i18n.core :as i18n]
            [sutra.kit.remote :as rd]
            [sutra-ui.builder.bottom-panel.common :as common]
            [sutra-ui.builder.bottom-panel.events :as ev]
            [sutra-ui.ui.table :as table]
            [sutra-ui.builder.bottom-panel.transform :as transform]
            [sutra-ui.ui.busy :as busy]
            [sutra-ui.object.card :as oc]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.panel :as panel]
            [sutra-ui.ui.style :as style]))

(defn about-tab [type-kw spec]
  [:div {:class "panel-tab__body"}
   [:div {:class "panel-meta"}
    [:div
     [:div {:class style/label} (i18n/t :ui/name)]
     [:div {:class "panel-meta__value"}
      (name type-kw)]]
    [common/copy-rid (:id spec)]
    [:div
     [:div {:class style/label} (i18n/t :ui/implements)]
     [:div {:class "panel-meta__value"}
      (if-let [is (seq (:implements spec))]
        (str/join ", " (map name is))
        "—")]]
    [:div
     [:div {:class style/label} (i18n/t :ui/attributes)]
     [:div {:class "panel-meta__value--plain"} (count (:attribute-list spec))]]]
   [:h4 {:class "panel-section__title u-muted"} (i18n/t :ui/columns)]
   (if-let [attrs (seq (:attribute-list spec))]
     [common/attr-table :ui/name attrs]
     [:div {:class "u-italic u-muted"}
      (i18n/t :ui/no-attributes)])])

(defn- prop-th
  [meta nm]
  [:span {:title (name nm)}
   (or (not-empty (get-in meta [:display nm :name])) (tbl/humanize (name nm)))])

(defn- objects-grid
  [type-kw attrs rows]
  (let [meta (oc/type-meta type-kw)]
    [table/data-table
     {:scroll?    true
      :rows       rows
      :row-key    #(str (:id %))
      :cols       (cons {:key    "lead" :label (i18n/t :ui/object)
                         :render #(vector oc/object-link (:type meta) (:id %)
                                          {:meta meta :row %})}
                        (for [{nm :attribute-name} attrs]
                          {:key    (str nm) :label [prop-th meta nm]
                           :render #(common/cell-text (get (:values %) nm))}))}]))

(defn preview-tab [type-kw spec]
  (let [{:keys [loading? error rows total]} (get @panel/preview-instances type-kw)
        attrs (common/match-columns [:preview type-kw] (:attribute-list spec) :attribute-name)]
    [:div {:class "panel-tab__body"}
     [:div {:class "panel-toolbar"}
      [:div {:class "colormenu__mode-label u-muted"}
       (cond loading? [busy/inline (i18n/t :ui/loading)]
             error    (i18n/t :ui/error-prefix {:error error})
             :else    (i18n/t :ui/panel-objects-props
                              {:rows (if (and total (> total (count rows)))
                                       (i18n/t :ui/panel-sample-of {:n (count rows) :total total})
                                       (count rows))
                               :cols (count attrs)}))]
      [common/column-search [:preview type-kw]]
      [ui/button {:small true :on-click #(panel/refresh-preview-instances! type-kw)}
       (i18n/t :ui/refresh)]]
     (cond
       (and (not loading?) (empty? rows))
       [:div {:class "u-italic u-muted"}
        (i18n/t :ui/panel-no-objects)]

       (seq rows)
       [objects-grid type-kw attrs rows])]))

(defn as-of-data-tab [type-kw run]
  (let [commit (:snapshot-id run)
        cache  (when commit (panel/panel :data-at commit type-kw))
        {:keys [loading? error rows unaddressable?]} cache
        spec-cache (when commit (panel/panel :schema-at commit type-kw))
        spec       (or (:spec spec-cache)
                       (get-in @catalog/canonical-schema [:object-types type-kw]))]
    [:div {:class "panel-tab__body--sans"}
     [:div {:class "panel-asof__status u-muted"}
      (cond (not commit)   (i18n/t :ui/run-no-commit)
            unaddressable? (i18n/t :ui/run-snapshot-not-commit-objects {:snapshot commit})
            loading?       [busy/inline (i18n/t :ui/loading)]
            error          (i18n/t :ui/error-prefix {:error error})
            :else          (i18n/t :ui/rows-at-commit
                                   {:n (count rows) :commit (subs (str commit) 0 7)}))]
     (cond
       (or (not commit) unaddressable? error)
       nil

       (and (not loading?) (empty? rows))
       [:div {:class "u-italic u-muted"}
        (i18n/t :ui/rows-none-at-commit)]

       (seq rows)
       [objects-grid type-kw (:attribute-list spec) rows])]))

(defn as-of-schema-tab [type-kw run]
  (let [commit (:snapshot-id run)
        cache  (when commit (panel/panel :schema-at commit type-kw))
        {:keys [loading? error spec unaddressable?]} cache]
    [:div {:class "panel-tab__body--sans"}
     [:div {:class "panel-asof__status u-muted"}
      (cond (not commit)   (i18n/t :ui/run-no-commit)
            unaddressable? (i18n/t :ui/run-snapshot-not-commit-schema {:snapshot commit})
            loading?       [busy/inline (i18n/t :ui/loading)]
            error          (i18n/t :ui/error-prefix {:error error})
            spec           (i18n/t :ui/schema-at {:commit (subs (str commit) 0 7)})
            :else          (i18n/t :ui/no-spec-at-commit))]
     (when spec
       [:<>
        [:div {:class "panel-meta--mb"}
         [:div [:div {:class style/label} (i18n/t :ui/implements)]
          [:div {:class "panel-meta__value"}
           (if-let [is (seq (:implements spec))]
             (str/join ", " (map name is)) "—")]]
         [:div [:div {:class style/label} (i18n/t :ui/attributes)]
          [:div {:class "panel-meta__value--plain"}
           (count (:attribute-list spec))]]]
        [common/attr-table :ui/name (:attribute-list spec)]
        (when-let [es (:external-source spec)]
          [:<>
           [:h4 {:class "panel-section__title u-muted"}
            (i18n/t :ui/external-source)]
           [:pre {:class "panel-code__pre"}
            (with-out-str (cljs.pprint/pprint es))]])])]))

(defn- code-tab [spec]
  (let [es (:external-source spec)]
    [:div {:class "panel-tab__body--mono"}
     [:div {:class "panel-code__note u-muted"}
      (i18n/t :ui/external-source-readonly)]
     [:pre {:class "panel-code__pre"}
      (with-out-str (cljs.pprint/pprint es))]]))

(defn object-code-tab
  [type-kw spec]
  (if-let [t-kw (common/producer-of :object-type type-kw)]
    [transform/code-tab t-kw true]
    [code-tab spec]))

(def edits-tab
  (machine/entering
   db/sutra (fn [type-kw] [:shared.panel/edits type-kw])
   (fn [type-kw]
     (let [d (or (ev/at :edits type-kw) rd/loading)]
       (remote/show
        d
        (fn [d]
          (let [txns (:transactions d)
                prov (:provenance d)
                kcol (fn [k]
                       (case (str k) "delete" "var(--c-danger)" "create" "var(--c-success)"
                             "var(--c-muted)"))]
            [:div {:class "panel-edits" :style {:padding "var(--sp-3) var(--sp-4)"}}
             [:div {:style {:margin-bottom "var(--sp-2)" :font-weight 600}}
              (i18n/t :ui/edit-transactions {:n (count txns)})]
             (if (empty? txns)
               [:div
                {:class "panel-tab__empty" :style {:color "var(--c-muted)"
                                                   :margin-bottom "var(--sp-4)"}}
                (i18n/t :ui/edits-none)]
               [ui/html-table {:compact true :class style/table-root
                               :style   {:margin-bottom "var(--sp-4)"}}
                [:thead [:tr [:th (i18n/t :ui/type)] [:th "Δ"] [:th (i18n/t :ui/actor)]
                         [:th (i18n/t :ui/commit)] [:th (i18n/t :ui/time)]]]
                (into [:tbody]
                      (for [t txns]
                        ^{:key (str (:txn_rid t))}
                        [:tr
                         [:td (str (:type t))]
                         [:td (str (:row_delta t))]
                         [:td (str (:job_rid t))]
                         [:td (common/short-uuid (:snapshot_id t))]
                         [:td (str (:started_at t))]]))])
             [:div {:style {:margin-bottom "var(--sp-2)" :font-weight 600}}
              (i18n/t :ui/edited-rows {:n (count prov)})]
             (if (empty? prov)
               [:div {:class "panel-tab__empty u-muted"}
                (i18n/t :ui/edited-rows-none)]
               [ui/html-table {:compact true :class style/table-root}
                [:thead [:tr [:th "PK"] [:th (i18n/t :ui/kind)] [:th (i18n/t :ui/last)]]]
                (into [:tbody]
                      (for [p prov]
                        ^{:key (str (:pk p))}
                        [:tr
                         [:td (common/short-uuid (:pk p))]
                         [:td {:style {:color (kcol (:edit_kind p))}} (str (:edit_kind p))]
                         [:td (str (:updated_at p))]]))])]))
        {:loading (i18n/t :ui/edit-history-loading)
         :failed  (i18n/t :ui/edit-history-unavailable)
         :class   "panel-tab__empty"})))))
