(ns sutra-ui.builder.bottom-panel.dataset
  (:require [sutra.i18n.core :as i18n]
            [sutra-ui.builder.bottom-panel.common :as common]
            [sutra-ui.builder.bottom-panel.grid :as grid]
            [sutra-ui.builder.bottom-panel.transform :as transform]
            [sutra-ui.ui.busy :as busy]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.lineage :as lineage]
            [sutra-ui.state.panel :as panel]))

(defn- grid-columns [cols]
  (mapv (fn [{nm :attribute-name t :attribute-type}] {:name nm :type t}) cols))

(defn- row-value
  [row c]
  (if (contains? row c)
    (get row c)
    (get-in row [:values c])))

(defn as-of-data
  [d-kw selected-run]
  (let [commit (:snapshot-id selected-run)
        {:keys [loading? error rows columns]}
        (when commit (panel/panel :dataset-data-at commit d-kw))]
    [:div
     [:div {:class "panel-asof__status u-muted"}
      (cond
        (not commit) (i18n/t :ui/pick-run-for-rows)
        loading?     [busy/inline (i18n/t :ui/loading)]
        error        (i18n/t :ui/error-prefix {:error error})
        :else        (i18n/t :ui/rows-in-table-as-of
                             {:n (count rows) :table (name d-kw)
                              :commit (common/short-commit commit)}))]
     (cond
       (or (not commit) error) nil

       (and (not loading?) (empty? rows))
       [:div {:class "u-italic u-muted"}
        (i18n/t :ui/rows-none-at-this-commit)]

       (seq rows)
       [grid/preview-grid {:columns  (grid-columns columns)
                           :rows     rows
                           :value-of row-value
                           :lead     {:label "id" :of #(common/short-uuid (:id %))}}])]))

(defn data-tab [_d-kw]
  (let [d @data/dataset-preview-data]
    [:div {:class "panel-tab__body"}
     (cond
       (or (nil? d) (:loading? d))
       [busy/block (i18n/t :ui/loading)]

       (:error d)
       [:div {:class "u-italic u-muted"}
        (i18n/t :ui/error-prefix {:error (:error d)})]

       (empty? (:columns d))
       [:div {:class "u-italic u-muted"}
        (i18n/t :ui/no-columns-run-producer)]

       :else
       [grid/preview-grid {:columns  (grid-columns (:columns d))
                           :rows     (:rows d)
                           :value-of row-value}])]))

(defn about-tab [d-kw]
  (let [spec (get-in @catalog/canonical-schema [:datasets d-kw])]
    [:div {:class "panel-tab__body"}
     (when (:description spec)
       [:div {:class "u-mb-8 u-muted"}
        (:description spec)])
     [common/attr-table :ui/column (:columns spec)]]))

(defn code-tab [d-kw]
  (if-let [t-kw (lineage/dataset-producer-transform @catalog/schema d-kw)]
    [transform/code-tab t-kw true]
    [:div {:class "panel-tab__body"}
     [:div {:class "u-italic u-muted"}
      (i18n/t :ui/no-producing-transform-dataset)]]))
