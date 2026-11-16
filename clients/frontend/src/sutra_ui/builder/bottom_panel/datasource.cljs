(ns sutra-ui.builder.bottom-panel.datasource
  (:require [sutra-ui.ui.core :as ui]
            [sutra.i18n.core :as i18n]
            [sutra-ui.builder.bottom-panel.common :as common]
            [sutra-ui.ui.table :as table]
            [sutra-ui.ui.busy :as busy]
            [sutra-ui.state.panel :as panel]
            [sutra-ui.ui.style :as style]))

(defn- staging-grid
  [cols rows]
  [table/data-table {:scroll?    true
                     :rows       rows
                     :cols       (for [[i c] cols]
                                   {:key i :label c
                                    :render #(common/cell-text (nth % i nil))})}])

(defn data-tab
  [ds-kw]
  (let [{:keys [loading? error tables]} (panel/panel :datasource-preview ds-kw)]
    [:div {:class "panel-tab__body--sans"}
     [:div {:class "panel-toolbar"}
      [:div {:class "colormenu__mode-label u-muted"}
       (cond loading? [busy/inline (i18n/t :ui/loading)]
             error    (i18n/t :ui/error-prefix {:error error})
             (empty? tables)
             (i18n/t :ui/staging-tables-none)
             :else (i18n/t (if (= 1 (count tables))
                             :ui/staging-tables-one :ui/staging-tables-many)
                           {:n (count tables)}))]
      [common/column-search [:preview ds-kw]]
      [ui/button {:small true :on-click #(panel/load-panel! :datasource-preview ds-kw)}
       (i18n/t :ui/refresh)]]
     (for [{:keys [table columns rows row-count truncated?]} tables
           :let [cols (common/match-columns [:preview ds-kw]
                                            (map-indexed (fn [i c] [i c]) columns)
                                            second)]]
       ^{:key table}
       [:div {:class "panel-staging-table"}
        [:div {:class "panel-staging-table__caption u-muted"}
         (str (i18n/t :ui/table-rows-columns
                      {:table table :rows row-count :columns (count cols)})
              (when truncated? (i18n/t :ui/truncated-suffix)))]
        (if (empty? rows)
          [:div {:class "panel-staging-table__empty u-muted"}
           (i18n/t :ui/empty-parens)]
          [staging-grid cols rows])])]))

(defn as-of-data
  [ds-kw selected-run]
  (let [commit (:snapshot-id selected-run)
        {:keys [loading? error tables]}
        (when commit (panel/panel :datasource-data-at commit ds-kw))]
    [:div
     [:div {:class "panel-asof__status u-muted"}
      (cond
        (not commit) (i18n/t :ui/pick-run-for-rows)
        loading?     [busy/inline (i18n/t :ui/loading)]
        error        (i18n/t :ui/error-prefix {:error error})
        (empty? tables)
        (i18n/t :ui/staging-tables-none-as-of {:commit (subs (str commit) 0 7)})
        :else
        (str (i18n/t (if (= 1 (count tables))
                       :ui/staging-tables-one :ui/staging-tables-many)
                     {:n (count tables)})
             (i18n/t :ui/as-of-suffix {:commit (subs (str commit) 0 7)})))]
     (for [{:keys [table columns rows row-count truncated? error]} tables]
       ^{:key table}
       [:div {:class "panel-staging-table"}
        [:div {:class "panel-staging-table__caption u-muted"}
         (str table
              (when (not error)
                (str (i18n/t :ui/rows-count-suffix {:rows row-count})
                     (when truncated? (i18n/t :ui/truncated-suffix)))))]
        (cond
          error
          [:div {:class "panel-staging-table__error"}
           (i18n/t :ui/not-at-commit {:error error})]

          (empty? rows)
          [:div {:class "panel-staging-table__empty u-muted"}
           (i18n/t :ui/empty-parens)]

          :else
          [staging-grid (map-indexed vector columns) rows])])]))

(defn- connection
  [spec]
  (if (= :csv (:type spec))
    [:div
     [:div {:class style/label} (i18n/t :ui/path)]
     [:div {:class "panel-meta__value--break"}
      (or (:path spec) "—")]]
    [:div
     [:div {:class style/label} (i18n/t :ui/jdbc-url)]
     [:div {:class "panel-meta__value--break"}
      (or (:jdbc-url spec) "—")]
     (when (seq (:user spec))
       [:<>
        [:div {:class [style/label "u-mt-10"]} (i18n/t :ui/user)]
        [:div {:class "u-mono-12"}
         (:user spec)]])
     (when (seq (:password spec))
       [:<>
        [:div {:class [style/label "u-mt-10"]} (i18n/t :ui/password)]
        [:div {:class "u-mono-12 u-muted"}
         "••••••"]])]))

(defn as-of-spec
  [ds-kw selected-run]
  (let [commit (:snapshot-id selected-run)
        {:keys [loading? error spec]}
        (when commit (panel/panel :datasource-spec-at commit ds-kw))]
    [:div
     [:div {:class "panel-asof__status u-muted"}
      (cond
        (not commit) (i18n/t :ui/pick-run-for-spec)
        loading?     [busy/inline (i18n/t :ui/loading)]
        error        (i18n/t :ui/error-prefix {:error error})
        (not spec)   (i18n/t :ui/datasource-absent-at-commit)
        :else        (i18n/t :ui/spec-as-of {:commit (subs (str commit) 0 7)}))]
     (when spec
       [:div
        [:div {:class "panel-meta--mb"}
         [:div
          [:div {:class style/label} (i18n/t :ui/type)]
          [:div {:class "panel-meta__value"}
           (some-> (:type spec) name)]]]
        [connection spec]])]))

(defn about-tab
  [ds-kw spec]
  [:div {:class "panel-tab__body"}
   [:div {:class "panel-meta"}
    [:div
     [:div {:class style/label} (i18n/t :ui/name)]
     [:div {:class "panel-meta__value"}
      (name ds-kw)]]
    [:div
     [:div {:class style/label} (i18n/t :ui/type)]
     [:div {:class "panel-meta__value"}
      (some-> (:type spec) name)]]]
   [:h4 {:class "panel-section__title u-muted"}
    (i18n/t :ui/link)]
   [connection spec]])
