(ns sutra-ui.builder.bottom-panel.grid
  (:require [sutra-ui.ui.core :as ui]
            [sutra-ui.ui.table :as table]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra.i18n.core :as i18n]
            [sutra-ui.builder.bottom-panel.common :as common]
            [sutra-ui.builder.bottom-panel.column-stats :as cstats]
            [sutra-ui.state.catalog :as catalog]))

(defn preview-grid
  [_]
  (let [q         (r/atom "")
        stats-col (r/atom nil)
        counted?  (r/atom false)]
    (fn [{:keys [columns rows value-of lead limit]
          :or   {limit 100}}]
      (let [ql    (str/lower-case (str/trim @q))
            shown (if (str/blank? ql)
                    columns
                    (filterv #(str/includes? (str/lower-case (name (:name %))) ql) columns))]
        [:<>
         [:div {:class "panel-toolbar"}
          [ui/input-group {:value @q
                           :placeholder (i18n/t :ui/search-columns)
                           :on-change #(reset! q (.. % -target -value))
                           :small true
                           :class "panel-health__name-input"
                           :title (i18n/t :ui/search-columns-hint)}]
          [:span {:class "colormenu__mode-label u-muted"}
           (str (i18n/t :ui/columns-shown {:shown (count shown) :total (count columns)})
                (when (> (count rows) limit)
                  (i18n/t :ui/showing-rows-suffix {:n limit})))]
          [:div {:class "u-flex-1"}]
          [ui/tooltip {:content (i18n/t :ui/count-rows-hint)}
           [ui/button {:small true :on-click #(reset! counted? true)}
            (if @counted?
              (i18n/t :ui/rows-count {:n (count rows)})
              (i18n/t :ui/count-rows))]]]
         (when-let [c @stats-col]
           [:div {:style {:padding "var(--sp-2) 0"}}
            [:div {:style {:display "flex" :gap "var(--sp-3)" :align-items "flex-start"}}
             [cstats/stats-card (cstats/stats-of c (map #(value-of % c) rows))]
             [ui/button {:icon :cross :small true :on-click #(reset! stats-col nil)}]]])
         [table/data-table
          {:scroll?    true
           :rows       (take limit rows)
           :cols
           (concat
            (when lead
              [{:key "lead" :label (:label lead) :mono? true :render (:of lead)}])
            (for [{nm :name t :type} shown]
              {:key        (str nm)
               :head-attrs {:style {:cursor "context-menu"}
                            :on-context-menu (fn [e]
                                               (.preventDefault e)
                                               (swap! stats-col #(when (not= % nm) nm)))}
               :label      [ui/tooltip {:content (i18n/t :ui/column-stats-hint)}
                            [:span (name nm)
                             (when t
                               [:span {:class "u-muted u-ml-6 u-fs-12"}
                                (catalog/value-type-label t)])]]
               :render     #(common/cell-text (value-of % nm))}))}]]))))
