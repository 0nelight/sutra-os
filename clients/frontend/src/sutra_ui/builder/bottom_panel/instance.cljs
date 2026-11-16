(ns sutra-ui.builder.bottom-panel.instance
  (:require [sutra-ui.ui.core :as ui]
            [sutra.capps.table :as tbl]
            [sutra.i18n.core :as i18n]
            [sutra-ui.builder.bottom-panel.common :as common]
            [sutra-ui.object.card :as oc]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.instances :as instances]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.ui.style :as style]))

(defn- end-link
  [end-id]
  (if-let [row (instances/object-by-id end-id)]
    [oc/object-link (:object-type row) (:id row) {:row row}]
    [:span {:class "u-mono-12" :title (str end-id)} (common/short-uuid end-id)]))

(defn instance-detail [id]
  (if-let [row (instances/object-by-id id)]
    (let [type-kw (:object-type row)
          spec  (get-in @catalog/canonical-schema [:object-types type-kw])
          attrs (:attribute-list spec)
          vs    (:values row)
          edges (instances/edges-touching id)
          id-str (str id)
          meta  (oc/type-meta type-kw)]
      [:div {:class "panel-tab__body"}
       [:div {:class "panel-meta--start"}
        [:div [:div {:class style/label} (i18n/t :ui/object-type)]
         [ui/tooltip {:content (name type-kw)}
          [:div {:class "panel-meta__value"}
           (oc/type-label meta)]]]
        [:div [:div {:class style/label} (i18n/t :ui/name)]
         [:div {:class "panel-meta__value--plain"} (catalog/instance-display-name row)]]
        [common/copy-rid (:id row)]
        [ui/tooltip {:content (i18n/t :ui/writeback-hint)}
         [ui/button {:small    true
                     :on-click #(shell/open-writeback-modal!
                                 {:object-type type-kw :object-id (:id row)})}
          (i18n/t :ui/writeback)]]
        [:div {:class "u-ml-auto"}
         [ui/tooltip {:content (i18n/t :ui/full-dashboard-hint)}
          [ui/button {:small true :on-click #(shell/open-object! type-kw id-str)}
           (i18n/t :ui/full-dashboard)]]]]
       [:h4 {:class "panel-section__title u-muted"}
        (i18n/t :ui/attributes)]
       (if (seq attrs)
         [ui/html-table {:compact true :class style/table-root}
          [:thead
           [:tr
            [:th (i18n/t :ui/name)]
            [:th (i18n/t :ui/value)]]]
          [:tbody
           (for [{nm :attribute-name} attrs]
             ^{:key (str nm)}
             [:tr [:td {:title (name nm)}
                   (or (not-empty (get-in meta [:display nm :name])) (tbl/humanize (name nm)))]
              [:td (common/cell-text (get vs nm))]])]]
         [:div {:class "u-italic u-muted"}
          (i18n/t :ui/no-attributes)])
       [:h4 {:class "panel-section__title u-muted"}
        (str (i18n/t :ui/links) " · " (count edges))]
       (if (seq edges)
         [ui/html-table {:compact true :class style/table-root}
          [:thead
           [:tr
            [:th (i18n/t :ui/link-type)]
            [:th (i18n/t :ui/other-end)]]]
          [:tbody
           (doall
            (for [c edges
                  :let [out?  (= id-str (str (:from-id c)))
                        other (if out? (:to-id c) (:from-id c))
                        side  (catalog/edge-side c id-str)]]
              ^{:key (str (:id c))}
              [:tr
               [:td {:title (some-> (:api-name side) name)}
                (or (:label side) (str (:link-type-ref c)))]
               [:td [end-link other]]]))]]
         [:div {:class "u-italic u-muted"}
          (i18n/t :ui/no-links)])])
    [:div {:class "panel-tab__empty u-muted"}
     (i18n/t :ui/object-not-on-branch)]))

(defn link-detail [id]
  (if-let [c (instances/link-by-id id)]
    [:div {:class "panel-tab__body"}
     [:div {:class "panel-meta"}
      [:div [:div {:class style/label} (i18n/t :ui/link-type)]
       (let [ln (catalog/name-of (:link-type-ref c))
             lt (some #(when (= (name ln) (name (:name %))) %) (:links @catalog/schema))]
         [ui/tooltip {:content (name ln)}
          [:div {:class "panel-meta__value"}
           (or (not-empty (:forward-name lt)) (tbl/humanize (name ln)))]])]
      [common/copy-rid (:id c)]]
     [:h4 {:class "panel-section__title u-muted"} (i18n/t :ui/endpoints)]
     [ui/html-table {:compact true :class style/table-root}
      [:thead
       [:tr
        [:th (i18n/t :ui/end)]
        [:th (i18n/t :ui/object)]]]
      [:tbody
       [:tr [:td (i18n/t :ui/from)]
        [:td [end-link (:from-id c)]]]
       [:tr [:td (i18n/t :ui/to)]
        [:td [end-link (:to-id c)]]]]]]
    [:div {:class "panel-tab__empty u-muted"}
     (i18n/t :ui/link-not-on-branch)]))
