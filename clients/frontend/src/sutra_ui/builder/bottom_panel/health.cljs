(ns sutra-ui.builder.bottom-panel.health
  (:require [sutra-ui.ui.core :as ui]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra.i18n.core :as i18n]
            [sutra.kit.remote :as rd]
            [sutra-ui.builder.bottom-panel.events :as ev]
            [sutra-ui.ui.busy :as busy]
            [sutra-ui.health.badge :as hb]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.ui.style :as style]))

(defn- check-form [node-kind node-key]
  (let [nm  (r/atom "")
        op  (r/atom ">=")
        val (r/atom "1")]
    (fn [_ _]
      [:div {:class "panel-health__form"}
       [ui/input-group {:value @nm :placeholder (i18n/t :ui/check-name)
                        :on-change #(reset! nm (.. % -target -value))
                        :small true
                        :class "panel-health__name-input"}]
       [:span {:class "dataconn-importrow__count u-muted"}
        (i18n/t :ui/row-count)]
       [ui/html-select {:value @op :on-change #(reset! op (.. % -target -value))
                        :options [">=" ">" "=" "<=" "<"]}]
       [ui/numeric-input {:value @val :small true :button-position :none
                          :style {:width "60px"}
                          :on-value-change #(reset! val %2)}]
       [ui/button
        {:intent :primary
         :disabled (str/blank? @nm)
         :on-click (fn [_]
                     (db/send! [:shared.panel/add-check node-kind node-key
                                {:name @nm
                                 :expects {:op (keyword @op) :value (js/parseInt @val 10)}}])
                     (reset! nm ""))}
        (i18n/t :ui/add-check)]])))

(def health-tab
  (machine/entering
   db/sutra (fn [kind key] [:shared.panel/health kind key true])
   (fn [node-kind node-key]
     (let [{:keys [show rows]} (rd/list-view (or (ev/at :health [node-kind node-key])
                                                 rd/loading))]
       [:div {:class "panel-health"}
        [check-form node-kind node-key]
        (case show
          :loading [busy/block (i18n/t :ui/loading)]
          :list
          [ui/html-table {:compact true :class style/table-root}
           [:thead [:tr [:th (i18n/t :ui/check)]
                    [:th (i18n/t :ui/status)]
                    [:th (i18n/t :ui/value)]
                    [:th ""]]]
           [:tbody
            (for [{:keys [id name latest] :as c} rows]
              ^{:key id}
              [:tr
               [:td name]
               [:td
                [:span {:class "u-bold" :style {:color (hb/status-color (:status latest))}}
                 (or (:status latest) "—")]]
               [:td {:class style/table-td-mono} (str (:value latest))]
               [:td
                [:span {:class "panel-health__run-link"
                        :on-click #(db/send! [:shared.panel/run-check node-kind node-key id])}
                 (i18n/t :ui/run)]
                [:span {:class "lineage-schedules__delete"
                        :on-click #(db/send! [:shared.panel/delete-check node-kind node-key c])}
                 (i18n/t :ui/delete)]]])]]
          [:div {:class "u-italic u-muted"}
           (i18n/t :ui/health-checks-none)])]))))
