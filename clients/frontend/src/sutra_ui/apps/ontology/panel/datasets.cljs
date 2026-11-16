(ns sutra-ui.apps.ontology.panel.datasets
  (:require [clojure.string]
            [sutra-ui.state.data :as data]
            [sutra.kit.remote :as rd]
            [sutra-ui.health.badge :as hb]
            [sutra-ui.ui.busy :as busy]
            [sutra.i18n.core :as i18n]
            [sutra-ui.ui.core :as ui :refer [tag]]
            [sutra-ui.ui.table :as table]
            [sutra-ui.apps.ontology.panel.common :refer [section-shell]]))

(defn- audit-op-badge [op]
  [tag {:minimal true} (or (some-> op clojure.core/name) "—")])

(defn- audit-row [{:keys [seq actor op target-type target-id detail created-at]}]
  [:div {:class "ontology-audit__row"}
   [audit-op-badge op]
   [:div
    [:div {:class "ontology-audit__target"}
     (some-> target-type str) (when target-id [:span {:class "ontology-audit__target-id"}
                                               (str " " target-id)])]
    (when (seq (str detail))
      [:div {:class "ontology-audit__detail"}
       (pr-str detail)])]
   [:div {:class "ontology-audit__meta"}
    [:div {:class "ontology-audit__actor"} (str actor)]
    [:div (some-> created-at str (subs 0 19) (clojure.string/replace "T" " "))]]])

(defn audit-pane []
  (data/refresh-audit!)
  (fn []
    (let [rows (rd/value-or @data/audit-entries [])]
      [section-shell
       [:div
        [:div {:class "u-flex-between"}
         [:div {:class "u-section-label"}
          (str (i18n/t :ui/audit-log) " (" (count rows) ")")]
         [ui/button {:minimal true :small true :icon :refresh :on-click #(data/refresh-audit!)}
          (i18n/t :ui/refresh)]]
        (if (empty? rows)
          [:div {:class "u-text-empty"}
           (i18n/t :ui/audit-log-none)]
          (for [r rows] ^{:key (:seq r)} [audit-row r]))]])))

(defn- dataset-kind-badge [kind]
  (let [[intent label] (case kind
                         :object-backing   [:primary "object"]
                         :transform-output [:warning "transform"]
                         :source-staged    [:success "source"]
                         [nil (some-> kind clojure.core/name)])]
    [tag {:minimal true :intent intent} label]))

(defn- dataset-preview-panel []
  (let [d @data/dataset-preview-data]
    (when d
      [:div {:class "ontology-dataset__preview"}
       (cond
         (:loading? d) [busy/block {:class "ontology-dataset__preview-loading"}
                        (i18n/t :ui/loading-preview)]
         (:error d) [:div {:class "ontology-dataset__preview-error"} (:error d)]
         :else
         (let [rows (:rows d)
               cols (mapv :attribute-name (:columns d))]
           [:div
            [:div {:class "ontology-list__header"}
             (i18n/t :ui/preview-rows {:n (count rows)})]
            (if (empty? cols)
              [:div {:class "u-text-empty"}
               (i18n/t :ui/no-schema-columns)]
              [table/data-table
               {:class   "ontology-dataset__table"
                :scroll? true
                :rows    (take 50 rows)
                :cols    (for [c cols]
                           {:key    c
                            :label  (clojure.core/name c)
                            :render #(str (or (get % c) (get-in % [:values c]) ""))})}])]))])))

(defn- dataset-row [id spec]
  (let [selected? (= id (:id @data/dataset-preview-data))]
    [:div {:on-click #(data/load-dataset-preview! id)
           :class    ["ontology-dataset__row" (when selected? "ontology-dataset__row--on")]}
     [dataset-kind-badge (:kind spec)]
     (when-let [k (:dataset-name spec)] [hb/health-dot :dataset (keyword k)])
     (let [code  (some-> (:dataset-name spec) clojure.core/name)
           title (not-empty (:title spec))]
       [:span {:class "ontology-dataset__row-name"}
        (or title code)
        (when title
          [:span {:class "u-ml-6 u-muted"} code])])
     [:span {:class "ontology-dataset__row-cols"}
      (i18n/t :ui/cols-count {:n (count (:columns spec))})]]))

(defn datasets-pane []
  (data/refresh-datasets!)
  (fn []
    (let [ds (sort-by (comp str :dataset-name val) (rd/value-or @data/datasets {}))]
      [section-shell
       [:div
        [:div {:class "ontology-list__header-row"}
         [:div {:class "u-section-label"}
          (str (i18n/t :ui/datasets) " (" (count ds) ")")]
         [ui/button
          {:minimal true :small true :icon :refresh :on-click #(data/refresh-datasets!)}
          (i18n/t :ui/refresh)]]
        [:div {:class "ontology-dataset__intro"}
         (i18n/t :ui/datasets-intro)]
        (if (empty? ds)
          [:div {:class "u-text-empty"}
           (i18n/t :ui/datasets-none)]
          (for [[id spec] ds] ^{:key id} [dataset-row id spec]))
        [dataset-preview-panel]]])))
