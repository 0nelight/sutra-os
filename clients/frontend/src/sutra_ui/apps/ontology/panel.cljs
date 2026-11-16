(ns sutra-ui.apps.ontology.panel
  (:require [sutra-ui.state.forms :as fs]
            [clojure.string :as str]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.apps.ontology.model :as ont-model]
            [sutra-ui.forms.group :as group-forms]
            [sutra-ui.forms.lists :as entity-lists]
            [sutra-ui.apps.ontology.ot-view :as ot-view]
            [sutra.i18n.core :as i18n]
            [sutra-ui.apps.ontology.panel.actions :refer [action-editor-form action-types-pane]]
            [sutra-ui.apps.ontology.panel.common :refer [left-rail new-button]]
            [sutra-ui.apps.ontology.panel.datasets :refer [audit-pane datasets-pane]]
            [sutra-ui.apps.ontology.panel.detail :refer [interface-detail-page link-detail-page
                                                         object-type-group-detail-page
                                                         object-type-view open-transform!]]
            [sutra-ui.apps.ontology.panel.discover :refer [discover-page]]
            [sutra-ui.apps.ontology.panel.functions :refer [function-detail-page functions-pane]]
            [sutra-ui.apps.ontology.panel.lists :refer [list-pane]]
            [sutra-ui.apps.ontology.panel.packages :refer [packages-pane]]
            [sutra-ui.apps.ontology.panel.search :refer [search-keys search-results]]
            [sutra-ui.resource.ontology-selector :as ontology-selector]
            [sutra-ui.ui.filter :as ui-filter]
            [sutra-ui.ui.page :as page]
            [sutra-ui.apphost.platform :as platform]
            [sutra-ui.apps.ontology.panel.shared-props :refer [shared-properties-pane]]
            [sutra-ui.apps.ontology.panel.transforms :refer [transforms-pane]]))

(defn- action-detail
  [id]
  (when-let [spec (get (catalog/declared-actions) id)]
    (fs/put! action-editor-form (ont-model/action->form id spec))
    (ont-model/edit! [] assoc :editor-open? true))
  (fn [_] [action-types-pane]))

(defn detail-page
  [kind id on-close]
  (case kind
    :object-type     [object-type-view id on-close]
    :link-type [link-detail-page id on-close]
    :interface  [interface-detail-page id on-close]
    :object-type-group [object-type-group-detail-page id on-close]
    :function        [function-detail-page id on-close]
    :action-type     [action-detail id]
    :transform       (do (open-transform! id) [:div])
    [:div]))

(declare close-detail!)

(defn- section-pane [section]
  (let [detail (:detail @ont-model/ui)
        q      (:search @ont-model/ui)]
    (cond
      (not (str/blank? q)) [search-results q]
      detail
      [detail-page (:kind detail) (:id detail) close-detail!]
      :else
      (case section
        :discover         [discover-page]
        :object-types     [list-pane :add-object    entity-lists/object-types-list]
        :object-type-groups [list-pane :add-object-type-group group-forms/object-type-groups-list]
        :link-types [list-pane :add-link entity-lists/link-types-list]
        :interfaces  [list-pane :add-interface entity-lists/interfaces-list]
        :action-types     [action-types-pane]
        :functions        [functions-pane]
        :transforms  [transforms-pane]
        :datasets         [datasets-pane]
        :shared-properties [shared-properties-pane]
        :packages         [packages-pane]
        :audit            [audit-pane]
        [discover-page]))))

(def ^:private section-headers
  {:discover          {:title :ui/discover          :subtitle :ui/discover-sub}
   :object-types      {:title :ui/object-types      :subtitle :ui/object-types-sub}
   :object-type-groups {:title :ui/object-type-groups :subtitle :ui/object-type-groups-sub}
   :link-types        {:title :ui/link-types        :subtitle :ui/link-types-sub}
   :action-types      {:title :ui/action-types      :subtitle :ui/action-types-sub}
   :interfaces        {:title :ui/interfaces        :subtitle :ui/interfaces-sub}
   :functions         {:title :ui/functions         :subtitle :ui/functions-sub}
   :transforms        {:title :ui/transforms        :subtitle :ui/transforms-sub}
   :datasets          {:title :ui/datasets          :subtitle :ui/datasets-sub}
   :shared-properties {:title :ui/shared-properties :subtitle :ui/shared-properties-sub}
   :packages          {:title :ui/packages          :subtitle :ui/packages-sub}
   :audit             {:title :ui/audit-log         :subtitle :ui/audit-log-sub}})

(defn- close-detail! [] (ont-model/edit! [] assoc :detail nil))

(defn- otv-id
  []
  (let [{:keys [detail search]} @ont-model/ui]
    (when (and (= :object-type (:kind detail)) (str/blank? search)) (:id detail))))

(def ^:private otv-open? (reify IDeref (-deref [_] (some? (otv-id)))))

(def ^:private list-open? (reify IDeref (-deref [_] (nil? (otv-id)))))

(defn- search-bar []
  [ui-filter/bar {:value       (:search @ont-model/ui)
                  :placeholder (i18n/t :ui/om-search-placeholder)
                  :on-key-down search-keys
                  :on-search   #(ont-model/edit! [] assoc :search % :search-idx 0 :detail nil)}])

(defn- header-view []
  (if-let [k (otv-id)]
    [ot-view/header k close-detail! [search-bar]]
    (let [{:keys [section detail]} @ont-model/ui
          meta (get section-headers (or section :discover) (:discover section-headers))]
      (when-not detail
        [page/header {:title (i18n/t (:title meta)) :subtitle (i18n/t (:subtitle meta))}
         [ontology-selector/selector] [search-bar] [new-button]]))))

(defn- sidebar-view []
  (if-let [k (otv-id)] [ot-view/sidebar k close-detail!] [left-rail]))

(page/view! :ontology/sidebar sidebar-view)
(page/view! :ontology/header header-view)
(page/view! :ontology/section (fn [] [section-pane (or (:section @ont-model/ui) :discover)]))
(page/view! :ontology/otv (fn [] (when-let [k (otv-id)] [object-type-view k close-detail!])))
(page/view! :ontology/dialogs (fn [] [ot-view/create-dialog]))

(def ^:private tree
  (page/werkzeug
   {:sidebar       :ontology/sidebar
    :sidebar-label :ui/ontology
    :main          {:kind  :rows
                    :box   {:grow? true}
                    :items [(page/view :ontology/header)
                            {:kind :flow :padding :compact :box {:grow? true} :when list-open?
                             :items [(page/view :ontology/section)]}
                            {:kind :flow :padding :compact :box {:grow? true} :when otv-open?
                             :items [(page/view :ontology/otv)]}
                            (page/view :ontology/dialogs)]}}))

(defn ontology-panel [] [platform/module :ontology tree])
