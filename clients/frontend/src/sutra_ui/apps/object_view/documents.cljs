(ns sutra-ui.apps.object-view.documents
  (:require [sutra-ui.ui.core :as ui :refer [html-table]]
            [sutra-ui.ui.busy :as busy]
            [sutra.i18n.core :as i18n]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.apps.object-view.model :as ov-model]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.ui.style :as style]
            [sutra-ui.ui.page :as page]
            [sutra-ui.apps.object-view.registry :as reg]))

(defn- open-document! [id]
  (catalog/open-resource! :documents id))

(def ^:private linked
  (machine/entering
   ov-model/object-view
   (fn [object-type id] [:object-view.documents/load object-type id])
   (fn [object-type id]
     (let [rows (get-in @ov-model/ui [:linked-documents [object-type (str id)]])]
       (cond
         (nil? rows)
         [busy/block {:class "explorer-attributes__empty"} nil]

         (empty? rows)
         [:div {:class "explorer-attributes__empty"}
          (i18n/t :ui/ov-documents-none)]

         :else
         [html-table {:compact true :class [style/table-root "u-w-full"]}
          [:thead
           [:tr
            [:th (i18n/t :ui/ov-documents-document)]
            [:th (i18n/t :ui/ov-documents-template)]
            [:th (i18n/t :ui/ov-documents-updated)]]]
          [:tbody
           (for [d rows]
             ^{:key (:id d)}
             [:tr {:on-click #(open-document! (:id d)) :class "u-pointer"}
              [:td (:name d)]
              [:td
               (str (or (:template d) "—")
                    (when (:template-version d) (str " v" (:template-version d))))]
              [:td
               (subs (str (:updated-at d)) 0 (min 16 (count (str (:updated-at d)))))]])]])))))

(defn root [{:keys [object-type id]}]
  (let [id (or id (ov-model/focused-instance-id))]
    (if (nil? id)
      [:div {:class "explorer-attributes__empty"} (i18n/t :ui/property-list-no-object)]
      ^{:key (str (name object-type) "/" id)} [linked object-type id])))

(page/view! :documents root)

(defmethod reg/config-form :documents [_ _ _]
  [:div {:class "explorer-config__none"}
   (i18n/t :ui/ov-documents-config-none)])

(defmethod reg/default-config :documents [_] {})
