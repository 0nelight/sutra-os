(ns sutra-ui.apps.ontology.panel.function-form
  (:require [sutra-ui.ui.core :as ui]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.forms :as fs]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.apps.ontology.fields :refer [pick named]]
            [sutra-ui.apps.ontology.model :as ont-model]
            [sutra-ui.ui.form :as form]
            [sutra-ui.ui.inputs :as inputs]
            [sutra-ui.ui.overlay :as overlay]
            [sutra.i18n.core :as i18n]
            [sutra.types.scaffold :as scaffold]
            [sutra-ui.apps.ontology.panel.common :refer [new-function-form]]))

(defn- target-type-options []
  (sort (keys (:objects @catalog/schema))))

(defn- param-types-editor
  [form-atom]
  (let [rows (or (:param-types @form-atom) [])]
    [form/field {:label (i18n/t :ui/param-types)}
     (if (empty? rows)
       [:div {:class "u-muted"}
        (i18n/t :ui/param-types-none)]
       (for [[i {:keys [type]}] (map-indexed vector rows)]
         ^{:key i}
         [form/row
          [inputs/text-input (fs/at form-atom [:param-types i :role])
           {:placeholder (i18n/t :ui/role-example) :class "u-flex-1" :small true}]
          [pick type (named (target-type-options))
           #(fs/edit! form-atom assoc-in [:param-types i :type] %) {:class "u-flex-1"}]
          [ui/tooltip {:content (i18n/t :ui/remove)}
           [ui/button {:minimal true :small true :icon :cross :on-click (fn [_]
                             (fs/edit! form-atom update :param-types
                                       (fn [v] (vec (concat (subvec v 0 i)
                                                            (subvec v (inc i)))))))
                       :aria-label (i18n/t :ui/remove)}]]]))
     [ui/button {:small true :on-click (fn [_]
                       (fs/edit! form-atom update :param-types
                                 (fnil conj []) {:role "" :type ""}))}
      (i18n/t :ui/add-param)]]))

(defn function-form
  [{:keys [form-atom on-save on-cancel save-label on-delete]}]
  (let [form @form-atom
        types (target-type-options)]
    [:div
     [:div {:class "ontology-function__grid2"}
      [form/field {:label (i18n/t :ui/name)}
       [inputs/text-input (fs/at form-atom :name)
        {:placeholder (i18n/t :ui/function-name-example) :fill true}]]
      [form/field {:label (i18n/t :ui/target-type)}
       [pick (str (or (:target-type form) "")) (named types)
        #(fs/edit! form-atom assoc :target-type %)]]]
     [form/field {:label (i18n/t :ui/language)}
      [pick (or (:language form) "clojure") (named ["clojure"])
       #(fs/edit! form-atom assoc :language %) {:clearable? false}]]
     [form/field {:label (i18n/t :ui/source)}
      [ui/text-area {:value (or (:source form) "")
                     :on-change #(fs/edit! form-atom assoc :source (.. % -target -value))
                     :rows 14
                     :class ["ontology-fn__code" "u-mono-12"] :fill true}]]
     [param-types-editor form-atom]
     [:div {:class "ontology-function__form-actions"}
      [overlay/form-actions {:on-cancel on-cancel
                             :ok-label  (or save-label (i18n/t :ui/save))
                             :on-ok     on-save}]
      (when on-delete
        [ui/button {:intent :danger :outlined true :on-click on-delete}
         (i18n/t :ui/delete)])]]))

(defn add-function-form []
  [:div {:class "ontology-function__addform"}
   [:h4 {:class "ontology-function__addform-title"} (i18n/t :ui/function-new)]
   [function-form
    {:form-atom  new-function-form
     :save-label (i18n/t :ui/create)
     :on-save (fn []
                (let [{:keys [name] :as form} @new-function-form]
                  (when (and (seq name) (nil? (scaffold/edit-function-problem form)))
                    (ont-model/add-function! @new-function-form)
                    (fs/reset-form! :new-function)
                    (shell/show-panel! :none))))
     :on-cancel (fn []
                  (fs/reset-form! :new-function)
                  (shell/show-panel! :none))}]])
