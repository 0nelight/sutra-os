(ns sutra-ui.forms.group
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [sutra-ui.object.card :as oc]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.forms :as fs]
            [sutra-ui.state.atlas :as kompass]
            [sutra-ui.state.ontology :as ontology]
            [sutra-ui.state.shell :as shell]
            [sutra.i18n.core :as i18n]
            [sutra-ui.ui.core :as ui]
            [sutra-ui.ui.form :as form]
            [sutra-ui.ui.inputs :as inputs]
            [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.forms.base :refer [edit-object-type-group-form
                                                new-object-type-group-form]]
            [sutra-ui.forms.lists :refer [list-section-header type-row]]))

(defn- member-picker
  [selected toggle!]
  (let [ots (sort (keys (:object-types @catalog/schema)))]
    [form/field {:label (i18n/t :ui/members)}
     (if (empty? ots)
       [:div {:class "form-list__empty"} (i18n/t :ui/object-types-none-to-add)]
       [:div {:class "ontology-group__member-grid"}
        (for [k ots]
          ^{:key k}
          [ui/checkbox {:checked (contains? selected k)
                        :on-change #(toggle! k)
                        :label (oc/type-label k)}])])]))

(defn object-type-group-form
  [{:keys [form-atom on-save on-cancel save-label]}]
  (let [{:keys [members]} @form-atom
        selected (set (map keyword members))
        toggle!  (fn [k]
                   (fs/edit! form-atom update :members
                             (fn [ms]
                               (let [s (set (map keyword ms))]
                                 (vec (if (contains? s k) (disj s k) (conj s k)))))))]
    [:div
     [form/field {:label (i18n/t :ui/name)}
      [inputs/text-input (fs/at form-atom :name)
       {:placeholder (i18n/t :ui/group-name-example)}]]
     [form/field {:label (i18n/t :ui/description)}
      [inputs/text-input (fs/at form-atom :description)
       {:placeholder (i18n/t :ui/group-description-hint)}]]
     [member-picker selected toggle!]
     [:div {:class "ui-form__actions u-mb-12"}
      [overlay/form-actions {:on-cancel on-cancel :on-ok on-save
                             :ok-label  (or save-label (i18n/t :ui/save))}]]]))

(defn add-object-type-group-form []
  [:div {:class "ontology-function__addform"}
   [:h4 {:class "ontology-function__addform-title"} (i18n/t :ui/object-type-group-new)]
   [object-type-group-form
    {:form-atom new-object-type-group-form
     :save-label (i18n/t :ui/create)
     :on-save (fn []
                (let [{:keys [name]} @new-object-type-group-form]
                  (when (seq name)
                    (ontology/add-object-type-group! @new-object-type-group-form)
                    (fs/reset-form! :new-object-type-group)
                    (shell/show-panel! :none))))
     :on-cancel (fn []
                  (fs/reset-form! :new-object-type-group)
                  (shell/show-panel! :none))}]])

(defn load-object-type-group-form! [g-kw]
  (when-let [g (get (catalog/object-type-groups) g-kw)]
    (fs/put! edit-object-type-group-form
             {:name (clojure.core/name g-kw)
              :description (or (:description g) "")
              :members (vec (:members g))})))

(defn open-object-type-group-detail! [g-kw]
  (load-object-type-group-form! g-kw)
  (kompass/note-recent! {:kind :object-type-group :id g-kw
                         :label (clojure.core/name g-kw)})
  (shell/close-detail-modal!)
  (catalog/open-resource! :object-type-groups g-kw))

(defn object-type-groups-list
  []
  (let [groups (sort-by key (catalog/object-type-groups))]
    [:div {:class "u-mt-10"}
     [list-section-header (i18n/t :ui/object-type-groups) (count groups)]
     (if (empty? groups)
       [:div {:class "form-list__empty"}
        (i18n/t :ui/object-type-groups-none)]
       (doall
        (for [[k g] groups]
          ^{:key k}
          (let [members (:members g)]
            [type-row
             {:primary (clojure.core/name k)
              :secondary (str (i18n/t (if (= 1 (count members)) :ui/members-one :ui/members-many)
                                      {:n (count members)})
                              (when (seq members)
                                (str " · " (clojure.string/join ", "
                                                                (map oc/type-label members))))
                              (when (seq (:description g))
                                (str " — " (:description g))))
              :fav-ref {:kind :object-type-group :id k :label (clojure.core/name k)}
              :on-click #(open-object-type-group-detail! k)}]))))]))

(defn object-groups-chips
  [_obj-kw]
  (let [open? (r/atom false)]
    (fn [obj-kw]
      (let [member-of (set (catalog/groups-of obj-kw))
            all-groups (sort (keys (catalog/object-type-groups)))]
        [form/field {:label (i18n/t :ui/groups) :class "ontology-detail__groups"}
         [:div {:class "ontology-group__chips"}
          (if (empty? member-of)
            [:span {:class "ontology-entityrow__sub"} (i18n/t :ui/in-no-group)]
            (for [g (sort member-of)]
              ^{:key g}
              [:span {:class "ontology-group__chip"} (clojure.core/name g)]))
          [ui/button {:small true :on-click #(swap! open? not)}
           (i18n/t :ui/groups-edit)]]
         (when @open?
           [:div {:class "ontology-group__chip-editor"}
            (if (empty? all-groups)
              [:div {:class "ontology-entityrow__sub"}
               (i18n/t :ui/groups-none)]
              (for [g all-groups]
                ^{:key g}
                [ui/checkbox
                 {:checked (contains? member-of g)
                  :on-change (fn [_]
                               (let [next (if (contains? member-of g)
                                            (disj member-of g)
                                            (conj member-of g))]
                                 (ontology/set-object-type-groups! obj-kw next)))
                  :label (clojure.core/name g)}]))])]))))
