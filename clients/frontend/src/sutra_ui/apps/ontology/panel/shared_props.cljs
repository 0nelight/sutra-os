(ns sutra-ui.apps.ontology.panel.shared-props
  (:require [sutra-ui.ui.core :as ui]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.apps.ontology.fields :refer [pick named]]
            [sutra-ui.apps.ontology.model :as ont-model]
            [sutra-ui.state.forms :as fs]
            [sutra-ui.ui.form :as form]
            [sutra-ui.ui.inputs :as inputs]
            [sutra-ui.forms.object :as object-forms]
            [sutra-ui.object.card :as oc]
            [sutra.i18n.core :as i18n]
            [sutra-ui.apps.ontology.panel.common :refer [confirm-delete! section-shell]]))

(defn- shared-property-users
  [k]
  (sort-by oc/type-label
           (for [[t ot] (:object-types @catalog/schema)
                 :when (some #(= (keyword k) (keyword (:name %))) (:fields ot))]
             t)))

(defn- shared-property-row [k spec]
  (let [users (shared-property-users k)]
    [:div {:class "ontology-sharedprop__row"}
     [:div {:class "ontology-entityrow__main"}
      [:span {:class "ontology-entityrow__icon--blue"}
       "◈"]
      [:div
       [:div {:class "u-bold"} (clojure.core/name k)]
       [:div {:class "ontology-entityrow__sub"}
        (some-> (:type spec) clojure.core/name)
        (when-let [vt (:value-type spec)]
          (str " · " (i18n/t :ui/value-type) " " (clojure.core/name vt)))]
       [:div {:class "ontology-entityrow__sub"}
        (if (empty? users)
          (i18n/t :ui/om-shared-prop-unused)
          (into [:span (i18n/t :ui/om-shared-prop-used-by) " "]
                (interpose ", "
                           (for [t users]
                             ^{:key t}
                             [:a {:href "#" :title (clojure.core/name t)
                                  :on-click (fn [e] (.preventDefault e)
                                              (object-forms/open-object-detail! t))}
                              (oc/type-label t)]))))]]]
     [ui/tooltip {:content (i18n/t :ui/delete)}
      [ui/button {:minimal true :small true :icon :cross :on-click (fn [_]
                        (confirm-delete! (i18n/t :ui/shared-property-delete-confirm
                                                 {:name (clojure.core/name k)})
                                         [[:dispatch [:ontology/mutate
                                                      :delete-shared-property {:name k}]]]))
                  :aria-label (i18n/t :ui/delete)}]]]))

(defn shared-properties-pane []
  (let [draft (r/atom {:name "" :type "string"})]
    (fn []
      (let [sps (sort-by key (or (:shared-properties @catalog/canonical-schema) {}))
            vts (or (:value-types @catalog/canonical-schema) {})
            d   @draft
            nm  (str/trim (str (:name d)))]
        [:div
         [:div
          [:div {:class "ontology-sharedprop__addbar"}
           [form/field {:label (i18n/t :ui/name) :class "ontology-sharedprop__name-field"}
            [inputs/text-input (fs/at draft :name)
             {:placeholder (i18n/t :ui/shared-property-name-example) :fill true}]]
           [form/field {:label (i18n/t :ui/type) :class "u-flex-1"}
            [pick (:type d)
             (named (distinct (cond-> ["string" "number" "boolean" "keyword" "uuid" "instant"]
                                (seq (:type d)) (conj (:type d)))))
             #(swap! draft assoc :type %)
             {:clearable? false :disabled? (boolean (seq (:value-type d)))}]]
           (when (seq vts)
             [form/field {:label (i18n/t :ui/value-type) :class "u-flex-1"}
              [pick (:value-type d)
               (for [[k vt] (sort-by key vts)]
                 {:value (clojure.core/name k)
                  :label (str (clojure.core/name k) " · " (ont-model/value-type-base (:type vt)))})
               (fn [v] (swap! draft assoc :value-type v
                              :type (if (seq v)
                                      (some-> (get-in vts [(keyword v) :type])
                                              ont-model/value-type-base)
                                      "string")))
               {:placeholder (i18n/t :ui/value-type-none)}]])
           [ui/button {:intent :primary :on-click (fn [_]
                             (when (seq nm)
                               (ont-model/add-shared-property! d)
                               (reset! draft {:name "" :type "string"}))) :disabled (empty? nm)}
            (i18n/t :ui/add)]]]
         [section-shell
          [:div
           [:div {:class "ontology-list__header"}
            (str (i18n/t :ui/shared-properties) " (" (count sps) ")")]
           (if (empty? sps)
             [:div {:class "u-text-empty"}
              (i18n/t :ui/shared-properties-none)]
             (for [[k spec] sps]
               ^{:key k} [shared-property-row k spec]))]]]))))
