(ns sutra-ui.apps.ontology.panel.transforms
  (:require [sutra-ui.ui.core :as ui :refer [divider]]
            [sutra.types.languages :as langs]
            [sutra.types.scaffold :as scaffold]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.data :as data]
            [sutra-ui.apps.ontology.fields :refer [pick named]]
            [sutra-ui.apps.ontology.model :as ont-model]
            [sutra-ui.state.forms :as fs]
            [sutra-ui.ui.form :as form]
            [sutra-ui.ui.inputs :as inputs]
            [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.object.card :as oc]
            [sutra.i18n.core :as i18n]
            [sutra-ui.apps.ontology.panel.common :refer [section-shell]]
            [sutra-ui.apps.ontology.panel.detail :refer [open-transform!]]))

(defn- transform-row [k spec]
  [ui/tooltip {:content (i18n/t :ui/om-transform-open-hint)}
   [:div {:class "ontology-transform__row u-pointer"
          :on-click #(open-transform! k)}
    [:div {:class "ontology-entityrow__main"}
     [:span {:class "ontology-entityrow__icon--blue"} "ƒ"]
     [:div
      [:div {:class "u-bold"} (clojure.core/name k)]
      [:div {:class "ontology-entityrow__sub"}
       (str (some-> (:language spec) clojure.core/name (or "map"))
            (when-let [o (get-in spec [:output :object-type])]
              (str " → " (oc/type-label (catalog/name-of o))))
            (when (seq (:inputs spec)) (str " · " (count (:inputs spec)) " input(s)")))]]]]])

(defn transforms-pane []
  (let [draft (r/atom {:name "" :language "python" :code "" :output-type ""
                       :primary-key "" :inputs []})
        open? (r/atom false)]
    (fn []
      (let [d        @draft
            xforms   (sort-by key (or (:transforms @catalog/canonical-schema) {}))
            obj-types (sort (keys (:object-types @catalog/schema)))
            out-attrs (when (seq (:output-type d))
                        (mapv (comp clojure.core/name :name)
                              (get-in @catalog/schema [:objects (keyword (:output-type d))])))
            sql?     (langs/query-language? (keyword (:language d)))
            ds-list  (sort (keys (data/datasource-map)))
            set-f    (fn [k v] (swap! draft assoc k v))
            inputs   (into {}
                           (for [{:keys [iname ds table]} (:inputs d)
                                 :when (and (seq iname) (seq ds))]
                             [(keyword iname)
                              (cond-> {:kind :datasource :datasource (keyword ds)}
                                (seq table) (assoc :table table))]))
            valid?   (and (seq (str/trim (:name d))) (seq (str/trim (:code d)))
                          (nil? (scaffold/transform-problem
                                 {:language    (keyword (:language d)) :inputs inputs
                                  :outputs     (when (seq (:output-type d)) [:object-type])
                                  :primary-key (not-empty (:primary-key d))})))
            create!  (fn []
                       (when valid?
                         (let [cm (into {} (for [a out-attrs] [(keyword a) (keyword a)]))]
                           (ont-model/add-transform!
                            {:name (:name d) :language (:language d) :code (:code d)
                             :output-type (:output-type d) :primary-key (:primary-key d)
                             :column-map cm :inputs inputs})
                           (reset! draft {:name "" :language "python" :code "" :output-type ""
                                          :primary-key "" :inputs []})
                           (reset! open? false))))]
        [section-shell
         [:div
          [:div {:class "ontology-list__header-row"}
           [:div {:class "ontology-list__header"}
            (str (i18n/t :ui/transforms) " (" (count xforms) ")")]
           (when-not @open?
             [ui/button {:intent :primary :on-click #(reset! open? true)}
              (i18n/t :ui/transform-new)])]
          (if (empty? xforms)
            [:div {:class "u-text-empty"}
             (i18n/t :ui/transforms-none)]
            (for [[k spec] xforms] ^{:key k} [transform-row k spec]))
          (when @open?
            [:<>
             [divider {:class "ontology-transform__divider"}]
             [:div {:class "ontology-list__header"}
              (i18n/t :ui/transform-new)]
             [:div {:class "ontology-transform__intro"}
              (i18n/t :ui/transform-new-intro-1)
              [:code "@compute_frame(Output(…))"]
              (i18n/t :ui/transform-new-intro-2)]
             [:div {:class "ontology-transform__grid2"}
              [form/field {:label (i18n/t :ui/name)}
               [inputs/text-input (fs/at draft :name)
                {:placeholder (i18n/t :ui/transform-name-example) :fill true}]]
              [form/field {:label (i18n/t :ui/language)}
               [pick (:language d) (named langs/selectable-languages) #(set-f :language %)
                {:clearable? false}]]]
             [form/field {:label (i18n/t :ui/output-object-type)}
              [pick (:output-type d)
               (for [t obj-types] {:value (clojure.core/name t) :label (oc/type-label t)})
               #(set-f :output-type %) {:placeholder (i18n/t :ui/pick-a-type)}]]
             (when (seq out-attrs)
               [form/field {:label (i18n/t :ui/primary-key)}
                [pick (:primary-key d) (named out-attrs) #(set-f :primary-key %)
                 {:placeholder (i18n/t :ui/pick-an-attribute)}]])
             [form/field {:label (i18n/t (if sql? :ui/sql-query :ui/python-code))}
              [ui/text-area
               {:value (:code d)
                :on-change #(set-f :code (.. % -target -value))
                :rows 12
                :placeholder (i18n/t (if sql? :ui/sql-placeholder :ui/python-placeholder))
                :class ["ontology-transform__code" "u-mono-12"] :fill true}]]
             [form/field {:label (i18n/t :ui/inputs-optional)}
              (for [[i {:keys [ds]}] (map-indexed vector (:inputs d))]
                ^{:key i}
                [form/row
                 [inputs/text-input (fs/at draft [:inputs i :iname])
                  {:placeholder (i18n/t :ui/name) :class "u-flex-1" :small true}]
                 [pick ds (named ds-list) #(swap! draft assoc-in [:inputs i :ds] %)
                  {:placeholder (i18n/t :ui/pick-a-datasource) :class "u-flex-1"}]
                 [inputs/text-input (fs/at draft [:inputs i :table])
                  {:placeholder (i18n/t :ui/table) :class "u-flex-1" :small true}]
                 [ui/button
                  {:minimal true :small true :icon :cross :on-click #(swap! draft update :inputs
                       (fn [v] (vec (concat (subvec v 0 i) (subvec v (inc i))))))}]])
              [ui/button
               {:small true
                :on-click #(swap! draft update :inputs (fnil conj []) {:iname "" :ds "" :table ""})}
               (i18n/t :ui/add-input)]]
             [:div {:class "ui-form__actions u-mt-12"}
              [overlay/form-actions {:on-cancel    #(reset! open? false)
                                     :ok-label     (i18n/t :ui/transform-create)
                                     :on-ok        create!
                                     :ok-disabled? (not valid?)}]]])]]))))
