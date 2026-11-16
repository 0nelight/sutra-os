(ns sutra-ui.apps.ontology.panel.functions
  (:require [sutra-ui.ui.core :as ui]
            [clojure.string :as str]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.apps.ontology.model :as ont-model]
            [sutra-ui.ui.form :as form]
            [sutra-ui.ui.inputs :as inputs]
            [sutra-ui.apps.ontology.fields :as fields]
            [sutra-ui.ui.style :as style]
            [sutra-ui.forms.lists :as entity-lists]
            [sutra-ui.object.card :as oc]
            [sutra-ui.resource.versions :as versions]
            [sutra.i18n.core :as i18n]
            [sutra-ui.apps.ontology.panel.common :refer [confirm-delete! edit-function-form
                                                         section-shell]]
            [sutra-ui.apps.ontology.panel.detail :refer [card card-eyebrow entity-detail-shell
                                                         function-label open-function-detail!]]
            [sutra-ui.apps.ontology.panel.function-form :refer [function-form]]
            [sutra-ui.apps.ontology.panel.lists :refer [inline-add-form]]))

(defn- function-row [wf-name spec]
  (let [label (function-label wf-name spec)]
    [:div {:on-click #(open-function-detail! wf-name spec)
           :class "ontology-function__list-row"
           :on-mouse-enter (fn [e] (set! (.. e -currentTarget -style -background) "var(--c-hover)"))
           :on-mouse-leave (fn [e] (set! (.. e -currentTarget -style -background) "transparent"))}
     [:div {:class "ontology-entityrow__main"}
      [:span {:class "ontology-function__list-icon"}
       "▶"]
      [:div {:class "u-flex-1-min-0"}
       [:div {:class "u-bold"} label]
       [:div {:class "ontology-entityrow__sub"}
        (when-let [t (:target-type spec)]
          (str (i18n/t :ui/om-function-target {:type (oc/type-label (catalog/name-of (keyword t)))})
               " · "))
        (some-> (:language spec) name)
        (when (seq (:param-types spec))
          (i18n/t :ui/params-suffix {:n (count (:param-types spec))}))]]
      [entity-lists/favorite-star {:kind :function :id wf-name :label label}]]]))

(defn functions-pane []
  (let [functions (sort-by (fn [[k spec]] (str/lower-case (function-label k spec)))
                           (or (:functions @catalog/canonical-schema) {}))]
    [:div
     [:div
      [inline-add-form :add-function]]
     [section-shell
      [:div
       [:div {:class "ontology-list__header-row"}
        [:div {:class "ontology-list__header"}
         (str (i18n/t :ui/functions) " (" (count functions) ")")]
        [ui/button {:intent :primary :on-click #(shell/show-panel! :add-function)}
         (i18n/t :ui/om-new-function)]]
       (if (empty? functions)
         [:div {:class "ontology-function__empty"}
          (i18n/t :ui/functions-none)]
         (for [[k spec] functions]
           ^{:key k} [function-row k spec]))]]]))

(defn- run-input-field
  [_role _type-kw]
  (let [list (str "fn-run-" (random-uuid))]
    (fn [role type-kw]
      (let [t    (catalog/name-of (keyword type-kw))
            run  (:fn-run @ont-model/ui)
            meta (oc/type-meta t)]
        [form/field {:label (str role " · " (oc/type-label t))}
         [inputs/text-input
          (fields/via (get-in run [:values role] "")
                      #(ont-model/send! [:ontology.run/value role t %]))
          {:list list :fill true :placeholder (i18n/t :ui/om-function-input-search)}]
         (into [:datalist {:id list}]
               (for [row (get-in run [:hits role]) :let [id (str (:id row))]]
                 ^{:key id} [:option {:value id :label (oc/title meta row id)}]))]))))

(defn- run-result [{:keys [compiled decisions]}]
  [:div {:class [style/soft-card "ontology-fn__try"]}
   [:strong (i18n/t :ui/compiled-label)]
   (i18n/t :ui/compiled-steps-sets
           {:steps (:steps compiled) :sets (count (:object-sets compiled))})
   [:br]
   [:strong (i18n/t :ui/decisions-label)] (count decisions)
   (when-let [d (first decisions)]
     [:pre {:class "ontology-function__run-decision"}
      (pr-str (:values d))])])

(defn- function-run-card [wf-name spec]
  (let [{:keys [inputs error result]} (:fn-run @ont-model/ui)]
    [card
     [:div
      [card-eyebrow (i18n/t :ui/run)]
      (if (seq (:param-types spec))
        (for [[role t] (sort-by key (:param-types spec))]
          ^{:key role} [run-input-field (clojure.core/name role) t])
        [:div {:class "u-muted"} (i18n/t :ui/om-function-no-inputs)])
      [ui/section
       {:title (i18n/t :ui/om-function-inputs-edn) :collapsible true :compact true
        :collapse-props {:default-is-open false} :class "u-mb-8"}
       [ui/section-card
        [:div {:class "u-muted"}
         (i18n/t :ui/function-inputs-hint-1) [:code ":ac"]
         (i18n/t :ui/function-inputs-hint-2)]
        [ui/text-area
         {:value (or inputs "")
          :placeholder "{:ac :ac}"
          :on-change #(ont-model/send! [:ontology.run/inputs (.. % -target -value)])
          :rows 3
          :class ["ontology-fn__args" "u-mono-12"] :fill true}]]]
      [ui/button {:intent :primary
                  :on-click #(ont-model/send! [:ontology.run/start wf-name])}
       (i18n/t :ui/function-run)]
      (when error
        [ui/problem error nil "u-mt-10"])
      (when result [run-result result])]]))

(defn function-detail-page [wf-name on-close]
  (let [spec  (get-in @catalog/canonical-schema [:functions wf-name])
        label (function-label wf-name spec)
        save! #(ont-model/update-function! wf-name @edit-function-form on-close)]
    [entity-detail-shell
     {:kind :function
      :entity-label label
      :api-name (clojure.core/name wf-name)
      :on-save save! :on-close on-close
      :on-delete (fn [_]
                   (confirm-delete! (i18n/t :ui/function-delete-confirm {:name label})
                                    [[:dispatch [:ontology.function/delete (keyword wf-name)
                                                 on-close]]]))
      :form-component [function-form
                       {:form-atom edit-function-form
                        :save-label (i18n/t :ui/save)
                        :on-save save! :on-cancel on-close}]}
     [function-run-card wf-name spec]
     ^{:key wf-name}
     [versions/panel {:section "functions" :name (clojure.core/name wf-name)}]]))
