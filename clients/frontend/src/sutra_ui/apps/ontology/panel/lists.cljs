(ns sutra-ui.apps.ontology.panel.lists
  (:require [sutra-ui.state.shell :as shell]
            [sutra-ui.forms.group :as group-forms]
            [sutra-ui.forms.interface :as iface-forms]
            [sutra.i18n.core :as i18n]
            [sutra-ui.ui.page :as page]
            [sutra-ui.ui.core :as ui]
            [sutra-ui.apps.ontology.panel.common :refer [section-shell]]
            [sutra-ui.apps.ontology.panel.function-form :refer [add-function-form]]))

(defn inline-add-form [panel-kind]
  (when (= panel-kind (:panel @shell/ui-state))
    [ui/card
     (case panel-kind
       :add-interface  [iface-forms/add-interface-form]
       :add-function   [add-function-form]
       :add-object-type-group [group-forms/add-object-type-group-form]
       nil)]))

(def ^:private new-button-label
  {:add-object :ui/om-new-object-type :add-link :ui/om-new-link-type
   :add-interface :ui/om-new-interface :add-object-type-group :ui/om-new-group})

(defn list-pane
  [add-form-key list-component]
  [:div
   [inline-add-form add-form-key]
   (when-not (= add-form-key (:panel @shell/ui-state))
     [page/toolbar
      [ui/button {:intent :primary :on-click #(shell/show-panel! add-form-key)}
       (i18n/t (new-button-label add-form-key))]])
   [section-shell [list-component]]])
