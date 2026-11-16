(ns sutra-ui.apps.ontology.panel.discover
  (:require [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.atlas :as kompass]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.state.sync :as sync]
            [sutra-ui.apps.ontology.model :as ont-model]
            [sutra-ui.forms.group :as group-forms]
            [sutra-ui.forms.interface :as iface-forms]
            [sutra-ui.forms.link :as link-forms]
            [sutra-ui.forms.object :as object-forms]
            [sutra.i18n.core :as i18n]
            [sutra-ui.ui.core :as ui :refer [h2 h6 tag card] :rename {card bp-card}]
            [sutra-ui.apps.ontology.panel.common :refer [kind-defs]]
            [sutra-ui.apps.ontology.panel.detail :refer [open-function-detail!]]))

(defn- discover-tile [{:keys [id label icon count-fn]}]
  [bp-card {:interactive true :class "ontology-discover__tile"
            :on-click #(ont-model/edit! [] assoc :section id)}
   [h6 [ui/icon {:icon icon}] " " (i18n/t label)]
   [h2 (or (when count-fn (count-fn)) 0)]])

(defn- open-ref!
  [{:keys [kind id]}]
  (case kind
    :object-type       (object-forms/open-object-detail! id)
    :link-type         (link-forms/open-link-detail! id)
    :interface         (iface-forms/open-interface-detail! id)
    :object-type-group (group-forms/open-object-type-group-detail! id)
    :function          (open-function-detail! id (get-in @catalog/canonical-schema [:functions id]))
    nil))

(defn- ref-exists?
  [{:keys [kind id]}]
  (case kind
    :object-type     (contains? (:object-types @catalog/schema) id)
    :interface  (contains? (:interfaces @catalog/schema) id)
    :link-type (boolean (some #(= id (catalog/link-id %))
                              (:links @catalog/schema)))
    :object-type-group (contains? (catalog/object-type-groups) id)
    :function   (contains? (:functions @catalog/canonical-schema) id)
    false))

(defn- ref-chip [{:keys [kind label] :as ref}]
  (let [icon (case kind :object-type :cube :link-type :link :interface :layers
                   :function :function :object-type-group :group-objects :star)]
    [tag {:large true :minimal true :round true :interactive true :icon icon
          :on-click #(open-ref! ref)}
     label]))

(defn- discover-chip-row [eyebrow refs]
  (let [refs (filter ref-exists? refs)]
    (when (seq refs)
      [:div
       [:div {:class "ontology-discover__eyebrow"}
        eyebrow]
       (into [:div {:class "ontology-discover__chip-row"}]
             (for [r refs] ^{:key (str (:kind r) (:id r))} [ref-chip r]))])))

(defn discover-page []
  (let [tiles (filter :count-fn kind-defs)
        plan  @data/import-plan
        ds    (:show (data/datasources-view))
        next-action
        (cond
          (#{:loading :failed} ds) nil
          (= :empty ds)
          {:label (i18n/t :ui/first-datasource)
           :hint  (i18n/t :ui/first-datasource-hint)
           :on-click #(shell/enter! :source-manager {:wizard? true})}
          (empty? (:object-types @catalog/schema))
          {:label (i18n/t :ui/first-object-type)
           :hint  (i18n/t :ui/first-object-type-hint)
           :on-click #(do (ont-model/edit! [] assoc :section :object-types)
                          (shell/show-panel! :add-object))}
          (or (seq (:objects plan)) (seq (:links plan)))
          {:label (i18n/t :ui/run-the-import)
           :hint  (i18n/t :ui/run-the-import-hint)
           :on-click #(do (shell/enter! :source-manager)
                          (sync/run-import!))})]
    [:div
     [:div {:class "ontology-discover__eyebrow"}
      (i18n/t :ui/overview)]
     [:div {:class "ontology-discover__tiles"}
      (for [t tiles]
        ^{:key (:id t)} [discover-tile t])]
     [discover-chip-row (i18n/t :ui/favorites) (kompass/om-favorites)]
     [discover-chip-row (i18n/t :ui/recently-viewed) (ont-model/om-recents)]
     (when next-action
       [ui/callout {:intent :primary :title (:label next-action)}
        (:hint next-action) " "
        [ui/button {:small true :intent :primary :on-click (:on-click next-action)}
         (i18n/t :ui/go)]])]))
