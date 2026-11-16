(ns sutra-ui.graph-editor.menus
  (:require [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra-ui.feature :as feature]
            [sutra-ui.state.canvas :as canvas]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.graph :as graph]
            [sutra-ui.state.lineage :as lineage]
            [sutra-ui.state.ontology :as ontology]
            [sutra-ui.state.panel :as panel]
            [sutra-ui.state.selection :as selection]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.state.db :as db]
            [sutra-ui.forms.object :as object-forms]
            [sutra-ui.graph-editor.events :as gev]
            [sutra-ui.ui.core :as ui]
            [sutra-ui.object.card :as oc]))

(defn pane-menu
  [{:keys [edit-schema? menu set-menu]}]
  [ui/context-menu-popover
   {:is-open       true
    :target-offset {:left (.-screenX ^js menu) :top (.-screenY ^js menu)}
    :on-close      #(set-menu nil)
    :content
    [:div {:on-click #(.stopPropagation %)}
     (into [ui/menu]
           [[ui/menu-divider
             {:title (i18n/t (if edit-schema? :ui/ge-add-object-type :ui/ge-add-object))}]
            (when-not edit-schema?
              [ui/menu-item {:text (i18n/t :ui/ge-rows-from-pipeline)
                             :disabled true
                             :html-title (i18n/t :ui/ge-rows-hint)}])
            [ui/menu-item {:text (i18n/t :ui/ge-new-object-type)
                           :on-click #(shell/show-panel! :add-object)}]])]}])

(defn- ontology-node->edit-target
  [t id]
  (let [tail (when (str/index-of (str id) "/")
               (subs id (inc (str/index-of id "/"))))]
    (case t
      "objectType"           {:kind :object-type    :name (keyword id)}
      "datasource"           {:kind :datasource     :name (keyword tail)}
      nil)))

(defn- node->lineage-ref
  [t id]
  (let [tail (when (str/index-of (str id) "/")
               (subs id (inc (str/index-of id "/"))))]
    (case t
      "objectType"           [:object-type (keyword id)]
      "dataset"              [:dataset (keyword tail)]
      "datasource"           [:datasource (keyword tail)]
      nil)))

(defn- ontology-node->delete-action
  [t id]
  (case t
    "objectType"           [:delete-object-type    {:name (keyword id)}]
    nil))

(defn color-group-items
  [{:keys [id ids multi?]}]
  (let [groups   (canvas/mode-color-groups (shell/canvas-mode))
        in-group (some (fn [g] (when (contains? (set (:members g)) id) g)) groups)]
    (concat
     [[ui/menu-divider {:title (i18n/t :ui/ge-color-nodes)}]]
     (for [g groups]
       [ui/menu-item {:text     [:<> [:span {:class "graph-menu__group-swatch"
                                             :style {:background (:color g)}}]
                                 (:name g)]
                      :on-click #(graph/assign-nodes-to-group! (:id g) ids)}])
     [[ui/menu-item
       {:text     (i18n/t :ui/ge-new-color)
        :on-click #(db/send! [:shared.graph/new-color-group (i18n/t :ui/ge-new-color-title)
                              (count groups) ids])}]
      (when (and in-group (not multi?))
        [ui/menu-item {:text     (i18n/t :ui/ge-remove-from-group {:name (:name in-group)})
                       :on-click #(graph/remove-node-from-groups! id)}])])))

(defn node-menu
  [{:keys [edit-schema? instances? caps node-menu set-node-menu]}]
  (let [can?    (fn [k] (contains? (or caps #{}) k))
        id      (.-id ^js node-menu)
        t       (.-type ^js node-menu)
        named?  (not instances?)
        short   (when (and instances? (seq id))
                  (subs id 0 (min 8 (count id))))
        sel     (selection/selected-node-ids)
        ids     (vec (if (contains? sel id) sel [id]))
        n-sel   (count ids)
        multi?  (> n-sel 1)
        kind-l  (case t
                  "objectType"     (i18n/t :ui/object-type)
                  "objectInstance" (i18n/t :ui/ge-kind-object)
                  "datasource"     (i18n/t :ui/datasource)
                  "dataset"        (i18n/t :ui/ge-kind-dataset)
                  t)
        header  (cond
                  multi?  (i18n/t :ui/ge-nodes-selected {:n n-sel})
                  (= t "objectType") (str kind-l " · " (oc/type-label (keyword id)))
                  named?  (str kind-l " · "
                               (or (some-> (ontology-node->edit-target t id)
                                           :name clojure.core/name)
                                   (when-let [i (str/index-of (str id) "/")] (subs id (inc i)))
                                   id))
                  :else   (str kind-l " · " short))
        edit-target (when edit-schema? (ontology-node->edit-target t id))
        lin-ref (node->lineage-ref t id)
        close!  (fn [] (set-node-menu nil))
        change!
        (fn []
          (if (= :object-type (:kind edit-target))
            (object-forms/open-edit-object! (:name edit-target))
            (shell/open-edit-node-modal! edit-target)))
        del-pair
        (when edit-schema? (ontology-node->delete-action t id))
        do-delete!
        (fn []
          (cond
            (and edit-schema? del-pair)
            (ontology/mutate! (first del-pair) (second del-pair))

            (and edit-schema? (= t "datasource"))
            (let [tail (subs id (inc (.indexOf id "/")))]
              (data/delete-datasource! tail))

            :else nil))
        delete!
        (fn []
          (db/send! [:shared.graph/confirm
                     (if (= t "objectType")
                       (gev/delete-type-ask (keyword id))
                       {:title   (i18n/t :ui/ge-delete-node-title {:name header})
                        :danger? true})
                     do-delete!]))]
    [ui/context-menu-popover
     {:is-open       true
      :target-offset {:left (.-screenX ^js node-menu) :top (.-screenY ^js node-menu)}
      :on-close      close!
      :content
      [:div {:on-click #(.stopPropagation %)}
       (into [ui/menu]
             (concat
              [[ui/menu-divider {:title header}]]
              (when (can? :expand)
                (concat
                 [(when (and lin-ref (not multi?))
                    [ui/menu-item
                     {:text (i18n/t :ui/ge-open)
                      :on-click #(panel/open-bottom-panel! (first lin-ref) (second lin-ref))}])
                  (when (and (= t "objectType") (not multi?))
                    (let [type-kw (keyword id)
                          n       (count (distinct
                                          (map :other
                                               (lineage/ot-relations @catalog/schema type-kw))))
                          open?   (lineage/relations-open? type-kw)]
                      (when (pos? n)
                        [ui/menu-item
                         {:text (i18n/t (if open? :ui/ge-related-hide :ui/ge-related-show) {:n n})
                          :on-click #(graph/toggle-relations! type-kw)}])))
                  (when (and (= t "dataset") (not multi?))
                    (let [d-kw (keyword (subs id (inc (.indexOf id "/"))))
                          n    (count (lineage/ots-defined-by-dataset @catalog/schema d-kw))]
                      (when (pos? n)
                        [ui/menu-item {:text (i18n/t :ui/ge-expose-object-types {:n n})
                                       :on-click #(graph/expose-dataset-object-types! d-kw)}])))]
                 (for [[dir levels lbl]
                       [[:upstream   1    :ui/ge-expand-upstream]
                        [:downstream 1    :ui/ge-expand-downstream]
                        [:upstream   :all :ui/ge-expand-all-upstream]
                        [:downstream :all :ui/ge-expand-all-downstream]]]
                   (let [n (lineage/lineage-expand-count-of ids dir levels)]
                     [ui/menu-item {:text (i18n/t lbl)
                                    :label (str "+" n)
                                    :class (when (zero? n) "graph-menu__item--empty")
                                    :on-click #(graph/expand-nodes! ids dir levels)}]))))
              (when (can? :color-groups)
                (cons [ui/menu-divider] (color-group-items {:id id :ids ids :multi? multi?})))
              [[ui/menu-divider]
               (when (can? :remove-tool)
                 [ui/menu-item {:text (i18n/t :ui/ge-remove-from-graph {:n n-sel})
                                :intent :danger
                                :on-click #(do (graph/remove-nodes-from-graph! ids)
                                             (selection/clear-selection!))}])
               (when (and edit-target (not multi?))
                 [ui/menu-item {:text (i18n/t :ui/ge-change)
                                :on-click #(change!)}])]
              (let [ctx {:id id :type t :multi? multi? :instances? instances?
                         :mode (shell/canvas-mode)}]
                (for [{:keys [label-key run] pred :when} (feature/contributions :node-menu)
                      :when (pred ctx)]
                  [ui/menu-item {:text (i18n/t label-key)
                                 :on-click #(run ctx)}]))
              [(when (and instances? (= t "objectInstance") (not multi?))
                 [ui/menu-item {:text (i18n/t :ui/ge-search-around)
                                :on-click #(canvas/expand-around! id)}])
               (when (and (not multi?) (or del-pair (and edit-schema? (= t "datasource"))))
                 [ui/menu-item {:text (i18n/t :ui/delete)
                                :intent :danger
                                :on-click #(delete!)}])]))]}]))
