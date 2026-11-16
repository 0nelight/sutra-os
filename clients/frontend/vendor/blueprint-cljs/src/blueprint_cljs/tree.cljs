(ns blueprint-cljs.tree
  "Tree over controlled data: `:contents` is a vector of node maps
   `{:id :label :icon :secondary-label :child-nodes :is-expanded :is-selected :disabled
     :has-caret :class :node-data}`. Handlers receive `(node path event)`, `path` being
   the vector of indices from the root."
  (:require [blueprint-cljs.core :as bp]
            [blueprint-cljs.collapse :as collapse]
            [blueprint-cljs.icon :as icon]))

(defn node-from-path
  "The node at `path` in `contents`."
  [path contents]
  (get-in contents (vec (interpose :child-nodes path))))

(defn update-node
  "`contents` with `f` applied to the node at `path`."
  [contents path f & args]
  (apply update-in contents (vec (interpose :child-nodes path)) f args))

(defn map-nodes
  "`contents` with `f` applied to every node, depth first."
  [contents f]
  (mapv (fn [n] (f (cond-> n (:child-nodes n) (update :child-nodes map-nodes f)))) contents))

(declare node-list)

(defn- tree-node
  [{:keys [label icon secondary-label child-nodes is-expanded is-selected disabled
           class]
    :as node}
   path handlers]
  (let [{:keys [on-node-click on-node-context-menu on-node-double-click on-node-expand
                on-node-collapse on-node-mouse-enter on-node-mouse-leave]} handlers
        call     (fn [f] (when f #(f node path %)))
        has-caret (if (contains? node :has-caret) (:has-caret node) (seq child-nodes))]
    [:li {:class (bp/cx (bp/cls "tree-node") class
                        {(bp/cls "disabled")           disabled
                         (bp/cls "tree-node-selected") is-selected
                         (bp/cls "tree-node-expanded") is-expanded})}
     [:div (merge {:class (bp/cx (bp/cls "tree-node-content")
                                 (bp/cls (str "tree-node-content-" (dec (count path)))))}
                  (when-not disabled
                    {:on-click        (call on-node-click)
                     :on-context-menu (call on-node-context-menu)
                     :on-double-click (call on-node-double-click)
                     :on-mouse-enter  (call on-node-mouse-enter)
                     :on-mouse-leave  (call on-node-mouse-leave)}))
      (if has-caret
        [icon/icon {:icon     :chevron-right
                    :title    (if is-expanded "Collapse group" "Expand group")
                    :class    (bp/cx (bp/cls "tree-node-caret")
                                     (bp/cls (if is-expanded "tree-node-caret-open"
                                                 "tree-node-caret-closed")))
                    :on-click (when-not disabled
                                (fn [e]
                                  (.stopPropagation e)
                                  (when-let [f (if is-expanded on-node-collapse on-node-expand)]
                                    (f node path e))))}]
        [:span {:class (bp/cls "tree-node-caret-none")}])
      (icon/render-icon icon {:class (bp/cls "tree-node-icon")})
      [:span {:class (bp/cls "tree-node-label")} label]
      (when (some? secondary-label)
        [:span {:class (bp/cls "tree-node-secondary-label")} secondary-label])]
     [collapse/collapse {:is-open (boolean is-expanded)}
      (when (seq child-nodes)
        [node-list child-nodes path handlers nil])]]))

(defn- node-list
  [nodes path handlers class]
  [:ul {:class (bp/cx (bp/cls "tree-node-list") class)}
   (map-indexed (fn [i n]
                  ^{:key (str (:id n))} [tree-node n (conj path i) handlers])
                nodes)])

(defn tree
  "Props: `:contents`, `:compact`, `:class` and the handlers `:on-node-click`,
   `:on-node-collapse`, `:on-node-context-menu`, `:on-node-double-click`,
   `:on-node-expand`, `:on-node-mouse-enter`, `:on-node-mouse-leave`."
  [{:keys [contents compact class] :as props}]
  [:div {:class (bp/cx (bp/cls "tree") class {(bp/cls "compact") compact})}
   [node-list contents [] (dissoc props :contents :compact :class) (bp/cls "tree-root")]])
