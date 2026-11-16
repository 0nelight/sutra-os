(ns sutra-ui.graph-editor
  (:require [reagent.core :as r]
            [sutra-ui.ui.core :as ui]
            [clojure.string :as str]
            [sutra-ui.state.canvas :as canvas]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.colors :as colors]
            [sutra-ui.state.forms :as fs]
            [sutra-ui.state.graph :as graph]
            [sutra-ui.state.instances :as instances]
            [sutra-ui.state.ontology :as ontology]
            [sutra-ui.state.panel :as panel]
            [sutra-ui.state.selection :as selection]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.forms.link :as link-forms]
            [sutra-ui.ui.hotkeys :as hotkeys]
            [sutra.i18n.core :as i18n]
            [sutra-ui.graph-editor.shared :as gs]
            [sutra-ui.graph-editor.edges :as gedge]
            [sutra-ui.graph-editor.events :as gev]
            [sutra-ui.state.db :as db]
            [sutra-ui.feature :as feature]
            [sutra-ui.graph-editor.menus :as gm]
            [sutra-ui.graph-editor.layout :as gl]
            [sutra-ui.graph-editor.kinds :as kinds]
            ["reactflow" :default ReactFlow
             :refer [Background Controls MiniMap
                     applyNodeChanges applyEdgeChanges]]
            ["react" :as react]))

(defn- node-indicators
  [^js data]
  (let [issues (.-issues data) syncs (.-syncs data) trashed? (.-trashed data)]
    (when (or (pos? issues) (pos? syncs) trashed?)
      [:div {:class "graph-node__indicators"}
       (when (pos? issues)
         [ui/tooltip {:content (i18n/t :ui/ge-open-issues {:n issues})}
          [:span {:class "graph-node__indicator graph-node__indicator--issues"}
           (str "! " issues)]])
       (when (pos? syncs)
         [ui/tooltip {:content (i18n/t :ui/ge-syncs {:n syncs})}
          [:span {:class "graph-node__indicator"}
           "⇄"]])
       (when trashed?
         [ui/tooltip {:content (i18n/t :ui/ge-trashed)}
          [:span {:class "graph-node__indicator"}
           "🗑"]])])))

(defn- node-class
  [base ^js data]
  (str base (when (.-trashed data) " is-trashed")))

(defn- ctrl-style
  [ctrl?]
  {:outline (when ctrl? (str "3px solid " gs/green))
   :cursor  (if ctrl? "crosshair" "default")})

(defn- confirm-delete-type!
  [type-kw]
  (db/send! [:shared.graph/confirm (gev/delete-type-ask type-kw)
             ontology/delete-object-type! type-kw]))

(defn- axis-button
  [{:keys [open? title on-click]} glyph]
  [ui/tooltip {:content title}
   [:button {:class (str "graph-node__rel" (when open? " graph-node__rel--on"))
             :on-click (fn [e] (.stopPropagation e) (on-click))}
    glyph]])

(defn- object-type-node-inner
  [props]
  (let [data       (.-data props)
        label      (.-label ^js data)
        sym        (or (.-symbol ^js data) (some-> label first str))
        display    (or (.-displayName ^js data) label)
        ctrl?      (.-ctrlHeld ^js data)
        staged?    (.-staged? ^js data)
        rel-count  (.-relCount ^js data)
        rel-open?  (.-relOpen ^js data)
        act-count  (.-actCount ^js data)
        act-open?  (.-actOpen ^js data)
        paint      (.-paint ^js data)
        upper-col  (cond (or staged? ctrl?) gs/green
                         paint              paint
                         :else              gs/blue)
        type-kw    (keyword label)]
    [:div {:class (node-class "object-node" data)
           :style {:outline (cond ctrl?   (str "3px solid " gs/green)
                                  staged? (str "2px dashed " gs/green))
                   :cursor (if ctrl? "crosshair" "default")
                   :box-shadow (gs/box-shadow-for {:staged? staged? :ctrl? ctrl?})}}
     (gs/direction-handles "source" "target")
     (gs/expand-arrows (.-expandId ^js data))
     (when ctrl? (gs/full-overlay-handle "source-full"))
     [node-indicators data]
     [:div {:class "graph-node__symbol"
            :style {:background upper-col :color (gs/ink-on upper-col)}}
      [:span sym]
      (when (feature/capable? (shell/canvas-mode) :delete-object-type)
        [ui/tooltip {:content (i18n/t :ui/delete-object-type-hint)}
         [ui/button {:small    true
                     :minimal  true
                     :icon     :cross
                     :class    "graph-node__delete"
                     :on-click (fn [e] (.stopPropagation e) (confirm-delete-type! type-kw))
                     :aria-label (i18n/t :ui/delete-object-type-hint)}]])
      [:div {:class "graph-node__axes"}
       (when (and rel-count (pos? rel-count))
         [axis-button {:open? rel-open?
                       :title (i18n/t (if rel-open? :ui/ge-relations-hide
                                          :ui/ge-relations-show)
                                      {:n rel-count})
                       :on-click #(graph/toggle-relations! type-kw)}
          "⛓"])
       (when (and act-count (pos? act-count))
         [axis-button {:open? act-open?
                       :title (i18n/t (if act-open? :ui/ge-actions-hide :ui/ge-actions-show)
                                      {:n act-count})
                       :on-click #(graph/toggle-type-actions! type-kw)}
          [gs/hammer-icon 12]])]]
     [:div {:class "graph-node__name"} [:span display]]]))

(defn- object-instance-node-inner [props]
  (let [data    (.-data props)
        color   (.-color data)
        sym     (.-symbol ^js data)
        staged? (.-staged? ^js data)
        ctrl?   (.-ctrlHeld ^js data)]
    [:div {:class (str "object-instance"
                       (when (.-selected props) " is-selected")
                       (when staged? " is-staged"))
           :style {:outline (when ctrl? (str "3px solid " gs/green))
                   :cursor  (if ctrl? "crosshair" "default")}}
     (when ctrl? (gs/full-overlay-handle "src-full"))
     (gs/direction-handles "src" "tgt")
     [:span {:class (str "object-instance__tile" (when sym " object-instance__tile--symbol"))
             :style {"--tile" color}}
      (or sym (some-> (.-typeLabel ^js data) (subs 0 1) str/upper-case))]
     [:span {:class "object-instance__text"}
      [:span {:class "object-instance__title"} (.-label data)]
      [:span {:class "object-instance__type"} (.-typeLabel ^js data)]]]))

(defn- flat-node-style
  [accent ctrl?]
  (assoc (ctrl-style ctrl?) :background accent :color (gs/ink-on accent)))

(defn- datasource-node-inner [props]
  (let [data    (.-data props)
        label   (.-label ^js data)
        db-type (.-dbType ^js data)
        ctrl?   (.-ctrlHeld ^js data)
        accent  (gs/kind-accent :datasource (.-paint ^js data))]
    [:div {:class (node-class "flat-node datasource-node" data)
           :style (flat-node-style accent ctrl?)}
     (gs/direction-handles "ds-src" "ds-tgt")
     (gs/expand-arrows (.-expandId ^js data))
     [node-indicators data]
     [:div {:class "flat-node__title"}
      [:span {:class "flat-node__glyph"} (colors/node-kind-icon :datasource)]
      [:span label]]
     (when (seq db-type)
       [:div {:class "flat-node__sub"} db-type])]))

(defn- dataset-node-inner
  [props]
  (let [data    (.-data props)
        label   (.-label ^js data)
        cols    (.-cols ^js data)
        defines (.-definesOts ^js data)
        ctrl?   (.-ctrlHeld ^js data)
        accent  (gs/kind-accent :dataset (.-paint ^js data))]
    [ui/tooltip {:content (str/join " · "
                                    (remove nil?
                                            [(when (and cols (pos? cols))
                                               (i18n/t :ui/cols-count {:n cols}))
                                             (when (.-unstructured ^js data)
                                               (i18n/t :ui/ge-unstructured))]))}
     [:div {:class (node-class "flat-node dataset-node" data)
            :style (cond-> (flat-node-style accent ctrl?)
                     (.-unstructured ^js data) (assoc :border-style "dashed"
                                                      :border-width "2px"
                                                      :border-color gs/ink-dark))}
      (gs/direction-handles "t-src" "t-tgt")
      (gs/expand-arrows (.-expandId ^js data))
      [node-indicators data]
      (when (and defines (pos? defines))
        [ui/tooltip
         {:content (i18n/t :ui/ge-defines-object-types {:n defines})}
         [:button {:class "graph-node__defines"
                   :on-click (fn [e]
                               (.stopPropagation e)
                               (graph/expose-dataset-object-types! (keyword label)))}]])
      [:div {:class "flat-node__title"} [:span label]]]]))

(defn- group-node-inner
  [props]
  (let [data  (.-data props)
        label (.-label ^js data)
        color (.-color ^js data)
        cnt   (.-count ^js data)]
    [:div {:class "group-node"
           :style {:border (str "3px solid " color)
                   :border-radius "12px" :background "var(--c-surface)"
                   :min-width "150px" :font-family "sans-serif"
                   :font-size "12px" :overflow "hidden" :cursor "pointer"
                   :box-shadow "0 2px 6px rgba(17,20,24,0.12)"}}
     (gs/direction-handles "g-src" "g-tgt")
     [:div {:class "graph-node__kind--group" :style {:background color}}
      [:span label] [:span "▣"]]
     [:div {:class "graph-node__group-count"}
      (i18n/t :ui/ge-group-count {:n cnt})]]))

(defn- action-node-inner
  [props]
  (let [data   (.-data props)
        label  (.-label ^js data)
        n      (.-rulesCount ^js data)
        ctrl?  (.-ctrlHeld ^js data)
        accent (gs/kind-accent :action (.-paint ^js data))]
    [ui/tooltip {:content (when (and n (pos? n))
                            (i18n/t (if (= 1 n) :ui/ge-rules-one :ui/ge-rules-many) {:n n}))}
     [:div {:class (node-class "action-node" data)
            :style (ctrl-style ctrl?)}
      (gs/direction-handles "ac-src" "ac-tgt")
      [node-indicators data]
      [:div {:class "action-node__band" :style {:background accent :color (gs/ink-on accent)}}
       [gs/hammer-icon 16]]
      [:div {:class "action-node__name"} (kinds/display-name :action label)]]]))

(defn- artifact-node-inner
  [props]
  (let [data   (.-data props)
        ctrl?  (.-ctrlHeld ^js data)
        status (.-status ^js data)
        accent (gs/kind-accent :application (.-paint ^js data))]
    [:div {:class (node-class "artifact-node" data)
           :style (ctrl-style ctrl?)}
     (gs/direction-handles "app-src" "app-tgt")
     (gs/expand-arrows (.-expandId ^js data))
     [node-indicators data]
     [:div {:class "artifact-node__band" :style {:background accent :color (gs/ink-on accent)}}
      [:span (colors/node-kind-icon :application)]
      [:span (str (i18n/t :ui/application) (when (seq status) (str " · " status)))]]
     [:div {:class "artifact-node__name"} (.-label ^js data)]]))

(defn- writeback-node-inner
  [props]
  (let [data   (.-data props)
        ctrl?  (.-ctrlHeld ^js data)
        accent (gs/kind-accent :writeback (.-paint ^js data))]
    [ui/tooltip
     {:content (i18n/t :ui/ge-writeback-hint {:name (.-label ^js data)})}
     [:div {:class "flat-node writeback-node"
            :style (flat-node-style accent ctrl?)}
      (gs/direction-handles "wb-src" "wb-tgt")
      (gs/expand-arrows (.-expandId ^js data))
      [:span {:class "graph-node__defines graph-node__defines--static"}]
      [:div {:class "flat-node__title"}
       [:span {:class "flat-node__glyph"} (colors/node-kind-icon :writeback)]
       [:span (.-label ^js data)]]
      [:div {:class "flat-node__sub"} (i18n/t :ui/kind-writeback)]]]))

(def node-types
  {"objectType"     (gs/as-node object-type-node-inner)
   "objectInstance" (gs/as-node object-instance-node-inner)
   "datasource"     (gs/as-node datasource-node-inner)
   "dataset"        (gs/as-node dataset-node-inner)
   "actionType"     (gs/as-node action-node-inner)
   "application"    (gs/as-node artifact-node-inner)
   "writeback"      (gs/as-node writeback-node-inner)
   "group"          (gs/as-node group-node-inner)})

(defn- ontology-edge [props]
  (gedge/floating-edge-impl
   {:stroke gs/grey :width 2 :dasharray nil
    :staged-width 2.5 :staged-dasharray "8,5"
    :confirm-delete? true
    :on-delete (fn [edge-id _data] (ontology/delete-link-by-id! edge-id))}
   props))

(defn- instance-edge [props]
  (gedge/floating-edge-impl
   {:stroke "var(--gray4)" :width 1.25 :dasharray nil
    :staged-width 2 :staged-dasharray "6,4"
    :font-size 10}
   props))

(defn- lineage-edge
  [props]
  (let [edits? (= "edits" (some-> ^js (.-data props) .-kind))]
    (gedge/floating-edge-impl
     {:stroke "var(--gray2)" :width 1 :dasharray (when edits? "5,4")
      :staged-width 2 :staged-dasharray "6,4"
      :font-size 10}
     props)))

(defn- relation-edge
  [props]
  (gedge/floating-edge-impl
   {:stroke gs/grey :width 2 :dasharray nil
    :staged-width 2 :staged-dasharray nil
    :font-size 10}
   props))

(def ^:private edge-types
  {"floatingEdge"         ontology-edge
   "instanceFloatingEdge" instance-edge
   "lineageEdge"          lineage-edge
   "relationEdge"         relation-edge})

(def ^:private node-types-js (clj->js node-types))

(def ^:private edge-types-js (clj->js edge-types))

(defn- spots
  [rf-nodes]
  (into {} (for [^js n rf-nodes
                 :let [p (.-position n)]
                 :when p]
             [(.-id n) {:x (.-x p) :y (.-y p)}])))

(defn- links
  [rf-edges]
  (mapv (fn [^js e] [(.-source e) (.-target e)]) rf-edges))

(defn- shift
  [id->xy dx dy]
  (update-vals id->xy (fn [{:keys [x y]}] {:x (+ x dx) :y (+ y dy)})))

(defn- top-left
  [ps]
  {:x (apply min (map :x ps)) :y (apply min (map :y ps))})

(defn- anchored
  [id->xy at]
  (let [o (top-left (vals id->xy))]
    (shift id->xy (- (:x at) (:x o)) (- (:y at) (:y o)))))

(defn- beside
  [fresh placed]
  (if (empty? placed)
    fresh
    (let [o (top-left (vals fresh))]
      (shift fresh
             (- (+ (apply max (map :x placed)) 420) (:x o))
             (- (apply min (map :y placed)) (:y o))))))

(defn- anchor-id
  [links missing]
  (some->> (for [[s t] links
                 x [(when (and (missing s) (not (missing t))) t)
                    (when (and (missing t) (not (missing s))) s)]
                 :when x]
             x)
           seq frequencies (apply max-key val) key))

(defn- next-to-anchor
  [fresh id->xy spots anchor]
  (let [p (get spots anchor) laid (get id->xy anchor)]
    (when (and p laid)
      (shift fresh (- (:x p) (:x laid)) (- (:y p) (:y laid))))))

(defn- seed-positions
  [id->xy missing spots links]
  (let [fresh (select-keys id->xy missing)]
    (when (seq fresh)
      (or (some->> (anchor-id links missing) (next-to-anchor fresh id->xy spots))
          (beside fresh (vals (apply dissoc spots missing)))))))

(defn- arrange-plan
  [rf-nodes rf-edges sel scope]
  (let [part? (= :selection scope)
        in?   (if part? #(contains? sel %) (constantly true))
        nodes (filter #(in? (.-id ^js %)) rf-nodes)]
    {:nodes nodes
     :edges (filter #(and (in? (.-source ^js %)) (in? (.-target ^js %))) rf-edges)
     :at    (when (and part? (seq nodes)) (top-left (vals (spots nodes))))}))

(defn- algorithm
  [how {:keys [reverse? root staggered? columns]}]
  (let [hier {:reverse? reverse? :root root}]
    (case how
      :auto       [gl/hierarchy-layout {:direction "LR"}]
      :hier-lr    [gl/hierarchy-layout (assoc hier :direction "LR")]
      :hier-tb    [gl/hierarchy-layout (assoc hier :direction "TB")]
      :clustered  [gl/clustered-layout (assoc hier :direction "LR")]
      :grid       [gl/grid-layout {:columns columns :staggered? staggered?}]
      :vertical   [gl/line-layout {:axis :y}]
      :horizontal [gl/line-layout {:axis :x}]
      :circular   [gl/circular-layout {}]
      :force      [gl/elk-layout {:algorithm "force"}]
      :tree       [gl/elk-layout {:algorithm "mrtree"}])))

(defn- arrange-scope
  [selected]
  (if (> (count selected) 1) :selection :all))

(defn- layout-key
  [{:keys [code mod? shift? alt?]} scope]
  (cond
    (and mod? shift? (= "KeyA" code)) [:auto :all]
    mod?                              nil
    (= "KeyL" code) [(cond alt? :clustered shift? :hier-lr :else :auto) scope]
    alt?            nil
    (= "KeyG" code) [:grid scope]
    (= "KeyV" code) [:vertical scope]
    (= "KeyH" code) [:horizontal scope]))

(defn- ctrl-key? [k] (contains? #{"Control" "Meta"} k))

(defn- keydown-intents
  [{:keys [key mod? shift? typing?] :as press} {:keys [connect? caps selected]}]
  (let [letter (str/lower-case key)
        layout (when-not typing? (layout-key press (arrange-scope selected)))]
    (cond-> []
      (and connect? (ctrl-key? key))
      (conj [:ctrl-held true])

      (and mod? (not shift?) (= "a" letter) (not typing?))
      (conj [:select-all])

      (and (= "Escape" key) (not typing?) (seq selected))
      (conj [:clear-selection])

      layout
      (conj (into [:arrange] layout))

      (and (contains? caps :delete-key) (contains? #{"Delete" "Backspace"} key)
           (not typing?) (seq selected))
      (conj [:remove-nodes (vec selected)])

      (and (contains? caps :undo-redo) mod? (= "z" letter) (not typing?))
      (conj [(if shift? :redo :undo)]))))

(defn- id-tail
  [id]
  (when-let [i (str/index-of (str id) "/")]
    (subs id (inc i))))

(defn- click-intent
  [schema? kind id]
  (when schema?
    (let [tail (keyword (id-tail id))]
      (case kind
        "objectType"  [:open :object-type (keyword id)]
        "dataset"     [:open :dataset tail]
        "datasource"  [:open :datasource tail]
        "actionType"  [:open :action tail]
        "application" [:open :application tail]
        "writeback"   [:open :object-type tail :edits]
        "group"       [:toggle-group (id-tail id)]
        nil))))

(defn- double-click-intent
  [schema? kind id]
  (let [tail (keyword (id-tail id))]
    (if schema?
      (case kind
        "objectType" [:open :object-type (keyword id)]
        "dataset"    [:open :dataset tail]
        "datasource" [:open :datasource tail]
        "transform"  [:open :transform tail]
        nil)
      (when (= "objectInstance" kind) [:expand id]))))

(defn- connect-intent
  [^js params rf-nodes]
  (let [src       (.-source params)
        tgt       (.-target params)
        src-h     (.-sourceHandle params)
        tgt-h     (.-targetHandle params)
        reversed? (and (string? src-h) (string? tgt-h)
                       (str/starts-with? src-h "target-")
                       (str/starts-with? tgt-h "source-"))
        [from to] (if reversed? [tgt src] [src tgt])
        kind-of   (fn [id] (some (fn [^js n] (when (= id (.-id n)) (.-type n))) rf-nodes))]
    (condp = [(kind-of from) (kind-of to)]
      ["objectInstance" "objectInstance"] [:error :ui/ge-instance-connect-retired]
      ["objectType" "objectType"]         [:link-modal from to]
      [:error :ui/ge-connect-refused])))

(defn- node-label [^js n]
  (or (some-> n .-data .-label str not-empty) (.-id n)))

(defn- root-candidates
  [rf-nodes sel root]
  (->> rf-nodes
       (filter #(or (contains? sel (.-id ^js %)) (= root (.-id ^js %))))
       (sort-by node-label)))

(defonce ^:private arrange-open? (r/atom false))
(defonce ^:private arrange-settings
  (r/atom {:reverse? false :root nil :staggered? false :columns nil}))

(defn- key-press
  [^js e]
  (let [^js target (.-target e)]
    {:key     (str (.-key e))
     :code    (.-code e)
     :mod?    (boolean (or (.-ctrlKey e) (.-metaKey e)))
     :shift?  (boolean (.-shiftKey e))
     :alt?    (boolean (.-altKey e))
     :typing? (boolean (or (contains? #{"INPUT" "TEXTAREA" "SELECT"}
                                      (some-> target .-tagName))
                           (some-> target .-isContentEditable)))}))

(defn arrange!
  [how scope]
  (when-let [^js inst @instances/rf-instance]
    (let [{:keys [nodes edges at]} (arrange-plan (.getNodes inst) (.getEdges inst)
                                                 (selection/selected-node-ids) scope)
          [algo opts] (algorithm how @arrange-settings)]
      (when (seq nodes)
        (db/send! [:shared.graph/lay-out
                   {:layout [algo (into-array nodes) (into-array edges) opts]
                    :place  #(cond-> % at (anchored at))
                    :mode   (shell/canvas-mode)
                    :busy?  true}])))))

(defn- view-arrangement
  [mode]
  (when (canvas/view-of-mode? mode)
    (some-> (get-in @canvas/current-view [:payload :arrangement]) keyword)))

(defn- when-measured!
  [f]
  (letfn [(attempt [left]
            (let [^js inst @instances/rf-instance
                  nodes    (some-> inst .getNodes)]
              (if (and nodes (pos? (.-length nodes)) (every? #(.-width ^js %) nodes))
                (f nodes (.getEdges inst))
                (when (pos? left) (js/requestAnimationFrame #(attempt (dec left)))))))]
    (attempt 300)))

(defn- seed-unplaced!
  [mode ^js rf-nodes rf-edges]
  (when (and (canvas/layout-ready? mode) (pos? (.-length rf-nodes)))
    (let [missing (canvas/unpositioned-ids mode rf-nodes)
          lay-out (fn [[algo opts] nodes edges]
                    (db/send! [:shared.graph/lay-out
                               {:layout [algo nodes edges opts]
                                :place  #(seed-positions % missing (spots nodes) (links edges))
                                :mode   mode}]))]
      (when (seq missing)
        (if-let [how (view-arrangement mode)]
          (when-measured! #(lay-out (algorithm how {}) %1 %2))
          (lay-out [gl/dagre-layout {:direction "LR"}] rf-nodes rf-edges))))))

(defn- effect!
  [[op & args]]
  (case op
    :ctrl-held    (shell/set-ctrl-held! (first args))
    :select-all   (selection/select-all-nodes!)
    :clear-selection (selection/clear-selection!)
    :arrange      (apply arrange! args)
    :remove-nodes (do (graph/remove-nodes-from-graph! (first args))
                      (selection/clear-selection!))
    :undo         (graph/graph-undo!)
    :redo         (graph/graph-redo!)
    :open         (let [[kind id tab] args]
                    (panel/open-bottom-panel! kind id)
                    (when tab
                      (panel/set-panel-tab! tab)))
    :toggle-group (graph/toggle-group-collapsed! (first args))
    :expand       (canvas/expand-around! (first args))
    :link-modal   (let [[from to] args]
                    (fs/set-form! :connect-modal
                                  {:name        ""
                                   :from        from
                                   :to          to
                                   :cardinality gs/default-cardinality})
                    (shell/open-connect-modal!))
    :error        (shell/fail! (i18n/t (first args)))
    nil))

(defn- on-keydown!
  [ctx ^js e]
  (let [intents (keydown-intents (key-press e) (assoc ctx :selected (selection/selected-node-ids)))]
    (when (some #(not= :ctrl-held (first %)) intents)
      (.preventDefault e))
    (run! effect! intents)))

(def ^:private whole-graph-layouts
  [[:auto      :ui/ge-layout-all       "Ctrl/⌘ ⇧ A"]
   [:hier-lr   :ui/ge-layout-hier-lr   "⇧ L"]
   [:hier-tb   :ui/ge-layout-hier-tb   nil]
   [:clustered :ui/ge-layout-clustered "Alt L"]
   [:grid      :ui/ge-layout-grid      "G"]
   [:circular  :ui/ge-layout-circular  nil]
   [:force     :ui/ge-layout-force     nil]
   [:tree      :ui/ge-layout-tree      nil]])

(def ^:private selection-layouts
  [[:auto       :ui/ge-layout-nodes      "L"]
   [:hier-lr    :ui/ge-layout-hier-lr    "⇧ L"]
   [:hier-tb    :ui/ge-layout-hier-tb    nil]
   [:clustered  :ui/ge-layout-clustered  "Alt L"]
   [:grid       :ui/ge-layout-grid       "G"]
   [:vertical   :ui/ge-sel-vertical      "V"]
   [:horizontal :ui/ge-sel-horizontal    "H"]])

(defn- arrange-settings-panel
  []
  (let [{:keys [reverse? root staggered? columns]} @arrange-settings
        roots (when-let [^js inst @instances/rf-instance]
                (root-candidates (.getNodes inst) (selection/selected-node-ids) root))]
    [:div {:style {:padding "var(--sp-1) var(--sp-4) var(--sp-3)" :font-size "var(--fs-sm)"
                   :display "flex" :flex-direction "column" :gap "var(--sp-2)"}
           :on-click #(.stopPropagation %)}
     [ui/checkbox {:checked reverse? :class "u-m-0"
                   :on-change #(swap! arrange-settings update :reverse? not)
                   :label (i18n/t :ui/ge-layout-reverse)}]
     [:label (i18n/t :ui/ge-layout-root) " "
      [ui/html-select {:value (or root "")
                       :on-change #(swap! arrange-settings assoc :root
                                          (not-empty (.. % -target -value)))}
       [:option {:value ""} (i18n/t :ui/ge-layout-root-auto)]
       (for [^js n roots]
         ^{:key (.-id n)} [:option {:value (.-id n)} (node-label n)])]]
     [ui/checkbox {:checked staggered? :class "u-m-0"
                   :on-change #(swap! arrange-settings update :staggered? not)
                   :label (i18n/t :ui/ge-layout-staggered)}]
     [:label {:class "u-flex-row-center-6"} (i18n/t :ui/ge-layout-columns)
      [ui/numeric-input
       {:min 1 :style {:width "56px"} :small true :button-position :none
        :placeholder (i18n/t :ui/ge-layout-root-auto)
        :value (or columns "")
        :on-value-change #(swap! arrange-settings assoc :columns
                                 (let [v (js/parseInt %2)]
                                   (when (pos? v) v)))}]]]))

(defn layout-button
  []
  (let [open? @arrange-open?
        busy? (gev/arranging?)
        n     (count (selection/selected-node-ids))
        many? (> n 1)]
    [:div {:class "graph-layout-toolbar"
           :on-click #(.stopPropagation %)
           :on-context-menu #(.preventDefault %)}
     [ui/popover
      {:is-open        (and open? (not busy?))
       :on-interaction (fn [o _] (reset! arrange-open? o))
       :placement      :bottom-start
       :content
       [:div {:style (assoc (dissoc gs/menu-wrapper-style :z-index)
                            :min-width "260px" :max-height "75vh"
                            :overflow-y "auto")}
        (into [ui/menu]
              (concat
               [[ui/menu-divider {:title (i18n/t :ui/ge-layout-whole)}]]
               (for [[how label k] whole-graph-layouts]
                 [ui/menu-item {:text (i18n/t label)
                                :label k
                                :on-click #(arrange! how :all)}])
               [(when (seq (canvas/current-color-groups))
                  [ui/menu-item {:text (i18n/t :ui/layout-by-color)
                                 :on-click #(graph/layout-by-color!)}])
                [ui/menu-divider {:title (if many? (i18n/t :ui/ge-layout-selection {:n n})
                                             (i18n/t :ui/ge-layout-selection-none))}]]
               (for [[how label k] selection-layouts]
                 [ui/menu-item {:text (i18n/t label)
                                :label k
                                :disabled (not many?)
                                :on-click #(arrange! how :selection)}])))
        [:div {:style gs/menu-header-style} (i18n/t :ui/ge-layout-settings)]
        [arrange-settings-panel]]}
      [ui/tooltip {:content (i18n/t :ui/ge-layout-hint)}
       [ui/button {:loading    busy?
                   :intent     :primary
                   :outlined   (not open?)
                   :small      true
                   :icon       :layout-auto
                   :right-icon :caret-down}
        (i18n/t :ui/ge-layout-button)]]]]))

(defn- with-selection
  [^js rf-nodes sel]
  (.map rf-nodes (fn [^js n]
                   (js/Object.assign #js {} n #js {:selected (contains? sel (.-id n))}))))

(defn- pane-menu-at
  [^js inst ^js event]
  (let [flow (.screenToFlowPosition inst #js {:x (.-clientX event) :y (.-clientY event)})]
    #js {:screenX (.-clientX event)
         :screenY (.-clientY event)
         :flowX   (.-x flow)
         :flowY   (.-y flow)}))

(defn- node-menu-at
  [^js event ^js node]
  #js {:screenX (.-clientX event)
       :screenY (.-clientY event)
       :id      (.-id node)
       :type    (.-type node)})

(def ^:private canvas-keys
  [["esc"         :ui/hotkeys-cancel nil]
   ["mod+a"       :ui/hotkeys-select-all nil]
   ["mod+shift+a" :ui/ge-layout-all nil]
   ["l"           :ui/ge-layout-nodes nil]
   ["shift+l"     :ui/ge-layout-hier-lr nil]
   ["alt+l"       :ui/ge-layout-clustered nil]
   ["g"           :ui/ge-layout-grid nil]
   ["v"           :ui/ge-sel-vertical nil]
   ["h"           :ui/ge-sel-horizontal nil]
   ["delete"      :ui/hotkeys-delete-nodes :delete-key]
   ["backspace"   :ui/hotkeys-delete-nodes :delete-key]
   ["mod+z"       :ui/hotkeys-undo :undo-redo]
   ["mod+shift+z" :ui/hotkeys-redo :undo-redo]])

(defn- use-window-keys!
  [connect? mode caps]
  (react/useEffect
   (fn []
     (let [ctx       {:connect? connect? :caps caps}
           off       #(shell/set-ctrl-held! false)
           id        (hotkeys/register!
                      (for [[combo k cap] canvas-keys]
                        {:combo       combo
                         :label       [i18n/t k]
                         :group       [i18n/t :ui/hotkeys-group-graph]
                         :disabled    (and cap (not (contains? caps cap)))
                         :on-key-down (partial on-keydown! ctx)}))
           listeners {"keydown" #(when (ctrl-key? (.-key ^js %)) (on-keydown! ctx %))
                      "keyup"   #(when (ctrl-key? (.-key ^js %)) (off))
                      "blur"    (fn [_] (off))}]
       (doseq [[t f] listeners] (.addEventListener js/window t f))
       #(do (hotkeys/unregister! id)
            (doseq [[t f] listeners] (.removeEventListener js/window t f)))))
   #js [connect? mode]))

(defn- graph-canvas-inner [props]
  (let [version      (.-version ^js props)
        instv        (.-instancesVersion ^js props)
        show-fields? (.-showFields ^js props)
        ctrl-held?   (.-ctrlHeld ^js props)
        sel-push     (.-selectionPush ^js props)
        drag-select? (.-dragSelect ^js props)
        view-id      (.-viewId ^js props)
        mode         (.-mode ^js props)
        mode-kw      (keyword mode)
        caps         (feature/capabilities mode)
        schema?      (feature/schema-positions? mode)
        edit-schema? (contains? caps :edit-schema)
        connect?     (contains? caps :connect)

        [nodes set-nodes]         (react/useState #js [])
        [edges set-edges]         (react/useState #js [])
        [menu set-menu]           (react/useState nil)
        [node-menu set-node-menu] (react/useState nil)
        rf-ref                    (react/useRef nil)
        close-menus               (fn [_]
                                    (when menu (set-menu nil))
                                    (when node-menu (set-node-menu nil)))]

    (react/useEffect
     (fn []
       (let [graph     (:graph (feature/canvas mode))
             [nds eds] (map clj->js
                            (graph {:schema             @catalog/schema
                                    :positions          @canvas/node-positions
                                    :instances          @instances/instances
                                    :instance-positions @canvas/instance-positions
                                    :show-fields?       show-fields?
                                    :ctrl-held?         ctrl-held?}))]
         (set-nodes nds)
         (set-edges eds)
         (seed-unplaced! mode-kw nds eds))
       js/undefined)
     #js [version instv show-fields? ctrl-held? mode])

    (react/useEffect
     (fn []
       (when-not (:current-view-id @shell/ui-state)
         (canvas/load-canvas-layout! mode-kw))
       js/undefined)
     #js [mode view-id])

    (react/useEffect
     (fn []
       (let [sel @selection/canvas-selection]
         (set-nodes #(with-selection % sel)))
       js/undefined)
     #js [sel-push])

    (use-window-keys! connect? mode caps)

    (r/as-element
     [:div {:class "graph-canvas" :on-click close-menus}
      [:> ReactFlow
       {:nodes          nodes
        :edges          edges
        :nodeTypes      node-types-js
        :edgeTypes      edge-types-js
        :proOptions     #js {:hideAttribution true}
        :onInit         (fn [inst]
                          (set! (.-current rf-ref) inst)
                          (instances/set-rf-instance! inst))
        :onPaneContextMenu
        (fn [event]
          (.preventDefault event)
          (when-let [inst (.-current rf-ref)]
            (set-menu (pane-menu-at inst event))))
        :onPaneClick    close-menus
        :onNodeContextMenu
        (fn [event node]
          (.preventDefault event)
          (when-not (contains? (selection/selected-node-ids) (.-id node))
            (selection/select-nodes! #{(.-id node)}))
          (set-node-menu (node-menu-at event node)))
        :onNodesChange  (fn [changes]
                          (set-nodes (fn [prev] (applyNodeChanges changes prev))))
        :onNodeDragStop (fn [_event ^js node]
                          (let [pos (.-position node)]
                            ((canvas/position-update-fn mode-kw)
                             (.-id node) (.-x pos) (.-y pos))))
        :onEdgesChange  (fn [changes]
                          (set-edges (fn [prev] (applyEdgeChanges changes prev))))
        :onNodeClick       (fn [_event ^js node]
                             (some-> (click-intent schema? (.-type node) (.-id node))
                                     effect!))
        :onEdgeClick       (fn [_event ^js edge]
                             (when-not schema?
                               (effect! [:open :link (.-id edge)])))
        :onNodeDoubleClick (fn [_event ^js node]
                             (some-> (double-click-intent schema? (.-type node) (.-id node))
                                     effect!))
        :onEdgeDoubleClick (fn [_event ^js edge]
                             (when edit-schema?
                               (link-forms/open-edit-link! (.-id edge))))
        :onConnect             #(effect! (connect-intent % nodes))
        :connectionMode        "loose"
        :multiSelectionKeyCode (if connect? "Shift" #js ["Shift" "Control" "Meta"])
        :selectionKeyCode      "Shift"
        :selectionOnDrag       (boolean drag-select?)
        :deleteKeyCode         nil
        :onSelectionChange     (fn [^js params]
                                 (selection/note-canvas-selection!
                                  (map #(.-id ^js %) (or (.-nodes params) #js []))))
        :nodesDraggable        (not ctrl-held?)
        :panOnDrag             (cond ctrl-held?   false
                                     drag-select? #js [1]
                                     :else        true)
        :minZoom        0.1
        :maxZoom        2.5
        :fitView        true
        :fitViewOptions #js {:padding 0.2}}
       [:> Background {:variant "dots" :gap 20 :size 1}]
       [:> Controls]
       (when (pos? (.-length nodes))
         [:> MiniMap {:nodeStrokeWidth 3 :zoomable true :pannable true}])]

      (when (and menu (contains? caps :create-nodes))
        [gm/pane-menu {:edit-schema? edit-schema?
                       :menu         menu
                       :set-menu     set-menu}])

      (when node-menu
        [gm/node-menu {:edit-schema?  edit-schema?
                       :instances?    (not schema?)
                       :caps          caps
                       :node-menu     node-menu
                       :set-node-menu set-node-menu}])])))

(defn- edge-mode-hint []
  (when (:ctrl-held? @shell/ui-state)
    [:div {:class "graph-edge-hint" :style {:background gs/green}}
     (i18n/t :ui/ge-edge-mode-hint)]))

(defn graph-canvas []
  (let [v     @catalog/schema-version
        iv    @instances/instances-version
        show? (:show-fields? @shell/ui-state)
        ctrl? (:ctrl-held? @shell/ui-state)
        mode  (shell/canvas-mode)
        vid   (:current-view-id @shell/ui-state)
        push  @selection/selection-push
        drag? (:drag-select? (feature/canvas-settings @shell/ui-state mode))
        _i    @instances/instances
        _ip   @canvas/instance-positions]
    [:<>
     [:> graph-canvas-inner {:version          v
                             :instancesVersion iv
                             :viewId           vid
                             :showFields       show?
                             :ctrlHeld         (boolean ctrl?)
                             :selectionPush    push
                             :dragSelect       (boolean drag?)
                             :mode             (name mode)}]
     [edge-mode-hint]]))
