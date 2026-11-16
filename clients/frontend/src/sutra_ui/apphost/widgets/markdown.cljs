(ns sutra-ui.apphost.widgets.markdown
  (:require [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.ui.sel :as sel]
            [reagent.core :as r]
            [sutra-ui.ui.core :as ui :refer [checkbox divider html-table]]
            [sutra.capps.markdown :as md]
            [sutra.capps.variables :refer [deref*]]
            [sutra.kit.coerce :refer [->kw]]
            [sutra-ui.apphost.events :refer [on-click-fn]]
            [sutra-ui.apphost.interactions :as interactions]))

(def cells md/cells)

(declare inline-nodes)

(defn- field-value
  [row k]
  (let [k (->kw k)]
    (if (contains? row k) (get row k) (get (:values row) k))))

(defn- annotation-mark
  [{:keys [env on-pick]} {:keys [row layer color display active?]} s at]
  (let [{:keys [tooltip interactions]} layer
        underline? (#{:underline :dashed :both :both-dashed} display)
        fill?      (#{:highlight :both :both-dashed} display)
        dashed?    (#{:dashed :both-dashed} display)
        c          (or color "#FBD065")]
    [:mark {:class    (cond-> "capp-md__anno" active? (str " capp-md__anno--on"))
            :style    (cond-> {:background "transparent"}
                        fill?      (assoc :background c)
                        underline? (assoc :border-bottom (str "2px " (if dashed? "dashed" "solid")
                                                              " " c)))
            :on-click (fn [e] (.stopPropagation e) (on-pick layer row))}
     [:span {:data-md-at at} s]
     (when (or (seq tooltip) (seq interactions))
       [:span.capp-md__tip
        (when (seq tooltip)
          (into [:span.capp-md__tip-props]
                (map (fn [p] [:span {:key (str p)} (str (name p) ": " (field-value row p))]))
                tooltip))
        (when (seq interactions)
          [interactions/bar {:env env :items interactions :row row :default-icon "✎"}])])]))

(defn- text-node
  [{:keys [interactive ranges] :as o} {:keys [s at]}]
  (cond
    (not interactive) s
    (nil? at)         s
    :else
    (into [:<>]
          (map (fn [{:keys [s at range]}]
                 (if range
                   [annotation-mark (assoc o :on-pick (:on-annotation interactive)) range s at]
                   [:span {:data-md-at at} s])))
          (md/slices s at ranges))))

(defn- reference
  [{:keys [interactive] :as o} {:keys [kids] :as node}]
  (if-let [{:keys [ref-types behavior selected-rows last-ref on-ref]} interactive]
    (let [color (md/ref-color (md/ref-type ref-types node) node)
          on?   (md/ref-selected? behavior node {:last-ref last-ref :selected-rows selected-rows})]
      (into [:a {:class    (cond-> "capp-md__ref" on? (str " capp-md__ref--on"))
                 :role     "button"
                 :style    (when color (if on? {:background color} {:border-bottom-color color}))
                 :on-click (fn [e] (.preventDefault e) (.stopPropagation e) (on-ref node))}]
            (inline-nodes o kids)))
    (into [:span] (inline-nodes o kids))))

(defn- inline-node
  [o {:keys [t kids] :as n}]
  (case t
    :text   (text-node o n)
    :br     [:br]
    :code   [:code.capp-md__code (:s n)]
    :image  [:img.capp-md__img {:src (:src n) :alt (:alt n)}]
    :link   (into [:a {:href (:href n) :target "_blank" :rel "noreferrer noopener"}]
                  (inline-nodes o kids))
    :strong (into [:strong] (inline-nodes o kids))
    :em     (into [:em] (inline-nodes o kids))
    :strike (into [:s] (inline-nodes o kids))
    :mark   (into [:mark.capp-md__highlight] (inline-nodes o kids))
    :ref    (reference o n)))

(defn- inline-nodes
  [o nodes]
  (mapv #(inline-node o %) nodes))

(defn- inline
  [o nodes]
  (into [:<>] (inline-nodes o nodes)))

(defn- table
  [o {:keys [aligns head rows]}]
  (let [style (fn [i] (when-let [a (get aligns i)] {:text-align (name a)}))
        cell  (fn [tag i nodes] [tag {:key i :style (style i)} (inline o nodes)])]
    [html-table {:class "capp-md__table" :compact true}
     [:thead (into [:tr] (map-indexed #(cell :th %1 %2)) head)]
     (into [:tbody]
           (map (fn [row] (into [:tr] (map-indexed #(cell :td %1 %2)) row)))
           rows)]))

(defn- list-item
  [o {:keys [task kids]}]
  [:li (cond-> nil task (assoc :class "capp-md__task"))
   (when task [checkbox {:disabled true :inline true :checked (= :done task)}])
   (inline o kids)])

(defn- block
  [o {:keys [t kids] :as n}]
  (case t
    :p     (into [:p] (inline-nodes o kids))
    :h     [(keyword (str "h" (:level n))) (inline o kids)]
    :hr    [divider {:class "capp-md__hr"}]
    :pre   [:pre.capp-md__pre [:code (:s n)]]
    :table (table o n)
    :ul    (into [:ul.capp-md__ul] (map #(list-item o %)) (:items n))
    :ol    (into [:ol.capp-md__ol] (map #(list-item o %)) (:items n))
    :quote (into [:blockquote.capp-md__quote] (map (fn [l] [:p (inline o l)])) (:lines n))))

(defn- root-class
  [{:keys [class monospace? scroll? wrap? align]}]
  (cond-> "capp-md"
    monospace?       (str " capp-md--mono")
    scroll?          (str " capp-md--scroll")
    (not= false wrap?) (str " capp-md--wrap")
    align            (str " capp-md--" (name (->kw align)))
    class            (str " " class)))

(defn- view
  [{:keys [text breaks?] :as opts} interactive ranges]
  (let [o {:interactive interactive :ranges ranges :env (:env opts)}]
    (into [:div {:class (root-class opts)}]
          (map #(block o %))
          (md/parse (or text "") {:breaks? breaks?}))))

(defn- end-of
  [^js node off]
  (when node
    (let [el (if (= 3 (.-nodeType node)) (.-parentElement node) node)
          at (some-> el (.closest "[data-md-at]") (.getAttribute "data-md-at") js/parseInt)]
      (when (and at (not (js/isNaN at))) [at off]))))

(defn- dom-selection
  [source]
  (let [^js sel (js/window.getSelection)]
    (when (and sel (not (.-isCollapsed sel)))
      (when-let [m (md/selection source
                                 (end-of (.-anchorNode sel) (.-anchorOffset sel))
                                 (end-of (.-focusNode sel) (.-focusOffset sel)))]
        (let [box (.getBoundingClientRect (.getRangeAt sel 0))]
          (assoc m :x (.-left box) :y (.-top box)))))))

(defn- interactive-view
  [_opts]
  (let [last-ref (r/atom nil)
        pending  (r/atom nil)]
    (fn [{:keys [text env layers create ref-types selection-behavior selected on-ref-select
                 selected-text selection-start selection-end] :as opts}]
      (let [ctx          (:ev-ctx env)
            on-ref       (fn [{:keys [object-type pk] :as node}]
                           (reset! last-ref [object-type pk])
                           (let [row (md/ref-row node)
                                 own (:on-select (md/ref-type ref-types node))]
                             (sel/wr! selected [row])
                             (when-let [f (on-click-fn (or own on-ref-select) ctx)] (f row))))
            on-anno      (fn [{:keys [active-handle on-select]} row]
                           (sel/wr! active-handle row)
                           (when-let [f (on-click-fn on-select ctx)] (f row)))
            on-mouse-up  (fn [_]
                           (let [m (dom-selection (or text ""))]
                             (sel/wr! selected-text (:text m))
                             (sel/wr! selection-start (:start m))
                             (sel/wr! selection-end (:end m))
                             (reset! pending m)))
            shown-layers (mapv #(assoc % :active-handle (:active %) :active (deref* (:active %)))
                               layers)]
        [:div.capp-md-host {:on-mouse-up on-mouse-up}
         [view opts
          {:ref-types     ref-types
           :behavior      selection-behavior
           :selected-rows (let [v (deref* selected)] (if (map? v) [v] v))
           :last-ref      @last-ref
           :on-ref        on-ref
           :on-annotation on-anno}
          (md/annotation-ranges shown-layers)]
         (when (and @pending (seq create))
           [overlay/popover {:at [(:x @pending) (:y @pending)] :placement :top-start
                         :on-close #(reset! pending nil)}
            [:div.capp-md__create {:on-mouse-up #(.stopPropagation %)}
             [interactions/bar {:env env :items create :default-icon "✚"
                                :row {:text   (:text @pending)
                                      :start  (:start @pending)
                                      :end    (:end @pending)
                                      :values (select-keys @pending
                                                           [:text :start :end])}}]]])]))))

(defn- interactive?
  [{:keys [layers create ref-types selected selected-text selection-start selection-end]}]
  (or (seq layers) (seq create) (seq ref-types) selected selected-text selection-start
      selection-end))

(defn markdown
  [x]
  (let [opts (if (map? x) x {:text x})]
    (if (interactive? opts)
      [interactive-view opts]
      (view opts nil nil))))
