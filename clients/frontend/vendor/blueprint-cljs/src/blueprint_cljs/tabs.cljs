(ns blueprint-cljs.tabs
  "Tabs: `[tabs {:id :t} [tab {:id :a :title \"A\" :panel [...]}] [tabs-expander] …]`.
   Children that are not `tab` elements render in the tab list as they are."
  (:require [reagent.core :as r]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.icon :as icon]
            [blueprint-cljs.tag :as tag]))

(defn tabs-expander
  "Fills the space between tabs, pushing the following ones to the end of the list."
  []
  [:div {:class (bp/cls "flex-expander")}])

(defn tab
  "Placeholder element read by `tabs`. Props: `:id` (required), `:title`, `:panel`
   (hiccup or `(fn [{:keys [tab-panel-id tab-title-id]}])`), `:panel-class-name`,
   `:disabled`, `:icon`, `:tag-content`, `:tag-props`, `:class`; children follow the title."
  [{:keys [panel class]}]
  [:div {:class (bp/cx (bp/cls "tab-panel") class) :role "tablist"}
   (if (fn? panel) (panel {:tab-panel-id "" :tab-title-id ""}) panel)])

(defn- id-str [id] (if (keyword? id) (name id) (str id)))

(defn- tab-ids
  [parent-id id]
  {:tab-panel-id (str (bp/cls "tab-panel") "_" (id-str parent-id) "_" (id-str id))
   :tab-title-id (str (bp/cls "tab") "-title_" (id-str parent-id) "_" (id-str id))})

(defn- tab-element? [x] (and (vector? x) (identical? (first x) tab)))

(defn- flatten-children
  [children]
  (mapcat #(if (seq? %) % [%]) children))

(def ^:private tab-own-keys
  [:id :title :panel :panel-class-name :disabled :icon :tag-content :tag-props])

(defn- tab-title
  [{:keys [id disabled title icon tag-content tag-props class] :as props}
   children parent-id selected? on-click]
  (let [{:keys [tab-panel-id tab-title-id]} (tab-ids parent-id id)]
    (into
     [:div (merge (bp/dom-props props tab-own-keys)
                  {:aria-controls tab-panel-id
                   :aria-disabled (boolean disabled)
                   :aria-expanded selected?
                   :aria-selected selected?
                   :class         (bp/cx (bp/cls "tab") class)
                   :data-tab-id   (id-str id)
                   :id            tab-title-id
                   :on-click      (when-not disabled #(on-click id %))
                   :role          "tab"
                   :tab-index     (when-not disabled (if selected? 0 -1))})
      (icon/render-icon icon {:class (bp/cls "tab-icon")})
      title]
     (concat
      children
      [(when (some? tag-content)
         [tag/tag (merge {:minimal true :intent (if selected? :primary :none)}
                         tag-props
                         {:class (bp/cx (bp/cls "tab-tag") (:class tag-props))})
          tag-content])]))))

(defn- tab-panel
  [{:keys [id panel class panel-class-name]} parent-id selected-id]
  (let [selected? (= id selected-id)
        {:keys [tab-panel-id tab-title-id]} (tab-ids parent-id id)]
    [:div {:aria-labelledby tab-title-id
           :aria-hidden     (not selected?)
           :class           (bp/cx (bp/cls "tab-panel") class panel-class-name)
           :id              tab-panel-id
           :role            "tabpanel"}
     (if (fn? panel) (panel {:tab-panel-id tab-panel-id :tab-title-id tab-title-id}) panel)]))

(defn- tab-children [children] (filter tab-element? (flatten-children children)))

(defn- tab-props [el] (let [p (second el)] (if (map? p) p {})))

(defn- move-indicator!
  [tablist selected animate? indicator]
  (when tablist
    (let [el    (.querySelector tablist (str "." (bp/cls "tab") "[data-tab-id=\""
                                             (id-str selected) "\"]"))
          style (if el
                  (cond-> {:height    (.-clientHeight el)
                           :width     (.-clientWidth el)
                           :transform (str "translateX(" (js/Math.floor (.-offsetLeft el))
                                           "px) translateY(" (js/Math.floor (.-offsetTop el))
                                           "px)")}
                    (not animate?) (assoc :transition "none"))
                  {:display "none"})]
      (when (not= style @indicator) (reset! indicator style)))))

(defn- on-key-down
  [tablist e]
  (let [k   (.-key e)
        dir (case k ("ArrowLeft" "ArrowUp") -1 ("ArrowRight" "ArrowDown") 1 nil)]
    (when-let [focused (and dir (some-> js/document.activeElement
                                        (.closest (str "." (bp/cls "tab")))))]
      (let [enabled (vec (array-seq (.querySelectorAll
                                     tablist
                                     (str "." (bp/cls "tab") "[aria-disabled=\"false\"]"))))
            idx     (.indexOf enabled focused)
            n       (count enabled)]
        (when (>= idx 0)
          (.preventDefault e)
          (.focus (nth enabled (mod (+ idx dir n) n))))))))

(defn- on-key-press
  [e]
  (when-let [t (some-> (.-target e) (.closest (str "." (bp/cls "tab"))))]
    (when (#{"Enter" " "} (.-key e))
      (.preventDefault e)
      (.click t))))

(defn tabs
  "Props: `:id` (required), `:selected-tab-id` (controlled), `:default-selected-tab-id`,
   `:on-change` `(fn [new-id prev-id event])`, `:animate` (true), `:fill`, `:large`,
   `:size` `:medium` | `:large`, `:render-active-tab-panel-only`, `:vertical`, `:class`."
  [{:keys [selected-tab-id default-selected-tab-id]} & children]
  (let [local     (r/atom (or selected-tab-id default-selected-tab-id
                              (some-> (first (tab-children children)) tab-props :id)))
        indicator (r/atom nil)
        tablist   (atom nil)
        props     (atom nil)
        selected  (fn [] (let [p @props]
                           (if (contains? p :selected-tab-id) (:selected-tab-id p) @local)))
        update!   (fn [first?]
                    (when (:animate @props true)
                      (move-indicator! @tablist (selected) (not first?) indicator)))]
    (r/create-class
     {:display-name "Tabs"
      :component-did-mount (fn [_] (update! true))
      :component-did-update (fn [_ _] (update! false))
      :reagent-render
      (fn [{:keys [id animate fill large size render-active-tab-panel-only vertical class
                   on-change]
            :or   {animate true}
            :as   p}
           & children]
        (reset! props p)
        (let [sel      (selected)
              kids     (flatten-children children)
              click    (fn [tid e]
                         (when on-change (on-change tid sel e))
                         (when-not (contains? @props :selected-tab-id) (reset! local tid)))
              size     (or size (when large :large))
              panels   (cond->> (map tab-props (filter tab-element? kids))
                         render-active-tab-panel-only (filter #(= (:id %) sel)))]
          [:div {:class (bp/cx (bp/cls "tabs") class
                               {(bp/cls "vertical") vertical (bp/cls "fill") fill})}
           (into
            [:div {:class        (bp/cx (bp/cls "tab-list")
                                        {(bp/cls "large") (= size :large)})
                   :on-key-down  #(on-key-down @tablist %)
                   :on-key-press on-key-press
                   :ref          #(reset! tablist %)
                   :role         "tablist"}
             (when animate
               [:div {:class (bp/cls "tab-indicator-wrapper") :style @indicator}
                [:div {:class (bp/cls "tab-indicator")}]])]
            (map-indexed
             (fn [i el]
               (with-meta
                 (if (tab-element? el)
                   (let [tp (tab-props el)]
                     (tab-title tp (drop (if (map? (second el)) 2 1) el) id
                                (= (:id tp) sel) click))
                   el)
                 {:key (if (tab-element? el) (str "t-" (id-str (:id (tab-props el)))) i)}))
             kids))
           (for [tp panels :when (some? (:panel tp))]
             ^{:key (id-str (:id tp))} [tab-panel tp id sel])]))})))

(defn tab-ids-for
  "The DOM ids Blueprint gives a tab's title and panel, for `aria-*` wiring by callers."
  [parent-id id]
  (tab-ids parent-id id))
