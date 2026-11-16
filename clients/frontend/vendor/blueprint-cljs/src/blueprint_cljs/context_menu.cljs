(ns blueprint-cljs.context-menu
  "ContextMenu: right-click opens a menu at the mouse position. `[context-menu {:content
   …} child…]` wraps its children; `show-context-menu!` / `hide-context-menu!` open one
   imperatively."
  (:require [reagent.core :as r]
            [reagent.dom.client :as rdc]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.popover :as popover]
            [blueprint-cljs.portal :as portal]))

(defn context-menu-popover
  "Props: `:content`, `:is-open`, `:target-offset` {:left :top} (px, viewport),
   `:on-close`, `:is-dark-theme`, `:popover-class-name`, `:transition-duration` (100), plus
   popover props."
  [{:keys [content is-open target-offset on-close is-dark-theme popover-class-name]
    :as props}]
  [popover/popover
   (merge {:placement           :right-start
           :transition-duration 100
           :modifiers           {:flip {:rootBoundary "viewport"}
                                 :shift {:rootBoundary "viewport"}}}
          (dissoc props :content :is-open :target-offset :on-close :is-dark-theme
                  :popover-class-name)
          {:content            [:div {:on-context-menu #(.preventDefault %)} content]
           :is-open            (boolean is-open)
           :enforce-focus      false
           :has-backdrop       true
           :backdrop-props     {:class (bp/cls "context-menu-backdrop")}
           :minimal            true
           :modifiers          (merge {:flip {:rootBoundary "viewport"}
                                       :shift {:rootBoundary "viewport"}}
                                      (:modifiers props) {:arrow false})
           :on-interaction     (fn [open? _] (when (and (not open?) on-close) (on-close)))
           :popover-class-name (bp/cx (bp/cls "context-menu-popover") popover-class-name
                                      {bp/dark-class is-dark-theme})
           :position-strategy  :fixed
           :render-target
           (fn [{:keys [ref]}]
             [portal/portal
              [:div {:class (bp/cls "context-menu-virtual-target")
                     :style (or target-offset {:left 0 :top 0})
                     :ref   ref}]])})])

(defn context-menu
  "Props: `:content` (hiccup, or `(fn [{:keys [target-offset mouse-event is-open]}])`),
   `:disabled`, `:on-close`, `:on-context-menu`, `:popover-props`, `:tag-name` (:div),
   `:class`; other keys go to the wrapper. Children: what is right-clicked."
  [& _]
  (let [state (r/atom {:open? false :offset nil :event nil})
        el    (atom nil)]
    (fn [& args]
      (let [[props children] (if (map? (first args)) [(first args) (rest args)] [{} args])
            {:keys [content disabled on-close on-context-menu popover-props tag-name class]
             :or   {tag-name :div}} props
            {:keys [open? offset event]} @state
            menu (when-not disabled
                   (if (fn? content)
                     (content {:target-offset offset :mouse-event event :is-open open?})
                     content))
            close! (fn []
                     (swap! state assoc :open? false)
                     (when on-close (on-close)))]
        (into [(keyword tag-name)
               (merge (bp/dom-props props [:content :disabled :on-close :on-context-menu
                                           :popover-props :tag-name :class])
                      {:class (bp/cx class (bp/cls "context-menu"))
                       :ref   #(do (reset! el %) nil)
                       :on-context-menu
                       (fn [e]
                         (when (and (not disabled) (some? content))
                           (.preventDefault e)
                           (.persist e)
                           (reset! state {:open? true
                                          :offset {:left (.-clientX e) :top (.-clientY e)}
                                          :event e}))
                         (when on-context-menu (on-context-menu e)))})
               (when menu
                 [context-menu-popover
                  (merge popover-props
                         {:content       menu
                          :is-open       open?
                          :target-offset offset
                          :on-close      close!
                          :is-dark-theme (boolean (some-> @el
                                                          (.closest (str "." bp/dark-class))))
                          :key           (if offset
                                           (str (:left offset) "x" (:top offset))
                                           "default")})])]
              children)))))

(defonce ^:private singleton (atom nil))

(defn hide-context-menu!
  []
  (when-let [{:keys [el root]} @singleton]
    (rdc/unmount root)
    (.remove el)
    (reset! singleton nil)))

(defn show-context-menu!
  "Opens a context menu outside any component. `props`: `:content`, `:target-offset`
   {:left :top}, `:is-dark-theme`, `:on-close`, popover props."
  [props]
  (hide-context-menu!)
  (let [el   (.createElement js/document "div")
        root (rdc/create-root el)]
    (.appendChild (.-body js/document) el)
    (reset! singleton {:el el :root root})
    (rdc/render root
                [context-menu-popover
                 (merge props
                        {:is-open  true
                         :on-close (fn []
                                     (when-let [f (:on-close props)] (f))
                                     (js/setTimeout hide-context-menu! 120))})])))
