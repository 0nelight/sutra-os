(ns blueprint-cljs.portal
  "Portal: renders its children into a `bp-portal` element appended to `:container`
   (default `document.body`). The portal element takes `bp-dark` while the place it is
   rendered from sits inside a `.bp-dark` ancestor, so portalled content keeps the theme."
  (:require [reagent.core :as r]
            ["react-dom" :as react-dom]
            [blueprint-cljs.core :as bp]))

(defn- split-classes [s]
  (when (and s (not= s "")) (.split (str s) " ")))

(defn- add-classes! [el s]
  (doseq [c (split-classes s) :when (not= c "")] (.add (.-classList el) c)))

(defn- remove-classes! [el s]
  (doseq [c (split-classes s) :when (not= c "")] (.remove (.-classList el) c)))

(defn- stop-propagation [e] (.stopPropagation e))

(defn portal
  "Props: `:container` DOM element, `:class-name` added to the portal element,
   `:dark-source` (fn [] element) whose `.bp-dark` ancestry decides the theme (default:
   where the portal sits in the tree), `:on-children-mount`, `:stop-propagation-events`
   (event names stopped at the portal element)."
  [_props & _children]
  (let [el       (r/atom nil)
        anchor   (atom nil)
        observer (atom nil)
        props-of (fn [this] (let [p (second (r/argv this))] (if (map? p) p {})))
        sync-dark!
        (fn [this]
          (when-let [e @el]
            (let [src   (or (some-> (:dark-source (props-of this)) (apply []))
                            @anchor)
                  sync! (fn [c]
                          (let [on? (boolean (some-> src (.closest (str "." c))))]
                            (when (not= on? (.contains (.-classList e) c))
                              (.toggle (.-classList e) c on?))))]
              (sync! bp/dark-class)
              (sync! (bp/cls "root")))))]
    (r/create-class
     {:display-name "blueprint-cljs.portal"
      :component-did-mount
      (fn [this]
        (let [{:keys [container class-name stop-propagation-events on-children-mount]}
              (props-of this)
              e (.createElement js/document "div")]
          (.add (.-classList e) (bp/cls "portal"))
          (add-classes! e class-name)
          (doseq [ev stop-propagation-events]
            (.addEventListener e (name ev) stop-propagation))
          (.appendChild (or container (.-body js/document)) e)
          (reset! el e)
          (sync-dark! this)
          (let [mo (js/MutationObserver. #(sync-dark! this))]
            (.observe mo (.-documentElement js/document)
                      #js {:attributes true :attributeFilter #js ["class"] :subtree true})
            (reset! observer mo))
          (when on-children-mount (js/requestAnimationFrame on-children-mount))))
      :component-did-update
      (fn [this [_ old]]
        (let [new (props-of this)]
          (when-let [e @el]
            (when (not= (:class-name old) (:class-name new))
              (remove-classes! e (:class-name old))
              (add-classes! e (:class-name new))))
          (sync-dark! this)))
      :component-will-unmount
      (fn [_]
        (some-> @observer .disconnect)
        (some-> @el .remove)
        (reset! el nil))
      :reagent-render
      (fn [props & children]
        (let [[p children] (if (map? props) [props children] [{} (cons props children)])]
          [:<>
           (when-not (:dark-source p)
             [:span {:ref    #(do (reset! anchor %) nil)
                     :hidden true
                     :class  (bp/cls "portal-anchor")}])
           (when-let [e @el]
             (react-dom/createPortal (r/as-element (into [:<>] children)) e))]))})))
