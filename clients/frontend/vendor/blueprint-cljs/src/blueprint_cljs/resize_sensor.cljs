(ns blueprint-cljs.resize-sensor
  "ResizeSensor: calls `:on-resize` with the ResizeObserver entries whenever its single
   child's DOM element (or, with `:observe-parents`, any ancestor) changes size."
  (:require [reagent.core :as r]))

(defn observe!
  "Observes `el` (and its ancestors when `parents?`); returns a fn that stops observing."
  [el on-resize parents?]
  (if (and el (exists? js/ResizeObserver))
    (let [obs (js/ResizeObserver. (fn [entries] (on-resize (vec entries))))]
      (.observe obs el)
      (when parents?
        (loop [p (.-parentElement el)]
          (when p (.observe obs p) (recur (.-parentElement p)))))
      #(.disconnect obs))
    (constantly nil)))

(defn- with-ref
  "The hiccup element `child` with `ref` as its :ref, called before any ref it had."
  [child ref]
  (if (and (vector? child) (keyword? (first child)))
    (let [[tag a & more] child
          attrs (if (map? a) a {})
          own   (:ref attrs)
          r     (if (fn? own) (fn [el] (ref el) (own el) nil) ref)]
      (with-meta (into [tag (assoc attrs :ref r)] (if (map? a) more (rest child)))
        (meta child)))
    child))

(defn resize-sensor
  "Props: `:on-resize` `(fn [entries])`, `:observe-parents`, `:target-ref` (a fn returning
   the element to observe). Exactly one child; without `:target-ref` it is a hiccup DOM
   element, whose node is observed."
  [_props _child]
  (let [stop    (atom nil)
        current (atom nil)
        props   (atom nil)
        node    (atom nil)
        ref     #(reset! node %)
        sync!   (fn [_ force?]
                  (let [{:keys [target-ref observe-parents]} @props
                        el (if target-ref (target-ref) @node)]
                    (when (or force? (not (identical? el @current)))
                      (some-> @stop (apply []))
                      (reset! current el)
                      (reset! stop (observe! el
                                             #(some-> (:on-resize @props) (apply [%]))
                                             observe-parents)))))]
    (r/create-class
     {:display-name "ResizeSensor"
      :component-did-mount (fn [this] (sync! this false))
      :component-did-update
      (fn [this [_ old]]
        (sync! this (not= (:observe-parents old) (:observe-parents @props))))
      :component-will-unmount (fn [_] (some-> @stop (apply [])) (reset! current nil))
      :reagent-render (fn [p child] (reset! props p) (with-ref child ref))})))
