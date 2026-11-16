(ns blueprint-cljs.text-area
  "TextArea. `[text-area {:value v :on-change f :auto-resize true :fill true}]`.
   Props: `:intent`, `:size`/`:large`/`:small`, `:fill`, `:auto-resize`, `:async-control`,
   `:input-ref` (fn); the rest goes to the `<textarea>`."
  (:require [reagent.core :as r]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.common :as c]
            [blueprint-cljs.async-input :as async-input]))

(defn text-area
  [& _]
  (let [node   (atom nil)
        height (r/atom nil)
        sync!  (fn [auto-resize]
                 (when-let [n @node]
                   (when auto-resize
                     (set! (.. n -style -height) "0px")
                     (let [h (.-scrollHeight n)]
                       (set! (.. n -style -height) (str h "px"))
                       (when (not= h @height) (reset! height h))))))
        props-of (fn [this] (first (c/split-args (rest (r/argv this)))))]
    (r/create-class
     {:display-name "TextArea"
      :component-did-mount #(sync! (:auto-resize (props-of %)))
      :component-did-update
      (fn [this [_ old]]
        (let [p (props-of this)]
          (when (or (not= (:value old) (:value p)) (not= (:style old) (:style p)))
            (sync! (:auto-resize p)))))
      :reagent-render
      (fn [{:keys [async-control auto-resize class fill input-ref intent on-change style]
            :as props}]
        (let [attrs (merge (bp/dom-props props [:async-control :auto-resize :class :fill
                                                :input-ref :intent :large :size :small])
                           {:class (bp/cx (bp/cls "input") (bp/intent-class intent)
                                          (bp/cls "text-area")
                                          (c/size-class props)
                                          {(bp/cls "fill")                  fill
                                           (bp/cls "text-area-auto-resize") auto-resize}
                                          class)
                            :style (cond-> (or style {})
                                     (and auto-resize @height) (assoc :height
                                                                      (str @height "px")))
                            :on-change (fn [e]
                                         (sync! auto-resize)
                                         (when on-change (on-change e)))})
              ref   (fn [n] (reset! node n) (when input-ref (input-ref n)))]
          (if async-control
            [async-input/async-text-area (assoc attrs :input-ref ref)]
            [:textarea (assoc attrs :ref ref)])))})))
