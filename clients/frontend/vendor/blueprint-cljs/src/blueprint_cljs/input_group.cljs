(ns blueprint-cljs.input-group
  "InputGroup: a text input with an icon or element on the left and an action on the right.
   `[input-group {:left-icon :search :placeholder \"Filter…\" :value v
                  :on-value-change (fn [value input-el]) :right-element [button …]}]`
   Props: `:intent`, `:size`/`:large`/`:small`, `:fill`, `:round`, `:disabled`,
   `:read-only`, `:left-icon`, `:left-element`, `:right-element`, `:input-ref` (fn),
   `:input-class-name`, `:input-size`, `:async-control`, `:tag-name`, `:type`; the rest
   goes to the `<input>`."
  (:require [reagent.core :as r]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.common :as c]
            [blueprint-cljs.icon :as icon]
            [blueprint-cljs.async-input :as async-input]))

(def ^:private own-keys
  [:async-control :class :fill :input-class-name :input-ref :input-size :intent :large
   :left-element :left-icon :on-value-change :right-element :round :size :small :tag-name
   :style])

(defn- measure!
  [widths k node]
  (let [w (when node (.-clientWidth node))]
    (when (or (and (nil? w) (some? (get @widths k)))
              (and w (or (nil? (get @widths k)) (> (js/Math.abs (- w (get @widths k))) 2))))
      (swap! widths assoc k w))))

(defn input-group
  [& _]
  (let [widths (r/atom {})
        left   (atom nil)
        right  (atom nil)
        update! (fn []
                  (measure! widths :left @left)
                  (measure! widths :right @right))]
    (r/create-class
     {:display-name         "InputGroup"
      :component-did-mount  update!
      :component-did-update update!
      :reagent-render
      (fn [{:keys [async-control class disabled fill input-class-name input-ref input-size
                   intent left-element left-icon on-change on-value-change read-only
                   right-element round style tag-name type]
            :or   {tag-name :div type "text"}
            :as   props}]
        (let [{lw :left rw :right} @widths
              input-props (merge (bp/dom-props props own-keys)
                                 {:type          type
                                  :aria-disabled disabled
                                  :class         (bp/cx (bp/cls "input") input-class-name)
                                  :size          input-size
                                  :style         (cond-> (or style {})
                                                   lw (assoc :padding-left lw)
                                                   rw (assoc :padding-right rw))
                                  :on-change     (fn [e]
                                                   (when on-change (on-change e))
                                                   (when on-value-change
                                                     (on-value-change (.. e -target -value)
                                                                      (.-target e))))})]
          [tag-name {:class (bp/cx (bp/cls "input-group") (bp/intent-class intent)
                                   {(bp/cls "disabled")  disabled
                                    (bp/cls "read-only") read-only
                                    (bp/cls "fill")      fill
                                    (bp/cls "round")     round}
                                   (c/size-class props)
                                   class)}
           (cond
             (some? left-element)
             [:span {:class (bp/cls "input-left-container")
                     :ref   #(reset! left %)}
              left-element]

             (some? left-icon)
             (do (reset! left nil)
                 (icon/render-icon left-icon {:aria-hidden true :tab-index -1})))
           (if async-control
             [async-input/async-input (assoc input-props :input-ref input-ref)]
             [:input (assoc input-props :ref input-ref)])
           (if (some? right-element)
             [:span {:class (bp/cls "input-action") :ref #(reset! right %)}
              right-element]
             (do (reset! right nil) nil))]))})))
