(ns blueprint-cljs.editable-text
  "EditableText: text that turns into an input on focus/click; Enter confirms, Escape
   cancels, blur confirms. `[editable-text {:placeholder \"Edit title…\" :value v
                                            :on-change f :on-confirm f}]`
   Props: `:default-value`, `:value`, `:is-editing`, `:multiline`, `:min-lines` 1,
   `:max-lines`, `:min-width` 80, `:max-length`, `:confirm-on-enter-key`,
   `:select-all-on-focus`, `:always-render-input`, `:disabled`, `:fill`, `:intent`,
   `:type`, `:content-id`, `:element-ref`, `:custom-input-attributes`,
   `:on-edit`, `:on-cancel`, `:on-change`, `:on-confirm` (each fn of the value)."
  (:require [reagent.core :as r]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.common :as c]))

(def ^:private buffer-width 5)

(defn- px [s] (or (some-> s js/parseInt) 0))

(defn- line-height
  [el]
  (let [cs (js/getComputedStyle el)
        lh (px (.-lineHeight cs))]
    (if (pos? lh) lh (* 1.28581 (px (.-fontSize cs))))))

(defn- insert-at-caret!
  [el text]
  (let [start (.-selectionStart el)
        end   (.-selectionEnd el)
        v     (.-value el)]
    (set! (.-value el) (str (subs v 0 start) text (subs v end)))
    (set! (.-selectionStart el) (+ start (count text)))
    (set! (.-selectionEnd el) (+ start (count text)))))

(def ^:private defaults
  {:default-value "" :max-lines js/Infinity :min-lines 1 :min-width 80
   :placeholder "Click to Edit" :type "text"})

(defn editable-text
  [& args]
  (let [[init] (c/split-args args)
        init   (merge defaults init)
        v0     (if (some? (:value init)) (:value init) (:default-value init))
        st     (r/atom {:editing?   (and (:is-editing init) (not (:disabled init)))
                        :value      v0
                        :last-value v0
                        :height     0
                        :width      0})
        content (atom nil)
        input   (atom nil)
        props*  (atom init)]
    (letfn [(update-dims! []
              (when-let [el @content]
                (let [{:keys [max-lines min-lines min-width multiline]} @props*
                      lh  (line-height el)
                      sh  (cond-> (.-scrollHeight el)
                            (and multiline (:editing? @st)
                                 (re-find #"\n$" (or (.-textContent el) "")))
                            (+ lh))
                      sh  (if (pos? lh)
                            (-> sh (max (* min-lines lh)) (min (* max-lines lh)))
                            sh)
                      sh  (max sh (inc (px (.-fontSize (js/getComputedStyle el))))
                               (line-height (.-parentElement el)))
                      sw  (max (+ (.-scrollWidth el) buffer-width) min-width)]
                  (when (or (not= sh (:height @st)) (not= sw (:width @st)))
                    (swap! st assoc :height sh :width sw)))))
            (cancel! []
              (let [{:keys [last-value value]} @st
                    {:keys [on-change on-cancel]} @props*]
                (swap! st assoc :editing? false :value last-value)
                (when (and (not= value last-value) on-change) (on-change last-value))
                (when on-cancel (on-cancel last-value))))
            (toggle! []
              (if (:editing? @st)
                (let [{:keys [value]} @st]
                  (swap! st assoc :editing? false :last-value value)
                  (when-let [f (:on-confirm @props*)] (f value)))
                (when-not (:disabled @props*)
                  (swap! st assoc :editing? true))))
            (text-change! [e]
              (let [v (.. e -target -value)
                    {:keys [value on-change]} @props*]
                (when (nil? value) (swap! st assoc :value v))
                (when on-change (on-change v))))
            (key-event! [^js e]
              (when-not (.. e -nativeEvent -isComposing)
                (let [{:keys [confirm-on-enter-key multiline]} @props*
                      modifier? (or (.-altKey e) (.-ctrlKey e) (.-metaKey e) (.-shiftKey e))]
                  (case (.-key e)
                    "Escape" (cancel!)
                    "Enter"  (do (when (or (.-altKey e) (.-shiftKey e)) (.preventDefault e))
                                 (cond
                                   (and confirm-on-enter-key multiline)
                                   (if modifier?
                                     (do (insert-at-caret! (.-target e) "\n")
                                         (text-change! e))
                                     (toggle!))

                                   (or (not multiline) modifier?)
                                   (toggle!)))
                    nil))))
            (input-ref! [el]
              (when el
                (reset! input el)
                (let [{:keys [always-render-input select-all-on-focus]} @props*]
                  (when-not always-render-input (.focus el))
                  (when (:editing? @st)
                    (let [n (count (.-value el))]
                      (try (.setSelectionRange el (if select-all-on-focus 0 n) n)
                           (catch :default _ nil))
                      (when-not select-all-on-focus
                        (set! (.-scrollLeft el) (.-scrollWidth el))))))))
            (render-input [value]
              (let [{:keys [custom-input-attributes disabled fill max-length multiline
                            placeholder type]} @props*
                    {:keys [height width]} @st
                    attrs (merge {:class       (bp/cls "editable-text-input")
                                  :disabled    disabled
                                  :max-length  max-length
                                  :on-blur     toggle!
                                  :on-change   text-change!
                                  :on-key-down key-event!
                                  :placeholder placeholder
                                  :value       (or value "")
                                  :ref         input-ref!}
                                 (when (and (pos? height) (pos? width))
                                   {:style {:height      height
                                            :line-height (when-not multiline
                                                           (str height "px"))
                                            :width       (if (or fill multiline)
                                                           "100%" width)}})
                                 custom-input-attributes)]
                (if multiline
                  [:textarea attrs]
                  [:input (assoc attrs :type type)])))]
      (r/create-class
       {:display-name "EditableText"
        :component-did-mount update-dims!
        :component-did-update
        (fn [_ [_ & old-args]]
          (let [[old] (c/split-args old-args)
                p     @props*]
            (when (and (not= (:value p) (:value old))
                       (or (some? (:value p)) (some? (:value old))))
              (swap! st assoc :value (:value p)))
            (when (and (some? (:is-editing p)) (not= (:is-editing p) (:is-editing old)))
              (swap! st assoc :editing? (:is-editing p)))
            (when (:disabled p) (swap! st assoc :editing? false))
            (update-dims!)))
        :reagent-render
        (fn [& args]
          (let [[props] (c/split-args args)
                p (merge defaults props)
                _ (reset! props* p)
                {:keys [always-render-input class content-id disabled element-ref fill
                        intent min-width multiline placeholder]} p
                {:keys [editing? height]} @st
                value (if (some? (:value p)) (:value p) (:value @st))
                has?  (and (some? value) (not= value ""))
                hide? (and always-render-input (not editing?))]
            [:div {:class     (bp/cx (bp/cls "editable-text") (bp/intent-class intent)
                                     {(bp/cls "disabled")              disabled
                                      (bp/cls "editable-text-editing") editing?
                                      (bp/cls "editable-text-placeholder") (not has?)
                                      (bp/cls "fill")                  fill
                                      (bp/cls "multiline")             multiline}
                                     class)
                   :on-focus  (fn [_]
                                (when-not disabled
                                  (when-not editing?
                                    (when-let [f (:on-edit p)] (f value)))
                                  (swap! st assoc :editing? true))
                                (when (and always-render-input (:select-all-on-focus p))
                                  (when-let [el @input]
                                    (.setSelectionRange el 0 (count (.-value el))))))
                   :tab-index (when-not (or always-render-input editing? disabled) 0)
                   :ref       element-ref}
             (when (or always-render-input editing?) (render-input value))
             (when-not hide?
               [:span {:id    content-id
                       :class (bp/cls "editable-text-content")
                       :ref   #(reset! content %)
                       :style (if multiline
                                {:height (when-not editing? height)}
                                {:height      height
                                 :line-height (str height "px")
                                 :min-width   min-width})}
                (if has? value placeholder)])]))}))))
