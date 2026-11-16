(ns blueprint-cljs.tag-input
  "TagInput: values as tags followed by a text input. Enter (or a separator in pasted
   text) adds, Backspace/Delete removes, ←/→ moves through the tags.
   `[tag-input {:values vs :on-change (fn [values]) :placeholder \"Add…\"
                :left-icon :tag}]`
   Props: `:on-add` (fn [new-values method]), `:on-remove` (fn [value index]),
   `:on-input-change` (fn [e]), `:input-value` (controlled), `:separator` (regex,
   default #\"[,\\n\\r]\", false = none), `:add-on-blur`, `:add-on-paste` (true),
   `:auto-resize`, `:tag-props` (map or fn [value index]), `:input-props`, `:input-ref`,
   `:right-element`, `:intent`, `:size`/`:large`, `:fill`, `:disabled`,
   `:on-key-down`/`:on-key-up` (fn [e active-index]). Children render after the tags."
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.common :as c]
            [blueprint-cljs.icon :as icon]
            [blueprint-cljs.tag :as tag]))

(def ^:private none -1)

(defn- resizable-input
  [_]
  (let [width (r/atom 0)
        span  (atom nil)
        sync! #(when-let [s @span]
                 (let [w (.-offsetWidth s)] (when (not= w @width) (reset! width w))))]
    (r/create-class
     {:component-did-mount  sync!
      :component-did-update sync!
      :reagent-render
      (fn [{:keys [style value] :as props}]
        [:<>
         [:span {:class (bp/cls "resizable-input-span") :ref #(reset! span %)
                 :aria-hidden true}
          (str/replace (str value) " " " ")]
         [:input (assoc props :type "text" :style (assoc (or style {}) :width @width))]])})))

(defn- split-values
  [separator s]
  (->> (if (false? separator) [s] (str/split s separator))
       (map str/trim)
       (remove str/blank?)
       vec))

(def ^:private defaults
  {:add-on-blur false :add-on-paste true :auto-resize false :input-props {}
   :separator #"[,\n\r]" :tag-props {}})

(defn tag-input
  [& args]
  (let [[init] (c/split-args args)
        st     (r/atom {:active none :input (or (:input-value init) "") :focused? false
                        :prev-input-prop (:input-value init)})
        node   (atom nil)
        props* (atom nil)]
    (letfn [(add! [s method]
              (let [{:keys [input-value on-add on-change values separator]} @props*
                    nv    (split-values separator s)
                    clear (and (not (false? (when on-add (on-add nv method))))
                               (nil? input-value))
                    clear (if (fn? on-change)
                            (and (not (false? (on-change (into (vec values) nv)))) clear)
                            clear)]
                (when clear (swap! st assoc :input ""))))
            (remove-index! [i]
              (let [{:keys [on-change on-remove values]} @props*]
                (when on-remove (on-remove (nth values i) i))
                (when on-change
                  (on-change (vec (keep-indexed (fn [j v] (when (not= i j) v)) values))))))
            (find-next [start dir]
              (let [vs (:values @props*)]
                (loop [i (+ start dir)]
                  (if (and (pos? i) (< i (count vs)) (not (nth vs i)))
                    (recur (+ i dir))
                    (-> i (max 0) (min (count vs)))))))
            (next-active [dir]
              (let [a (:active @st)]
                (if (= a none)
                  (if (neg? dir) (find-next (count (:values @props*)) -1) none)
                  (find-next a dir))))
            (valid? [i] (and (not= i none) (< i (count (:values @props*)))))
            (emit-key [k e active]
              (when-let [f (get @props* k)] (f e (when (not= active none) active)))
              (when-let [f (get-in @props* [:input-props k])] (f e)))
            (key-down! [^js e]
              (let [t    (.-currentTarget e)
                    v    (.-value t)
                    act0 (:active @st)
                    emit (atom act0)]
                (cond
                  (and (= (.-key e) "Enter") (not (.. e -nativeEvent -isComposing))
                       (pos? (count v)))
                  (add! v :default)

                  (and (zero? (.-selectionEnd t)) (seq (:values @props*)))
                  (if-let [d ({"ArrowLeft" -1 "ArrowRight" 1} (.-key e))]
                    (let [n (next-active d)]
                      (when (not= n act0)
                        (.stopPropagation e)
                        (reset! emit n)
                        (swap! st assoc :active n)))
                    (case (.-key e)
                      "Backspace" (do (swap! st assoc :active (next-active -1))
                                      (when (valid? act0)
                                        (.stopPropagation e)
                                        (remove-index! act0)))
                      "Delete"    (when (valid? act0)
                                    (.stopPropagation e)
                                    (remove-index! act0))
                      nil)))
                (emit-key :on-key-down e @emit)))
            (paste! [e]
              (let [{:keys [add-on-paste separator]} @props*
                    v (.. e -clipboardData (getData "text"))]
                (when (and add-on-paste (pos? (count v)) (not (false? separator))
                           (> (count (str/split v separator)) 1))
                  (.preventDefault e)
                  (add! v :paste))))
            (container-blur! [e]
              (let [ct (.-currentTarget e)]
                (js/requestAnimationFrame
                 (fn []
                   (when-not (.contains ct js/document.activeElement)
                     (let [{:keys [add-on-blur]} @props*
                           in (:input @st)]
                       (when (and add-on-blur (seq in)) (add! in :blur))
                       (swap! st assoc :active none :focused? false)))))))]
      (fn [& args]
        (let [[props children] (c/split-args args)
              p (merge defaults props)
              _ (reset! props* p)
              {:keys [auto-resize class disabled fill input-props input-ref input-value
                      intent large left-icon on-input-change placeholder right-element
                      tag-props values]} p
              _ (when (not= input-value (:prev-input-prop @st))
                  (swap! st assoc :input input-value :prev-input-prop input-value))
              large?  (c/large? p)
              holder  (if (or (nil? placeholder) (some identity values))
                        (:placeholder input-props)
                        placeholder)
              in-props (merge {:value (or (:input @st) "")}
                              input-props
                              {:class       (bp/cx (bp/cls "input-ghost") (:class input-props))
                               :disabled    disabled
                               :on-change   (fn [e]
                                              (swap! st assoc :active none
                                                     :input (.. e -currentTarget -value))
                                              (when on-input-change (on-input-change e))
                                              (when-let [f (:on-change input-props)] (f e)))
                               :on-focus    (fn [e]
                                              (swap! st assoc :focused? true)
                                              (when-let [f (:on-focus input-props)] (f e)))
                               :on-key-down key-down!
                               :on-key-up   #(emit-key :on-key-up % (:active @st))
                               :on-paste    paste!
                               :placeholder holder
                               :ref         (fn [n] (reset! node n)
                                              (when input-ref (input-ref n)))})]
          [:div {:class    (bp/cx (bp/cls "input") (bp/cls "tag-input")
                                  {(bp/cls "active")   (:focused? @st)
                                   (bp/cls "disabled") disabled
                                   (bp/cls "fill")     fill}
                                  (bp/intent-class intent)
                                  (c/size-class {:size (:size p) :large large})
                                  class)
                 :on-blur  container-blur!
                 :on-click #(some-> @node .focus)}
           [icon/icon {:class (bp/cls "tag-input-icon") :icon left-icon
                       :size  (if large? 20 16)}]
           ;; tags as one seq, so the input keeps its slot when a tag comes or goes
           (-> [:div {:class (bp/cls "tag-input-values")}]
               (conj (doall
                      (keep-indexed
                       (fn [i v]
                         (when v
                           ^{:key (str v "__" i)}
                           [tag/tag (merge {:active         (= i (:active @st))
                                            :data-tag-index i
                                            :large          large
                                            :size           (:size p)
                                            :on-remove      (when-not disabled
                                                              (fn [_ _] (remove-index! i)))}
                                           (if (fn? tag-props) (tag-props v i) tag-props))
                            v]))
                       values)))
               (conj (seq children))
               (conj (if auto-resize
                       [resizable-input in-props]
                       [:input in-props])))
           right-element])))))
