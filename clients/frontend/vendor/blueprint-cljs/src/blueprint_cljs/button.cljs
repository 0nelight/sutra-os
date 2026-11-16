(ns blueprint-cljs.button
  "Button, AnchorButton, ButtonGroup.
   `[button {:intent :primary :icon :add :on-click f} \"Save\"]` — `:text` and children
   both render as the label."
  (:require [blueprint-cljs.core :as bp]
            [blueprint-cljs.common :as c]
            [blueprint-cljs.icon :as icon]
            [blueprint-cljs.spinner :as spinner]
            [blueprint-cljs.text :as text]))

(def ^:private own-keys
  (into [:align-text :fill :large :loading :minimal :outlined :size :small :variant
         :intent :icon :right-icon :end-icon :text :text-class-name :ellipsize-text
         :disabled :class]
        c/interactive-keys))

(defn- button-class
  [{:keys [align-text fill intent class] :as props} active? disabled? loading?]
  (bp/cx (bp/cls "button")
         {(bp/cls "active")   active?
          (bp/cls "disabled") disabled?
          (bp/cls "fill")     fill
          (bp/cls "loading")  loading?}
         (c/alignment-class align-text)
         (bp/intent-class intent)
         (c/size-class props)
         (c/variant-class props)
         class))

(defn- contents
  [{:keys [loading icon right-icon end-icon text text-class-name ellipsize-text]} children]
  (let [has-text? (not (and (c/empty-node? text) (every? c/empty-node? children)))]
    [:<>
     (when loading [spinner/spinner {:class (bp/cls "button-spinner") :size spinner/size-small}])
     (icon/render-icon icon)
     (when has-text?
       (into [text/text {:class     (bp/cx (bp/cls "button-text") text-class-name)
                         :ellipsize ellipsize-text
                         :tag-name  :span}
              text]
             children))
     (icon/render-icon (or end-icon right-icon))]))

(defn button
  "Props: `:intent`, `:icon`, `:right-icon`/`:end-icon`, `:text`, `:variant`
   (:solid :minimal :outlined) or `:minimal`/`:outlined`, `:size` (:small :medium :large)
   or `:small`/`:large`, `:fill`, `:align-text`, `:loading`, `:disabled`, `:active`,
   `:ellipsize-text`, `:text-class-name`, `:type`; the rest goes to the <button>."
  [& _]
  (let [state (c/interactive-state)]
    (fn [& args]
      (let [[props children] (c/split-args args)
            loading?         (boolean (:loading props))
            disabled?        (boolean (or (:disabled props) loading?))
            [active? attrs]  (c/interactive-attrs (not disabled?) props state {})]
        [:button (merge {:type "button"}
                        (bp/dom-props props own-keys)
                        attrs
                        {:class    (button-class props active? disabled? loading?)
                         :disabled disabled?})
         [contents props children]]))))

(defn anchor-button
  "Button as an <a>: `:href`, `:target`; disabled drops the href."
  [& _]
  (let [state (c/interactive-state)]
    (fn [& args]
      (let [[props children] (c/split-args args)
            loading?         (boolean (:loading props))
            disabled?        (boolean (or (:disabled props) loading?))
            [active? attrs]  (c/interactive-attrs (not disabled?) props state
                                                  {:default-tab-index 0
                                                   :disabled-tab-index -1})]
        [:a (merge {:role "button"}
                   (bp/dom-props props own-keys)
                   attrs
                   {:class         (button-class props active? disabled? loading?)
                    :aria-disabled disabled?
                    :href          (when-not disabled? (:href props))})
         [contents props children]]))))

(defn button-group
  "Props: `:align-text`, `:fill`, `:vertical`, `:variant`/`:minimal`/`:outlined`,
   `:size`/`:large`."
  [& args]
  (let [[{:keys [align-text fill vertical class] :as props} children] (c/split-args args)]
    (into [:div (merge (bp/dom-props props [:align-text :fill :vertical :class :variant
                                            :minimal :outlined :size :large :small])
                       {:class (bp/cx (bp/cls "button-group")
                                      {(bp/cls "fill")     fill
                                       (bp/cls "vertical") vertical}
                                      (c/alignment-class align-text)
                                      (c/size-class (dissoc props :small))
                                      (c/variant-class props)
                                      class)})]
          children)))
