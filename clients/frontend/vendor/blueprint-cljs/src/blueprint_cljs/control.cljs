(ns blueprint-cljs.control
  "The markup Checkbox, Radio and Switch share (Blueprint `ControlInternal`):
   `<label.bp-control><input/><span.bp-control-indicator/>label…</label>`."
  (:require [blueprint-cljs.core :as bp]
            [blueprint-cljs.common :as c]))

(def own-keys
  [:align-indicator :class :inline :input-ref :label :label-element :large :size :style
   :tag-name :type :type-class :indicator-children])

(defn control
  "Props: `:label`, `:label-element`, `:inline`, `:align-indicator` (:start :end :left
   :right), `:size`/`:large`, `:disabled`, `:checked`, `:default-checked`, `:on-change`,
   `:input-ref`, `:tag-name` (default :label); the rest goes to the `<input>`."
  [{:keys [align-indicator class inline input-ref label label-element style tag-name type
           type-class indicator-children disabled]
    :or   {tag-name :label}
    :as   props}
   children]
  (into [tag-name {:class (bp/cx (bp/cls "control") type-class
                                 {(bp/cls "disabled") disabled
                                  (bp/cls "inline")   inline}
                                 (c/alignment-class align-indicator)
                                 (c/size-class (dissoc props :small))
                                 class)
                   :style style}
         [:input (merge (bp/dom-props props own-keys)
                        {:class (bp/cls "control-input")
                         :type  type
                         :ref   input-ref})]
         (into [:span {:class (bp/cls "control-indicator")}] indicator-children)
         label
         label-element]
        children))
