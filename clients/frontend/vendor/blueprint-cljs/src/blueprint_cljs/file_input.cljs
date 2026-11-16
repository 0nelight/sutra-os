(ns blueprint-cljs.file-input
  "FileInput: a styled native file picker.
   `[file-input {:text \"Choose file…\" :button-text \"Browse\" :has-selection false
                 :on-input-change f :input-props {:accept \".csv\"}}]`
   Props: `:fill`, `:disabled`, `:size`/`:large`/`:small`; the rest goes to the `<label>`."
  (:require [blueprint-cljs.core :as bp]
            [blueprint-cljs.common :as c]))

(defn file-input
  [& args]
  (let [[{:keys [button-text class disabled fill has-selection input-props on-input-change
                 text]
          :or   {text "Choose file..."}
          :as   props}] (c/split-args args)]
    [:label (merge (bp/dom-props props [:button-text :class :disabled :fill :has-selection
                                        :input-props :on-input-change :text :size :large
                                        :small])
                   {:class (bp/cx class (bp/cls "file-input")
                                  {(bp/cls "disabled")            disabled
                                   (bp/cls "fill")                fill
                                   (bp/cls "file-input-has-selection") has-selection}
                                  (c/size-class props))})
     [:input (merge input-props
                    {:type      "file"
                     :disabled  disabled
                     :on-change (fn [e]
                                  (when on-input-change (on-input-change e))
                                  (when-let [f (:on-change input-props)] (f e)))})]
     [:span {:bp-button-text button-text
             :class          (bp/cx (bp/cls "file-upload-input")
                                    {(bp/cls "file-upload-input-custom-text")
                                     (boolean button-text)})}
      text]]))
