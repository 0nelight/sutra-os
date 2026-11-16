(ns blueprint-cljs.checkbox
  "Checkbox. `[checkbox {:label \"Enabled\" :checked v :on-change f}]`; `:indeterminate`
   (controlled) or `:default-indeterminate` shows the partial state."
  (:require [reagent.core :as r]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.common :as c]
            [blueprint-cljs.control :as control]))

(defn checkbox
  [& _]
  (let [node   (atom nil)
        local  (r/atom nil)
        sync!  (fn [indeterminate]
                 (when-let [n @node]
                   (set! (.-indeterminate n) (boolean indeterminate))))]
    (r/create-class
     {:display-name "Checkbox"
      :component-did-mount
      (fn [this]
        (let [[props] (c/split-args (rest (r/argv this)))]
          (when (nil? @local)
            (reset! local (boolean (:default-indeterminate props))))
          (sync! (if (some? (:indeterminate props)) (:indeterminate props) @local))))
      :component-did-update
      (fn [this]
        (let [[props] (c/split-args (rest (r/argv this)))]
          (sync! (if (some? (:indeterminate props)) (:indeterminate props) @local))))
      :reagent-render
      (fn [& args]
        (let [[{:keys [indeterminate on-change input-ref] :as props} children]
              (c/split-args args)]
          @local
          [control/control
           (-> (dissoc props :indeterminate :default-indeterminate)
               (assoc :type "checkbox"
                      :type-class (bp/cls "checkbox")
                      :input-ref (fn [n] (reset! node n) (when input-ref (input-ref n)))
                      :on-change (fn [e]
                                   (when (nil? indeterminate)
                                     (reset! local (.. e -target -indeterminate)))
                                   (when on-change (on-change e)))))
           children]))})))
