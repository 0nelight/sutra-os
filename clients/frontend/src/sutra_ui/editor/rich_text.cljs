(ns sutra-ui.editor.rich-text
  (:require [reagent.core :as r]
            [shadow.loader :as loader]))

(defonce ^:private impl (r/atom nil))

(defn provide! [m] (reset! impl m))

(defn ensure!
  []
  (if-let [m @impl]
    (js/Promise.resolve m)
    (-> (try
          (loader/load "rich-text")
          (catch :default e
            (js/console.warn "[rich-text] no module loader in this build — not fetching" e)
            (js/Promise.resolve nil)))
        (.then (fn [_] @impl)))))

(defn view
  [_k & _args]
  (r/create-class
   {:display-name        "rich-text-lazy"
    :component-did-mount (fn [_] (ensure!))
    :reagent-render
    (fn [k & args]
      (if-let [c (get @impl k)]
        (into [c] args)
        [:div {:class "sutra-loading" :role "status" :aria-busy "true"}]))}))
