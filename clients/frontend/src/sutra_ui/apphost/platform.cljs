(ns sutra-ui.apphost.platform
  (:require [sutra-ui.apphost.registry :as capps]))

(defn module
  [id tree]
  (let [desc {:layout tree :name (name id) :platform? true}
        data {}]
    (fn [id _]
      (capps/frame {:class "capp-platform" :data-app (name id)}
                   [capps/render desc data]))))
