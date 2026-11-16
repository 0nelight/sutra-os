(ns sutra-ui.apphost.registry
  (:require [sutra-ui.ui.busy :as busy]
            [clojure.string :as str]
            [sutra.capps.variables :as vars]
            [sutra-ui.apphost.sdk :as app]
            [sutra-ui.apphost.modules :as mod]
            [sutra-ui.apphost.runtime :as rt]
            [sutra-ui.apphost.procurement-tracking]
            [sutra.i18n.core :as i18n]))

(def render-app rt/render-app)

(mod/set-data-builder!
 (fn [descriptor]
   (assoc (mod/module (let [interface (vars/input-vars (get-in descriptor [:layout :vars]))]
                        {:inputs interface :outputs interface})
                      (fn [{:keys [in emit descriptor data]}]
                        [app/render-data-app
                         (update descriptor :layout assoc :module-inputs in :module-emit emit)
                         data]))
          :descriptor descriptor)))

(defn error-trace [e]
  (when-let [trace (:error-trace @mod/sci)] (trace e)))

(defn- source-app
  [descriptor data]
  (if-let [render (:render @mod/sci)]
    (render descriptor data)
    (do (mod/ensure-sci!)
        [busy/block {:class "loading"} (i18n/t :ui/loading)])))

(defn frame
  [attrs & children]
  (into [:div (update attrs :class #(str/trim (str % " consumer__app")))] children))

(defn render
  [descriptor data]
  (cond
    (app/data-layout? (:layout descriptor))
    [app/render-data-app descriptor data]

    (not (str/blank? (str (:source descriptor))))
    [source-app descriptor data]

    :else
    [render-app descriptor data]))
