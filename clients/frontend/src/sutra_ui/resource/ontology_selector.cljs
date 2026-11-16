(ns sutra-ui.resource.ontology-selector
  (:require [reagent.core :as r]
            [sutra.i18n.core :as i18n]
            [sutra-ui.api :as api]
            [sutra-ui.ui.core :as ui]
            [sutra-ui.ui.style :as style]))

(defonce ^:private listed (r/atom nil))

(defn- load! []
  (when (nil? @listed)
    (reset! listed [])
    (-> (api/get! :ontologies)
        (.then (fn [{:keys [ontologies]}] (reset! listed (vec ontologies))))
        (.catch (fn [_] (reset! listed []))))))

(defn- choose! [v]
  (api/set-page-ontology! (when (not= "default" v) v))
  (.reload (.-location js/window)))

(defn selector
  []
  (load!)
  (let [os @listed]
    (when (> (count os) 1)
      [:div {:class "u-flex-row-center-6" :title (i18n/t :ui/ontology-selector-hint)}
       [:span {:class style/small-muted} (i18n/t :ui/ontology-selector)]
       [ui/html-select
        {:minimal   true
         :value     (or (api/selected-ontology) "default")
         :on-change #(choose! (.. % -target -value))
         :options   (for [{nm :name kind :kind} os]
                      {:value nm
                       :label (str nm (when (= "shared" (some-> kind name))
                                        (str " · " (i18n/t :ui/ontology-shared))))})}]])))
