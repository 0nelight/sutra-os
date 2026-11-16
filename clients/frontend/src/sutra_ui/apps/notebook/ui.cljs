(ns sutra-ui.apps.notebook.ui
  (:require [sutra-ui.ui.core :as ui]))

(defn fmt-ts
  [t]
  (let [d (js/Date. (str t))]
    (if (js/isNaN (.getTime d))
      (let [s (str t)] (subs s 0 (min 16 (count s))))
      (.toLocaleString d "de-AT" #js {:dateStyle "medium" :timeStyle "short"}))))

(defn btn [{:keys [label on-click disabled? tone title class]}]
  [ui/tooltip {:content title}
   [ui/button {:on-click on-click :disabled disabled? :class class
               :minimal (= :ghost tone)
               :intent (case tone :danger :danger :ghost nil :primary)}
    label]])

(defn action-btn [{:keys [label on-click disabled? tone title]}]
  [ui/button {:on-click on-click :disabled (boolean disabled?) :title title
              :intent (when-not (= :ghost tone) :primary)}
   label])

(defn field-label [text]
  [:label {:class "np-field-label"} text])
