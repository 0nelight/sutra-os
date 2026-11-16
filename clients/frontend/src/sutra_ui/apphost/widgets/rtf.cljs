(ns sutra-ui.apphost.widgets.rtf
  (:require [sutra-ui.ui.sel :as sel :refer [rd wr!]]
            [sutra-ui.ui.core :as ui :refer [text-area]]
            [sutra.capps.rtf :as rtf]))

(defn rtf-text
  [sel {:keys [rows placeholder class]}]
  (let [roh (str (or (rd sel) ""))]
    [text-area
     {:class       (cond-> "capp-rtf" class (str " " class))
      :rows        (or rows 8)
      :value       (rtf/edit-text roh)
      :placeholder placeholder
      :on-change   (fn [e]
                     (wr! sel (rtf/patch roh (.. e -target -value))))}]))
