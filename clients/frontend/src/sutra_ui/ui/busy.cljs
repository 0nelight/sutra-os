(ns sutra-ui.ui.busy
  (:require [sutra-ui.ui.core :as ui]))

(defn inline
  ([text] (inline nil text))
  ([{:keys [size class]} text]
   [:span {:class ["sutra-loading sutra-loading--inline" class] :role "status" :aria-busy "true"}
    [ui/spinner {:size (or size 16) :tag-name :span}]
    (when text [:span text])]))

(defn block
  ([text] (block nil text))
  ([{:keys [class]} text]
   [:div {:class ["sutra-loading" class] :role "status" :aria-busy "true"}
    [ui/spinner {:size 16 :tag-name :div}]
    (when text [:span text])]))
