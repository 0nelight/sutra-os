(ns sutra-ui.shell.unavailable
  (:require [sutra-ui.ui.core :as ui]))

(defn unavailable
  ([title] (unavailable title nil))
  ([title action]
   [ui/empty-state {:icon :error :title title :action action}]))
