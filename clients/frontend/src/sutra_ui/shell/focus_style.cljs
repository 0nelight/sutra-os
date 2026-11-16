(ns sutra-ui.shell.focus-style
  (:require [sutra-ui.ui.core :as ui]))

(def ^:private class-name (ui/cls "focus-disabled"))
(defonce ^:private state (atom nil))

(defn- root [] (.-documentElement js/document))

(declare mouse!)

(defn- keyboard! [^js e]
  (when (= "Tab" (.-key e))
    (.remove (.-classList (root)) class-name)
    (.removeEventListener js/document "keydown" keyboard! true)
    (.addEventListener js/document "mousedown" mouse! true)))

(defn- mouse! [_]
  (.add (.-classList (root)) class-name)
  (.removeEventListener js/document "mousedown" mouse! true)
  (.addEventListener js/document "keydown" keyboard! true))

(defn on! []
  (when-not @state
    (reset! state :on)
    (.addEventListener js/document "mousedown" mouse! true)))
