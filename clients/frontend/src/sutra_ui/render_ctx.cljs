(ns sutra-ui.render-ctx
  (:require [reagent.core :as r]
            [sutra.capps.ctx :as ctx]))

(defonce ^:private session
  (r/atom (ctx/of)))

(defn- host-zone
  []
  (try (.. (js/Intl.DateTimeFormat.) resolvedOptions -timeZone)
       (catch :default _ nil)))

(defn install!
  [tag]
  (reset! session (ctx/of {:locale tag :zone (host-zone)})))

(defn current
  []
  (assoc @session :now (js/Date.now)))
