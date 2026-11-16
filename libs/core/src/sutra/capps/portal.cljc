(ns sutra.capps.portal
  (:require [sutra.kit.remote :as rd]))

(defn tray
  [apps shelves]
  (if (seq shelves)
    (filterv (comp empty? :workspaces) apps)
    (vec apps)))

(defn- listed
  [x]
  (if (rd/absent? x) [] x))

(defn view
  [apps workspaces]
  (let [shelves (if (rd/failed? workspaces) [] (listed workspaces))]
    (rd/show (rd/all [(listed apps) shelves])
             {:value (fn [[apps shelves]]
                       (let [loose (tray apps shelves)]
                         {:show    (cond
                                     (and (empty? apps) (empty? shelves)) :no-apps
                                     (and (seq shelves) (empty? loose))   :all-assigned
                                     :else                                :list)
                          :apps    apps
                          :shelves shelves
                          :loose   loose}))})))

(defn workspace-tables
  [runtime]
  (rd/show runtime
           {:value (fn [d] (if (map? d)
                             {:show :ready :descriptor d}
                             {:show :failed :reason nil}))}))
