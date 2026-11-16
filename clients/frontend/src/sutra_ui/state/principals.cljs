(ns sutra-ui.state.principals
  (:require [sutra-ui.api :as api]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.requests :as rq]))

(defonce principal-names (db/cursor [:principals :principal-names] {}))

(defonce principal-admins (db/cursor [:principals :principal-admins] #{}))

(defn- by-user [{ps :principals}]
  {:principal-names  (into {} (map (juxt :user-id :username)) ps)
   :principal-admins (into #{} (comp (filter :admin?) (map :user-id)) ps)})

(db/reg :sutra.principals/refresh
        (fn [db _] (rq/request db {:ask [api/get! :principals] :path [:principals] :pick by-user})))

(defn refresh-principals! [] (db/send! [:sutra.principals/refresh]))

(defn principal-label
  [principal]
  (let [p (str principal)]
    (or (get @principal-names p)
        (when (>= (count p) 12) (str (subs p 0 8) "…"))
        p)))
