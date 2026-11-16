(ns sutra.types.paths.schedules
  (:require [sutra.types.paths.rules :refer [reads writes]]))

(defn- c
  [k]
  (keyword "sutra.foundation.contracts.c" (name k)))

(def endpoints
  {:schedule-of {:url "/api/schedules/:id"
                 :response (c :schedule-deleted)
                 :methods {:delete {:effects writes :requires {:op :update}}}
                 :service :conductor
                 :branch? true}
   :schedule-run {:url "/api/schedules/:id/run"
                  :request (c :schedule-run-in)
                  :response (c :schedule-started)
                  :methods {:post {:effects writes :requires {:op :update}}}
                  :process :conductor
                  :service :conductor
                  :branch? true}
   :schedules {:url "/api/schedules"
               :methods {:get {:response (c :schedules)
                               :effects reads :requires {:op :read}}
                         :post {:request (c :schedule-in)
                                :response (c :schedule)
                                :effects writes :requires {:op :update}}}
               :service :conductor
               :branch? true}})
