(ns sutra.schedules.http
  (:require [sutra.store.control :as ctl]
            [sutra.http.endpoint :as ep]
            [sutra.http.responses :refer [ok]]
            [sutra.schedules.dispatch :as dispatch]
            [sutra.schedules.schedules :as schedules]))

(set! *warn-on-reflection* true)

(def list-schedules
  (ep/handler :schedules
              {:call #(schedules/enriched-schedules (:store %) (:schema %) (:branch %))
               :wrap :schedules}))

(def upsert-schedule
  (ep/handler :schedules
              {:respond (fn [{:keys [store branch body]}]
                          (ok (schedules/upsert! store branch body)))}))

(def delete-schedule
  (ep/handler :schedule-of {:call #(schedules/delete! (:store %) (:branch %) (:id %))}))

(def run-schedule
  (ep/handler :schedule-run
              {:with {:sched [#(schedules/get-one (:store %) (:branch %) (:id %))
                              :refusal.schedules/no-such-schedule]}
               :call (fn [{:keys [store branch body sched]}]
                       (assoc (dispatch/start! store branch sched (:parameters body))
                              :status     "started"
                              :target     (:target sched)
                              :started-at (ctl/clock store)))}))

(def module
  {:name        :schedules-http
   :provides    {:schedules/around-write schedules/around-write}
   :contributes {:repos/riders {:schedules {:upsert! schedules/upsert! :delete! schedules/delete!}}
                 :http/routes
                 {:schedule-of  {:delete delete-schedule}
                  :schedule-run {:post run-schedule}
                  :schedules    {:get list-schedules :post upsert-schedule}}}})
