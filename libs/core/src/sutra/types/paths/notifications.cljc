(ns sutra.types.paths.notifications
  (:require [sutra.types.paths.rules :refer [reads writes]]))

(defn- c
  [k]
  (keyword "sutra.foundation.contracts.c" (name k)))

(def endpoints
  {:notifications {:url "/api/notifications"
                   :response (c :notification-inbox)
                   :methods {:get {:effects reads}}
                   :service :platform
                   :auth :consumer
                   :query {:limit [:long 50] :unread :bool}}
   :notifications-read {:url "/api/notifications/read"
                        :request (c :notifications-read-in)
                        :response (c :notifications-marked)
                        :methods {:post {:effects writes}}
                        :service :platform
                        :auth :consumer}
   :notifications-stream {:url "/api/notifications/stream"
                          :methods {:get {:effects reads}}
                          :service :platform
                          :auth :consumer}
   :notification-preferences {:url "/api/notifications/preferences"
                              :methods {:get {:response (c :notification-settings)
                                              :effects reads}
                                        :put {:request (c :notification-settings-in)
                                              :response (c :notification-settings)
                                              :effects writes}}
                              :service :platform
                              :auth :consumer}})
