(ns sutra.types.paths.feedback
  (:require [sutra.types.paths.rules :refer [reads writes]]))

(defn- b
  [k]
  (keyword "sutra.foundation.contracts.b" (name k)))

(def endpoints
  {:feedback {:url "/api/feedback"
              :methods {:get {:effects reads
                              :response (b :feedback-read)}
                        :post {:effects writes
                               :request (b :feedback-start-in)
                               :response (b :feedback-session)}}
              :service :platform
              :auth :consumer
              :query {:id :str :benutzer :str :verlauf :bool}}
   :feedback-ende {:url "/api/feedback/ende"
                   :methods {:post {:effects writes
                                    :request (b :feedback-finish-in)
                                    :response (b :feedback-session)}}
                   :service :platform
                   :auth :consumer}
   :feedback-erledigt {:url "/api/feedback/erledigt"
                       :methods {:post {:effects writes
                                        :request (b :feedback-settle-in)
                                        :response (b :feedback-session)}}
                       :service :platform
                       :auth :consumer}
   :feedback-ereignisse {:url "/api/feedback/ereignisse"
                         :methods {:post {:effects writes
                                          :request (b :feedback-events-in)
                                          :response (b :feedback-events)}}
                         :service :platform
                         :auth :consumer}})
