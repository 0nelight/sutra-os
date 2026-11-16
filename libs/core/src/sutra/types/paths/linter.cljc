(ns sutra.types.paths.linter
  (:require [sutra.types.paths.rules :refer [reads writes m-instance]]))

(defn- b
  [k]
  (keyword "sutra.foundation.contracts.b" (name k)))

(def endpoints
  {:linter-rules {:url "/api/linter/rules"
                  :methods {:get {:effects reads
                                  :requires m-instance
                                  :response (b :linter-rules)}}
                  :service :conductor}
   :linter-list {:url "/api/linter/recommendations"
                 :methods {:get {:effects reads :requires {:op :read}
                                 :response (b :linter-list)}}
                 :service :conductor
                 :branch? true
                 :query {:mode :str, :rule :str, :project :str, :assignee :str, :snoozed :bool}}
   :linter-sweep {:url "/api/linter/sweep"
                  :methods {:post {:effects writes
                                   :requires {:op :update}
                                   :response (b :linter-sweep)}}
                  :service :conductor
                  :branch? true}
   :linter-snooze {:url "/api/linter/snooze"
                   :methods {:post {:effects writes :requires {:op :update}
                                    :request (b :linter-target-in)
                                    :response (b :linter-target)}}
                   :service :conductor
                   :branch? true}
   :linter-assign {:url "/api/linter/assign"
                   :methods {:post {:effects writes :requires {:op :update}
                                    :request (b :linter-target-in)
                                    :response (b :linter-target)}}
                   :service :conductor
                   :branch? true}})
