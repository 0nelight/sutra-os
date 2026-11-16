(ns sutra.types.paths.checkouts
  (:require [sutra.types.paths.rules :refer [reads writes m-onto]]))

(defn- b
  [k]
  (keyword "sutra.foundation.contracts.b" (name k)))

(def endpoints
  {:slices {:url "/api/slices"
            :methods {:get {:effects reads :requires m-onto
                            :response (b :slice-pull)}
                      :post {:effects writes :requires {:op :update}
                             :request (b :slice-push-in)
                             :response (b :slice-push)}}
            :service :platform
            :branch? true
            :query {:nodes :str}}})
