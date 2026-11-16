(ns sutra.types.paths.widget-sets
  (:require [sutra.types.paths.rules :refer [reads writes c-app c-widget-set]]))

(defn- c
  [k]
  (keyword "sutra.foundation.contracts.c" (name k)))

(def endpoints
  {:widget-sets {:url "/api/widget-sets"
                 :methods {:get {:response (c :widget-sets)
                                 :effects reads :requires c-widget-set}
                           :post {:request (c :widget-set-in)
                                  :response (c :widget-set-one)
                                  :effects writes :requires c-widget-set}}
                 :service :platform}
   :widget-set {:url "/api/widget-sets/:name"
                :methods {:delete {:response (c :ok)
                                   :effects writes :requires c-widget-set}
                          :get {:response (c :widget-set-one)
                                :effects reads
                                :requires c-widget-set}}
                :service :platform}
   :widget-set-releases {:url "/api/widget-sets/:name/releases"
                         :request (c :widget-release-in)
                         :response (c :widget-release-one)
                         :methods {:post {:effects writes :requires c-widget-set}}
                         :service :platform}
   :widget-set-dev {:url "/api/widget-sets/:name/dev"
                    :response (c :ok)
                    :methods {:delete {:effects writes :requires c-widget-set}}
                    :service :platform}
   :widget-set-bundle {:url "/api/widget-sets/:name/bundle"
                       :response (c :widget-bundle)
                       :methods {:get {:effects reads :requires c-widget-set}}
                       :service :platform
                       :query {:version :trim}}
   :widget-set-release {:url "/api/widget-sets/:name/releases/:version"
                        :response (c :widget-release-one)
                        :methods {:get {:effects reads :requires c-widget-set}}
                        :service :platform}
   :my-application-widget-set {:url "/api/my-application/:rid/widget-set/:name"
                               :response (c :widget-bundle-negotiated)
                               :methods {:get {:effects reads :requires c-app}}
                               :service :platform
                               :branch? true
                               :query {:version :trim, :v :trim}}})
