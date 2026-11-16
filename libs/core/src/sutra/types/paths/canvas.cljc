(ns sutra.types.paths.canvas
  (:require [sutra.types.paths.rules :refer [reads writes m-onto]]))

(def endpoints
  {:explorer-dashboard {:url "/api/object-workspace/dashboards/:object-type"
                        :methods {:get {:effects reads
                                        :requires {:op :read, :on [:object-types :object-type]}
                                        :response :a/explorer-dashboard.get.res}
                                  :put {:effects writes
                                        :requires {:op :update, :on [:object-types :object-type]}
                                        :response :a/explorer-dashboard.put.res
                                        :request :a/explorer-dashboard.put.req}}
                        :service :platform}
   :graph {:url "/api/graphs/:id"
           :methods {:get {:effects reads :requires m-onto}}
           :service :platform
           :branch? true}
   :graph-commit {:url "/api/graphs/:id/commit"
                  :methods {:post {:effects writes :requires {:op :update, :on [:graphs :id]}}}
                  :service :platform}
   :graph-dot {:url "/api/graphs/:id/dot"
               :methods {:get {:effects reads :requires m-onto}}
               :service :platform
               :query {:layout :kw, :richtung :kw}}
   :graph-layout {:url "/api/graphs/:id/layout"
                  :methods {:post {:effects writes :requires {:op :update, :on [:graphs :id]}
                                   :response :a/graph-layout.post.res
                                   :request :a/graph-layout.post.req}}
                  :service :platform}
   :graph-open {:url "/api/graph"
                :methods {:get {:effects reads :requires m-onto
                                :response :a/graph-open.get.res}}
                :service :platform
                :query {:handle :str}}
   :graph-scheme {:url "/api/graphs/:id/color-schemes/:scheme-id"
                  :methods {:delete {:effects writes :requires {:op :update, :on [:graphs :id]}}
                            :put {:effects writes :requires {:op :update, :on [:graphs :id]}}}
                  :service :platform}
   :graph-schemes {:url "/api/graphs/:id/color-schemes"
                   :methods {:get {:effects reads :requires m-onto}
                             :post {:effects writes :requires {:op :update, :on [:graphs :id]}
                                    :response :a/graph-schemes.post.res
                                    :request :a/graph-schemes.post.req}}
                   :service :platform}
   :graph-svg {:url "/api/graphs/:id/svg"
               :methods {:get {:effects reads :requires m-onto}}
               :service :platform
               :query {:layout :kw, :richtung :kw}}
   :graphs {:url "/api/graphs"
            :methods {:get {:effects reads :requires m-onto
                            :response :a/graphs.get.res}}
            :service :platform
            :branch? true
            :query {:area :str, :mode :kw, :project :str}}
   :view {:url "/api/views/:id"
          :methods {:get {:effects reads :requires m-onto}}
          :service :platform
          :branch? true}
   :view-commit {:url "/api/views/:id/commit"
                 :methods {:post {:effects writes :requires {:op :update, :on [:graphs :id]}}}
                 :service :platform}
   :view-layout {:url "/api/views/:id/layout"
                 :methods {:post {:effects writes :requires {:op :update, :on [:graphs :id]}}}
                 :service :platform}
   :view-scheme {:url "/api/views/:id/color-schemes/:scheme-id"
                 :methods {:delete {:effects writes :requires {:op :update, :on [:graphs :id]}}
                           :put {:effects writes :requires {:op :update, :on [:graphs :id]}}}
                 :service :platform}
   :view-schemes {:url "/api/views/:id/color-schemes"
                  :methods {:get {:effects reads :requires m-onto}
                            :post {:effects writes :requires {:op :update, :on [:graphs :id]}}}
                  :service :platform}
   :views {:url "/api/views"
           :methods {:get {:effects reads :requires m-onto
                           :response :a/views.get.res}}
           :service :platform
           :branch? true
           :query {:area :str, :mode :kw, :project :str}}})
