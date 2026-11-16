(ns sutra.types.paths.objects
  (:require [sutra.types.paths.rules
             :refer [reads writes m-onto m-counts m-own c-read c-exploration]]))

(defn- c
  [k]
  (keyword "sutra.foundation.contracts.c" (name k)))

(def endpoints
  {:at-objects {:url "/api/at/:commit/objects/:type"
                :response (c :objects-at-commit)
                :methods {:get {:effects reads :requires {:op :read, :on [:object-types :type]}}}
                :service :oss
                :branch? true}
   :exploration {:url "/api/explorations/:id"
                 :methods {:delete {:response (c :exploration-deleted)
                                    :effects writes :requires c-exploration}
                           :get {:response (c :exploration)
                                 :effects reads :requires c-exploration}
                           :put {:request (c :exploration-change-in)
                                 :response (c :exploration)
                                 :effects writes :requires c-exploration}}
                 :service :oss}
   :explorations {:url "/api/explorations"
                  :methods {:get {:response (c :explorations)
                                  :effects reads :requires c-exploration}
                            :post {:request (c :exploration-in)
                                   :response (c :exploration)
                                   :effects writes :requires c-exploration}}
                  :service :oss
                  :query {:object-type :kw}}
   :instances {:url "/api/instances"
               :response (c :instances)
               :methods {:get {:effects reads :requires c-read}}
               :service :oss
               :query {:graph :raw, :since :trim}}
   :instances-around {:url "/api/instances/around"
                      :request (c :instances-around-in)
                      :response (c :instances-around)
                      :methods {:post {:effects writes :requires c-read}}
                      :service :oss}
   :instances-search {:url "/api/instances/search"
                      :response (c :instances-search)
                      :methods {:get {:effects reads :requires c-read}}
                      :service :oss
                      :query {:q :raw, :type :raw, :limit [:long 50]}}
   :links {:url "/api/links"
           :response (c :links)
           :methods {:get {:effects reads :requires c-read}}
           :service :oss
           :query {:type :str, :from :uuid, :to :uuid, :limit :long}}
   :object-counts {:url "/api/object-counts"
                   :response (c :object-counts)
                   :methods {:get {:effects reads :requires m-counts}}
                   :service :oss
                   :branch? true}
   :object-set-temp-by {:url "/api/ontology/object-sets/temporary/:rid"
                        :methods {:delete {:effects writes :requires m-own}}
                        :service :oss
                        :response :sutra.foundation.contracts/deleted}
   :object-sets {:url "/api/ontology/object-sets"
                 :methods {:get {:effects reads :requires c-read}}
                 :service :oss
                 :response :sutra.foundation.contracts/object-set-listing}
   :object-sets-temp {:url "/api/ontology/object-sets/temporary"
                      :methods {:post {:effects writes :requires c-read}}
                      :service :oss
                      :response :sutra.foundation.contracts/object-set-ref}
   :objects-knn {:url "/api/objects/:type/knn"
                 :response (c :knn-result)
                 :methods {:get {:effects reads :requires {:op :read, :on [:object-types :type]}}}
                 :service :oss
                 :query {:vector :str, :q :str, :k :long, :metric :kw}}
   :objects-of {:url "/api/objects/:type"
                :methods {:get {:effects reads :requires {:op :read, :on [:object-types :type]}}}
                :service :oss
                :query {:limit [:clamp 1 10000 1000], :offset [:long 0]}
                :response :sutra.foundation.contracts/object-page}
   :ontology-query {:url "/api/ontology/query"
                    :methods {:post {:effects reads :requires c-read}}
                    :service :oss
                    :query {:explain :bool, :accuracy :kw, :engine :kw}
                    :encode :keywordize-query
                    :response :sutra.foundation.contracts/query-result}
   :ontology-series {:url "/api/ontology/series"
                     :request (c :series-in)
                     :response (c :series)
                     :methods {:post {:effects reads :requires c-read}}
                     :service :oss}
   :project-map {:url "/api/projects/map"
                 :response (c :project-map)
                 :methods {:get {:effects reads :requires m-onto}}
                 :service :platform
                 :branch? true
                 :query {:project :str}}})
