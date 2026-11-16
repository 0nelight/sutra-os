(ns sutra.types.paths.compute
  (:require [sutra.types.paths.rules :refer [reads writes]]))

(defn- b
  [k]
  (keyword "sutra.foundation.contracts.b" (name k)))

(def endpoints
  {:container-images {:url "/api/container-images"
                      :methods
                      {:get
                       {:effects reads
                        :requires {:custom "each image's project decides: listed where readable"}
                        :response (b :container-images)}
                       :post
                       {:effects writes
                        :requires {:custom "the image's project is in the body: :update there"}
                        :request (b :container-image-push-in)
                        :response (b :container-image-pushed)}}
                      :service :platform}
   :container-image {:url "/api/container-images/:name/:tag"
                     :methods {:delete {:effects writes
                                        :requires {:custom "the image's project decides: :update"}
                                        :response (b :container-image-deleted)}
                               :get {:effects reads
                                     :requires {:custom "the image's project decides: :read"}
                                     :response (b :container-image-one)}}
                     :service :platform}})
