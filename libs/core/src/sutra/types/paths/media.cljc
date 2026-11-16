(ns sutra.types.paths.media
  (:require [sutra.types.paths.rules :refer [reads writes m-blob c-blob c-media]]))

(defn- c
  [k]
  (keyword "sutra.foundation.contracts.c" (name k)))

(def endpoints
  {:blob {:url "/api/blob/:hash"
          :methods {:get {:effects reads :requires c-blob}}
          :service :platform
          :encode :bytes
          :query {:type :str, :id :str, :media-set :str}
          :reason "a blob, read through whatever governs it: an OBJECT (type+id), where the ontology decides because a media set delegates to it, or its MEDIA SET (media-set), whose own project and markings decide — a dataset holding the reference governs nothing"}
   :blob-upload {:url "/api/blob"
                 :response (c :blob-stored)
                 :methods {:post {:effects writes :requires m-blob}}
                 :service :platform
                 :encode :bytes}
   :xfer-media {:url "/api/xfer/media/:hash"
                :methods {:get {:effects reads}}
                :service :platform
                :encode :bytes
                :auth :build
                :reason "a media item of a declared input, opened by the transform worker of the run that declares it — on that item's build grant (sutra.foundation.media-grant), never a session"}
   :media-gc {:url "/api/media/gc"
              :request (c :media-gc-in)
              :response (c :media-gc)
              :methods {:post {:effects writes}}
              :service :platform
              :auth :admin
              :reason "sweeping unreferenced media blobs"}
   :media-land {:url "/api/media/land"
                :request (c :media-land-in)
                :response (c :media-landed)
                :methods {:post {:effects writes :requires c-media}}
                :service :platform}})
