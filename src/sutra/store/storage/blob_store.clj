(ns sutra.store.storage.blob-store
  (:require [clojure.string :as str]
            [sutra.blobs.fs :as blobs-fs]
            [sutra.kit.system :as sys]
            [sutra.foundation.env :as env]))

(set! *warn-on-reflection* true)

(defn kind
  []
  (if (= "pg" (some-> (env/getenv "SUTRA_BLOB_STORE") str/trim str/lower-case)) :pg :fs))

(defn- dir []
  (or (env/getenv "SUTRA_BLOB_DIR")
      (str (System/getProperty "user.home") java.io.File/separator ".sutra" java.io.File/separator
           "blobs")))

(defonce ^:private store
  (delay
    (case (kind)
      :pg ((sys/required (sys/process) :blobs/pg) (dir))
      :fs (blobs-fs/blobs (dir)))))

(def module
  {:name :blob-store :provides {:blobs/db (fn [] @store)}})
