(ns sutra.media.virtual
  (:require [sutra.kit.refusal :as refusal]
            [sutra.blobs.api :as blobs]
            [sutra.media.sets :as sets]
            [sutra.media.ports :as ports]
            [sutra.store.access.scope :as ctx]
            [sutra.store.seat :as seat]
            [sutra.types.file-sync :as file-sync])
  (:import (java.io FilterInputStream InputStream)))

(set! *warn-on-reflection* true)

(defn- source-spec
  [schema source-name]
  (some (fn [[k v]] (when (#{(name k) (some-> (:source-name v) name)} source-name) v))
        (:sources schema)))

(defn- on-share-path
  [schema {:keys [media-set physical-path]}]
  (let [[_ ms] (sets/declared schema media-set)
        sub    (file-sync/normal-path (:subfolder ms))]
    (if (seq sub) (str sub "/" physical-path) physical-path)))

(defn- closing-source
  ^InputStream [^InputStream stream close! s]
  (proxy [FilterInputStream] [stream]
    (close []
      (try (.close stream)
           (finally (close! s))))))

(defn open!
  [store {:keys [branch source] :as item} range-of]
  (let [schema (seat/rebuild-schema-from-storage (ctx/at store branch))
        path   (on-share-path schema item)
        spec   (or (source-spec schema source)
                   (refusal/refuse! :refusal.media/virtual-source-gone {:source source}))
        s      ((ctx/connect nil) (dissoc spec :markings))
        {:keys [invoke! close!]} (ports/source-ops store)]
    (try
      (if-let [stat (invoke! s :stat {:path path})]
        (assoc stat :stream (closing-source
                             (invoke! s :read (assoc (range-of stat) :path path)) close! s))
        (do (close! s) nil))
      (catch Throwable t
        (close! s)
        (throw t)))))

(defrecord ReadThrough [inner]
  blobs/Blobs
  (-put! [_ in] (blobs/-put! inner in))
  (-open [_ hash]
    (or (blobs/-open inner hash)
        (let [store (ctx/store)]
          (when-let [item (sets/virtual-item store hash)]
            (:stream (open! store item (constantly nil)))))))
  (-exists? [_ hash] (blobs/-exists? inner hash))
  (-size-of [_ hash] (blobs/-size-of inner hash))
  (-stored-at [_ hash] (blobs/-stored-at inner hash))
  (-hashes [_] (blobs/-hashes inner))
  (-delete! [_ hash landed-before] (blobs/-delete! inner hash landed-before)))

(def module
  {:name     :media-virtual
   :provides {:blobs/read-through ->ReadThrough}})
