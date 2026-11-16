(ns sutra.blobs.mem
  (:require [sutra.blobs.api :as blobs])
  (:import (java.io ByteArrayInputStream InputStream)))

(set! *warn-on-reflection* true)

(defn- bytes-of ^bytes [in]
  (if (bytes? in) in (with-open [^InputStream is in] (.readAllBytes is))))

(defrecord MemBlobs [state clock]
  blobs/Blobs
  (-put! [_ in]
    (let [bs (bytes-of in)
          h  (blobs/address-of bs)]
      (swap! state
             (fn [m]
               (if (contains? m h) (assoc-in m [h :at] (clock))
                 (assoc m h {:bytes bs :at (clock)}))))
      {:hash h :size (alength bs)}))
  (-open [_ h] (some-> (get @state h) :bytes (ByteArrayInputStream.)))
  (-exists? [_ h] (contains? @state h))
  (-size-of [_ h] (some-> ^bytes (:bytes (get @state h)) alength long))
  (-stored-at [_ h] (:at (get @state h)))
  (-hashes [_] (vec (keys @state)))
  (-delete! [_ h before]
    (let [[old new] (swap-vals! state
                                (fn [m]
                                  (if (and before (some-> (get-in m [h :at]) (>= before))) m
                                    (dissoc m h))))]
      (and (contains? old h) (not (contains? new h))))))

(defn blobs
  ([] (blobs {}))
  ([{:keys [clock] :or {clock #(System/currentTimeMillis)}}]
   (->MemBlobs (atom {}) clock)))
