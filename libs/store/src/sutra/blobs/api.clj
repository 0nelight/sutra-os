(ns sutra.blobs.api
  (:require [sutra.port :as port])
  (:import (java.io ByteArrayInputStream InputStream)
           (java.security MessageDigest)))

(set! *warn-on-reflection* true)

(defprotocol Blobs
  (-put! [b in])
  (-open [b hash])
  (-exists? [b hash])
  (-size-of [b hash])
  (-stored-at [b hash])
  (-hashes [b])
  (-delete! [b hash landed-before]))

(defn valid-address?
  [h]
  (boolean (and (string? h) (re-matches #"[0-9a-f]{64}" h))))

(defn address-of
  [^bytes bs]
  (let [d (.digest (MessageDigest/getInstance "SHA-256") bs)]
    (apply str (map #(format "%02x" (bit-and % 0xff)) d))))

(def ^:private address [:fn valid-address?])

(defn put! [b in]
  (port/answered! :blobs [:map [:hash address] [:size nat-int?]] (-put! b in) :put))
(defn open ^InputStream [b hash]
  (when (valid-address? hash)
    (port/answered! :blobs [:maybe [:fn #(instance? InputStream %)]] (-open b hash) :open)))
(defn exists? [b hash] (boolean (and (valid-address? hash) (-exists? b hash))))
(defn size-of [b hash]
  (when (valid-address? hash) (port/answered! :blobs [:maybe nat-int?] (-size-of b hash) :size-of)))
(defn stored-at [b hash]
  (when (valid-address? hash) (port/answered! :blobs [:maybe :int] (-stored-at b hash) :stored-at)))
(defn hashes [b] (port/answered! :blobs [:sequential address] (vec (-hashes b)) :hashes))
(defn delete!
  ([b hash] (delete! b hash nil))
  ([b hash landed-before]
   (and (valid-address? hash)
        (port/answered! :blobs :boolean
                        (-delete! b hash
                                  (port/asked! :blobs [:maybe :int] landed-before :landed-before))
                        :delete))))

(defn open-range
  [b hash start length]
  (when-let [^InputStream is (open b hash)]
    (with-open [in is]
      (loop [need (long start)]
        (when (pos? need)
          (let [s (.skip in need)]
            (when (pos? s) (recur (- need s))))))
      (let [buf (byte-array (int length))
            n   (.readNBytes in buf 0 (int length))]
        (ByteArrayInputStream. buf 0 n)))))
