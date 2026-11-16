(ns sutra.blobs.fs
  (:require [sutra.blobs.api :as blobs]
            [clojure.java.io :as io])
  (:import (java.io File)
           (java.security MessageDigest)))

(set! *warn-on-reflection* true)

(defn- blob-file ^File [^File root hash] (io/file root (subs hash 0 2) hash))

(defn- land!
  [^File tmp ^File dst]
  (io/make-parents dst)
  (if (.exists dst)
    (.delete tmp)
    (when-not (.renameTo tmp dst)
      (io/copy tmp dst) (.delete tmp))))

(defn spool!
  [in ^File dir]
  (let [md   (MessageDigest/getInstance "SHA-256")
        tmp  (File/createTempFile "blob" ".tmp" dir)
        size (with-open [is (io/input-stream in)
                         os (io/output-stream tmp)]
               (let [buf (byte-array 65536)]
                 (loop [total 0]
                   (let [n (.read is buf)]
                     (if (neg? n)
                       total
                       (do (.update md buf 0 n)
                           (.write os buf 0 n)
                           (recur (+ total n))))))))]
    [(apply str (map #(format "%02x" (bit-and % 0xff)) (.digest md))) size tmp]))

(defrecord FsBlobs [^File root]
  blobs/Blobs
  (-put! [_ in]
    (if (bytes? in)
      (let [^bytes bs in
            h   (blobs/address-of bs)
            dst (blob-file root h)]
        (if (.exists dst)
          (.setLastModified dst (System/currentTimeMillis))
          (let [tmp (File/createTempFile "blob" ".tmp" root)]
            (with-open [os (io/output-stream tmp)] (.write os bs))
            (land! tmp dst)))
        {:hash h :size (alength bs)})
      (let [[h size tmp] (spool! in root)]
        (land! tmp (blob-file root h))
        {:hash h :size size})))
  (-open [_ h] (let [f (blob-file root h)] (when (.exists f) (io/input-stream f))))
  (-exists? [_ h] (.exists (blob-file root h)))
  (-size-of [_ h] (let [f (blob-file root h)] (when (.exists f) (.length f))))
  (-stored-at [_ h] (let [f (blob-file root h)] (when (.exists f) (.lastModified f))))
  (-hashes [_] (->> (file-seq root)
                    (filter #(.isFile ^File %))
                    (map #(.getName ^File %))
                    (filter blobs/valid-address?)))
  (-delete! [_ h before]
    (let [f (blob-file root h)]
      (and (.exists f) (or (nil? before) (< (.lastModified f) before)) (.delete f)))))

(defn blobs
  [dir]
  (let [root (io/file dir)]
    (.mkdirs root)
    (->FsBlobs root)))
