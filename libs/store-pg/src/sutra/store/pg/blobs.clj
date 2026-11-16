(ns sutra.store.pg.blobs
  (:require [sutra.blobs.api :as blobs]
            [sutra.blobs.fs :as blobs-fs]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs])
  (:import (java.io ByteArrayInputStream File InputStream SequenceInputStream)))

(set! *warn-on-reflection* true)

(def ^:private chunk-bytes (* 1024 1024))

(def ^:private unq {:builder-fn rs/as-unqualified-lower-maps})

(defn- chunks-of
  [^InputStream in]
  (lazy-seq
   (let [buf (.readNBytes in (int chunk-bytes))]
     (when (pos? (alength buf))
       (cons buf (chunks-of in))))))

(defn- row [ds hash]
  (when-let [r (jdbc/execute-one! ds
                                  ["SELECT size, stored_at FROM control_blobs WHERE hash = ?" hash]
                                  unq)]
    (assoc r :ms (some-> ^java.sql.Timestamp (:stored_at r) .getTime))))

(defn- land!
  [ds hash size ^InputStream in]
  (jdbc/with-transaction [tx ds]
    (when (zero?
           (or
            (:next.jdbc/update-count
             (jdbc/execute-one! tx
                                ["UPDATE control_blobs SET stored_at = CURRENT_TIMESTAMP WHERE hash = ?" hash]))
            0))
      (doseq [[n bs] (map-indexed vector (chunks-of in))]
        (jdbc/execute-one! tx ["INSERT INTO control_blob_chunks (hash, n, bytes) VALUES (?, ?, ?)
                                ON CONFLICT DO NOTHING" hash n bs]))
      (jdbc/execute-one! tx ["INSERT INTO control_blobs (hash, size) VALUES (?, ?)
                              ON CONFLICT (hash) DO UPDATE SET stored_at = CURRENT_TIMESTAMP"
                              hash size]))))

(defn- stream ^InputStream [ds hash]
  (letfn [(step [n]
            (lazy-seq
             (when-let [bs (:bytes (jdbc/execute-one! ds ["SELECT bytes FROM control_blob_chunks
                                                           WHERE hash = ? AND n = ?" hash n] unq))]
               (cons (ByteArrayInputStream. ^bytes bs) (step (inc n))))))]
    (SequenceInputStream. (java.util.Collections/enumeration (step 0)))))

(defrecord PgBlobs [ds-fn legacy]
  blobs/Blobs
  (-put! [_ in]
    (let [ds (ds-fn)]
      (if (bytes? in)
        (let [^bytes bs in
              h (blobs/address-of bs)]
          (land! ds h (alength bs) (ByteArrayInputStream. bs))
          {:hash h :size (alength bs)})
        (let [[h size
               ^File tmp] (blobs-fs/spool! in (io/file (System/getProperty "java.io.tmpdir")))]
          (try (with-open [is (io/input-stream tmp)] (land! ds h size is))
               (finally (.delete tmp)))
          {:hash h :size size}))))
  (-open [_ h] (if (row (ds-fn) h) (stream (ds-fn) h) (some-> legacy (blobs/open h))))
  (-exists? [_ h] (boolean (or (row (ds-fn) h) (and legacy (blobs/exists? legacy h)))))
  (-size-of [_ h] (or (:size (row (ds-fn) h)) (some-> legacy (blobs/size-of h))))
  (-stored-at [_ h] (or (:ms (row (ds-fn) h)) (some-> legacy (blobs/stored-at h))))
  (-hashes [_]
           (distinct (concat (map (comp str/trim :hash)
                                  (jdbc/execute! (ds-fn) ["SELECT hash FROM control_blobs"] unq))
                             (some-> legacy blobs/hashes))))
  (-delete! [_ h before]
    (let [n (jdbc/with-transaction [tx (ds-fn)]
              (let [n (:next.jdbc/update-count
                       (jdbc/execute-one! tx
                                          (if before
                                            ["DELETE FROM control_blobs
                                              WHERE hash = ? AND stored_at < ?"
                                             h (java.sql.Timestamp. (long before))]
                                            ["DELETE FROM control_blobs WHERE hash = ?" h])))]
                (when (pos? (or n 0))
                  (jdbc/execute-one! tx ["DELETE FROM control_blob_chunks WHERE hash = ?" h]))
                n))
          gone-legacy? (and legacy (blobs/delete! legacy h before))]
      (boolean (or (pos? (or n 0)) gone-legacy?)))))

(defn blobs
  [ds-fn dir]
  (->PgBlobs ds-fn (let [d (io/file dir)] (when (.isDirectory d) (blobs-fs/blobs d)))))

(defn drain!
  [{:keys [legacy] :as s}]
  (let [pg-only (assoc s :legacy nil)]
    (reduce (fn [acc h]
              (if (blobs/exists? pg-only h)
                acc
                (let [[got _ ^File tmp] (with-open [in (blobs/open legacy h)]
                                          (blobs-fs/spool!
                                           in (io/file (System/getProperty "java.io.tmpdir"))))]
                  (try
                    (if (= got h)
                      (do (with-open [is (io/input-stream tmp)] (blobs/put! pg-only is))
                          (update acc :copied inc))
                      (update acc :corrupt conj h))
                    (finally (.delete tmp))))))
            {:copied 0 :corrupt []}
            (if legacy (blobs/hashes legacy) []))))

(def module
  {:name :blobs-pg :requires [:pg/ds]
   :provides {:blobs/drain drain!}
   :opens {:blobs/pg (fn [_ {pools :pg/ds}] (fn [dir] (blobs #(pools nil) dir)))}})
