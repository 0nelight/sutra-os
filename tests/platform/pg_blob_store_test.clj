(ns ^:integration platform.pg-blob-store-test
  (:require [sutra.store.pg.blobs :as pg-blobs]
            [sutra.blobs.api :as blobs]
            [sutra.blobs.fs :as blobs-fs]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [next.jdbc :as jdbc]
            [sutra.store.pg.control-tables :as ct]
            [sutra.store.control.tables :as cc]
            [support.pod :as pod]
            [next.jdbc.result-set])
  (:import (java.io ByteArrayInputStream ByteArrayOutputStream File)))

(def ^:private ^:dynamic *ds* nil)

(use-fixtures :once
  (fn [t]
    (let [h (pod/start-pg!)]
      (try
        (let [ds (jdbc/get-datasource {:jdbcUrl (:url h) :user (:user h) :password (:password h)})]
          (doseq [stmt (ct/ddl cc/tables cc/renamed-tables)] (jdbc/execute-one! ds [stmt]))
          (binding [*ds* ds] (t)))
        (finally (pod/stop-pg! h))))))

(use-fixtures :each
  (fn [t]
    (jdbc/execute-one! *ds* ["TRUNCATE control_blobs, control_blob_chunks"])
    (t)))

(defn- tmp-dir []
  (doto (File. (str (System/getProperty "java.io.tmpdir") "/cas-" (random-uuid))) (.mkdirs)))

(defn- store
  ([] (store (File. (str (System/getProperty "java.io.tmpdir") "/no-legacy-" (random-uuid)))))
  ([dir] (pg-blobs/blobs (constantly *ds*) dir)))

(defn- slurp-bytes [in]
  (with-open [is in]
    (let [bos (ByteArrayOutputStream.)] (io/copy is bos) (.toByteArray bos))))

(defn- rows [table]
  (:n (jdbc/execute-one! *ds* [(str "SELECT count(*)::int AS n FROM " table)]
                         {:builder-fn next.jdbc.result-set/as-unqualified-maps})))

(def ^:private big
  (let [bs (byte-array (+ (* 2 1024 1024) 12345))]
    (.nextBytes (java.util.Random. 42) bs)
    bs))

(deftest bytes-and-streams-round-trip-across-chunks
  (let [s (store)
        {:keys [hash size]} (blobs/put! s (ByteArrayInputStream. big))]
    (is (= (blobs/address-of big) hash) "the address is the SHA-256 of the bytes, as on disk")
    (is (= (alength big) size))
    (is (= 3 (rows "control_blob_chunks")) "2 MiB + a bit is three 1 MiB chunks")
    (is (java.util.Arrays/equals ^bytes big ^bytes (slurp-bytes (blobs/open s hash))))
    (testing "a range that straddles a chunk boundary"
      (let [start (- (* 1024 1024) 10)]
        (is
         (java.util.Arrays/equals
          (java.util.Arrays/copyOfRange ^bytes big (int start) (int (+ start 20)))
          ^bytes (slurp-bytes (blobs/open-range s hash start 20))))))
    (testing "metadata"
      (is (blobs/exists? s hash))
      (is (= (alength big) (blobs/size-of s hash)))
      (is (< (- (System/currentTimeMillis) 60000) (blobs/stored-at s hash)
             (+ (System/currentTimeMillis) 60000)))
      (is (= [hash] (vec (blobs/hashes s)))))
    (testing "a re-put of the same bytes, as bytes or as a stream, writes nothing"
      (is (= hash (:hash (blobs/put! s big))))
      (is (= hash (:hash (blobs/put! s (ByteArrayInputStream. big)))))
      (is (= 1 (rows "control_blobs")))
      (is (= 3 (rows "control_blob_chunks"))))
    (testing "delete takes the row and every chunk"
      (is (true? (blobs/delete! s hash)))
      (is (not (blobs/exists? s hash)))
      (is (nil? (blobs/open s hash)))
      (is (= 0 (rows "control_blob_chunks")))
      (is (false? (blobs/delete! s hash))))))

(deftest the-empty-blob-exists-and-reads-empty
  (let [s (store)
        {:keys [hash size]} (blobs/put! s (byte-array 0))]
    (is (= 0 size))
    (is (blobs/exists? s hash))
    (is (= 0 (alength ^bytes (slurp-bytes (blobs/open s hash)))))))

(deftest the-legacy-directory-is-read-through-then-drained
  (let [dir    (tmp-dir)
        legacy (blobs-fs/blobs dir)
        old    (.getBytes "written before the switch" "UTF-8")
        h      (:hash (blobs/put! legacy old))
        s      (store dir)]
    (testing "before the drain the bytes are served from the directory"
      (is (blobs/exists? s h))
      (is (= (seq old) (seq (slurp-bytes (blobs/open s h)))))
      (is (= 0 (rows "control_blobs"))))
    (testing "new bytes go to Postgres only"
      (let [h2 (:hash (blobs/put! s (.getBytes "written after" "UTF-8")))]
        (is (not (blobs/exists? legacy h2)))
        (is (= #{h h2} (set (blobs/hashes s))))))
    (testing "the drain copies what Postgres lacks, re-hashing it"
      (is (= {:copied 1 :corrupt []} (pg-blobs/drain! s)))
      (is (= 2 (rows "control_blobs")))
      (is (= {:copied 0 :corrupt []} (pg-blobs/drain! s)) "idempotent"))
    (testing "a file whose bytes no longer match its name is reported, not copied"
      (let [bad (apply str (repeat 64 "a"))
            f   (io/file dir "aa" bad)]
        (io/make-parents f)
        (spit f "not what the name promises")
        (is (= {:copied 0 :corrupt [bad]} (pg-blobs/drain! s)))
        (is (= 2 (rows "control_blobs")) "the mismatching bytes did not stay behind")))))
