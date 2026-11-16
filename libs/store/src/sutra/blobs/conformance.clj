(ns sutra.blobs.conformance
  (:require [sutra.blobs.api :as blobs]
            [clojure.test :refer [is testing]])
  (:import (java.io ByteArrayInputStream)))

(set! *warn-on-reflection* true)

(defn- slurp-bytes [in] (when in (with-open [^java.io.InputStream is in] (vec (.readAllBytes is)))))

(defn check-all [make]
  (testing "content addressing"
    (let [bs (.getBytes "hello blobs" "UTF-8")
          s  (make)
          {:keys [hash size]} (blobs/put! s bs)]
      (is (= (blobs/address-of bs) hash) "the address is the sha-256 of the bytes")
      (is (= (alength bs) size))
      (is (= hash (:hash (blobs/put! s (ByteArrayInputStream. bs))))
          "a stream of the same bytes is the same blob")
      (is (blobs/exists? s hash))
      (is (= (vec bs) (slurp-bytes (blobs/open s hash))))
      (is (= (vec (.getBytes "blobs" "UTF-8")) (slurp-bytes (blobs/open-range s hash 6 5))))
      (is (= size (blobs/size-of s hash)))
      (is (number? (blobs/stored-at s hash)))
      (is (some #{hash} (blobs/hashes s)))
      (testing "an address that is not one is never asked"
        (is (nil? (blobs/open s "../etc/passwd")))
        (is (false? (blobs/exists? s "nope"))))
      (testing "delete"
        (is (true? (blobs/delete! s hash)))
        (is (false? (blobs/exists? s hash)))
        (is (nil? (blobs/open s hash)))
        (is (false? (blobs/delete! s hash))))))
  (testing "a re-put pins the blob against a sweep that scanned it before"
    (let [bs (.getBytes "pinned blob" "UTF-8")
          s  (make)
          {:keys [hash]} (blobs/put! s bs)
          cutoff (inc (blobs/stored-at s hash))]
      (Thread/sleep 20)
      (blobs/put! s bs)
      (is (false? (blobs/delete! s hash cutoff)))
      (is (blobs/exists? s hash))
      (is (true? (blobs/delete! s hash (inc (blobs/stored-at s hash))))))))
