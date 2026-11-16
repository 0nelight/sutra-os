(ns ^:unit platform.gzip-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.serve.gzip :as gzip])
  (:import (java.io ByteArrayInputStream)
           (java.util.zip GZIPInputStream)))

(def ^:private body
  (apply str (repeat 3000 "{\"id\":\"abc\",\"values\":{\"name\":\"x\"}},")))

(defn- gunzip [^bytes b]
  (slurp (GZIPInputStream. (ByteArrayInputStream. b))))

(defn- run
  [accept-encoding response]
  ((gzip/wrap-gzip (constantly response))
   {:headers (cond-> {} accept-encoding (assoc "accept-encoding" accept-encoding))}))

(defn- transit-response [body]
  {:status 200 :headers {"Content-Type" "application/transit+json"} :body body})

(deftest compresses-transit-when-accepted
  (let [resp (run "gzip, deflate, br" (transit-response body))]
    (testing "body is gzipped and round-trips back to the original bytes"
      (is (= "gzip" (get-in resp [:headers "Content-Encoding"])))
      (is (bytes? (:body resp)))
      (is (= body (gunzip (:body resp)))))
    (testing "Content-Length describes the COMPRESSED body, not the original"
      (is (= (str (alength ^bytes (:body resp)))
             (get-in resp [:headers "Content-Length"]))))
    (testing "Vary, so a URL-keyed cache can't serve gzip to a client without it"
      (is (= "Accept-Encoding" (get-in resp [:headers "Vary"]))))
    (testing "it actually shrinks the payload"
      (is (< (alength ^bytes (:body resp)) (count body))))))

(deftest leaves-body-alone-when-client-cannot-decode
  (testing "no Accept-Encoding at all"
    (let [resp (run nil (transit-response body))]
      (is (nil? (get-in resp [:headers "Content-Encoding"])))
      (is (= body (:body resp)))))
  (testing "Accept-Encoding without gzip"
    (let [resp (run "deflate, br" (transit-response body))]
      (is (nil? (get-in resp [:headers "Content-Encoding"])))
      (is (= body (:body resp))))))

(deftest skips-already-compressed-content-types
  (let [stream (ByteArrayInputStream. (.getBytes "%PDF-1.4 …"))
        resp   (run "gzip" {:status 200
                            :headers {"Content-Type" "application/pdf"}
                            :body stream})]
    (is (nil? (get-in resp [:headers "Content-Encoding"])))
    (is (identical? stream (:body resp)))))

(deftest never-double-encodes
  (let [resp (run "gzip" (-> (transit-response body)
                             (assoc-in [:headers "Content-Encoding"] "gzip")))]
    (is (= body (:body resp)) "an already-encoded body passes through verbatim")))

(deftest skips-provably-tiny-bodies
  (testing "small body with a known length is left alone"
    (let [resp (run "gzip" {:status 200
                            :headers {"Content-Type" "application/transit+json"
                                      "Content-Length" "12"}
                            :body "hello world!"})]
      (is (nil? (get-in resp [:headers "Content-Encoding"])))))
  (testing "big body with a known length is still compressed"
    (let [resp (run "gzip" {:status 200
                            :headers {"Content-Type" "application/transit+json"
                                      "Content-Length" (str (count body))}
                            :body body})]
      (is (= "gzip" (get-in resp [:headers "Content-Encoding"]))))))

(deftest header-lookup-is-case-insensitive
  (testing "lower-case response headers are still recognized"
    (let [resp (run "gzip" {:status 200
                            :headers {"content-type" "application/transit+json"}
                            :body body})]
      (is (= "gzip" (get-in resp [:headers "Content-Encoding"])))))
  (testing "upper-case Accept-Encoding on the request is still honoured"
    (let [resp ((gzip/wrap-gzip (constantly (transit-response body)))
                {:headers {"Accept-Encoding" "GZIP"}})]
      (is (= "gzip" (get-in resp [:headers "Content-Encoding"]))))))
