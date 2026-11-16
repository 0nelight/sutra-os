(ns sutra.serve.gzip
  (:require [clojure.string :as str]
            [ring.core.protocols :as ring-io]
            [sutra.foundation.profile :as prof])
  (:import (java.io ByteArrayOutputStream)
           (java.util.zip GZIPOutputStream)))

(set! *warn-on-reflection* true)

(def ^:private gzip-content-types
  #{"application/transit+json" "application/json" "application/edn"
    "text/html" "text/css" "text/plain" "text/csv"
    "application/javascript" "text/javascript" "image/svg+xml"})

(def ^:private gzip-min-bytes 1400)

(defn- header-get
  [headers h]
  (let [h (str/lower-case h)]
    (some (fn [[k v]] (when (= h (str/lower-case (name k))) v)) headers)))

(defn- gzip-response-bytes
  ^bytes [response]
  (let [bos (ByteArrayOutputStream. 65536)
        gz  (GZIPOutputStream. bos)]
    (ring-io/write-body-to-stream (:body response) response gz)
    (.close gz)
    (.toByteArray bos)))

(defn- gzip-candidate? [request response]
  (and (:body response)
       (not (header-get (:headers response) "Content-Encoding"))
       (some-> (header-get (:headers request) "Accept-Encoding")
               str/lower-case
               (str/includes? "gzip"))
       (contains? gzip-content-types
                  (some-> (header-get (:headers response) "Content-Type")
                          (str/split #";") first str/trim str/lower-case))
       (let [len (some-> (header-get (:headers response) "Content-Length")
                         parse-long)]
         (or (nil? len) (>= len gzip-min-bytes)))))

(defn body-bytes
  [response]
  (or (some-> (header-get (:headers response) "Content-Length") parse-long)
      (let [body (:body response)]
        (cond (bytes? body)  (alength ^bytes body)
              (string? body) (count ^String body)))))

(defn wrap-gzip
  [handler]
  (fn [request]
    (let [response (handler request)]
      (if (and response (gzip-candidate? request response))
        (let [before (body-bytes response)
              gz (prof/with-mark-result :Gzip
                   (fn [^bytes b] (cond-> {:bytes (alength b)}
                                    before (assoc :bytes-in before)))
                   (gzip-response-bytes response))]
          (-> response
              (assoc :body gz)
              (update :headers merge {"Content-Encoding" "gzip"
                                      "Content-Length"   (str (alength gz))
                                      "Vary"             "Accept-Encoding"})))
        response))))
