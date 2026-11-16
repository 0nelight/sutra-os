(ns conformance.http
  (:require [clojure.string :as str])
  (:import (java.net URI URLEncoder)
           (java.net.http HttpClient HttpClient$Redirect HttpRequest HttpRequest$BodyPublishers
                          HttpResponse$BodyHandlers)
           (java.nio.charset StandardCharsets)
           (java.time Duration)
           (java.util Base64)))

(def ^:private client
  (delay (-> (HttpClient/newBuilder)
             (.connectTimeout (Duration/ofSeconds 10))
             (.followRedirects HttpClient$Redirect/NORMAL)
             (.build))))

(defn request
  [{:keys [method url headers body timeout-s] :or {method :get timeout-s 20}}]
  (let [b    (-> (HttpRequest/newBuilder (URI. url))
                 (.timeout (Duration/ofSeconds timeout-s)))
        pub  (if body
               (HttpRequest$BodyPublishers/ofString body StandardCharsets/UTF_8)
               (HttpRequest$BodyPublishers/noBody))
        _    (doseq [[k v] headers] (.header b k v))
        req  (.build (.method b (str/upper-case (name method)) pub))
        resp (.send ^HttpClient @client req (HttpResponse$BodyHandlers/ofString))]
    {:status  (.statusCode resp)
     :headers (into {} (map (fn [[k vs]] [(str/lower-case k) (first vs)]))
                    (.map (.headers resp)))
     :body    (.body resp)}))

(defn get-url [url & [headers]] (request {:url url :headers headers}))

(defn form
  [m]
  (str/join "&" (for [[k v] m]
                  (str (URLEncoder/encode (str k) "UTF-8") "="
                       (URLEncoder/encode (str v) "UTF-8")))))

(defn basic-auth
  [user password]
  (str "Basic " (.encodeToString (Base64/getEncoder)
                                 (.getBytes (str user ":" password) StandardCharsets/UTF_8))))

(defn post-form
  [url params & [headers]]
  (request {:method :post :url url :body (form params)
            :headers (merge {"Content-Type" "application/x-www-form-urlencoded"
                             "Accept" "application/json"}
                            headers)}))

(defn json-content? [resp]
  (str/starts-with? (str/lower-case (str (get-in resp [:headers "content-type"])))
                    "application/json"))
