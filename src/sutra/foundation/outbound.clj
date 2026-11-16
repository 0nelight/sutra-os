(ns sutra.foundation.outbound
  (:require [clojure.string :as str]
            [org.httpkit.client :as http]
            [sutra.kit.effects :as effects])
  (:import (java.net URI)
           (java.net.http HttpClient HttpClient$Redirect HttpRequest HttpRequest$Builder
                          HttpResponse HttpResponse$BodyHandlers)))

(set! *warn-on-reflection* true)

(defn target
  [{:keys [method url]}]
  (let [^java.net.URI u (try (java.net.URI. (str url)) (catch java.net.URISyntaxException _ nil))]
    (str (str/upper-case (name (or method :get))) " "
         (if (and u (.getHost u))
           (str (.getScheme u) "://" (.getHost u)
                (when (pos? (.getPort u)) (str ":" (.getPort u))) (.getPath u))
           (first (str/split (str url) #"\?"))))))

(defn request
  ([req] (request effects/*work* req))
  ([token req]
   (effects/call-async! token (target req)
                        #(http/request (merge {:follow-redirects false} req) %))))

(defn request!
  ([req] @(request req))
  ([token req] @(request token req)))

(defn post! [url opts] (request! (assoc opts :method :post :url url)))

(defn get! [url opts] (request! (assoc opts :method :get :url url)))

(defonce ^:private streaming-client
  (delay (.build (.followRedirects (java.net.http.HttpClient/newBuilder)
                                   HttpClient$Redirect/NEVER))))

(defn stream!
  ([url headers] (stream! effects/*work* url headers))
  ([token url headers]
   (effects/call! token (target {:method :get :url url})
                  (fn []
                    (let [b (reduce-kv (fn [^HttpRequest$Builder b k v] (.header b k v))
                                       (HttpRequest/newBuilder (URI. (str url)))
                                       headers)
                          ^HttpResponse resp
                          (.send ^HttpClient @streaming-client
                                 (.build (.GET ^HttpRequest$Builder b))
                                 (HttpResponse$BodyHandlers/ofInputStream))]
                      [(.statusCode resp) (.body resp)])))))
