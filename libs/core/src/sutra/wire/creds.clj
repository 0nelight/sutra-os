(ns sutra.wire.creds
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn normalize-base
  [url]
  (try
    (let [u      (java.net.URI. (str/trim (str url)))
          scheme (some-> (.getScheme u) str/lower-case)
          host   (some-> (.getHost u) str/lower-case)
          port   (let [p (.getPort u)]
                   (if (pos? p) p (case scheme "https" 443 "http" 80 -1)))]
      (when (and scheme host) (str scheme "://" host ":" port)))
    (catch java.net.URISyntaxException _ nil)))

(defn creds-base
  [creds legacy-base]
  (normalize-base (or (:base-url creds) legacy-base)))

(defn- https-of
  [base]
  (some-> base (str/replace #"^http://(.+):80$" "https://$1:443")))

(defn bound-user
  [creds base as legacy-base]
  (let [b     (normalize-base base)
        bound (creds-base creds legacy-base)]
    (if (and (seq (:users creds))
             (not (and b (or (= b bound) (= b (https-of bound))))))
      {:refused {:von (or (creds-base creds legacy-base) "eine Datei ohne :base-url") :nach b}}
      {:user (get-in creds [:users as])})))
