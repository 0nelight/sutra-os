(ns sutra.sources.tls
  (:require [clojure.string :as str])
  (:import (java.net URI)
           (java.security KeyStore MessageDigest)
           (java.security.cert CertificateException X509Certificate)
           (java.util HexFormat)
           (javax.net.ssl SSLContext SSLEngine TrustManager TrustManagerFactory
                          X509TrustManager)))

(set! *warn-on-reflection* true)

(defn pin-of
  [pin]
  (some-> pin str (str/replace #"(?i)^sha-?256:" "") (str/replace #"[:\s]" "") str/lower-case
          not-empty))

(defn default-trust
  ^X509TrustManager []
  (let [^KeyStore system-cas nil
        f (doto (TrustManagerFactory/getInstance (TrustManagerFactory/getDefaultAlgorithm))
            (.init system-cas))]
    (first (filter #(instance? X509TrustManager %) (.getTrustManagers f)))))

(defn pinned-trust
  ^X509TrustManager [pin]
  (reify X509TrustManager
    (getAcceptedIssuers [_] (make-array X509Certificate 0))
    (checkClientTrusted [_ _ _] (throw (CertificateException. "no client certificates")))
    (checkServerTrusted [_ chain _]
      (let [^X509Certificate leaf (first chain)
            seen (.formatHex (HexFormat/of)
                             (.digest (MessageDigest/getInstance "SHA-256") (.getEncoded leaf)))]
        (when-not (= pin seen)
          (throw (CertificateException.
                  (str "server certificate sha256:" seen " is not the pinned one"))))))))

(defn context
  ^SSLContext [pin]
  (doto (SSLContext/getInstance "TLS")
    (.init nil (into-array TrustManager [(if pin (pinned-trust pin) (default-trust))]) nil)))

(defn pinned-engine
  ^SSLEngine [pin url]
  (let [u    (URI. (str url))
        port (if (pos? (.getPort u)) (.getPort u) 443)]
    (doto (.createSSLEngine (context (pin-of pin)) (.getHost u) (int port))
      (.setUseClientMode true))))
