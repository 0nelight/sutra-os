(ns sutra.sources.proxy.seal
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [sutra.foundation.egress :as egress]
            [sutra.store.storage.instance-config :as ic])
  (:import (java.security KeyFactory KeyPairGenerator SecureRandom)
           (java.security.spec MGF1ParameterSpec PKCS8EncodedKeySpec RSAPublicKeySpec)
           (java.util Base64)
           (javax.crypto Cipher Mac)
           (javax.crypto.spec GCMParameterSpec OAEPParameterSpec PSource$PSpecified SecretKeySpec)))

(set! *warn-on-reflection* true)

(def ^:private cfg-key "source-seal-key")

(def ^:private iv-bytes 12)

(defn- b64 ^String [^bytes b] (.encodeToString (Base64/getEncoder) b))

(defn- unb64 ^bytes [^String s] (.decode (Base64/getDecoder) s))

(defn- wrap-key
  ^SecretKeySpec [^String master]
  (let [mac (doto (Mac/getInstance "HmacSHA256")
              (.init (SecretKeySpec. (.getBytes master "UTF-8") "HmacSHA256")))]
    (SecretKeySpec. (.doFinal mac (.getBytes "sutra/source-credentials/v1" "UTF-8")) "AES")))

(defn wrap
  ^String [master ^bytes plain]
  (let [iv (byte-array iv-bytes)
        _  (.nextBytes (SecureRandom.) iv)
        c  (doto (Cipher/getInstance "AES/GCM/NoPadding")
             (.init Cipher/ENCRYPT_MODE (wrap-key master) (GCMParameterSpec. 128 iv)))
        ct (.doFinal c plain)
        out (byte-array (+ iv-bytes (alength ct)))]
    (System/arraycopy iv 0 out 0 iv-bytes)
    (System/arraycopy ct 0 out iv-bytes (alength ct))
    (b64 out)))

(defn unwrap
  ^bytes [master ^String sealed]
  (let [raw (unb64 sealed)
        c   (doto (Cipher/getInstance "AES/GCM/NoPadding")
              (.init Cipher/DECRYPT_MODE (wrap-key master) (GCMParameterSpec. 128 raw 0 iv-bytes)))]
    (.doFinal c raw iv-bytes (- (alength raw) iv-bytes))))

(defn- new-wrapped-key [master]
  (let [kp (.generateKeyPair (doto (KeyPairGenerator/getInstance "RSA") (.initialize 2048)))]
    (wrap master (.getEncoded (.getPrivate kp)))))

(defn- private-key
  [store]
  (let [master  (egress/egress-master)
        wrapped (or (ic/get-cfg store cfg-key)
                    (ic/swap-cfg! store cfg-key #(or % (new-wrapped-key master))))]
    (.generatePrivate (KeyFactory/getInstance "RSA")
                      (PKCS8EncodedKeySpec. (unwrap master wrapped)))))

(defn public-key
  [store]
  (let [^java.security.interfaces.RSAPrivateCrtKey priv (private-key store)
        pub (.generatePublic (KeyFactory/getInstance "RSA")
                             (RSAPublicKeySpec. (.getModulus priv) (.getPublicExponent priv)))]
    {:public-key (b64 (.getEncoded pub)) :alg "RSA-OAEP-256"}))

(defn- oaep ^OAEPParameterSpec []
  (OAEPParameterSpec. "SHA-256" "MGF1" MGF1ParameterSpec/SHA256 PSource$PSpecified/DEFAULT))

(defn- open-one [priv ^String sealed]
  (let [c (doto (Cipher/getInstance "RSA/ECB/OAEPWithSHA-256AndMGF1Padding")
            (.init Cipher/DECRYPT_MODE ^java.security.PrivateKey priv (oaep)))]
    (edn/read-string (String. (.doFinal c (unb64 sealed)) "UTF-8"))))

(defn open-cipher
  [store cipher]
  (let [priv (private-key store)]
    (reduce (fn [acc c] (merge acc (open-one priv c)))
            {}
            (str/split (str/trim cipher) #"\s+"))))
