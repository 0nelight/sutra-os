(ns sutra.access.oidc
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [sutra.http.request :as http-request]
            [sutra.kit.anomaly :as anomaly]
            [sutra.kit.digest :as digest]
            [sutra.store.storage.instance-config :as ic])
  (:import (java.math BigInteger)
           (java.nio.charset StandardCharsets)
           (java.security KeyFactory KeyPairGenerator Signature)
           (java.security.interfaces RSAPrivateCrtKey RSAPublicKey)
           (java.security.spec PKCS8EncodedKeySpec RSAPublicKeySpec)
           (java.util Base64)))

(set! *warn-on-reflection* true)

(def signing-key-cfg "oidc_signing_key")

(def oidc-scopes
  #{"openid" "profile" "email"})

(def id-token-ttl-seconds 300)

(defn b64url ^String [^bytes bs]
  (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) bs))

(defn- unb64url ^bytes [^String s] (.decode (Base64/getUrlDecoder) s))

(defn- utf8 ^bytes [^String s] (.getBytes s StandardCharsets/UTF_8))

(defn- unsigned-bytes
  ^bytes [^BigInteger n]
  (let [bs (.toByteArray n)]
    (if (and (> (alength bs) 1) (zero? (aget bs 0)))
      (java.util.Arrays/copyOfRange bs 1 (alength bs))
      bs)))

(defn generate-private-key
  ^String []
  (let [kp (.generateKeyPair (doto (KeyPairGenerator/getInstance "RSA") (.initialize 2048)))]
    (.encodeToString (Base64/getEncoder) (.getEncoded (.getPrivate kp)))))

(defn private-key
  ^RSAPrivateCrtKey [^String pkcs8-b64]
  (.generatePrivate (KeyFactory/getInstance "RSA")
                    (PKCS8EncodedKeySpec. (.decode (Base64/getDecoder) pkcs8-b64))))

(defn public-key
  ^RSAPublicKey [^RSAPrivateCrtKey priv]
  (.generatePublic (KeyFactory/getInstance "RSA")
                   (RSAPublicKeySpec. (.getModulus priv) (.getPublicExponent priv))))

(defn kid
  [^RSAPublicKey pub]
  (let [e (b64url (unsigned-bytes (.getPublicExponent pub)))
        n (b64url (unsigned-bytes (.getModulus pub)))]
    (b64url (digest/sha256 (str "{\"e\":\"" e "\",\"kty\":\"RSA\",\"n\":\"" n "\"}")))))

(defn jwk
  [^RSAPublicKey pub]
  {:kty "RSA" :use "sig" :alg "RS256" :kid (kid pub)
   :n (b64url (unsigned-bytes (.getModulus pub)))
   :e (b64url (unsigned-bytes (.getPublicExponent pub)))})

(defn signing-key!
  ^RSAPrivateCrtKey [store]
  (private-key
   (or (ic/get-cfg store signing-key-cfg)
       (ic/swap-cfg! store signing-key-cfg #(or (not-empty %) (generate-private-key))))))

(defn jwks
  [store]
  {:keys [(jwk (public-key (signing-key! store)))]})

(defn- json-part [m] (b64url (utf8 (json/write-str m))))

(defn sign
  [^RSAPrivateCrtKey priv claims]
  (let [input (str (json-part {:alg "RS256" :typ "JWT" :kid (kid (public-key priv))}) "."
                   (json-part claims))
        sig   (.sign (doto (Signature/getInstance "SHA256withRSA")
                       (.initSign priv)
                       (.update (utf8 input))))]
    (str input "." (b64url sig))))

(defn verify
  [^RSAPublicKey pub ^String jwt]
  (let [[h p s :as parts] (str/split (str jwt) #"\.")]
    (when (= 3 (count parts))
      (try
        (let [header (json/read-str (String. (unb64url h) StandardCharsets/UTF_8)
                                    :key-fn keyword)]
          (when (and (= "RS256" (:alg header))
                     (.verify (doto (Signature/getInstance "SHA256withRSA")
                                (.initVerify pub)
                                (.update (utf8 (str h "." p))))
                              (unb64url s)))
            (json/read-str (String. (unb64url p) StandardCharsets/UTF_8) :key-fn keyword)))
        (catch Exception _ nil)))))

(defn- trimmed-base [s] (some-> s str/trim not-empty (str/replace #"/+$" "")))

(defn issuer-settings
  []
  {:public-url (System/getenv "SUTRA_PUBLIC_URL")
   :domain     (System/getenv "SUTRA_DOMAIN")
   :deployed?  (some? (not-empty (System/getenv "SUTRA_IMAGE_ID")))})

(defn issuer
  ([req] (issuer req (issuer-settings)))
  ([{:keys [headers] :as req} {:keys [public-url domain deployed?]}]
   (str (or (trimmed-base public-url)
            (some->> (trimmed-base domain) (str "https://"))
            (when-not deployed?
              (str (if (http-request/secure? req) "https" "http") "://"
                   (get headers "host" "localhost")))
            (anomaly/raise! :fault (str "no OIDC issuer: a deployed instance needs "
                                        "SUTRA_PUBLIC_URL or SUTRA_DOMAIN")
                            {}))
        "/api/oauth")))

(defn discovery
  [iss]
  {:issuer                                iss
   :authorization_endpoint                (str iss "/authorize")
   :token_endpoint                        (str iss "/token")
   :jwks_uri                              (str iss "/jwks")
   :userinfo_endpoint                     (str iss "/userinfo")
   :introspection_endpoint                (str iss "/introspect")
   :revocation_endpoint                   (str iss "/revoke")
   :scopes_supported                      (vec (sort oidc-scopes))
   :response_types_supported              ["code"]
   :response_modes_supported              ["query"]
   :grant_types_supported                 ["authorization_code" "client_credentials"]
   :subject_types_supported               ["public"]
   :id_token_signing_alg_values_supported ["RS256"]
   :token_endpoint_auth_methods_supported ["client_secret_post" "client_secret_basic"]
   :introspection_endpoint_auth_methods_supported ["client_secret_post" "client_secret_basic"]
   :revocation_endpoint_auth_methods_supported ["client_secret_post" "client_secret_basic"]
   :code_challenge_methods_supported      ["S256"]
   :claims_supported                      ["iss" "sub" "aud" "exp" "iat" "auth_time" "nonce"
                                           "preferred_username"]})

(defn openid?
  [scope]
  (contains? (set (str/split (str/trim (str scope)) #"[\s,]+")) "openid"))

(defn subject
  [p]
  (str (or (:user-object-id p) (:user-id p) (:username p))))

(defn id-token-claims
  [{:keys [iss client-id p nonce auth-time now-s]}]
  (cond-> {:iss iss :sub (subject p) :aud client-id
           :iat now-s :exp (+ now-s id-token-ttl-seconds)
           :preferred_username (str (:username p))}
    (some? auth-time)       (assoc :auth_time auth-time)
    (not (str/blank? nonce)) (assoc :nonce (str nonce))))

(defn id-token
  [store opts]
  (sign (signing-key! store) (id-token-claims opts)))

(defn client-credentials
  [req params]
  (if-let [b (some->> (get-in req [:headers "authorization"])
                      (re-find #"(?i)^Basic\s+(\S+)$") second)]
    (let [raw (try (String. (.decode (Base64/getDecoder) ^String b) StandardCharsets/UTF_8)
                   (catch IllegalArgumentException _ ""))
          i   (str/index-of raw ":")
          dec #(java.net.URLDecoder/decode ^String % "UTF-8")]
      (when i [(dec (subs raw 0 i)) (dec (subs raw (inc i)))]))
    [(get params "client_id") (get params "client_secret")]))

(defn wire-params
  [{:keys [form-params body-params]}]
  (merge (into {} (map (fn [[k v]] [(name k) v])) (or body-params {}))
         (into {} (map (fn [[k v]] [(name k) v])) (or form-params {}))))

(defn json-response
  [status body]
  {:status  status
   :headers {"Content-Type" "application/json;charset=UTF-8"
             "Cache-Control" "no-store" "Pragma" "no-cache"}
   :body    (json/write-str body)})

(defn ensure-signing-key!
  [store]
  (signing-key! store)
  nil)
