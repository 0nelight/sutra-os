(ns conformance.kits.identity
  (:require [clojure.string :as str]
            [conformance.http :as http]
            [conformance.json :as json]
            [conformance.report :as r])
  (:import (java.math BigInteger)
           (java.nio.charset StandardCharsets)
           (java.security KeyFactory Signature)
           (java.security.spec RSAPublicKeySpec)
           (java.util Base64)))

(def well-known "/.well-known/openid-configuration")

(def required-metadata
  ["issuer" "authorization_endpoint" "token_endpoint" "jwks_uri" "response_types_supported"
   "subject_types_supported" "id_token_signing_alg_values_supported"])

(defn- b64url [^String s] (.decode (Base64/getUrlDecoder) s))

(defn- rsa-key [{:strs [n e]}]
  (.generatePublic (KeyFactory/getInstance "RSA")
                   (RSAPublicKeySpec. (BigInteger. 1 ^bytes (b64url n))
                                      (BigInteger. 1 ^bytes (b64url e)))))

(defn jwks-problems
  [jwks]
  (let [ks (get jwks "keys")]
    (cond
      (not (sequential? ks)) "no keys array"
      (empty? ks) "the key set is empty"
      (some #(str/blank? (get % "kid")) ks) "a key without kid"
      (not= (count ks) (count (set (map #(get % "kid") ks)))) "two keys share a kid"
      (some #(and (= "RSA" (get % "kty")) (not (and (get % "n") (get % "e")))) ks)
      "an RSA key without n or e"
      (not-any? #(and (= "RSA" (get % "kty")) (#{nil "RS256"} (get % "alg"))
                      (#{nil "sig"} (get % "use")))
                ks)
      "no RSA signing key for RS256")))

(defn id-token-problems
  [jwt jwks issuer client-id now-s]
  (let [[h p s] (str/split (str jwt) #"\.")
        header  (some-> h b64url (String. StandardCharsets/UTF_8) json/parse-or-nil)
        claims  (some-> p b64url (String. StandardCharsets/UTF_8) json/parse-or-nil)
        k       (some #(when (= (get % "kid") (get header "kid")) %) (get jwks "keys"))
        aud     (get claims "aud")]
    (cond
      (not (and header claims s)) "not a compact JWS"
      (not= "RS256" (get header "alg")) (str "alg is " (pr-str (get header "alg")))
      (nil? k) (str "no key of the JWKS has kid " (pr-str (get header "kid")))
      (not (let [sig (doto (Signature/getInstance "SHA256withRSA") (.initVerify (rsa-key k)))]
             (.update sig (.getBytes (str h "." p) StandardCharsets/US_ASCII))
             (.verify sig (b64url s))))
      "the signature does not verify"
      (not= issuer (get claims "iss")) (str "iss is " (pr-str (get claims "iss")))
      (str/blank? (str (get claims "sub"))) "no sub"
      (and client-id (not (if (sequential? aud) (some #{client-id} aud) (= client-id aud))))
      (str "aud " (pr-str aud) " lacks " client-id)
      (not (number? (get claims "iat"))) "no iat"
      (not (and (number? (get claims "exp")) (< now-s (get claims "exp")))) "expired or no exp")))

(defn discovery-problems
  [doc url]
  (let [missing (remove #(some? (get doc %)) required-metadata)]
    (cond
      (not (map? doc)) "not a JSON object"
      (seq missing) (str "missing " (str/join ", " missing))
      (not= (get doc "issuer") (subs url 0 (- (count url) (count well-known))))
      (str "issuer " (pr-str (get doc "issuer")) " is not the URL it was fetched under")
      (not (some #{"RS256"} (get doc "id_token_signing_alg_values_supported")))
      "RS256 is not among id_token_signing_alg_values_supported")))

(defn checks
  [{:keys [base issuer client-id client-secret id-token allow-http]}]
  (let [base   (str/replace base #"/+$" "")
        iss    (or issuer (str base "/api/oauth"))
        durl   (str iss well-known)
        disc   (delay (http/get-url durl {"Accept" "application/json"}))
        doc    (delay (json/parse-or-nil (:body @disc)))
        ep     #(get @doc %)
        jwks   (delay (json/parse-or-nil (:body (http/get-url (ep "jwks_uri")))))
        creds? (and client-id client-secret)
        auth   #(hash-map "Authorization" (http/basic-auth client-id client-secret))
        need   (fn [k f] #(if (ep k) (f) (r/skip (str k " is not published"))))]
    [(r/check "discovery" "the discovery document conforms to OIDC Discovery 1.0 §3, §4"
              #(if (not= 200 (:status @disc))
                 (str "status " (:status @disc) " at " durl)
                 (discovery-problems @doc durl)))
     (r/check "issuer-https" "the issuer is an https URL without query or fragment"
              #(let [u (java.net.URI. (str (ep "issuer")))]
                 (cond (or (.getQuery u) (.getFragment u)) "query or fragment in the issuer"
                       (= "https" (.getScheme u)) nil
                       allow-http (r/skip "http issuer allowed by --allow-http")
                       :else (str "scheme " (.getScheme u)))))
     (r/check "pkce-s256" "PKCE S256 is offered (RFC 7636, RFC 8414)"
              #(when-not (some #{"S256"} (ep "code_challenge_methods_supported"))
                 "S256 is not among code_challenge_methods_supported"))
     (r/check "jwks" "the JWK Set holds an RSA signing key with kid (RFC 7517)"
              #(jwks-problems @jwks))
     (r/check "token-error" "a bad grant is refused with an RFC 6749 §5.2 error"
              #(let [resp (http/post-form (ep "token_endpoint")
                                          {"grant_type" "authorization_code"
                                           "code" "conformance-invalid"
                                           "redirect_uri" "https://invalid.example/cb"
                                           "client_id" "conformance-invalid"
                                           "code_verifier" (apply str (repeat 43 "a"))})
                     body (json/parse-or-nil (:body resp))]
                 (cond (not (#{400 401} (:status resp))) (str "status " (:status resp))
                       (not (string? (get body "error"))) "the body names no error")))
     (r/check "userinfo-bearer" "userinfo refuses a call without bearer (RFC 6750 §3)"
              (need "userinfo_endpoint"
                    #(let [resp (http/get-url (ep "userinfo_endpoint"))]
                       (cond (not= 401 (:status resp)) (str "status " (:status resp))
                             (nil? (get-in resp [:headers "www-authenticate"]))
                             "no WWW-Authenticate header"))))
     (r/check "introspection-auth" "introspection requires client authentication (RFC 7662 §2.1)"
              (need "introspection_endpoint"
                    #(let [resp (http/post-form (ep "introspection_endpoint")
                                                {"token" "conformance-invalid"})]
                       (when-not (<= 400 (:status resp) 499) (str "status " (:status resp))))))
     (r/check "introspection-inactive" "an unknown token introspects as inactive (RFC 7662 §2.2)"
              (if-not creds?
                (constantly (r/skip "no --client-id/--client-secret"))
                (need "introspection_endpoint"
                      #(let [resp (http/post-form (ep "introspection_endpoint")
                                                  {"token" "conformance-invalid"} (auth))]
                         (cond (not= 200 (:status resp)) (str "status " (:status resp))
                               (not (false? (get (json/parse-or-nil (:body resp)) "active")))
                               (str "body " (:body resp)))))))
     (r/check "revocation-unknown" "revoking an unknown token answers 200 (RFC 7009 §2.2)"
              (if-not creds?
                (constantly (r/skip "no --client-id/--client-secret"))
                (need "revocation_endpoint"
                      #(let [resp (http/post-form (ep "revocation_endpoint")
                                                  {"token" "conformance-invalid"} (auth))]
                         (when (not= 200 (:status resp)) (str "status " (:status resp)))))))
     (r/check "id-token" "a handed-in id_token verifies against the JWKS (OIDC Core §3.1.3.7)"
              #(if-not id-token
                 (r/skip "no --id-token (a login flow needs a user)")
                 (id-token-problems id-token @jwks (ep "issuer") client-id
                                    (quot (System/currentTimeMillis) 1000))))]))
