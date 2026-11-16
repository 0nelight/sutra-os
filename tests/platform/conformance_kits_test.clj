(ns ^:unit platform.conformance-kits-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [conformance.http :as http]
            [conformance.json :as json]
            [conformance.kits.events :as events]
            [conformance.kits.http-api :as http-api]
            [conformance.kits.identity :as identity]
            [conformance.kits.transform-runtime :as runtime]
            [conformance.main :as main]
            [conformance.report :as r]
            [conformance.schema :as schema])
  (:import (com.sun.net.httpserver HttpExchange HttpHandler HttpServer)
           (java.net InetSocketAddress ServerSocket)
           (java.nio.charset StandardCharsets)
           (java.security KeyPairGenerator Signature)
           (java.util Base64)))

(defn- status-of [checks id] (:status (first (filter #(= id (:id %)) checks))))

(def ^:private doc
  {"$defs" {"row" {"type" "object" "required" ["id"] "additionalProperties" false
                   "properties" {"id" {"type" "string" "minLength" 1 "pattern" "^[a-z]+$"}
                                 "n"  {"type" ["integer" "null"] "minimum" 0}
                                 "t"  {"type" "string" "format" "date-time"}
                                 "k"  {"enum" ["a" "b"]}}}
            "rows" {"type" "array" "items" {"$ref" "#/$defs/row"} "minItems" 1}
            "either" {"anyOf" [{"type" "string"} {"type" "integer"}]}}})

(defn- are-errs [v needle]
  (is (some #(str/includes? % needle) (schema/errors-at doc "#/$defs/rows" v))
      (str (pr-str v) " → " needle)))

(deftest schema-validates-the-published-subset
  (is (= [] (schema/errors-at doc "#/$defs/rows"
                              [{"id" "ab" "n" 2 "t" "2026-11-11T10:00:00Z"}])))
  (testing "each keyword refuses"
    (are-errs [{"n" 1}] "missing required")
    (are-errs [{"id" "AB"}] "does not match")
    (are-errs [{"id" "a" "x" 1}] "not allowed")
    (are-errs [{"id" "a" "n" -1}] "below")
    (are-errs [{"id" "a" "n" 1.5}] "expected")
    (are-errs [{"id" "a" "t" "yesterday"}] "date-time")
    (are-errs [{"id" "a" "k" "c"}] "is not one of")
    (are-errs [] "fewer than"))
  (is (= [] (schema/errors-at doc "#/$defs/either" 3)))
  (is (seq (schema/errors-at doc "#/$defs/either" true)))
  (is (seq (schema/errors-at doc "#/$defs/nowhere" 1)) "an unresolvable $ref fails"))

(deftest checks-report-pass-fail-skip
  (let [cs [(r/check "a" "passes" (constantly nil))
            (r/check "b" "fails" (constantly "why"))
            (r/check "c" "skips" #(r/skip "no creds"))
            (r/check "d" "throws" #(throw (ex-info "boom" {})))]]
    (is (= [:pass :fail :skip :fail] (map :status cs)))
    (is (r/failed? cs))
    (is (str/includes? (r/junit-xml "k" cs) "<failure message=\"fails\">why</failure>"))
    (is (str/includes? (r/junit-xml "k" cs) "<skipped message=\"no creds\"/>"))
    (is (= {"pass" 1 "fail" 2 "skip" 1} (get (r/summary "k" "t" cs) "counts")))))

(deftest arguments-parse
  (is (= {:kit "transform-runtime" :positional [] :command ["python3" "-m" "x"]
          :opts {:env {"A" "1=2"} :timeout "5"}}
         (main/parse-args ["transform-runtime" "--env" "A=1=2" "--timeout" "5"
                           "--" "python3" "-m" "x"])))
  (is (= {:kit "identity" :positional ["http://h"] :command []
          :opts {:allow-http true :client-id "c"}}
         (main/parse-args ["identity" "--allow-http" "http://h" "--client-id" "c"]))))

(defn- free-port [] (with-open [s (ServerSocket. 0)] (.getLocalPort s)))

(defn- serve
  [f]
  (let [s (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]
    (.createContext s "/"
                    (reify HttpHandler
                      (handle [_ ex]
                        (let [^HttpExchange ex ex
                              req {:method  (.getRequestMethod ex)
                                   :path    (.getPath (.getRequestURI ex))
                                   :headers (into {} (for [[k vs] (.getRequestHeaders ex)]
                                                       [(str/lower-case k) (first vs)]))
                                   :body    (slurp (.getRequestBody ex))}
                              {:keys [status headers body]} (f req)
                              bs (.getBytes (str body) StandardCharsets/UTF_8)]
                          (doseq [[k v] headers] (.add (.getResponseHeaders ex) k v))
                          (.sendResponseHeaders ex status (if (seq bs) (count bs) -1))
                          (with-open [o (.getResponseBody ex)] (.write o bs))))))
    (.start s)
    s))

(defmacro with-server [[base f] & body]
  `(let [s# (serve ~f)
         ~base (str "http://127.0.0.1:" (.getPort (.getAddress s#)))]
     (try ~@body (finally (.stop s# 0)))))

(defn- json-answer [status x & [headers]]
  {:status status :headers (merge {"Content-Type" "application/json"} headers)
   :body (json/write x)})

(def ^:private published
  {"openapi" "3.1.0" "info" {"version" "1.0.0"}
   "paths" {"/api/openapi.json" {"get" {"security" []}}
            "/api/hello" {"get" {"security" []}}
            "/api/secret" {"get" {"security" [{"bearer" []}]}}
            "/api/thing/{id}" {"get" {"security" [{"bearer" []}]}}}})

(defn- api-server [{:keys [header? secret-status drop-path]}]
  (fn [{:keys [path]}]
    (let [v (when header? {"Sutra-API-Version" "1.2.0"})]
      (case path
        "/api/openapi.json" (json-answer 200 (update published "paths" dissoc drop-path) v)
        "/api/hello"        (json-answer 200 {"hello" "world"} v)
        "/api/secret"       (json-answer secret-status {"error" "no"} v)
        {:status 404 :body ""}))))

(deftest http-api-kit
  (with-server [base (api-server {:header? true :secret-status 401})]
    (let [cs (http-api/checks {:base base :published published})]
      (is (every? #(= :pass (:status %)) cs) (pr-str (remove #(= :pass (:status %)) cs)))))
  (with-server [base (api-server {:header? false :secret-status 500 :drop-path "/api/hello"})]
    (let [cs (http-api/checks {:base base :published published})]
      (is (= :fail (status-of cs "version-header")))
      (is (= :fail (status-of cs "operations-present")))
      (is (= :fail (status-of cs "anonymous-refused")))
      (is (= :fail (status-of cs "no-server-error"))))))

(defn- b64url [^bytes bs] (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) bs))

(defn- unsigned-bytes [^java.math.BigInteger n]
  (let [bs (.toByteArray n)]
    (if (and (< 1 (alength bs)) (zero? (aget bs 0)))
      (java.util.Arrays/copyOfRange bs 1 (alength bs))
      bs)))

(def ^:private keypair
  (delay (.generateKeyPair (doto (KeyPairGenerator/getInstance "RSA") (.initialize 2048)))))

(defn- jwks []
  (let [pub ^java.security.interfaces.RSAPublicKey (.getPublic ^java.security.KeyPair @keypair)]
    {"keys" [{"kty" "RSA" "kid" "k1" "alg" "RS256" "use" "sig"
              "n" (b64url (unsigned-bytes (.getModulus pub)))
              "e" (b64url (unsigned-bytes (.getPublicExponent pub)))}]}))

(defn- sign [claims]
  (let [enc #(b64url (.getBytes (json/write %) StandardCharsets/UTF_8))
        si  (str (enc {"alg" "RS256" "kid" "k1" "typ" "JWT"}) "." (enc claims))
        sig (doto (Signature/getInstance "SHA256withRSA")
              (.initSign (.getPrivate ^java.security.KeyPair @keypair))
              (.update (.getBytes si StandardCharsets/US_ASCII)))]
    (str si "." (b64url (.sign sig)))))

(defn- idp [iss-of]
  (fn [{:keys [method path headers]}]
    (let [iss (iss-of)
          authed? (= (http/basic-auth "c" "s") (get headers "authorization"))]
      (case path
        "/api/oauth/.well-known/openid-configuration"
        (json-answer 200 {"issuer" iss
                          "authorization_endpoint" (str iss "/authorize")
                          "token_endpoint" (str iss "/token")
                          "jwks_uri" (str iss "/jwks")
                          "userinfo_endpoint" (str iss "/userinfo")
                          "introspection_endpoint" (str iss "/introspect")
                          "revocation_endpoint" (str iss "/revoke")
                          "response_types_supported" ["code"]
                          "subject_types_supported" ["public"]
                          "id_token_signing_alg_values_supported" ["RS256"]
                          "code_challenge_methods_supported" ["S256"]})
        "/api/oauth/jwks" (json-answer 200 (jwks))
        "/api/oauth/token" (json-answer 400 {"error" "invalid_grant"})
        "/api/oauth/userinfo" (json-answer 401 {} {"WWW-Authenticate" "Bearer"})
        "/api/oauth/introspect" (if authed?
                                  (json-answer 200 {"active" false})
                                  (json-answer 401 {"error" "invalid_client"}))
        "/api/oauth/revoke" (if (and authed? (= "POST" method))
                              {:status 200 :body ""}
                              (json-answer 401 {"error" "invalid_client"}))
        {:status 404 :body ""}))))

(deftest identity-kit
  (let [base-p (promise)]
    (with-server [base (idp #(str @base-p "/api/oauth"))]
      (deliver base-p base)
      (let [iss   (str base "/api/oauth")
            now   (quot (System/currentTimeMillis) 1000)
            token (sign {"iss" iss "sub" "u1" "aud" "c" "iat" now "exp" (+ now 300)})
            cs    (identity/checks {:base base :client-id "c" :client-secret "s"
                                    :id-token token :allow-http true})]
        (is (= #{:pass} (set (map :status (remove #(= "issuer-https" (:id %)) cs))))
            (pr-str (remove #(= :pass (:status %)) cs)))
        (is (= :skip (status-of cs "issuer-https")))
        (testing "without creds the client checks skip"
          (let [cs (identity/checks {:base base :allow-http true})]
            (is (= :skip (status-of cs "introspection-inactive")))
            (is (= :skip (status-of cs "revocation-unknown")))
            (is (= :skip (status-of cs "id-token")))))
        (testing "an id_token that is tampered with, expired or of another issuer fails"
          (let [[h p s] (str/split token #"\.")
                bad #(identity/id-token-problems % (jwks) iss "c" now)]
            (is (bad (str h "." (b64url (.getBytes "{\"iss\":\"x\"}")) "." s)))
            (is (bad (sign {"iss" iss "sub" "u" "aud" "c" "iat" 1 "exp" 2})))
            (is (bad (sign {"iss" "https://other" "sub" "u" "aud" "c" "iat" now
                            "exp" (+ now 9)})))
            (is (nil? (bad token)))))))
    (testing "an issuer that differs from where the document was fetched fails"
      (with-server [base (idp (constantly "https://elsewhere/api/oauth"))]
        (is (= :fail (status-of (identity/checks {:base base :allow-http true}) "discovery")))))))

(def ^:private protocol
  (delay (json/read-file "boundaries/transform-runtime/protocol.schema.json")))

(deftest transform-runtime-kit-passes-the-reference-runtime
  (let [cs (runtime/checks {:command ["python3" "-m" "sutra_transforms"]
                            :env {"PYTHONPATH" (.getAbsolutePath
                                                (io/file "src/data-plane/python"))}
                            :root @protocol :timeout-s 60})]
    (is (= 8 (count cs)))
    (is (every? #(= :pass (:status %)) cs) (pr-str (remove #(= :pass (:status %)) cs)))))

(deftest transform-runtime-kit-fails-a-runtime-that-ignores-the-protocol
  (let [cs (runtime/checks {:command ["python3" "-c" "print('hello')"] :env {}
                            :root @protocol :timeout-s 30})]
    (is (= :fail (status-of cs "rows-run")))
    (is (= :fail (status-of cs "major-refused")))
    (is (= :fail (status-of cs "error-line")))))

(def ^:private cloudevent (delay (json/read-file "boundaries/events/cloudevent.schema.json")))

(deftest events-kit
  (let [port (free-port)
        send #(http/request {:method :post :url (str "http://127.0.0.1:" port "/hook")
                             :headers % :body "{}"})
        cs   (events/checks {:port port :root @cloudevent :count 2 :timeout-s 20}
                            (fn []
                              (future
                                (send {"ce-specversion" "1.0" "ce-id" "d-1"
                                       "ce-source" "/sources/glpi"
                                       "ce-type" "com.sutra.actions.webhook"
                                       "ce-time" "2026-11-11T10:00:00Z"
                                       "ce-subject" "create%20ticket"
                                       "Idempotency-Key" "d-1"})
                                (send {"ce-specversion" "0.3" "ce-id" "d-2"
                                       "ce-source" "glpi" "ce-type" "other"
                                       "Idempotency-Key" "x"}))))]
    (is (= :pass (status-of cs "received")))
    (is (= :pass (status-of cs "delivery-1")) (pr-str cs))
    (is (= :fail (status-of cs "delivery-2")))
    (is (= "create ticket" (get (events/attributes {"ce-subject" "create%20ticket"}) "subject")))))
