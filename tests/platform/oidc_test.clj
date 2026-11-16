(ns ^:unit platform.oidc-test
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.access.http.host :as host]
            [sutra.access.http.oidc :as http-oidc]
            [sutra.access.oauth :as oauth]
            [sutra.access.oidc :as oidc]
            [sutra.access.principals :as pr]
            [sutra.access.sessions :as sessions]
            [sutra.access.throttle :as throttle]
            [sutra.apps.http.custom :as apps]
            [sutra.kit.system :as sys]
            [support.mem :as mem])
  (:import (java.math BigInteger)
           (java.security KeyFactory)
           (java.security.spec RSAPublicKeySpec)
           (java.util Base64)))

(defn- store []
  (assoc (mem/store) ::sys/system (sys/configure (sys/assemble [throttle/module]) {})))

(def ^:private priv (oidc/private-key (oidc/generate-private-key)))

(defn- from-jwk
  [{:keys [n e]}]
  (let [big #(BigInteger. 1 (.decode (Base64/getUrlDecoder) ^String %))]
    (.generatePublic (KeyFactory/getInstance "RSA") (RSAPublicKeySpec. (big n) (big e)))))

(deftest a-jwt-verifies-under-the-key-its-jwk-publishes
  (let [jwt (oidc/sign priv {:sub "u1" :aud "c1"})
        pub (from-jwk (oidc/jwk (oidc/public-key priv)))]
    (is (= {:sub "u1" :aud "c1"} (oidc/verify pub jwt)))
    (testing "the header names the key by its thumbprint"
      (let [h (json/read-str (String. (.decode (Base64/getUrlDecoder)
                                               ^String (first (str/split jwt #"\.")))
                                      "UTF-8")
                             :key-fn keyword)]
        (is (= {:alg "RS256" :typ "JWT" :kid (oidc/kid (oidc/public-key priv))} h))))))

(deftest a-tampered-or-unsigned-jwt-is-refused
  (let [pub (oidc/public-key priv)
        [h _ s] (str/split (oidc/sign priv {:sub "u1"}) #"\.")
        other (oidc/b64url (.getBytes "{\"sub\":\"admin\"}" "UTF-8"))
        none  (oidc/b64url (.getBytes "{\"alg\":\"none\"}" "UTF-8"))]
    (is (nil? (oidc/verify pub (str h "." other "." s))) "a changed payload")
    (is (nil? (oidc/verify pub (str none "." other "."))) "alg none")
    (is (nil? (oidc/verify pub "not.a.jwt")))
    (is (nil? (oidc/verify (oidc/public-key (oidc/private-key (oidc/generate-private-key)))
                           (oidc/sign priv {:sub "u1"})))
        "another key")))

(deftest the-discovery-document-carries-what-discovery-requires
  (let [iss "https://sutra.example/api/oauth"
        d   (oidc/discovery iss)]
    (is (every? #(contains? d %) [:issuer :authorization_endpoint :token_endpoint :jwks_uri
                                  :response_types_supported :subject_types_supported
                                  :id_token_signing_alg_values_supported]))
    (is (= iss (:issuer d)))
    (is (every? #(str/starts-with? % iss)
                (keep d [:authorization_endpoint :token_endpoint :jwks_uri :userinfo_endpoint
                         :introspection_endpoint :revocation_endpoint])))
    (is (= ["S256"] (:code_challenge_methods_supported d)) "PKCE S256 only")))

(deftest the-issuer-is-configured-never-the-host-of-a-deployed-instance
  (let [req {:scheme :https :headers {"host" "evil"}}]
    (is (= "https://sutra.example/api/oauth"
           (oidc/issuer req {:public-url "https://sutra.example/" :domain "other"
                             :deployed? true})))
    (is (= "https://o.example.com/api/oauth"
           (oidc/issuer req {:domain "o.example.com" :deployed? true})))
    (is (thrown? clojure.lang.ExceptionInfo (oidc/issuer req {:deployed? true}))
        "a deployed instance without an address refuses, never answers the Host")
    (is (= "https://evil/api/oauth" (oidc/issuer req {:deployed? false}))
        "only a dev process falls back to the request")))

(deftest a-client-authenticates-by-basic-or-by-the-form
  (let [basic (str "Basic " (.encodeToString (Base64/getEncoder)
                                             (.getBytes "cid-1:s%3Aec%20ret" "UTF-8")))]
    (is (= ["cid-1" "s:ec ret"]
           (oidc/client-credentials {:headers {"authorization" basic}} {})))
    (is (= ["cid-2" "x"]
           (oidc/client-credentials {:headers {}} {"client_id" "cid-2" "client_secret" "x"})))
    (is (nil? (oidc/client-credentials {:headers {"authorization" "Basic !!"}} {})))))

(def ^:private app-rid "ri.sutra.main.custom-application.oidc")

(def ^:private schema
  {:custom-applications
   {app-rid {:custom-application-name :portal
             :status :published
             :permissions {:users ["alice"]}
             :client {:grant-types [:authorization-code]}
             :scope {:operations [:actions]}}}})

(def ^:private redirect "https://portal.example/cb")

(def ^:private verifier (apply str (repeat 50 "v")))

(defn- req [store m]
  (merge {:sutra/store store :sutra/schema schema :sutra/branch "main"
          :sutra/host host/capabilities
          :scheme :https :headers {"host" "sutra.lan"}}
         m))

(defn- body [r] (json/read-str (:body r) :key-fn keyword))

(defn- client! [store]
  (oauth/register-client! store {:app app-rid :app-name :portal
                                 :grant-types [:authorization-code]
                                 :redirect-uris [redirect] :created-by "admin"}))

(defn- code! [store c scope]
  (pr/register-principal! store {:username "alice" :password "Correct-horse-battery-1"
                                 :user-id "u-alice"})
  (let [login (sessions/open! store (pr/fetch store "alice") {:provider :password} {})]
    (-> (apps/oauth-authorize
         (req store {:principal {:username "alice" :user-id "u-alice" :role "viewer"}
                     :session-token login
                     :body-params {:client-id (:client-id c) :redirect-uri redirect
                                   :code_challenge (oauth/challenge-of verifier)
                                   :code_challenge_method "S256"
                                   :scope scope :nonce "n-0S6"}}))
        :body :code)))

(defn- token! [store c code]
  (apps/oauth-token
   (req store {:headers {"host" "sutra.lan"
                         "content-type" "application/x-www-form-urlencoded"}
               :form-params {"grant_type" "authorization_code" "code" code
                             "redirect_uri" redirect "code_verifier" verifier
                             "client_id" (:client-id c)
                             "client_secret" (:client-secret c)}})))

(defn- basic [c]
  (str "Basic " (.encodeToString (Base64/getEncoder)
                                 (.getBytes (str (:client-id c) ":" (:client-secret c))
                                            "UTF-8"))))

(defn- introspect [store c token]
  (body (http-oidc/introspect (req store {:headers {"authorization" (basic c)}
                                          :form-params {"token" token}}))))

(deftest openid-adds-a-signed-id-token-to-the-code-flow
  (let [store (store)
        c     (client! store)
        r     (token! store c (code! store c "openid actions"))
        b     (body r)
        jwks  (body (http-oidc/jwks (req store {})))
        claims (oidc/verify (from-jwk (first (:keys jwks))) (:id_token b))]
    (is (= 200 (:status r)))
    (is (str/starts-with? (get-in r [:headers "Content-Type"]) "application/json")
        "a form-encoded request is a standard client's: it is answered in JSON")
    (is (= "actions" (:scope b)) "openid widens no API namespace")
    (is (some? claims) "the id_token verifies under the published JWK set")
    (is (= {:iss "https://sutra.lan/api/oauth" :sub "u-alice" :aud (:client-id c)
            :nonce "n-0S6" :preferred_username "alice"}
           (select-keys claims [:iss :sub :aud :nonce :preferred_username])))
    (is (int? (:auth_time claims)))
    (is (< (:iat claims) (:exp claims)))))

(deftest without-openid-there-is-no-id-token
  (let [store (store)
        c     (client! store)
        b     (body (token! store c (code! store c "actions")))]
    (is (:access_token b))
    (is (not (contains? b :id_token)))))

(deftest introspection-and-revocation-follow-the-rfcs
  (let [store (store)
        c     (client! store)
        other (client! store)
        token (:access_token (body (token! store c (code! store c "actions"))))]
    (testing "the holder sees its live token"
      (let [i (introspect store c token)]
        (is (true? (:active i)))
        (is (= (:client-id c) (:client_id i)))
        (is (= "alice" (:username i)))))
    (testing "another client learns nothing about it"
      (is (= {:active false} (introspect store other token))))
    (testing "a wrong secret is refused"
      (is (= 401 (:status (http-oidc/introspect
                           (req store {:form-params {"token" token "client_id" (:client-id c)
                                                     "client_secret" "wrong"}}))))))
    (testing "another client's revocation is answered 200 and changes nothing"
      (is (= 200 (:status (http-oidc/revoke (req store {:headers {"authorization" (basic other)}
                                                         :form-params {"token" token}})))))
      (is (true? (:active (introspect store c token)))))
    (testing "the holder revokes it; an unknown token is revoked just the same"
      (is (= 200 (:status (http-oidc/revoke (req store {:headers {"authorization" (basic c)}
                                                         :form-params {"token" token}})))))
      (is (= {:active false} (introspect store c token)))
      (is (= 200 (:status (http-oidc/revoke (req store {:headers {"authorization" (basic c)}
                                                         :form-params {"token" "nope"}}))))))))

(deftest userinfo-names-the-bearer
  (is (= {:sub "u-alice" :preferred_username "alice"}
         (body (http-oidc/userinfo {:principal {:username "alice" :user-id "u-alice"}}))))
  (is (= 401 (:status (http-oidc/userinfo {})))))
