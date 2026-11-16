(ns ^:unit platform.sso-auth-test
  (:require [sutra.serve.errors :as errors]
            [sutra.catalog.grants :as grants]
            [sutra.access.principals :as pr]
            [clojure.test :refer [deftest is testing]]
            [support.mem :as mem]
            [sutra.serve.server :as server]
            [sutra.access.http.auth :as auth]))

(deftest password-provider-still-works
  (let [store (mem/store)]
  (testing "a local principal authenticates via the password path (seam regression)"
    (pr/register-principal! store {:username "alice" :password "secret1" :user-id "u-alice"})
    (is (some? (auth/login! store "alice" "secret1")) "correct password mints a session")
    (is (nil?  (auth/login! store "alice" "wrong"))   "wrong password is rejected")
    (is (nil?  (auth/login! store "nobody" "secret1")) "unknown user is rejected"))))

(deftest federated-principal-cannot-password-login
  (let [store (mem/store)]
  (testing "an SSO principal (pw_hash NULL) is never authenticable by password"
    (pr/register-principal! store {:username "bob@EXAMPLE.COM" :user-id "u-bob"
                             :auth-source :spnego
                             :external-subject "bob@EXAMPLE.COM"})
    (is (nil? (auth/login! store "bob@EXAMPLE.COM" "")) "blank password rejected")
    (is (nil? (auth/login! store "bob@EXAMPLE.COM" "anything")) "any password rejected"))
  (testing "the seam resolves the federated principal to a session directly"
    (is (some? (auth/establish-session! store {:provider :spnego :subject "bob@EXAMPLE.COM"}))
        "a proven Kerberos subject establishes a session")
    (is (nil? (auth/establish-session! store {:provider :spnego :subject "ghost@EXAMPLE.COM"}))
        "an unprovisioned subject does NOT (pre-provisioning — no auto-create)"))))

(deftest kerberos-subject-resolves-case-insensitively
  (let [store (mem/store)]
  (testing "a ticket's UPPERCASE realm resolves the row a directory sync wrote lower-cased"
    (pr/register-principal! store {:username "milo" :user-id "u-milo"
                             :auth-source :spnego
                             :external-subject "milo@example.com"})
    (is (some? (auth/establish-session! store {:provider :spnego :subject "milo@EXAMPLE.COM"})))
    (is (some? (auth/establish-session! store {:provider :spnego :subject "milo@example.com"}))))
  (testing "an OIDC subject is opaque — never folded"
    (pr/register-principal! store {:username "dina" :user-id "u-dina"
                             :auth-source :oidc :external-subject "abc"})
    (is (some? (pr/fetch-by-external store "oidc" "abc")))
    (is (nil?  (pr/fetch-by-external store "oidc" "ABC"))))
  (testing "two spnego rows differing only in case are ambiguous — the fold resolves neither"
    (pr/register-principal! store {:username "eve-lower" :user-id "u-eve-l"
                             :auth-source :spnego :external-subject "eve@example.com"})
    (pr/register-principal! store {:username "eve-upper" :user-id "u-eve-u"
                             :auth-source :spnego :external-subject "EVE@example.com"})
    (is (= "eve-lower" (:username (pr/fetch-by-external store "spnego" "eve@example.com")))
        "an exact match still wins")
    (is (nil? (pr/fetch-by-external store "spnego" "Eve@Example.com"))
        "a non-exact subject picks NEITHER identity"))))

(defn- register [store req]
  ((errors/wrap-exceptions (get-in (server/handler-bindings) [:register :post])) (assoc req :sutra/store store)))

(deftest register-handler-is-admin-only
  (let [store (mem/store)]
  (testing "the public /api/register account-takeover hole is closed at the bound handler"
    (pr/register-principal! store {:username grants/admin-username :password "admin-pw"
                             :user-id "u-admin"})
    (pr/register-principal! store {:username "victim@EXAMPLE.COM" :user-id "u-victim"
                             :auth-source :spnego :external-subject "victim@EXAMPLE.COM"})
    (testing "no session (principal nil) ⇒ 403, no write"
      (is (= 403 (:status (register store
                           {:body-params {:username "mallory" :password "x" :user-id "u-m"}}))))
      (is (nil? (auth/login! store "mallory" "x")) "the principal was NOT created"))
    (testing "a non-admin session cannot overwrite the admin credential"
      (let [resp (register store
                  {:principal {:username "victim@EXAMPLE.COM"}
                   :body-params {:username grants/admin-username :password "hacked"
                                 :user-id "u-admin"}})]
        (is (= 403 (:status resp)))
        (is (some? (auth/login! store grants/admin-username "admin-pw")) "admin password intact")
        (is (nil?  (auth/login! store grants/admin-username "hacked"))   "overwrite did NOT land")))
    (testing "the admin principal may register (the legitimate provisioning path)"
      (is (= 200 (:status (register store
                           {:principal {:username grants/admin-username}
                            :body-params {:username "carol" :password "carols long passphrase"
                                          :user-id "u-carol"}}))))
      (is (some? (auth/login! store "carol" "carols long passphrase"))
          "admin-issued registration works")))))