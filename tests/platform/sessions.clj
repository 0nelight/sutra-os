(ns ^:unit platform.sessions
  (:require [sutra.access.principals :as pr]
            [clojure.test :refer [deftest is testing]]
            [support.mem :as mem]
            [sutra.access.http.auth :as auth]
            [sutra.access.sessions :as sessions]
            [sutra.store.control :as ctl]))

(defn- at [secs] (java.util.Date. (* 1000 secs)))

(deftest login-mints-a-resolvable-session
  (let [store (mem/store)]
    (pr/register-principal! store
                            {:username "carol" :password "pw-carol" :branch "main"
                             :user-id "u-carol"})
    (let [token (auth/login! store "carol" "pw-carol")]
      (is (string? token) "a successful login returns a token")
      (is (= {:username "carol" :user-id "u-carol" :branch "main" :scope nil}
             (auth/session-for store token))))))

(deftest logout-revokes-the-session
  (let [store (mem/store)]
    (pr/register-principal! store
                            {:username "erin" :password "pw-erin" :branch "main" :user-id "u-erin"})
    (let [token (auth/login! store "erin" "pw-erin")]
      (is (some? (auth/session-for store token)))
      (auth/logout! store token)
      (is (nil? (auth/session-for store token)) "a revoked token is dead")
      (testing "ending it again, or ending nothing, is a harmless no-op"
        (is (nil? (sessions/end! store token)))
        (is (nil? (sessions/end! store "no-such-token")))
        (is (nil? (sessions/end! store nil)))))))

(deftest an-unknown-token-is-nil-not-an-error
  (let [store (mem/store)]
    (is (nil? (sessions/fetch store "never-minted")))
    (is (nil? (sessions/fetch store nil)))))

(deftest a-clone-token-is-a-session-scoped-to-its-repositories
  (let [store  (mem/store)
        p   {:username "carol" :user-object-id "u-3" :branch "main"}
        one (sessions/fetch store (sessions/mint-clone-token! store p ["ri.r.1"]))
        two (sessions/fetch store (sessions/mint-clone-token! store p ["ri.r.1" "ri.r.2"]))]
    (is (= "ri.r.1" (:scope one)))
    (is (false? (sessions/scoped-to? one "ri.r.2"))
        "a token for repo A must not even be able to learn whether repo B exists")
    (is (true? (sessions/scoped-to? two "ri.r.2")))
    (is (false? (sessions/scoped-to? two "ri.r.3")))))

(deftest expiry-is-on-the-store-clock
  (let [t  (atom (at 0))
        store (mem/store #(deref t))
        tok (sessions/mint! store {:username "dave" :user-object-id "u-4" :branch "main"}
                            {:ttl 60})]
    (is (some? (sessions/fetch store tok)) "a fresh session is live")
    (reset! t (at 61))
    (is (nil? (sessions/fetch store tok)) "past its ttl it reads as absent")
    (is (= 1 (sessions/sweep-expired! store)) "and the boot sweep removes it")
    (is (zero? (sessions/sweep-expired! store)))))

(deftest a-login-lives-its-kinds-lifetime-whatever-it-was-minted-with
  (let [t     (atom (at 0))
        store (mem/store #(deref t))
        p     {:username "gina" :user-object-id "u-8" :branch "main"}
        login (sessions/mint! store p {:ttl (* 60 60 24 30)})
        clone (sessions/mint-clone-token! store p ["ri.r.1"])]
    (reset! t (at (dec (sessions/ttl-of :login))))
    (is (nil? (sessions/fetch store login)) "unused for longer than its idle limit")
    (ctl/plan! (ctl/served store :sessions)
               [(fn [s at'] (ctl/updating s :sessions {:token (sessions/token-key login)}
                                          {:last-seen-at @t} at'))])
    (is (some? (sessions/fetch store login)) "in use, it lives to its cap")
    (reset! t (at (inc (sessions/ttl-of :login))))
    (is (nil? (sessions/fetch store login)) "a login lives 16 h, as a session token")
    (is (some? (sessions/fetch store clone)) "a clone token lives its week")
    (is (= 1 (sessions/sweep-expired! store)))))

(deftest a-principal-upsert-updates-rather-than-duplicating
  (let [store (mem/store)]
    (pr/register-principal! store {:username "erin" :password "pw1" :branch "main" :user-id "u-5"})
    (pr/register-principal! store {:username "erin" :password "pw2" :branch "other" :user-id "u-5"})
    (is (= "other" (:branch (pr/fetch store "erin"))) "the second write updated the record")
    (is (= 1 (count (filter #(= "erin" (:username %)) (pr/all-rows store)))))))

(deftest a-federated-principal-has-no-password-and-resolves-by-subject
  (let [store (mem/store)]
    (pr/register-principal! store {:username "frank" :password nil :branch "main" :user-id "u-6"
                                :auth-source :kerberos :external-subject "frank@REALM"})
    (let [row (pr/fetch-by-external store "kerberos" "frank@REALM")]
      (is (= "frank" (:username row)))
      (is (nil? (:pw-hash row)) "a federated principal must never carry a local password"))
    (is (false? (:has-password (first (pr/all-rows store)))))
    (testing "one external subject signs in as one principal"
      (is (thrown? clojure.lang.ExceptionInfo
                   (pr/register-principal! store {:username "frankie" :user-id "u-7"
                                               :auth-source :kerberos
                                               :external-subject "frank@REALM"}))))))

(deftest a-session-is-ended-from-another-client
  (let [store (mem/store)]
    (pr/register-principal! store
                            {:username "finn" :password "pw" :branch "main" :user-id "u-finn"})
    (let [laptop (auth/login! store "finn" "pw" {:user-agent "Firefox/1 Linux" :ip "10.0.0.1"})
          phone  (auth/login! store "finn" "pw" {:user-agent "Safari/1 iPhone" :ip "10.0.0.2"})
          office (auth/login! store "finn" "pw")
          listed (sessions/of-user store "finn" laptop)]
      (testing "the list names every client without its token, marking this one"
        (is (= 3 (count listed)))
        (is (not-any? #(contains? (set (vals %)) laptop) listed))
        (is (= #{"10.0.0.1" "10.0.0.2"} (set (keep :ip listed))))
        (is (= [(sessions/session-id laptop)] (map :id (filter :current? listed)))))
      (testing "ending one by its id ends that one only"
        (is (= 1 (sessions/revoke! store "finn" {:id (sessions/session-id phone)})))
        (is (nil? (auth/session-for store phone)))
        (is (some? (auth/session-for store office))))
      (testing "another principal's id ends nothing"
        (is (zero? (sessions/revoke! store "someone" {:id (sessions/session-id office)}))))
      (testing "all others: only this client stays"
        (let [mine (sessions/session-id laptop)]
          (is (= 1 (sessions/revoke! store "finn" {:except mine}))))
        (is (some? (auth/session-for store laptop)))
        (is (nil? (auth/session-for store office)))))))
