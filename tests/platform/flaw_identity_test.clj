(ns ^:unit platform.flaw-identity-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.access.http.auth :as auth]
            [sutra.store.control :as ctl]
            [sutra.access.oauth :as oauth]
            [sutra.access.principals :as pr]
            [sutra.access.sessions :as sessions]
            [support.mem :as mem])
  (:import (java.util.concurrent CyclicBarrier TimeUnit)))

(defn- uid [] (subs (str (random-uuid)) 0 8))

(defn- user! [store]
  (let [u (str "id-" (uid))]
    (pr/register-principal! store {:username u :password "pw" :branch "main" :user-id (str "u-" u)})
    u))

(defn- client! [store user]
  (oauth/register-client! store {:app (str "ri.app." (uid)) :app-name :flaw-app
                           :grant-types [:authorization-code :client-credentials]
                           :created-by user}))

(defn- token! [store {:keys [client-id client-secret]} user]
  (oauth/issuing! store client-id client-secret
                  (fn [store at c]
                    (let [m ((sessions/minting (str (random-uuid)) (pr/fetch store user)
                                               {:scope "app:flaw" :client-id client-id :ttl 3600
                                                :secret-gen (:secret-gen c)})
                             store at)]
                      (assoc m :result {:status 200 :token (:result m)})))))

(defn- together
  [f]
  (let [b  (CyclicBarrier. 2)
        fs (vec (repeatedly 2 #(future (.await b 5 TimeUnit/SECONDS) (f))))]
    (mapv deref fs)))

(defn- audit [store user]
  (set (map :outcome (sessions/read-login-log store {:username user} 20))))

(deftest double-redeem
  (let [store (mem/store) u (user! store) c (client! store u)
        code (oauth/mint-code! store {:client-id (:client-id c) :user-id "x" :username u})
        rs (together #(first (ctl/plan! store [(oauth/redeeming code (:client-id c))])))]
    (is (= 1 (count (remove nil? rs))) "two redemptions of one code: exactly one gets it")))

(deftest revoked-token-used
  (let [store (mem/store) u (user! store) c (client! store u)
        t (:token (token! store c u))]
    (is (some? (sessions/fetch store t)))
    (is (true? (oauth/revoke-client! store (:client-id c))))
    (is (nil? (sessions/fetch store t)) "a revoked client's token starts no request")
    (is (nil? (token! store c u)) "and the client issues none")))

(deftest rotated-secret-accepted
  (let [store (mem/store) u (user! store) c (client! store u)
        t (:token (token! store c u))
        {new-secret :client-secret} (oauth/rotate-secret! store (:client-id c))]
    (is (nil? (sessions/fetch store t)) "a token of the old secret generation is not served")
    (is (nil? (token! store c u)) "the old secret issues nothing")
    (is (some? (token! store (assoc c :client-secret new-secret) u)) "the new one does")))

(deftest rotate-revoked
  (let [store (mem/store) u (user! store) c (client! store u)]
    (oauth/revoke-client! store (:client-id c))
    (is (nil? (oauth/rotate-secret! store (:client-id c)))
        "a revoked client is not rotated back to life")
    (is (nil? (oauth/fetch-client store (:client-id c))))))

(deftest lost-rotation
  (let [store (mem/store) u (user! store) c (client! store u)
        [a b] (together #(oauth/rotate-secret! store (:client-id c)))
        ok?  (fn [s] (some? (token! store (assoc c :client-secret (:client-secret s)) u)))]
    (testing "two rotations at once go one after the other: the later one's secret counts"
      (is (= 1 (count (filter ok? [a b]))))
      (is (= 2
             (:secret-gen
              (first (oauth/clients-of store
                                       (:app (oauth/fetch-client store (:client-id c)))))))))))

(deftest unaudited-login-and-logout
  (let [store (mem/store) u (user! store)
        t (auth/login! store u "pw")]
    (is (some? t))
    (is (contains? (audit store u) "success") "the session has its login audit")
    (auth/logout! store t)
    (is (nil? (auth/session-for store t)))
    (is (contains? (audit store u) "logout") "and its logout audit")))

(deftest stale-credential-session
  (let [store (mem/store) u (user! store)
        t (auth/login! store u "pw")]
    (is (some? (auth/session-for store t)))
    (pr/register-principal! store
                            {:username u :password "neu" :branch "main" :user-id (str "u-" u)})
    (is (nil? (auth/session-for store t)) "a session does not outlive a credential change")
    (is (contains? (audit store u) "stale") "its end is audited")
    (is (some? (auth/login! store u "neu")) "the new credential logs in")))
