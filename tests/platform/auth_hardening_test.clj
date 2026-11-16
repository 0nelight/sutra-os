(ns ^:unit platform.auth-hardening-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.access.http.auth :as auth]
            [sutra.access.oauth :as oauth]
            [sutra.access.password :as password]
            [sutra.access.password-policy :as policy]
            [sutra.access.principals :as pr]
            [sutra.access.sessions :as sessions]
            [sutra.access.throttle :as throttle]
            [sutra.http.request :as request]
            [sutra.kit.digest :as digest]
            [sutra.store.control :as ctl]
            [support.mem :as mem]))

(def ^:private cheap {:m 64 :t 1 :p 1})

(deftest argon2id-round-trip
  (let [h (password/encode "correct horse battery" cheap)]
    (is (re-matches #"\$argon2id\$v=19\$m=64,t=1,p=1\$[^$]+\$[^$]+" h))
    (is (password/verify? "correct horse battery" h))
    (is (not (password/verify? "correct horse batterY" h)))
    (is (not= h (password/encode "correct horse battery" cheap)) "a fresh salt each time")
    (is (password/needs-rehash? h) "weaker params than the current ones")
    (is (not (password/needs-rehash? (password/encode "x"))))
    (is (not (password/verify? "x" "garbage")) "an unreadable hash verifies nothing")))

(deftest legacy-hash-verifies-and-is-upgraded-at-login
  (let [store  (mem/store)
        legacy (digest/salted "0a1b2c3d" "an old long password")]
    (pr/register-principal! store {:username "lea" :user-id "u-lea" :branch "main"})
    (ctl/plan! (ctl/served store :principals)
               [(fn [s at] (ctl/swapping s :principals {:username "lea"}
                                         #(assoc % :pw-hash legacy) at))])
    (is (password/verify? "an old long password" legacy))
    (is (password/needs-rehash? legacy))
    (is (nil? (auth/login! store "lea" "wrong")))
    (is (= legacy (:pw-hash (ctl/get store :principals {:username "lea"})))
        "a failed login upgrades nothing")
    (let [gen (:cred-gen (ctl/get store :principals {:username "lea"}))]
      (is (some? (auth/login! store "lea" "an old long password")))
      (let [row (ctl/get store :principals {:username "lea"})]
        (is (re-find #"^\$argon2id\$" (:pw-hash row)) "the login stored an Argon2id hash")
        (is (= gen (:cred-gen row)) "an upgrade ends no session"))
      (is (some? (auth/login! store "lea" "an old long password")) "and it still verifies"))))

(deftest set-password-ends-every-session
  (let [store (mem/store)]
    (pr/register-principal! store {:username "sam" :user-id "u-sam" :branch "main"
                                   :password "first passphrase here"})
    (let [t (auth/login! store "sam" "first passphrase here")]
      (is (some? (sessions/fetch store t)))
      (pr/set-password! store "sam" "second passphrase here")
      (is (nil? (sessions/fetch store t)) "the old session is no longer served")
      (is (nil? (auth/login! store "sam" "first passphrase here")))
      (is (some? (auth/login! store "sam" "second passphrase here"))))))

(deftest session-tokens-are-random-and-stored-as-their-hash
  (let [store (mem/store)]
    (pr/register-principal! store {:username "tia" :user-id "u-tia" :branch "main"
                                   :password "a passphrase for tia"})
    (let [t (auth/login! store "tia" "a passphrase for tia")]
      (is (re-matches #"[A-Za-z0-9_-]{43}" t) "256 bits, base64url")
      (is (nil? (ctl/get store :sessions {:token t})) "the raw token is not a key")
      (is (some? (ctl/get store :sessions {:token (sessions/token-key t)})))
      (is (nil? (sessions/fetch store (sessions/token-key t)))
          "the stored key itself opens nothing")
      (is (= "tia" (:username (sessions/fetch store t)))))))

(deftest an-idle-login-session-ends
  (let [store (mem/store)]
    (pr/register-principal! store {:username "ida" :user-id "u-ida" :branch "main"
                                   :password "a passphrase for ida"})
    (let [t   (auth/login! store "ida" "a passphrase for ida")
          k   (sessions/token-key t)
          old (java.util.Date. (- (System/currentTimeMillis)
                                  (* 1000 (inc sessions/idle-seconds))))]
      (is (some? (sessions/fetch store t)))
      (ctl/plan! (ctl/served store :sessions)
                 [(fn [s at] (ctl/updating s :sessions {:token k} {:last-seen-at old} at))])
      (is (nil? (sessions/fetch store t)) "unused for longer than the idle limit"))))

(deftest throttle-decides
  (let [{:keys [capacity per-ms]} (:account throttle/policy)
        k   [:account "bob"]
        ip  [:ip "10.0.0.9"]
        run (fn [n] (reduce #(throttle/failed %1 k %2) {} (repeat n 0)))
        one (quot per-ms capacity)]
    (is (zero? (throttle/retry-after-ms (run 4) [k] 0)) "the free attempts go through")
    (is (= one (throttle/retry-after-ms (run 5) [k] 0)) "then one attempt refills at a time")
    (is (= one (throttle/retry-after-ms (run 40) [k] 0)) "never longer than one refill")
    (is (zero? (throttle/retry-after-ms (run 7) [k] one)) "the wait passes")
    (is (zero? (throttle/retry-after-ms (throttle/succeeded (run 7) [k ip]) [k] 0))
        "a success clears the account")
    (let [s (reduce #(throttle/failed %1 ip %2) {} (repeat 21 0))]
      (is (zero? (throttle/retry-after-ms (run 5) [ip] 0)) "an IP has its own, larger budget")
      (is (pos? (throttle/retry-after-ms s [k ip] 0)))
      (is (= s (throttle/succeeded s [k ip])) "and a success does not clear it"))
    (is (= [[:account "bob"] [:ip "1.2.3.4"]]
           (throttle/keys-for {:account " Bob " :ip "1.2.3.4" :client ""})))))

(deftest password-policy
  (let [ps #(policy/problems policy/defaults @policy/common %)]
    (is (= [] (ps {:username "eva" :password "lila wolken über dem see"})))
    (is (= [:too-short] (ps {:username "eva" :password "kurz-x9!"})))
    (is (some #{:too-long} (ps {:username "eva" :password (apply str (repeat 1025 "q"))})))
    (is (some #{:common} (ps {:username "eva" :password "Password2026!"})))
    (is (some #{:common} (ps {:username "eva" :password "123456789012"})))
    (is (some #{:common} (ps {:username "eva" :password "abcabcabcabc"})))
    (is (some #{:common} (ps {:username "eva" :password "qwertyuiopasdfghjkl"})))
    (is (some #{:is-username} (ps {:username "longusername" :password "LongUserName"})))
    (is (some #{:is-username} (ps {:username "longusername" :password "longusername2026"})))
    (is (not (some #{:common} (ps {:username "eva" :password "x7Kq2-vN9a!pR"})))
        "no composition rule and no false hit on a random one")))

(deftest pkce-s256
  (let [verifier  "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
        challenge "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"]
    (is (= challenge (oauth/challenge-of verifier)) "RFC 7636 appendix B")
    (is (oauth/verifier-ok? challenge verifier))
    (is (not (oauth/verifier-ok? challenge (str verifier "x"))))
    (is (not (oauth/verifier-ok? challenge "short")))
    (is (not (oauth/verifier-ok? nil verifier)) "a code without a challenge redeems nothing")
    (is (nil? (oauth/challenge-problem challenge "S256")))
    (is (some? (oauth/challenge-problem challenge "plain")) "plain is refused")
    (is (some? (oauth/challenge-problem nil "S256")) "PKCE is required")))

(deftest pkce-code-is-single-use-and-bound
  (let [store (mem/store)
        c     (oauth/register-client! store {:app "ri.app.x" :app-name :x
                                             :grant-types [:authorization-code]
                                             :created-by "admin"})
        code  (oauth/mint-code! store {:client-id (:client-id c) :username "u"
                                       :code-challenge "abc" :redirect-uri "https://a/cb"})
        redeem #(first (ctl/plan! store [(oauth/redeeming %1 %2)]))]
    (is (nil? (ctl/get store :oauth-codes {:code code})) "only the code's hash is stored")
    (is (nil? (redeem code "another-client")))
    (is (= "abc" (:code-challenge (redeem code (:client-id c)))))
    (is (nil? (redeem code (:client-id c))) "a code redeems once")))

(deftest client-ip-trusts-only-the-proxy
  (let [req (fn [peer hs] {:remote-addr peer :headers hs})]
    (is (= "203.0.113.7"
           (request/client-ip (req "127.0.0.1" {"x-real-ip" "203.0.113.7"
                                                "x-forwarded-for" "1.1.1.1, 203.0.113.7"}))))
    (is (= "203.0.113.7"
           (request/client-ip (req "127.0.0.1" {"x-forwarded-for" "1.1.1.1, 203.0.113.7"})))
        "the last hop is the one the proxy appended")
    (is (= "198.51.100.4"
           (request/client-ip (req "198.51.100.4" {"x-real-ip" "1.1.1.1"
                                                   "x-forwarded-for" "1.1.1.1"})))
        "a client that is no proxy cannot name another address")
    (is (request/secure? (req "127.0.0.1" {"x-forwarded-proto" "https"})))
    (is (not (request/secure? (req "198.51.100.4" {"x-forwarded-proto" "https"}))))
    (is (= "10.0.0.2" (request/client-ip (req "10.0.0.5" {"x-real-ip" "10.0.0.2"})
                                         #{"10.0.0.5"})))))

(deftest session-cookie-is-secure-over-tls
  (let [tls   {:remote-addr "127.0.0.1" :headers {"x-forwarded-proto" "https"}}
        plain {:remote-addr "127.0.0.1" :headers {}}
        [c clear] (auth/session-cookies tls "tok")]
    (is (re-find #"^__Host-sutra-session=tok; Path=/; HttpOnly; SameSite=Lax; .*; Secure$" c))
    (is (re-find #"^sutra-session=; .*Max-Age=0" clear) "the old name is cleared")
    (is (= 1 (count (auth/session-cookies plain "tok"))))
    (is (not (re-find #"Secure" (first (auth/session-cookies plain "tok")))))))

(deftest login-body-is-checked-at-the-edge
  (is (= {:username "ann" :password " pw "} (auth/login-body {:username " ann " :password " pw "})))
  (is (nil? (auth/login-body {:username {:a 1} :password "x"})))
  (is (nil? (auth/login-body {:username "ann"})))
  (is (nil? (auth/login-body "ann")))
  (is (nil? (auth/login-body {:username "ann" :password (apply str (repeat 5000 "x"))}))))
