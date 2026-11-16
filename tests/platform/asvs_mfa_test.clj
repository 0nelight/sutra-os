(ns ^:unit platform.asvs-mfa-test
  (:require [sutra.serve.errors :as errors]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.access.http.auth :as auth]
            [sutra.access.mfa :as mfa]
            [sutra.access.password-policy :as policy]
            [sutra.access.principals :as pr]
            [sutra.access.sessions :as sessions]
            [sutra.access.throttle :as throttle]
            [sutra.access.totp :as totp]
            [sutra.foundation.env :as env]
            [sutra.kit.system :as sys]
            [sutra.notifications.mail :as mail]
            [sutra.store.access.scope :as scope]
            [sutra.store.control :as ctl]
            [support.mem :as mem])
  (:import (java.io BufferedReader InputStreamReader OutputStreamWriter)
           (java.net ServerSocket)))

(defn- store []
  (assoc (mem/store) ::sys/system (sys/configure (sys/assemble [throttle/module]) {})))

(def ^:private rfc-seed (totp/base32 (.getBytes "12345678901234567890" "US-ASCII")))

(deftest totp-matches-rfc-6238
  (is (= "GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQ" rfc-seed))
  (is (= "287082" (totp/code rfc-seed (totp/step-at 59000))))
  (is (= "081804" (totp/code rfc-seed (totp/step-at 1111111109000))))
  (is (= "005924" (totp/code rfc-seed (totp/step-at 1234567890000))))
  (is (= (seq (.getBytes "12345678901234567890" "US-ASCII")) (seq (totp/unbase32 rfc-seed)))))

(deftest a-code-counts-once-and-near-its-step
  (let [ms 1111111109000
        st (totp/step-at ms)]
    (is (= st (totp/verify rfc-seed "081804" ms nil)))
    (is (= st (totp/verify rfc-seed "081 804" (+ ms 30000) nil)) "one step late still counts")
    (is (nil? (totp/verify rfc-seed "081804" (+ ms 90000) nil)) "three steps late does not")
    (is (nil? (totp/verify rfc-seed "081804" ms st)) "a used step is never taken again")
    (is (nil? (totp/verify rfc-seed "12345" ms nil)))
    (is (nil? (totp/verify "not base32!" "081804" ms nil)))))

(deftest a-seed-is-sealed
  (let [k (.getBytes "a key" "UTF-8")
        s (totp/seal k "JBSWY3DPEHPK3PXP")]
    (is (not (str/includes? s "JBSWY3DP")))
    (is (not= s (totp/seal k "JBSWY3DPEHPK3PXP")) "a fresh nonce each time")
    (is (= "JBSWY3DPEHPK3PXP" (totp/unseal k s)))
    (is (nil? (totp/unseal (.getBytes "another key" "UTF-8") s)))))

(deftest the-uri-is-what-authenticator-apps-read
  (is (= (str "otpauth://totp/Sutra:a%20b?secret=ABC&issuer=Sutra"
              "&algorithm=SHA1&digits=6&period=30")
         (totp/uri "ABC" "a b" "Sutra"))))

(deftest who-must-enrol
  (is (mfa/required-for? "all" "lea"))
  (is (mfa/required-for? "true" "lea"))
  (is (mfa/required-for? "admin" "admin"))
  (is (not (mfa/required-for? "admin" "lea")))
  (is (not (mfa/required-for? nil "lea"))))

(defn- now [s] (ctl/millis s))

(defn- current-code
  [seed s]
  (totp/code seed (totp/step-at (now s))))

(deftest sign-in-with-a-second-factor
  (let [s (store)]
    (pr/register-principal! s {:username "mia" :password "a passphrase for mia"
                               :user-id "u-mia"})
    (let [{:keys [seed uri]} (mfa/begin! s "mia")]
      (is (str/starts-with? uri "otpauth://totp/Sutra:mia?secret="))
      (testing "a pending seed does not yet guard the sign-in"
        (is (some? (auth/login! s "mia" "a passphrase for mia"))))
      (is (not (mfa/confirm! s "mia" "000000" (now s))))
      (is (mfa/confirm! s "mia" (current-code seed s) (now s)))
      (is (:mfa (pr/fetch s "mia")))
      (is (not-any? #(contains? % :mfa-seed) (pr/all-rows s)) "the seed is never listed")
      (testing "the password alone no longer signs in"
        (is (= {:mfa-required true}
               (auth/sign-in! s "mia" "a passphrase for mia" nil nil)))
        (is (nil? (auth/login! s "mia" "a passphrase for mia"))))
      (testing "a wrong code fails, a wrong password with a code fails"
        (is (nil? (auth/sign-in! s "mia" "a passphrase for mia" "000000" nil)))
        (is (nil? (auth/sign-in! s "mia" "wrong" (current-code seed s) nil))))
      (testing "the code of the step the confirmation used is spent"
        (is (nil? (:token (auth/sign-in! s "mia" "a passphrase for mia"
                                         (current-code seed s) nil)))))
      (testing "the next step's code signs in, once"
        (let [later (+ (now s) 30000)
              code  (totp/code seed (totp/step-at later))]
          (with-redefs [ctl/millis (fn ^long [_] later)]
            (is (some? (:token (auth/sign-in! s "mia" "a passphrase for mia" code nil))))
            (is (nil? (:token (auth/sign-in! s "mia" "a passphrase for mia" code nil)))))))
      (mfa/disable! s "mia")
      (is (some? (auth/login! s "mia" "a passphrase for mia"))))))

(deftest an-initial-password-signs-in-only-to-be-changed
  (let [s (store)]
    (pr/register-principal! s {:username "ivo" :password "an initial passphrase"
                               :user-id "u-ivo" :initial? true})
    (let [{:keys [token limited]} (auth/sign-in! s "ivo" "an initial passphrase" nil nil)
          sess (sessions/fetch s token)]
      (is (= #{:password} limited))
      (is (sessions/limited-scoped? sess))
      (is (= #{:password} (sessions/limited-reasons sess))))
    (pr/set-password! s "ivo" "a passphrase ivo chose")
    (is (not (:pw-must-change (pr/fetch s "ivo"))))
    (is (nil? (:limited (auth/sign-in! s "ivo" "a passphrase ivo chose" nil nil))))))

(deftest an-initial-password-expires
  (let [s (store)]
    (pr/register-principal! s {:username "ute" :password "an initial passphrase"
                               :user-id "u-ute" :initial? true})
    (with-redefs [ctl/millis (let [t (+ (System/currentTimeMillis)
                                        (* (inc auth/initial-password-hours) 3600 1000))]
                               (fn ^long [_] t))]
      (is (= {:refused :initial-password-expired}
             (auth/sign-in! s "ute" "an initial passphrase" nil nil))))))

(deftest a-required-factor-limits-the-session-until-enrolled
  (let [s (store)]
    (pr/register-principal! s {:username "eva" :password "a passphrase for eva"
                               :user-id "u-eva"})
    (with-redefs [mfa/required-for? (constantly true)]
      (is (= #{:mfa} (:limited (auth/sign-in! s "eva" "a passphrase for eva" nil nil)))))))

(defn- edge
  [s token uri]
  (with-redefs [scope/store (constantly s)]
    ((errors/wrap-exceptions (auth/wrap-auth (fn [_] {:status 200 :body {:ok true}})))
     {:uri uri :request-method :get :headers {"authorization" (str "Bearer " token)}})))

(deftest a-limited-session-reaches-only-its-first-steps
  (let [s (store)]
    (pr/register-principal! s {:username "ole" :password "an initial passphrase"
                               :user-id "u-ole" :initial? true})
    (let [{:keys [token]} (auth/sign-in! s "ole" "an initial passphrase" nil nil)]
      (is (= 403 (:status (edge s token "/api/projects"))))
      (is (true? (get-in (edge s token "/api/projects") [:body :limited]))))))

(deftest hardening-is-off-unless-asked-for
  (with-redefs [env/getenv (constantly nil)]
    (is (false? (env/auth-hardening?)))
    (is (nil? (sessions/max-logins)) "no cap on parallel sessions")))

(deftest parallel-sign-ins-are-capped
  (with-redefs [env/auth-hardening? (constantly true)]
    (let [s   (store)
          cap (sessions/max-logins)]
      (pr/register-principal! s {:username "kim" :password "a passphrase for kim"
                                 :user-id "u-kim"})
      (let [tokens (doall (repeatedly (+ 3 cap)
                                      #(auth/login! s "kim" "a passphrase for kim")))]
        (is (= cap (count (sessions/of-user s "kim" nil))))
        (is (some? (sessions/fetch s (last tokens))) "the newest is kept")
        (is (nil? (sessions/fetch s (first tokens))) "the oldest went")))))

(deftest the-admin-ends-another-principals-sessions
  (let [s (store)]
    (pr/register-principal! s {:username "max" :password "a passphrase for max"
                               :user-id "u-max"})
    (let [t   (auth/login! s "max" "a passphrase for max")
          end (fn [who] ((errors/wrap-exceptions auth/sessions-end-handler)
                         {:principal {:username who} :sutra/store s
                          :body-params {:username "max"} :headers {}}))]
      (is (= 403 (:status (end "lea"))))
      (is (some? (sessions/fetch s t)))
      (is (= 200 (:status (end "admin"))))
      (is (nil? (sessions/fetch s t))))))

(deftest context-words-make-a-weak-password
  (let [words (policy/context-words "o.example-firm.com" nil "acme,projectx")]
    (is (= #{"sutra" "example-firm" "acme" "projectx"} words))
    (is (= #{"sutra" "intranet"} (policy/context-words nil "https://intranet.lan/" nil)))
    (let [p #(policy/problems {:min-length 8 :max-length 1024 :context words} #{}
                              {:username "u" :password %})]
      (is (some #{:context} (p "Sutra2026!!!")))
      (is (some #{:context} (p "acme-acme-1234")))
      (is (empty? (p "sutra is a river in another country"))))))

(deftest a-login-is-never-sent-in-plain-text
  (is (re-find #"needs TLS" (mail/refusal {:tls :none :user "u" :password "p"})))
  (is (re-find #"no password" (mail/refusal {:tls :starttls :user "u" :password nil})))
  (is (nil? (mail/refusal {:tls :starttls :user "u" :password "p"})))
  (is (nil? (mail/refusal {:tls :none})))
  (is (= "AHVzZXIAcGFzcw==" (mail/auth-plain "user" "pass"))))

(defn- fake-relay
  [ehlo-lines]
  (let [srv  (ServerSocket. 0)
        seen (atom [])]
    (future
      (with-open [sock (.accept srv)
                  in   (BufferedReader. (InputStreamReader. (.getInputStream sock)))
                  out  (OutputStreamWriter. (.getOutputStream sock))]
        (let [say #(do (.write out (str % "\r\n")) (.flush out))]
          (say "220 fake")
          (loop []
            (when-let [l (.readLine in)]
              (swap! seen conj l)
              (cond
                (str/starts-with? l "EHLO") (doseq [e ehlo-lines] (say e))
                (= l "DATA")                (say "354 go")
                (= l ".")                   (say "250 queued")
                (= l "QUIT")                (say "221 bye")
                (or (str/starts-with? l "MAIL") (str/starts-with? l "RCPT")) (say "250 ok")
                :else                       nil)
              (when-not (= l "QUIT") (recur))))))
      (.close srv))
    {:port (.getLocalPort srv) :seen seen}))

(def ^:private msg {:to "a@b.example" :subject "s" :body "b"})

(deftest starttls-is-required-unless-named-off
  (let [{:keys [port seen]} (fake-relay ["250-fake" "250 SIZE 1000"])]
    (is (re-find #"no STARTTLS"
                 (str (mail/send! {:host "127.0.0.1" :port port :from "s@b.example"
                                   :domain "b.example" :tls :starttls}
                                  msg))))
    (is (not-any? #(str/starts-with? % "MAIL") @seen) "nothing is sent without TLS"))
  (let [{:keys [port seen]} (fake-relay ["250 fake"])]
    (is (nil? (mail/send! {:host "127.0.0.1" :port port :from "s@b.example"
                           :domain "b.example" :tls :none}
                          msg)))
    (is (some #(= "RCPT TO:<a@b.example>" %) @seen))))
