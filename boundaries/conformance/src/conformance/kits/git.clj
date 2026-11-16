(ns conformance.kits.git
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [conformance.http :as http]
            [conformance.report :as r])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn repo-url
  [base repo]
  (str (str/replace base #"/+$" "") "/git/" repo ".git"))

(defn- refs-url [base repo service]
  (str (repo-url base repo) "/info/refs?service=" service))

(defn pkt-lines
  [^String s]
  (loop [i 0 out []]
    (cond
      (>= i (count s)) out
      (< (count s) (+ i 4)) nil
      :else
      (let [n (try (Integer/parseInt (subs s i (+ i 4)) 16) (catch Exception _ -1))]
        (cond
          (neg? n)    nil
          (zero? n)   (recur (+ i 4) (conj out :flush))
          (< n 4)     nil
          (< (count s) (+ i n)) nil
          :else (recur (+ i n) (conj out (subs s (+ i 4) (+ i n)))))))))

(defn advertisement-problem
  [service body]
  (let [ls (pkt-lines (str body))]
    (cond
      (nil? ls) "the body is not pkt-line framed"
      (= "version 2\n" (first ls)) nil
      (not= (str "# service=" service "\n") (first ls))
      (str "first pkt-line is " (pr-str (first ls)) ", not \"# service=" service "\"")
      (not= :flush (second ls)) "no flush-pkt after the service line"
      (not= :flush (last ls)) "the ref list does not end in a flush-pkt"
      :else
      (let [refs (remove #{:flush} (drop 2 ls))
            bad  (remove #(re-matches #"(?s)[0-9a-f]{40} [^\s\u0000]+(\u0000.*)?\n?" %) refs)]
        (when (seq bad) (str "malformed ref line " (pr-str (first bad))))))))

(defn capabilities
  [body]
  (some->> (pkt-lines (str body)) (remove #{:flush}) (drop 1) first
           (re-find #"\u0000(.*)") second str/trim (#(str/split % #" ")) set))

(defn- auth [token] (when token {"Authorization" (http/basic-auth "conformance" token)}))

(defn- git-bin [] (try (zero? (:exit (sh/sh "git" "--version"))) (catch Exception _ false)))

(defn- tmp-dir [] (str (Files/createTempDirectory "conformance-git" (make-array FileAttribute 0))))

(defn- clone! [base repo token dir]
  (sh/sh "git" "-c" (str "http.extraHeader=Authorization: " (get (auth token) "Authorization"))
         "clone" "--quiet" (repo-url base repo) dir
         :env (assoc (into {} (System/getenv)) "GIT_TERMINAL_PROMPT" "0")))

(defn checks
  [{:keys [base repo token allow-push]}]
  (let [up    "git-upload-pack"
        anon  (delay (http/get-url (refs-url base repo up)))
        wrong (delay (http/get-url (refs-url base repo up) (auth "not-a-token")))
        good  (delay (http/get-url (refs-url base repo up) (auth token)))
        v2    (delay (http/get-url (refs-url base repo up)
                                   (merge (auth token) {"Git-Protocol" "version=2"})))
        need  #(if token (%) (r/skip "no --token given"))]
    [(r/check "git.enabled" "the git surface answers (not 404 for every address)"
              #(when (= 404 (:status @anon))
                 "404 without credentials: the surface is off (SUTRA_GIT_REPOS) or hides 401"))
     (r/check "git.anonymous-401" "an advertisement without credentials is 401 with Basic"
              #(let [{:keys [status headers]} @anon]
                 (cond (not= 401 status) (str "status " status)
                       (not (str/starts-with? (str (get headers "www-authenticate")) "Basic"))
                       (str "WWW-Authenticate " (pr-str (get headers "www-authenticate"))))))
     (r/check "git.wrong-token-401" "an unknown token is 401"
              #(when-not (= 401 (:status @wrong)) (str "status " (:status @wrong))))
     (r/check "git.receive-pack-guarded" "the push advertisement needs credentials too"
              #(let [s (:status (http/get-url (refs-url base repo "git-receive-pack")))]
                 (when-not (= 401 s) (str "status " s))))
     (r/check "git.advertisement" "with the token: 200, the smart content type, pkt-lines"
              (fn [] (need
                      #(let [{:keys [status headers body]} @good
                             ct (str (get headers "content-type"))]
                         (cond (not= 200 status) (str "status " status)
                               (not= (str "application/x-" up "-advertisement") ct)
                               (str "Content-Type " (pr-str ct))
                               :else (advertisement-problem up body))))))
     (r/check "git.no-cache" "the advertisement is not cached (Cache-Control: no-cache)"
              (fn [] (need
                      #(let [cc (str (get-in @good [:headers "cache-control"]))]
                         (when-not (str/includes? cc "no-cache")
                           (str "Cache-Control " (pr-str cc)))))))
     (r/check "git.capabilities" "the first ref advertises capabilities (side-band-64k)"
              (fn [] (need
                      #(let [caps (capabilities (:body @good))]
                         (cond (nil? caps) (r/skip "no refs to carry capabilities (empty repo)")
                               (not (caps "side-band-64k"))
                               (str "capabilities " (str/join " " (sort caps))))))))
     (r/check "git.protocol-v2" "Git-Protocol: version=2 is honoured or falls back to v0"
              (fn [] (need
                      #(let [{:keys [status body]} @v2]
                         (cond (not= 200 status) (str "status " status)
                               (str/starts-with? (str body) "000eversion 2\n") nil
                               (nil? (advertisement-problem up body))
                               (r/skip "answers v0 — allowed, v2 not offered")
                               :else (advertisement-problem up body))))))
     (r/check "git.unknown-repo" "an address that does not resolve is 401 or 404, never 5xx"
              (fn [] (need
                      #(let [s (:status (http/get-url (refs-url base "no-such/repository" up)
                                                      (auth token)))]
                         (when-not (#{401 404} s) (str "status " s))))))
     (r/check "git.clone" "plain git clones the repository with the token"
              (fn [] (cond (not token) (r/skip "no --token given")
                           (not (git-bin)) (r/skip "no git binary on the PATH")
                           :else (let [d (str (io/file (tmp-dir) "clone"))
                                       {:keys [exit err]} (clone! base repo token d)]
                                   (when-not (zero? exit) (str "git clone: " (str/trim err)))))))
     (r/check "git.push" "a push is judged and answered with a receive-pack report"
              (fn [] (cond (not allow-push) (r/skip "read-only run (--allow-push not given)")
                           (not (and token (git-bin))) (r/skip "needs --token and git")
                           :else
                           (let [d (str (io/file (tmp-dir) "push"))
                                 _ (clone! base repo token d)
                                 g #(apply sh/sh "git" "-C" d %&)
                                 b (str "conformance-" (System/currentTimeMillis))]
                             (g "checkout" "-q" "-b" b)
                             (spit (io/file d "CONFORMANCE.md") "conformance probe\n")
                             (g "add" "CONFORMANCE.md")
                             (g "-c" "user.name=conformance" "-c" "user.email=c@example.invalid"
                                "commit" "-q" "-m" "conformance probe")
                             (let [{:keys [err out]}
                                   (g "-c" (str "http.extraHeader=Authorization: "
                                                (get (auth token) "Authorization"))
                                      "push" "--porcelain" "origin" b)
                                   txt (str out err)]
                               (when-not (re-find #"(?m)^[*=!+-]\s|\[rejected\]|\[new branch\]"
                                                  txt)
                                 (str "no ref status in the push answer: " (str/trim txt))))))))]))
