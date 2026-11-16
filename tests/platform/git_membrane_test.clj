(ns ^:unit platform.git-membrane-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.parser.project-export :as pe]
            [sutra.branching.http.git :as git-http]
            [sutra.branching.push :as push]
            [sutra.repos.api :as repos]
            [sutra.access.http.host :as host]
            [sutra.types.branch-kinds :as branch-kinds]
            [sutra.serve.gzip :as gzip]
            [sutra.serve.server :as server]
            [sutra.access.http.auth :as auth]
            [sutra.access.sessions :as sessions]
            [sutra.catalog.grants :as grants]
            [sutra.types.rid :as rid])
  (:import (java.util Base64)))

(deftest every-mounted-prefix-has-an-authority-cell
  (testing "`endpoint-authority` declares who may call each ENDPOINT, and the
            router derives its gates from it. A handler mounted inside
            `ring/routes` bypasses that join entirely, so a prefix must carry its
            own cell or the invariant quietly becomes 'written once, except
            outside the router'."
    (is (seq (server/prefix-authority)))
    (doseq [[prefix cell] (server/prefix-authority)]
      (is (str/starts-with? prefix "/") (str prefix " must be a URI path"))
      (is (#{:prefix :exact} (:match cell))
          (str prefix " must say whether it is a PREFIX or an EXACT path — the"
               " ontology repository is served from the instance root, so"
               " 'everything here is a prefix' stopped being true"))
      (when (= :prefix (:match cell))
        (is (str/ends-with? prefix "/")
            (str prefix " must end in / — a prefix that does not is a prefix that"
                 " matches /gitlab as well as /git/")))
      (when (= :exact (:match cell))
        (is (not (str/ends-with? prefix "/"))
            (str prefix " is claimed exactly; a trailing / would make it a"
                 " directory nobody asks for")))
      (is (keyword? (:auth cell)) (str prefix " declares no :auth level"))
      (is (string? (:reason cell)) (str prefix " declares no :reason")))))

(deftest the-git-prefix-is-declared-and-mounted
  (is (contains? (server/prefix-authority) "/git/"))
  (is (= :repo-token (get-in (server/prefix-authority) ["/git/" :auth]))
      "not the session cookie, not Bearer — a repository-scoped clone token")
  (is (fn? (server/plugin-handler))))

(deftest the-git-handler-declines-everything-outside-its-prefix
  (testing "it sits ahead of the resource handler and the SPA fallback, so
            returning a response for a non-git URI would shadow the whole app"
    (doseq [uri ["/api/schema" "/" "/index.html" "/gitlab/x" "/gitsomething"]]
      (is (nil? ((git-http/make-handler {}) {:uri uri :request-method :get}))
          (str uri " must fall through to the next handler")))))

(deftest wrap-gzip-never-compresses-a-git-body
  (testing "git's own framing is length-delimited and a client does not send
            Accept-Encoding for it; compressing one would produce a body git
            reads as a protocol error. The exemption is by OMISSION from a
            content-type whitelist, which is exactly the kind of fact one added
            entry deletes silently."
    (doseq [ctype ["application/x-git-upload-pack-result"
                   "application/x-git-receive-pack-result"
                   "application/x-git-upload-pack-advertisement"
                   "application/x-git-receive-pack-advertisement"]]
      (let [body (apply str (repeat 4000 "x"))
            resp ((gzip/wrap-gzip
                   (constantly {:status 200 :headers {"Content-Type" ctype} :body body}))
                  {:headers {"accept-encoding" "gzip, deflate"}})]
        (is (nil? (get-in resp [:headers "Content-Encoding"]))
            (str ctype " must not be compressed"))
        (is (= body (:body resp)) (str ctype " body must pass through untouched"))))))

(deftest a-gzipped-rpc-body-is-decompressed-before-jgit-sees-it
  (testing "`git` gzips the `git-upload-pack` RPC body and says so in
            `Content-Encoding`. Handed to JGit still compressed, gzip's `1f 8b`
            magic is read as a pkt-line length and the fetch 500s with `Invalid
            packet line header` — while `ls-remote`, a GET with no body, keeps
            working. Measured on prod: every `git fetch` failed for days and it
            read as a projection fault, because the half that answers is the half
            without a body."
    (let [wants "0032want 0000000000000000000000000000000000000000\n0000"
          gz    (let [bos (java.io.ByteArrayOutputStream.)]
                  (with-open [gzo (java.util.zip.GZIPOutputStream. bos)]
                    (.write gzo (.getBytes wants "UTF-8")))
                  (.toByteArray bos))
          read-all #(slurp (#'git-http/request-body %))]
      (is (= wants (read-all {:headers {"content-encoding" "gzip"}
                              :body (java.io.ByteArrayInputStream. gz)}))
          "the compressed body must arrive at JGit as pkt-lines")
      (is (= wants (read-all {:headers {"content-encoding" "x-gzip"}
                              :body (java.io.ByteArrayInputStream. gz)}))
          "`x-gzip` is the same encoding under an older name")
      (is (= wants (read-all {:headers {}
                              :body (java.io.ByteArrayInputStream.
                                     (.getBytes wants "UTF-8"))}))
          "an uncompressed body must pass through untouched — a push sends one
           and must not be handed a gzip reader"))))

(deftest request-token-still-refuses-basic
  (testing "teaching the whole membrane to accept Basic in order to serve one
            prefix is how a narrow need becomes a global auth surface. The git
            handler does its OWN credential step; this seam is not widened."
    (let [basic (str "Basic " (.encodeToString (Base64/getEncoder)
                                               (.getBytes "u:secret" "UTF-8")))
          f     #'auth/request-token]
      (is (nil? (f {:headers {"authorization" basic}})))
      (is (= "tok" (f {:headers {"authorization" "Bearer tok"}})))
      (is (= "tok" (f {:headers {"cookie" "sutra-session=tok"}}))))))

(deftest a-clone-token-cannot-call-the-platform-api
  (testing "a clone token is minted to be pasted into ~/.git-credentials, so it
            has the widest exposure of any credential this system issues. One
            that could also drive /api/actions would be a session with extra steps."
    (is (false? (sessions/scoped-to? {:scope "ri.r.1"} "ri.r.2")))
    (is (true?  (sessions/scoped-to? {:scope "ri.r.1"} "ri.r.1")))
    (is (true?  (sessions/scoped-to? {:scope nil} "ri.r.1"))
        "an unscoped session is a full login and reaches any repository it may read")))

(deftest a-clone-token-reaches-exactly-the-repositories-it-names
  (testing "`sutra clone ontology` lays down the ontology and every Source Repository
            beside it from one credential: it mints one token naming all of them.
            The ontology repository has no reach of its own."
    (let [both {:scope (str rid/ontology-repo-rid " ri.r.1")}]
      (is (true? (git-http/token-reaches? host/capabilities both "ri.r.1")))
      (is (true? (git-http/token-reaches? host/capabilities both rid/ontology-repo-rid)))
      (is (false? (git-http/token-reaches? host/capabilities both "ri.r.2"))))
    (is (false? (git-http/token-reaches? host/capabilities {:scope rid/ontology-repo-rid}
                                         "ri.r.1")))
    (is (false? (git-http/token-reaches? host/capabilities {:scope "ri.r.1"}
                                         rid/ontology-repo-rid)))
    (is (false? (git-http/token-reaches? host/capabilities {:scope "ri.r.1"} "ri.r.2")))
    (is (false? (git-http/token-reaches? host/capabilities {:scope "ri.r.10"} "ri.r.1"))
        "membership, not a prefix match")))

(def ^:private repo-schema
  {:projects     {"ri.p.1" {:project-name :controlling}
                  "ri.p.2" {:project-name :andere}}
   :repositories {"ri.r.1" {:repository-name :etl :project "ri.p.1"}}
   :transforms   {"ri.t.1" {:transform-name :a :repository "ri.r.1" :project "ri.p.1"}}
   :functions    {"ri.f.1" {:function-name :b :repository "ri.r.1" :project "ri.p.1"}}
   :applications {"ri.a.1" {:name :c :repository "ri.r.1" :project "ri.p.1"}}
   :pipelines    {"ri.pl.1" {:pipeline-name :d :repository "ri.r.1" :project "ri.p.1"}}
   :templates    {"ri.tp.1" {:name :e :repository "ri.r.1" :project "ri.p.1"}}})

(defn- readable-with-one-member-out-of-reach
  [section]
  (repos/repo-readable?
   {:project-operations {"ri.p.1" #{:read}}}
   (assoc-in repo-schema [section (key (first (get repo-schema section))) :project]
             "ri.p.2")
   "ri.r.1"))

(deftest a-repo-is-reachable-only-if-EVERY-member-kind-is
  (testing "the rule is 'a repository containing one member the caller lacks
            clearance for is unreachable as a whole', and it is only true if this
            predicate and `repo-tree` agree on what a member is. They did not:
            membership was listed here as transforms+functions while the tree
            also carries the repo's applications and pipelines — so an app's
            authored `apps/<name>.cljs` shipped in a clone whose reader was never
            checked against it."
    (doseq [section pe/code-sections]
      (is (false? (readable-with-one-member-out-of-reach section))
          (str "a " section " member the caller may not read left the repository"
               " readable as a whole — that member's file ships in the clone")))))

(deftest membership-is-read-from-ONE-declaration
  (testing "`code-sections` is what `repo-tree` scopes a repo's tree by, so it is
            what decides reachability too. A second list here is a list that can
            drift, and it did."
    (is (= #{:transforms :functions :applications :pipelines :templates}
           pe/code-sections)
        "a new :tree :repo section belongs in the fixture below too — the loop
         over `code-sections` is what proves the predicate gates it")))

(deftest an-unrestricted-principal-reaches-the-repo
  (testing "grants/all is a full login — the gate must not refuse it a repository"
    (is (true? (repos/repo-readable?
                {:project-operations grants/all} repo-schema "ri.r.1")))))

(deftest a-repository-that-does-not-exist-is-not-readable
  (is (false? (repos/repo-readable? {} repo-schema "ri.r.nope"))))

(deftest the-clone-token-ttl-is-the-documented-one
  (is (= (* 60 60 24 7) sessions/clone-ttl-seconds)
      "a clone token lives for seven days"))

(deftest the-three-url-forms-parse
  (is (= {:address {:kind :canonical :rid "ri.r.1"}
          :op :advertise :service "git-upload-pack"}
         (git-http/parse-path "/git/r/ri.r.1.git/info/refs" "service=git-upload-pack")))
  (is (= {:address {:kind :alias :project "controlling" :repo "etl"}
          :op :receive-pack :service "git-receive-pack"}
         (git-http/parse-path "/git/controlling/etl.git/git-receive-pack" nil)))
  (testing "the INSTANCE ROOT — what `git clone http://<host>` actually asks for"
    (is (= {:address {:kind :ontology} :op :advertise :service "git-upload-pack"}
           (git-http/parse-path "/info/refs" "service=git-upload-pack")))
    (is (= {:address {:kind :ontology} :op :upload-pack :service "git-upload-pack"}
           (git-http/parse-path "/git-upload-pack" nil)))
    (is (= {:address {:kind :ontology} :op :receive-pack :service "git-receive-pack"}
           (git-http/parse-path "/git-receive-pack" nil))))
  (testing "and the readable alias for the same repository, which is what a
            checkout's `remote.origin.url` and nginx's rewrite both carry"
    (is (= {:address {:kind :ontology} :op :advertise :service "git-upload-pack"}
           (git-http/parse-path "/git/ontology.git/info/refs" "service=git-upload-pack"))))
  (testing "a path this surface serves but cannot address names its own form, so
            it 404s as `no such repository` instead of falling through to the SPA"
    (is (= {:kind :unknown :raw "a/b/c"}
           (:address (git-http/parse-path "/git/a/b/c.git/info/refs" nil))))
    (is (nil? (git-http/resolve-repo-rid {} {:kind :unknown :raw "a/b/c"}))))
  (testing "anything outside the prefix is not ours"
    (is (nil? (git-http/parse-path "/api/repositories" nil)))))

(deftest the-root-form-claims-three-paths-and-not-the-site
  (testing "the SPA keeps every route it has. A prefix at `/` would have to be
            excluded from everything; three exact names cost nothing anyone wants."
    (doseq [uri ["/" "/index.html" "/info" "/info/refs/extra" "/objects/info/packs"
                 "/git-upload-pack/x" "/x/info/refs" "/HEAD"]]
      (is (nil? (git-http/parse-path uri nil))
          (str uri " must fall through to the SPA")))))

(deftest the-ontology-address-resolves-without-a-schema-row
  (testing "the ontology repository has no `:repositories` row by design, so a
            resolver that looked it up would answer nil and 404 the root clone"
    (is (= rid/ontology-repo-rid (git-http/resolve-repo-rid {} {:kind :ontology})))
    (is (= rid/ontology-repo-rid
           (git-http/resolve-repo-rid
            {} (:address (git-http/parse-path
                          (str "/git/r/" rid/ontology-repo-rid ".git/info/refs") nil))))
        "the canonical form too — `clone-token` hands that back as :remote, and
         routing it through the parser is what proves the two halves agree"))
  (testing "and an unknown name is still nil rather than a fallback to it"
    (is (nil? (git-http/resolve-repo-rid
               {} (:address (git-http/parse-path "/git/ontologyy.git/info/refs" nil)))))
    (is (nil? (git-http/resolve-repo-rid {} {:kind :alias :project "controlling"
                                             :repo "nope"})))))

(deftest the-reserved-rid-cannot-collide-with-a-minted-one
  (testing "`new-rid` always ends in a UUID; this ends in a word. A collision
            would hand somebody else's repository the ontology's tree."
    (is (rid/ontology-repo? rid/ontology-repo-rid))
    (is (not (rid/ontology-repo? (rid/new-rid :repository (random-uuid)))))
    (is (not (rid/ontology-repo? nil)))
    (is (str/ends-with? rid/ontology-repo-rid ".ontology"))))

(deftest the-tier-is-off-by-default
  (testing "SUTRA_GIT_REPOS, default off — matching how every other tier in this
            codebase lands, so the phase is revertible by an environment variable
            rather than by a deploy"
    (is (false? (boolean (binding [git-http/*enabled-override* false]
                           (git-http/enabled?)))))
    (let [r (binding [git-http/*enabled-override* false]
              ((server/plugin-handler) {:uri "/git/r/ri.r.1.git/info/refs"
                                   :query-string "service=git-upload-pack"
                                   :request-method :get}))]
      (is (= 404 (:status r))
          "and while off it 404s rather than 500ing or falling through to the SPA"))))

(deftest die-entscheidenden-checks-laufen-vor-dem-ref-update
  (testing "every ordinary branch is judged first"
    (is (push/judge-before-adoption? "feature-x"))
    (is (push/judge-before-adoption? "main")))
  (testing "…and `sketch/` is the valve — adopted first, checks recorded after"
    (is (not (push/judge-before-adoption? "sketch/wip")))
    (is (not (push/judge-before-adoption? "sketch/anything/deep"))))
  (testing "a ref with no branch (a tag) has no gates of its own"
    (is (not (push/judge-before-adoption? nil))))
  (testing "the valve is a REAL branch, not a machine namespace: clonable, projectable"
    (is (branch-kinds/projectable-branch? "sketch/wip"))
    (is (not (branch-kinds/projectable-branch? "staging/alma")))
    (is (not-any? #(str/starts-with? branch-kinds/sketch-namespace %)
                  branch-kinds/machine-namespaces)
        "sketch/ must not be swallowed by a machine prefix — it would stop being pushable")))
