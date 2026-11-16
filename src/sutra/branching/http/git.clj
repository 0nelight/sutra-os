(ns sutra.branching.http.git
  (:require [sutra.kit.refusal :as refusal]
            [sutra.branching.push :as push]
            [sutra.store.access.scope :as ctx]
            [sutra.store.control :as ctl]
            [sutra.repos.api :as repos]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.types.rid :as rid]
            [sutra.types.ctx :as tctx]
            [sutra.foundation.env :as env]
            [sutra.calx.io.zip :as zip])
  (:import (java.io ByteArrayOutputStream InputStream)
           (java.util Base64)
           (java.util.zip GZIPInputStream)))

(set! *warn-on-reflection* true)

(def ^:private repos-env?
  (= "true" (some-> (env/getenv "SUTRA_GIT_REPOS") str/lower-case)))

(def ^:dynamic *enabled-override* nil)

(defn enabled?
  []
  (if (some? *enabled-override*) *enabled-override* repos-env?))

(def ^:private smart-http-tails
  {"info/refs"        {:op :advertise
                       :reason "advertising the ontology repository's refs"}
   "git-upload-pack"  {:op :upload-pack  :service "git-upload-pack"
                       :reason "cloning or fetching the ontology repository"}
   "git-receive-pack" {:op :receive-pack :service "git-receive-pack"
                       :reason "pushing to the ontology repository"}})

(def prefix-authority
  (into {"/git/" {:auth   :repo-token
                  :match  :prefix
                  :reason "cloning or pushing a code repository"}}
        (map (fn [[tail {:keys [reason]}]]
               [(str "/" tail) {:auth :repo-token :match :exact :reason reason}]))
        smart-http-tails))

(def ^:private git-prefix "/git/")

(def ontology-address
  "ontology")

(defn- op-of [tail]
  (get-in smart-http-tails [tail :op]))

(defn- service-of [tail query]
  (or (second (re-find #"service=([a-z-]+)" (str query)))
      (get-in smart-http-tails [tail :service])))

(defn parse-path
  [uri query]
  (let [u (str uri)]
    (cond
      (str/starts-with? u git-prefix)
      (let [rest' (subs u (count git-prefix))
            [addr tail] (let [i (str/index-of rest' ".git/")]
                          (if i
                            [(subs rest' 0 i) (subs rest' (+ i 5))]
                            (let [j (str/index-of rest' ".git")]
                              (when j [(subs rest' 0 j) ""]))))
            segs (when addr (str/split addr #"/"))]
        (when addr
          {:address (cond
                      (= ontology-address addr) {:kind :ontology}
                      (not= 2 (count segs))     {:kind :unknown :raw addr}
                      (= "r" (first segs))      {:kind :canonical :rid (second segs)}
                      :else                     {:kind    :alias
                                                 :project (first segs)
                                                 :repo    (second segs)})
           :op      (op-of tail)
           :service (service-of tail query)}))

      :else
      (when-let [op (op-of (when (str/starts-with? u "/") (subs u 1)))]
        {:address {:kind :ontology}
         :op      op
         :service (service-of (subs u 1) query)}))))

(defn resolve-repo-rid
  [schema address]
  (case (:kind address)
    :ontology rid/ontology-repo-rid

    :canonical
    (let [rid (:rid address)]
      (if (rid/ontology-repo? rid)
        rid/ontology-repo-rid
        (when (contains? (:repositories schema) rid) rid)))

    :alias
    (let [pname (:project address)
          rname (:repo address)
          prid  (some (fn [[rid s]]
                        (when (= pname (some-> (:project-name s) name)) rid))
                      (:projects schema))]
      (some (fn [[rid s]]
              (when (and (= rname (some-> (:repository-name s) name))
                         (or (nil? prid) (= prid (:project s))))
                rid))
            (:repositories schema)))

    nil))

(defn token-reaches?
  [host sess repo-rid]
  ((-> host :sessions :scoped-to?) sess repo-rid))

(defn- basic-credential
  [req]
  (when-let [raw (some->> (get-in req [:headers "authorization"])
                          (re-find #"(?i)^Basic\s+(.+)$") second)]
    (let [decoded (try (String. (.decode (Base64/getDecoder) ^String raw) "UTF-8")
                       (catch IllegalArgumentException _ nil))
          i       (some-> decoded (str/index-of ":"))]
      (when i [(subs decoded 0 i) (subs decoded (inc i))]))))

(def ^:private unauthorized
  {:status  401
   :headers {"WWW-Authenticate" "Basic realm=\"Sutra-OS\""
             "Content-Type"     "text/plain; charset=utf-8"}
   :body    "authentication required: use a repository clone token as the password"})

(defn- token-for-another-repo [rid]
  {:status  401
   :headers {"WWW-Authenticate" "Basic realm=\"Sutra-OS\""
             "Content-Type"     "text/plain; charset=utf-8"}
   :body    (str "Token gilt nicht für " rid " — neues Clone-Token holen\n")})

(defn- git-not-found [msg]
  {:status 404 :headers {"Content-Type" "text/plain; charset=utf-8"} :body (str msg "\n")})

(defn- ok-stream
  [ctype body]
  {:status  200
   :headers {"Content-Type"  ctype
             "Cache-Control" "no-cache, max-age=0, must-revalidate"
             "Pragma"        "no-cache"
             "Expires"       "Fri, 01 Jan 1980 00:00:00 GMT"}
   :body    body})

(def ^:private max-push-bytes (* 1024 1024 1024))

(defn- request-body
  ^InputStream [req]
  (let [enc (str/lower-case (or (get-in req [:headers "content-encoding"]) ""))]
    (if (str/includes? enc "gzip")
      (zip/bounded (GZIPInputStream. (io/input-stream (:body req))) max-push-bytes)
      (:body req))))

(defn- dispatch
  [store auth schema branch rid {:keys [op service]} req]
  (cond
    (nil? rid)                    (git-not-found "no such repository")
    (not (repos/repo-readable? auth schema rid))
    (git-not-found "no such repository")

    (= op :advertise)
    (let [out     (ByteArrayOutputStream.)
          service (or service "git-upload-pack")
          ctype   (str "application/x-" service "-advertisement")]
      (push/advance-projection! store rid)
      (repos/advertise (ctx/repos) rid service out)
      (ok-stream ctype (.toByteArray out)))

    (= op :upload-pack)
    (let [out (ByteArrayOutputStream.)]
      (repos/upload-pack (ctx/repos) rid (request-body req) out {})
      (ok-stream "application/x-git-upload-pack-result" (.toByteArray out)))

    (= op :receive-pack) (ok-stream "application/x-git-receive-pack-result"
                                    (push/run-push! store rid schema branch (request-body req)))

    :else (git-not-found "not a git endpoint")))

(defn make-handler
  [{:keys [host principal-context with-principal session-for]}]
  (fn [req]
    (when-let [parsed (parse-path (:uri req) (:query-string req))]
      (cond
        (not (enabled?))
        (git-not-found "the source-repository git surface is not enabled on this instance")

        :else
        (let [[_ secret] (basic-credential req)
              sess       (some-> secret session-for)]
          (cond
            (nil? sess) unauthorized

            :else
            (let [pctx (principal-context sess)
                  auth (tctx/authority pctx)]
              (with-principal pctx
                (fn [store schema branch]
                  (let [rid (resolve-repo-rid schema (:address parsed))]
                    (cond
                      (and rid (repos/repo-readable? auth schema rid)
                           (not (token-reaches? host sess rid)))
                      (token-for-another-repo rid)

                      :else (dispatch store auth schema branch rid parsed req))))))))))))

(defn- servable-repo?
  [auth schema rid]
  (boolean (and rid
                (or (rid/ontology-repo? rid) (contains? (:repositories schema) rid))
                (repos/repo-readable? auth schema rid))))

(defn commit-repo-files
  [{store :sutra/store auth :sutra/auth schema :sutra/schema branch :sutra/branch
    body :body-params :as req}]
  (let [rid (get-in req [:path-params :rid])]
    (cond
      (not (servable-repo? auth schema rid))
      (refusal/refused :refusal.branching/no-such-repository {})

      (rid/ontology-repo? rid)
      (refusal/refused :refusal.branching/the-ontology-is-changed-through {})

      (not (map? (:files body)))
      (refusal/refused :refusal.branching/a-commit-needs-files-path {})

      :else
      {:status 200
       :body   (push/commit-files! store schema rid (or branch ctl/trunk)
                                   (select-keys body [:files :message :author]))})))

(defn platform-files
  [{store :sutra/store auth :sutra/auth schema :sutra/schema :as req}]
  (let [rid (get-in req [:path-params :rid])]
    (if-not (servable-repo? auth schema rid)
      (refusal/refused :refusal.branching/no-such-repository {})
      {:status 200
       :body {:files (into (sorted-map)
                           (map (fn [[path ^bytes b]] [path (String. b "UTF-8")]))
                           (repos/platform-files store rid))}})))

(defn- tip
  [rid branch]
  (let [rs (ctx/repos)]
    (or (repos/ref-sha rs rid (repos/branch->ref (or branch ctl/trunk)))
        (repos/ref-sha rs rid (repos/branch->ref ctl/trunk)))))

(defn repo-tree
  [{auth :sutra/auth schema :sutra/schema branch :sutra/branch :as req}]
  (let [rid (get-in req [:path-params :rid])]
    (if-not (servable-repo? auth schema rid)
      (refusal/refused :refusal.branching/no-such-repository {})
      (let [sha (tip rid branch)]
        {:status 200
         :body   {:branch (or branch ctl/trunk) :commit sha
                  :files  (if sha
                            (vec (sort (keys (repos/read-tree (ctx/repos) rid sha))))
                            [])}}))))

(defn repo-file
  [{auth :sutra/auth schema :sutra/schema branch :sutra/branch :as req}]
  (let [rid  (get-in req [:path-params :rid])
        path (str (get-in req [:query-params "path"]))
        sha  (when (servable-repo? auth schema rid) (tip rid branch))
        blob (when sha (get-in (repos/read-tree (ctx/repos) rid sha) [path :blob]))]
    (cond
      (not (servable-repo? auth schema rid)) (refusal/refused :refusal.branching/no-such-repository {})
      (nil? blob) (refusal/refused :refusal.branching/no-file {} {:path path})
      :else {:status 200
             :body   {:path path :commit sha
                      :text (String. ^bytes (repos/blob-bytes (ctx/repos) rid blob) "UTF-8")}})))

(defn clone-token
  [{:keys [principal body-params] store :sutra/store auth :sutra/auth schema :sutra/schema
    branch :sutra/branch
    {{:keys [mint-clone-token! clone-ttl-seconds]} :sessions} :sutra/host :as req}]
  (let [rid  (get-in req [:path-params :rid])
        rids (into [rid] (map str) (:with body-params))
        bad  (remove #(servable-repo? auth schema %) rids)]
    (cond
      (nil? branch)
      (refusal/refused :refusal.branching/no-branch {})

      (seq bad)
      (refusal/refused :refusal.branching/no-such-repository {:repository (first bad)})

      :else
      (let [token (mint-clone-token!
                   store
                   {:username (:username principal)
                    :user-object-id (:user-id principal)
                    :branch (:branch principal)}
                   rids)]
        {:status 200
         :body {:token   token
                :expires-in clone-ttl-seconds
                :remote  (if (rid/ontology-repo? rid)
                           ""
                           (str "/git/r/" rid ".git"))
                :note    (str "Use any username and this token as the password."
                              " It expires in "
                              (quot clone-ttl-seconds (* 60 60 24))
                              " days and reaches only "
                              (if (next (distinct rids)) "these repositories."
                                  "this repository."))}}))))

(defn repo-checks
  [{store :sutra/store auth :sutra/auth schema :sutra/schema branch :sutra/branch :as req}]
  (let [rid (get-in req [:path-params :rid])
        sha (get-in req [:query-params "commit"])]
    (cond
      (nil? branch) (refusal/refused :refusal.branching/no-branch {})
      (not (servable-repo? auth schema rid)) (refusal/refused :refusal.branching/no-such-repository {})
      (str/blank? (str sha)) (refusal/refused :refusal.branching/checks-needs-a-commit-sha {})
      :else {:status 200 :body (repos/summary store rid sha)})))

(def module
  {:name        :git-http
   :jobs        [{:name :git-gc :every :git-gc-ms :tick push/gc-tick!}]
   :contributes {:http/routes
                 {:repo-checks         {:get repo-checks}
                  :repo-clone-token    {:post clone-token}
                  :repo-commit         {:post commit-repo-files}
                  :repo-platform-files {:get platform-files}
                  :repo-tree           {:get repo-tree}
                  :repo-file           {:get repo-file}}
                 :http/prefix-authority prefix-authority
                 :http/raw-handlers     {:git make-handler}}})
