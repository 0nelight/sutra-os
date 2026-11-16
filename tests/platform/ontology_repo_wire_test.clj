(ns ^:api platform.ontology-repo-wire-test
  (:require [sutra.catalog.grants :as grants]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.branching.http.git :as git-http]
            [sutra.serve.server :as server]
            [sutra.store.access.scope :as ctx]
            [sutra.repos.objects :as g]
            [sutra.access.principals :as pr]
            [sutra.types.rid :as rid]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]
            [support.pod :as pod])
  (:import (java.util Base64)))

(use-fixtures :once pod/with-mem-pod-auth)

(defn- basic [token]
  (str "Basic " (.encodeToString (Base64/getEncoder)
                                 (.getBytes (str "sutra:" token) "UTF-8"))))

(defn- GET
  [uri query token]
  (server/app {:uri uri :query-string query :request-method :get
               :headers (cond-> {} token (assoc "authorization" (basic token)))}))

(deftest the-instance-root-is-a-git-repository
  (let [admin (-> (api/post! :login {:username grants/admin-username
                                     :password pr/default-admin-password})
                  :body :token)
        _     (is (some? admin) "admin bootstrap login")
        token (tx/with-token admin
                (:token (:body (api/post! :repo-clone-token {:rid rid/ontology-repo-rid} {}))))]

    (tx/with-token admin
      (is (= 200
             (:status (api/post! :install
                                 {:schema {:projects {:geraete {}}
                                           :objects
                                             {:widget {:project :geraete
                                                       :primary-key :id :attributes [[:id :string]]
                                                       :plural "Widgets"}}}})))
          "install a type to publish")
      (is (= 200 (:status (api/post! :staging)))))

    (testing "a clone token is mintable for a repository with no :repositories row
              — `contains?` on that map was the check, and it would 404 here"
      (is (some? token)))

    (testing "and it names the BARE INSTANCE ROOT, not the RID form and not the
              readable alias. The rename-stable address buys nothing for a
              repository that cannot be renamed, and the root is the address the
              feature is named after — the one a hand-typed
              `git clone http://<host>` leaves behind"
      (is (= ""
             (tx/with-token admin
               (:remote (:body (api/post! :repo-clone-token {:rid rid/ontology-repo-rid} {})))))))

    (binding [git-http/*enabled-override* true]
      (testing "the ref advertisement git asks for FIRST, at the bare root"
        (let [r (GET "/info/refs" "service=git-upload-pack" token)]
          (is (= 200 (:status r)))
          (is (= "application/x-git-upload-pack-advertisement"
                 (get-in r [:headers "Content-Type"]))
              "git parses the content-type before the body; a text/html 200 from
               the SPA fallback is exactly the 'not a valid repository' failure")
          (let [body (String. ^bytes (:body r) "UTF-8")]
            (is (str/includes? body "git-upload-pack")
                "the service line — the advertisement's first packet")
            (is (str/includes? body "refs/heads/main")
                "and a real ref, which only exists because the READ path advanced
                 the projection for a repository nothing ever created"))))

      (testing "the advertised tip is a commit whose tree IS the type layer —
                the advertisement alone would be satisfied by an empty repo with
                a ref, which is the failure mode worth ruling out"
        (let [tip  (g/ref-sha (ctx/repos) rid/ontology-repo-rid "refs/heads/main")
              tree (g/read-tree (ctx/repos) rid/ontology-repo-rid tip)
              ent  (some-> (get tree "geraete.objects.ent") :blob
                           (->> (g/blob-bytes (ctx/repos) rid/ontology-repo-rid))
                           ^bytes (String. "UTF-8"))]
          (is (contains? tree "ontology.ent"))
          (is (contains? tree "geraete.objects.ent")
              "a type lives in its project's text")
          (is (str/includes? (str ent) "widget") "the type just installed is IN the checkout")))

      (testing "the same repository through its readable alias — one repo, two
                addresses, because nginx rewrites the root form into this one"
        (let [r (GET "/git/ontology.git/info/refs" "service=git-upload-pack" token)]
          (is (= 200 (:status r)))))

      (testing "a code repository does NOT appear in the ontology's tree"
        (tx/with-token admin
          (is (= 200 (:status (api/mutate :create-project
                                          {:name :beschaffung} {:stage? true}))))
          (is (= 200 (:status (api/post! :staging))))
          (is (= 200 (:status (api/mutate :create-repository
                                          {:name :beschaffung :project :beschaffung}
                                          {:stage? true}))))
          (is (= 200 (:status (api/post! :staging)))))

        (is (= 200 (:status (GET "/info/refs" "service=git-upload-pack" token))))

        (let [code-rid (some (fn [[rid s]]
                               (when (= :beschaffung (:repository-name s)) rid))
                             (:repositories (:body (tx/with-token admin (api/get! :schema)))))
              tip      (g/ref-sha (ctx/repos) rid/ontology-repo-rid "refs/heads/main")
              tree     (g/read-tree (ctx/repos) rid/ontology-repo-rid tip)]
          (is (some? code-rid))
          (is (not (contains? tree "beschaffung"))
              "no pointer: `ontology@X` may not name `beschaffung@Y`, or one
               logical change costs two commits in a fixed order")
          (is (not (contains? tree ".gitmodules"))
              "and no mapping — the layout is the CLIENT's, not the tree's")

          (testing "the ontology's own token does not reach the code repository"
            (is (= 401 (:status (GET "/git/beschaffung/beschaffung.git/info/refs"
                                  "service=git-upload-pack" token)))))

          (testing "one token naming both reaches the code repository's address, which
                    is what `sutra clone ontology` needs to lay the project tree down
                    from one credential. This is also the read that advances THAT
                    repository's projection — the ontology's read no longer does it
                    for anybody else."
            (let [both (tx/with-token admin
                         (:token (:body (api/post! :repo-clone-token {:rid rid/ontology-repo-rid}
                                                   {:with [code-rid]}))))]
              (is (= 200 (:status (GET "/git/beschaffung/beschaffung.git/info/refs"
                                    "service=git-upload-pack" both))))
              (is (some? (g/ref-sha (ctx/repos) code-rid "refs/heads/main"))
                  "its own advertisement projected it")
              (is (= 200 (:status (GET "/info/refs" "service=git-upload-pack" both))))))))

      (testing "no credential is a 401 with git's own challenge, never a 200"
        (let [r (GET "/info/refs" "service=git-upload-pack" nil)]
          (is (= 401 (:status r)))
          (is (str/starts-with? (str (get-in r [:headers "WWW-Authenticate"])) "Basic"))))

      (testing "and the SPA still owns everything else at the root"
        (doseq [uri ["/" "/index.html" "/objects/info/packs"]]
          (let [r (GET uri nil token)]
            (is (not= "application/x-git-upload-pack-advertisement"
                      (get-in r [:headers "Content-Type"]))
                (str uri " must not be claimed by the git surface"))))))

    (testing "with the tier off the root 404s and SAYS SO, rather than falling
              through to the SPA and letting git blame the server for having no
              git surface at all"
      (binding [git-http/*enabled-override* false]
        (let [r (GET "/info/refs" "service=git-upload-pack" token)]
          (is (= 404 (:status r)))
          (is (str/includes? (str (:body r)) "not enabled")))))))
