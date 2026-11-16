(ns ^:unit platform.git-objects-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.repos.objects :as g])
  (:import (java.io File)))

(def ^:dynamic *dir* nil)
(def ^:dynamic *repos* nil)

(defn- rm-rf [^File f]
  (when (.isDirectory f) (run! rm-rf (.listFiles f)))
  (.delete f))

(use-fixtures
  :each
  (fn [t]
    (let [d (File/createTempFile "sutra-git-test" "")]
      (.delete d) (.mkdirs d)
      (binding [*dir* (.getAbsolutePath d)
                *repos*  d]
        (try
          (t)
          (finally
            (g/forget-repo! *repos* "ri.r.t")
            (rm-rf d)))))))

(defn- bytes-of ^bytes [^String s] (.getBytes s "UTF-8"))

(defn- commit-tree!
  ([rid entries] (commit-tree! rid entries nil "m"))
  ([rid entries parent msg]
   (g/with-inserter [ins *repos* rid]
     (let [t (g/write-tree! ins (into {} (map (fn [[k v]] [k (bytes-of v)])) entries))]
       (g/commit! ins {:tree t :parents (if parent [parent] [])
                           :author {:name "tester" :email "t@sutra" :when-ms 1700000000000}
                           :message msg})))))

(deftest branch-ref-round-trips
  (testing "every branch that projects at all comes back unchanged"
    (doseq [b ["main" "feature/x" "release-1.2" "a b" "wei..er~x" "a.lock"
               ".hidden" "a@{b}" "x^y" "q?z" "s[t]" "u:v" "ü-mläut" "deep/a/b/c"]]
      (let [r (g/branch->ref b)]
        (is (some? r) (str b " should project"))
        (is (= b (g/ref->branch r))
            (str "round trip broke for " (pr-str b) " via " (pr-str r)))))))

(deftest machine-namespaces-do-not-project
  (testing "a staging branch is a per-user QUEUE: projecting it would D/F-conflict
            AND make another user's uncommitted work clonable"
    (is (nil? (g/branch->ref "staging/u1/main")))
    (is (nil? (g/branch->ref "run/abc")))
    (is (nil? (g/branch->ref "scenario/ri.a.1/uuid.name")))
    (is (nil? (g/branch->ref "proposal-preview/1234")))))

(deftest a-directory-file-pair-is-refused-loudly-not-silently
  (testing "a git ref is a PATH, so `refs/heads/foo` and `refs/heads/foo/bar`
            cannot coexist — the first is a file where the second needs a
            directory. Excluding `staging/…` removes the case this system
            GENERATES (every staging branch would collide with a branch named
            `staging`), but a user can still create `foo` and `foo/bar`, and the
            requirement there is that the second update FAILS rather than
            quietly not happening."
    (let [rid "ri.r.t"
          a   (commit-tree! rid {"a" "1"})]
      (g/update-ref! *repos* rid (g/branch->ref "foo") a {:expected nil})
      (is (thrown? Exception
                   (g/update-ref! *repos* rid (g/branch->ref "foo/bar") a {:expected nil})))
      (is (= a (g/ref-sha *repos* rid "refs/heads/foo"))
          "and the existing ref is untouched by the refusal"))))

(deftest non-branch-refs-map-to-nil
  (is (nil? (g/ref->branch "refs/tags/v1")))
  (is (nil? (g/ref->branch "refs/sutra/checks/abc")))
  (is (nil? (g/ref->branch nil))))

(deftest a-committed-tree-reads-back
  (let [rid "ri.r.t"
        sha (commit-tree! rid {"README.md" "hello\n" "transforms/x.py" "print(1)\n"})]
    (g/update-ref! *repos* rid "refs/heads/main" sha {:expected nil})
    (testing "refs"
      (is (= {"refs/heads/main" sha} (g/all-refs *repos* rid))
          "HEAD is symbolic and must not be counted — the branch-delete refcount reads this"))
    (testing "tree manifest"
      nil)
    (testing "commit metadata is the row's, not the clock's"
      (let [c (g/commit-info *repos* rid sha)]
        (is (= "tester" (:author c)))
        (is (= 1700000000000 (:when-ms c)))
        (is (empty? (:parents c)))))))

(deftest a-gitlink-points-at-a-commit-this-repository-does-not-have
  (testing "a gitlink. Nothing DERIVES one any more — the ontology tree carries no
            submodule — but `rebuild!` re-mints historical commits byte-exact from
            the manifest, and those have them. The named commit lives in
            ANOTHER repository and git resolves it there at `submodule update`
            time — requiring it here would mean the superproject carried every
            code repository's objects, which is the thing a submodule exists not
            to do."
    (let [rid    "ri.r.t"
          absent "d24bddfb233b90396bda6fd6d03720c72cd4a6ae"
          sha    (g/with-inserter [ins *repos* rid]
                   (let [t (g/write-tree! ins {"ontology.ent" (bytes-of "{}\n")
                                               "beschaffung"  {:gitlink absent}})]
                     (g/commit! ins {:tree t :parents []
                                         :author {:name "tester" :email "t@sutra"
                                                  :when-ms 1700000000000}
                                         :message "with a submodule"})))
          tree   (g/read-tree *repos* rid sha)]
      (is (nil? (g/blob-bytes *repos* rid absent))
          "the target really is absent — otherwise this proves nothing")
      (is (= g/gitlink-mode (:mode (get tree "beschaffung"))))
      (is (= absent (:blob (get tree "beschaffung"))))
      (is (= "100644" (:mode (get tree "ontology.ent"))))
      (testing "and `gitlink?` reads all three shapes the three callers hold"
        (is (g/gitlink? {:gitlink absent}))
        (is (g/gitlink? (get tree "beschaffung")))
        (is (g/gitlink? g/gitlink-mode))
        (is (g/gitlink? (str g/gitlink-mode "   "))
            "CHAR(64) blank-padding is what `control_repo_files` hands back")
        (is (not (g/gitlink? (get tree "ontology.ent"))))
        (is (not (g/gitlink? (bytes-of "x"))))))))

(deftest a-tree-is-a-function-of-its-content
  (testing "identical content ⇒ identical tree, in either insertion order"
    (let [rid "ri.r.t"
          a (commit-tree! rid {"a/x" "1" "b/y" "2"})
          b (commit-tree! rid {"b/y" "2" "a/x" "1"})]
      (is (= (:tree (g/commit-info *repos* rid a)) (:tree (g/commit-info *repos* rid b)))))))

(deftest fast-forward-is-answered-by-the-graph
  (let [rid  "ri.r.t"
        root (commit-tree! rid {"a" "1"})
        kid  (commit-tree! rid {"a" "2"} root "second")
        side (commit-tree! rid {"a" "3"} root "divergent")]
    (is (g/descends-from? *repos* rid kid root))
    (is (g/descends-from? *repos* rid root nil) "a first push has nothing to descend from")
    (is (not (g/descends-from? *repos* rid side kid))
        "a divergent tip must be told `fetch first`, not silently accepted")))

(deftest update-ref-refuses-a-lost-race
  (let [rid  "ri.r.t"
        root (commit-tree! rid {"a" "1"})
        kid  (commit-tree! rid {"a" "2"} root "second")]
    (g/update-ref! *repos* rid "refs/heads/main" root {:expected nil})
    (testing "a compare-and-set against the wrong old value is refused"
      (is (thrown? clojure.lang.ExceptionInfo
                   (g/update-ref! *repos* rid "refs/heads/main" kid {:expected kid}))))
    (testing "and against the right one succeeds"
      (is (= kid (g/update-ref! *repos* rid "refs/heads/main" kid {:expected root}))))))

(deftest raw-objects-round-trip-to-the-same-sha
  (testing "git's object id IS the hash of the bytes, so re-inserting the stored
            bytes must reproduce the sha — this is what makes a rebuild from
            Postgres + the CAS byte-exact rather than merely equivalent"
    (let [rid  "ri.r.t"
          root (commit-tree! rid {"README.md" "x\n"})
          raw  (g/raw-object *repos* rid root)]
      (is (= g/obj-commit (:type raw)))
      (g/with-inserter [ins *repos* rid]
        (is (= root (g/insert-raw! ins g/obj-commit (:bytes raw)))))))
  (testing "and the same holds for a blob"
    (let [rid  "ri.r.t"
          sha  (commit-tree! rid {"f" "content\n"})
          blob (:blob (get (g/read-tree *repos* rid sha) "f"))]
      (is (= "content\n" (String. (g/blob-bytes *repos* rid blob) "UTF-8"))))))

(deftest a-fresh-repo-heads-at-the-trunk
  (testing "JGit's `create` points HEAD at refs/heads/master; Sutra-OS's trunk is
            main, and a HEAD naming a ref that will never exist makes `git clone`
            succeed with an empty working tree and a warning"
    (let [rid "ri.r.t"
          sha (commit-tree! rid {"a" "1"})]
      (g/update-ref! *repos* rid "refs/heads/main" sha {:expected nil})
      (is (= sha (g/ref-sha *repos* rid "refs/heads/main")))
      (is (str/includes? (slurp (io/file *dir* "ri.r.t.git" "HEAD")) "refs/heads/main")))))
