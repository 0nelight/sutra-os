(ns ^:integration platform.flaw-git-projection-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.blobs.mem :as blobs-mem]
            [sutra.branching.retention :as governance]
            [sutra.store.control.collections :as collections]
            [sutra.repos.records :as git-rows]
            [sutra.repos.projection :as gp]
            [sutra.parser.authoring.standard :as standard]
            [sutra.types.world-rules :as rules]
            [sutra.store.mem :as mem]
            [sutra.repos.code-blobs :as cb]
            [sutra.repos.objects :as g]
            [sutra.store.world :as world]
            [support.system :as system]
            [sutra.governance.branches :as onto-branches])
  (:import (java.io File)))

(def ^:private repo-rid "ri.r.flaw")
(def ^:private actor {:principal "tester"})
(def ^:private ^:dynamic *store* nil)
(def ^:private ^:dynamic *cas* nil)
(def ^:private ^:dynamic *git-dir* nil)

(defn- repos [] (File. ^String *git-dir*))

(defn- rm-rf [^File f]
  (when (.isDirectory f) (run! rm-rf (.listFiles f)))
  (.delete f))

(defn- fresh-store []
  {:db    (mem/store mem/empty-seed {:collections collections/all :signature rules/signature})
   :clock #(java.util.Date.) :branch "main"})

(defn- store ([] @*store*) ([b] (assoc @*store* :branch (str b))))

(defn- with-store [f]
  (let [dir (doto (File/createTempFile "sutra-flaw-gitproj" "") (.delete) (.mkdirs))]
    (binding [*git-dir* (.getAbsolutePath dir)]
      (system/provide! :git/dir (constantly *git-dir*))
      (binding [*store* (atom (fresh-store))
                *cas*   (blobs-mem/blobs)]
        (try (f)
             (finally (g/forget-repo! (repos) repo-rid)
                      (system/provide! :git/dir nil)
                      (rm-rf dir)))))))

(use-fixtures :each with-store)

(defn- schema-with [code]
  {:projections    standard/projections
   :repositories {repo-rid {:repository-name :etl :type :transforms}}
   :transforms   {"ri.t.a" {:transform-name :a :language :python :code code :repository repo-rid}}})

(defn- write-on! [b m]
  (world/install! (world/at (store b) b) (first (cb/lift *cas* m))
                  {:actor actor :allow-schema-wipe? true}))

(defn- project! [b] (gp/project-branch! (store b) (repos) *cas* repo-rid b))
(defn- ref-of [b] (g/ref-sha (repos) repo-rid (str "refs/heads/" b)))
(defn- fork! [b] (onto-branches/create-branch! (store) {:parent "main" :name b}))

(deftest ref-regress
  (write-on! "main" (schema-with "print(1)"))
  (project! "main")
  (let [t0 (:tip-sha (git-rows/repo-anchor (store) repo-rid "main"))]
    (write-on! "main" (schema-with "print(2)"))
    (project! "main")
    (let [t1 (:tip-sha (git-rows/repo-anchor (store) repo-rid "main"))]
      (is (not= t0 t1))
      (testing "a writer that read the older tip cannot move the anchor back to it"
        (is (thrown? clojure.lang.ExceptionInfo
                     (git-rows/set-repo-anchor! (store) repo-rid "main"
                                                {:tip-sha t0 :expect-tip t0})))
        (is (= t1 (:tip-sha (git-rows/repo-anchor (store) repo-rid "main")))))
      (testing "the writer that read the current tip moves it"
        (git-rows/set-repo-anchor! (store) repo-rid "main" {:tip-sha t1 :expect-tip t1})
        (is (= t1 (:tip-sha (git-rows/repo-anchor (store) repo-rid "main"))))))))

(deftest revived-ref
  (write-on! "main" (schema-with "print(1)"))
  (project! "main")
  (fork! "feat-r")
  (write-on! "feat-r" (schema-with "print(2)"))
  (testing "a purge while the branch is being projected: neither ref nor anchor survive it"
    (let [inside (promise) go-on (promise)
          orig   g/update-ref!]
      (with-redefs [g/update-ref! (fn [store rid ref sha opts]
                                    (when (= ref "refs/heads/feat-r") (deliver inside true) @go-on)
                                    (orig store rid ref sha opts))]
        (let [proj  (future (try (project! "feat-r") (catch Exception e e)))
              _     (deref inside 10000 nil)
              purge (future (governance/purge-branch-data! (store) "feat-r"))]
          (Thread/sleep 300)
          (deliver go-on true)
          @proj @purge))
      (is (nil? (ref-of "feat-r")))
      (is (nil? (git-rows/raw-repo-anchor (store) repo-rid "feat-r")))))
  (testing "a projection of a purged branch writes nothing"
    (is (thrown? clojure.lang.ExceptionInfo (project! "feat-r")))
    (is (nil? (ref-of "feat-r")))
    (is (nil? (git-rows/raw-repo-anchor (store) repo-rid "feat-r")))))

(deftest git-reborn
  (write-on! "main" (schema-with "print(1)"))
  (project! "main")
  (fork! "feat-g")
  (write-on! "feat-g" (schema-with "print(old)"))
  (project! "feat-g")
  (let [old-tip (ref-of "feat-g")]
    (is (some? old-tip))
    (testing "the store branch retired, its ref and anchor left behind"
      (onto-branches/drop-branch! (store) "feat-g")
      (is (= old-tip (ref-of "feat-g"))))
    (testing "a new branch of that name starts from its parent, not from the old one's tip"
      (fork! "feat-g")
      (write-on! "feat-g" (schema-with "print(new)"))
      (project! "feat-g")
      (let [new-tip (ref-of "feat-g")]
        (is (some? new-tip))
        (is (not (g/descends-from? (repos) repo-rid new-tip old-tip))
            "the old branch's commit is not in its history")
        (is (= new-tip (:tip-sha (git-rows/repo-anchor (store) repo-rid "feat-g"))))))))
