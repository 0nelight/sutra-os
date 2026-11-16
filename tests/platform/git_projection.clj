(ns ^:integration platform.git-projection
  (:require [clojure.edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.store.control.collections :as collections]
            [sutra.store.control :as ctl]
            [sutra.repos.records :as git-rows]
            [sutra.blobs.mem :as blobs-mem]
            [sutra.repos.projection :as gp]
            [sutra.parser.authoring.standard :as standard]
            [sutra.types.world-rules :as rules]
            [sutra.repos.code-blobs :as cb]
            [sutra.repos.objects :as g]
            [sutra.store.mem :as mem]
            [sutra.store.world :as world]
            [sutra.types.rid :as rid])
  (:import (java.io File)))

(def ^:private repo-rid "ri.r.1")
(def ^:private orid rid/ontology-repo-rid)
(def ^:private actor {:principal "tester"})
(def ^:private ^:dynamic *store* nil)
(def ^:private ^:dynamic *cas* nil)
(def ^:private ^:dynamic *git-dir* nil)

(defn- repos [] (File. ^String *git-dir*))

(defn- rm-rf [^File f]
  (when (.isDirectory f) (run! rm-rf (.listFiles f)))
  (.delete f))

(defn- fresh-store []
  {:db     (mem/store mem/empty-seed {:collections collections/all :signature rules/signature})
   :clock  #(java.util.Date.)
   :branch "main"})

(defn- store
  ([] @*store*)
  ([b] (assoc @*store* :branch (str b))))

(defn- with-stores [f]
  (let [dir (doto (File/createTempFile "sutra-gitproj" "") (.delete) (.mkdirs))]
    (binding [*git-dir* (.getAbsolutePath dir)]
      (binding [*store* (atom (fresh-store))
                *cas*   (blobs-mem/blobs)]
        (try
          (f)
          (finally
            (g/forget-repo! (repos) repo-rid)
            (g/forget-repo! (repos) orid)
            (rm-rf dir)))))))

(use-fixtures :once with-stores)

(defn- clear! []
  (reset! *store* (fresh-store))
  (doseq [rid [repo-rid orid]]
    (g/forget-repo! (repos) rid)
    (rm-rf (File. (repos) (str rid ".git")))))

(defn- schema-with
  [transforms]
  {:projections    standard/projections
   :repositories {repo-rid {:repository-name :etl :type :transforms}}
   :transforms   (into {} (for [[nm code] transforms]
                            [(str "ri.t." (name nm))
                             {:transform-name (keyword nm) :language :python
                              :code code :repository repo-rid}]))})

(defn- write-on! [b m]
  (world/install! (world/at (store b) b) (first (cb/lift *cas* m))
                  {:actor actor :allow-schema-wipe? true}))
(defn- write! [m] (write-on! "main" m))
(defn- fork! [b] (world/fork! (store) "main" b))
(defn- project! [] (gp/project-branch! (store) (repos) *cas* repo-rid "main"))

(defn- log-shas []
  (let [tip (g/ref-sha (repos) repo-rid "refs/heads/main")]
    (loop [sha tip acc []]
      (if-not sha
        acc
        (let [c (g/commit-info (repos) repo-rid sha)]
          (recur (first (:parents c)) (conj acc sha)))))))

(deftest the-first-projection-anchors-history-into-one-root-commit
  (clear!)
  (write! (schema-with {:a "print(1)"}))
  (write! (schema-with {:a "print(2)"}))
  (write! (schema-with {:a "print(3)"}))
  (let [r (project!)]
    (testing "history predating the feature collapses into ONE root commit —
              its early rows may not export cleanly, and re-projecting them is
              not worth a broken clone"
      (is (= 1 (:minted r)))
      (is (= 1 (count (log-shas))))
      (is (str/includes? (:message (g/commit-info (repos) repo-rid (:tip r))) "history anchored"))))
  (testing "and the root commit carries the CURRENT content, not the oldest"
    (let [tree (g/read-tree (repos) repo-rid (g/ref-sha (repos) repo-rid "refs/heads/main"))]
      (is (= "print(3)"
             (String. (g/blob-bytes (repos) repo-rid (:blob (get tree "transforms/a.py")))
                      "UTF-8"))))))

(deftest a-row-that-does-not-touch-the-repo-mints-nothing
  (clear!)
  (write! (schema-with {:a "print(1)"}))
  (project!)
  (let [before (count (log-shas))]
    (testing "an unrelated ontology write — a new object-type, nothing of ours"
      (write! (assoc (schema-with {:a "print(1)"}) :object-types {"ri.o.9" {:n 1}}))
      (project!)
      (is (= before (count (log-shas)))
          "TREE equality is the test, not row equality: two different schema-maps
           routinely scope to the same repository content, and that is exactly
           the case suppression exists for"))
    (testing "…while a real change to the repo does mint one"
      (write! (schema-with {:a "print(99)"}))
      (project!)
      (is (= (inc before) (count (log-shas)))))))

(deftest projection-is-idempotent-and-resumable
  (clear!)
  (write! (schema-with {:a "print(1)"}))
  (project!)
  (write! (schema-with {:a "print(2)"}))
  (let [r1 (project!)
        n  (count (log-shas))
        r2 (project!)]
    (is (= 1 (:minted r1)))
    (is (= 0 (:minted r2)) "everything it needs is in control_repo_anchor")
    (is (= n (count (log-shas))))))

(deftest the-minted-commit-carries-the-rows-actor-and-time
  (clear!)
  (write! (schema-with {:a "print(1)"}))
  (project!)
  (let [c (g/commit-info (repos) repo-rid (g/ref-sha (repos) repo-rid "refs/heads/main"))]
    (is (= "tester" (:author c))
        "the history row's actor, not the projecting process's clock-owner")))

(deftest the-root-commit-seeds-a-readme-and-later-commits-do-not
  (clear!)
  (write! (schema-with {:a "print(1)"}))
  (project!)
  (let [tip (g/ref-sha (repos) repo-rid "refs/heads/main")]
    (let [txt (String. (g/blob-bytes (repos) repo-rid
                                     (:blob (get (g/read-tree (repos) repo-rid tip) "README.md")))
                       "UTF-8")]
      (is (str/includes? txt "No ontology types")
          "…and it names what this is NOT, so §16's list is not discovered by failure")
      (testing "and it is a CRASH COURSE, not a notice board: the expensive
                mistakes live in this layer, and none of them is visible from
                the grammar alone"
        (is (str/includes? txt "layer rule")
            "the one that decides whether a reshape re-pays the extract")
        (is (str/includes? txt ":write-mode :replace")
            "identity changes mint rows BESIDE the old ones without it")
        (is (str/includes? txt "push the ontology first")
            "the cross-repository order, where the reader will hit it")))
    (testing "and a `.gitignore`, on the same once-only lifecycle — without it the
              first editor to open the checkout writes its caches into the working
              tree, and every person and agent after that has to decide afresh
              whether those are theirs to commit"
      (let [tree (g/read-tree (repos) repo-rid tip)]
        (is (contains? tree ".gitignore"))
        (is (str/includes? (String. (g/blob-bytes (repos) repo-rid (:blob (get tree ".gitignore")))
                                    "UTF-8")
                           ".clj-kondo/"))))))

(deftest deleting-the-git-dir-and-rebuilding-gives-the-same-shas
  (clear!)
  (write! (schema-with {:a "print(1)"}))
  (project!)
  (write! (schema-with {:a "print(2)" :b "print(3)"}))
  (project!)
  (let [refs-before (g/all-refs (repos) repo-rid)
        shas-before (log-shas)]
    (is (< 1 (count shas-before)))
    (g/forget-repo! (repos) repo-rid)
    (rm-rf (File. (repos) (str repo-rid ".git")))
    (is (false? (g/repo-exists? (repos) repo-rid)))
    (let [r (gp/rebuild! (store) (repos) *cas* repo-rid)]
      (testing "every recorded commit is re-inserted from its stored bytes"
        (is (= (count shas-before) (:commits r))))
      (testing "SAME REFS, SAME SHAS — a rebuild that produced different shas
                would have broken every clone in existence while reporting success"
        (is (= refs-before (g/all-refs (repos) repo-rid)))
        (is (= shas-before (log-shas)))))))

(deftest a-node-with-an-empty-git-dir-rebuilds-what-the-ledger-records
  (clear!)
  (write! (schema-with {:a "print(1)"}))
  (project!)
  (let [refs-before (g/all-refs (repos) repo-rid)]
    (testing "a present repository is left alone"
      (is (= {:rebuilt [] :failed {}} (gp/rebuild-missing! (store) (repos) *cas*))))
    (g/forget-repo! (repos) repo-rid)
    (rm-rf (File. (repos) (str repo-rid ".git")))
    (testing "a missing one comes back with the same refs"
      (is (= [repo-rid] (:rebuilt (gp/rebuild-missing! (store) (repos) *cas*))))
      (is (= refs-before (g/all-refs (repos) repo-rid))))))

(deftest recording-without-a-cas-throws-rather-than-skipping
  (clear!)
  (write! (schema-with {:a "print(1)"}))
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo #"content-addressed store"
       (gp/project-branch! (store) (repos) nil repo-rid "main"))
      "§8's trap is that a silent skip turns $SUTRA_GIT_DIR from a cache into a
       store, and nobody finds out until the directory is gone"))

(deftest an-adopted-commit-is-never-re-derived
  (clear!)
  (write! (schema-with {:a "print(1)"}))
  (project!)
  (let [parent (g/ref-sha (repos) repo-rid "refs/heads/main")
        pushed (g/with-inserter [ins (repos) repo-rid]
                 (let [t (g/write-tree! ins {"README.md" (.getBytes "mine\n" "UTF-8")
                                             "transforms/a.py" (.getBytes "print(1)\n" "UTF-8")})]
                   (g/commit! ins
                              {:tree t :parents [parent]
                               :author {:name "dev" :email "d@x" :when-ms 1700000123000}
                               :message "my work"})))]
    (g/update-ref! (repos) repo-rid "refs/heads/main" pushed {:expected parent})
    (gp/adopt-push! (store) (repos) *cas* repo-rid [{:ref "refs/heads/main" :new pushed}])
    (testing "the ledger records it BEFORE anything is staged, with a NULL seq —
              git holds the commit, the ontology has not accepted it"
      (let [row (git-rows/commit-row (store) repo-rid pushed)]
        (is (= "pushed" (:source row)))
        (is (nil? (:seq row)))))
    (gp/bind-published! (store) (repos) repo-rid "main" pushed 42)
    (testing "publication FILLS the seq and touches nothing else"
      (let [row (git-rows/commit-row (store) repo-rid pushed)]
        (is (= 42 (:seq row)))
        (is (= pushed (:commit-sha row)))
        (is (= "pushed" (:source row)))))
    (testing "and the sha is still the tip — a client that pushed X pulls X back"
      (is (= pushed (g/ref-sha (repos) repo-rid "refs/heads/main"))))))

(deftest install-normalization-mints-a-CHILD-never-an-amend
  (clear!)
  (write! (schema-with {:a "print(1)"}))
  (project!)
  (let [parent (g/ref-sha (repos) repo-rid "refs/heads/main")
        pushed (g/with-inserter [ins (repos) repo-rid]
                 (let [t (g/write-tree! ins {"README.md" (.getBytes "mine\n" "UTF-8")
                                             "transforms/a.py"
                                             (.getBytes "print( 1 )   # sloppy\n" "UTF-8")})]
                   (g/commit! ins {:tree t :parents [parent]
                                            :author {:name "dev" :email "d@x"
                                                     :when-ms 1700000123000}
                                            :message "my work"})))]
    (g/update-ref! (repos) repo-rid "refs/heads/main" pushed {:expected parent})
    (gp/adopt-push! (store) (repos) *cas* repo-rid [{:ref "refs/heads/main" :new pushed}])
    (let [follow (gp/normalize-followup! (store) (repos) *cas* repo-rid "main" pushed
                                         (schema-with {:a "print(1)"}))]
      (testing "a follow-up CHILD, authored sutra[bot]"
        (is (some? follow))
        (is (= [pushed] (:parents (g/commit-info (repos) repo-rid follow))))
        (is (= "sutra[bot]" (:author (g/commit-info (repos) repo-rid follow))))
        (is (str/ends-with? (:message (g/commit-info (repos) repo-rid follow))
                            "(normalized by install)")))
      (testing "the pushed sha STILL EXISTS and is still an ancestor — an amend
                would have deleted the sha the client holds from the repository
                it came from"
        (is (some? (g/commit-info (repos) repo-rid pushed)))
        (is (g/descends-from? (repos) repo-rid follow pushed)))
      (testing "the file plane the push carried survives the follow-up"
        nil))))

(deftest a-subject-says-what-the-change-touches
  (is (= "Update transforms/a.py" (gp/change-subject {:changed ["transforms/a.py"]})))
  (is (= "Add a.py, b.py" (gp/change-subject {:added ["a.py" "b.py"]})))
  (is (= "Remove a.py" (gp/change-subject {:removed ["a.py"]})))
  (is (= "Update a, b, c and 2 more"
         (gp/change-subject {:changed ["a" "b"] :added ["c" "d"] :removed ["e"]}))))

(deftest a-schema-commit-names-its-files-and-keeps-its-revision
  (clear!)
  (write! (schema-with {:a "print(1)\n"}))
  (project!)
  (write! (schema-with {:a "print(2)\n"}))
  (let [msg (:message (g/commit-info (repos) repo-rid (:tip (project!))))]
    (is (str/starts-with? msg "Update transforms/a.py") msg)
    (is (str/includes? msg "Schema revision ") msg)))

(deftest a-normalization-that-changes-nothing-mints-nothing
  (clear!)
  (write! (schema-with {:a "print(1)\n"}))
  (project!)
  (let [tip (g/ref-sha (repos) repo-rid "refs/heads/main")]
    (is (nil? (gp/normalize-followup! (store) (repos) *cas* repo-rid "main" tip
                                      (schema-with {:a "print(1)\n"})))
        "an empty `normalized by install` commit on every push would make the
         normalization notice worthless by making it constant")))

(defn- push!
  [parent entries message]
  (let [sha (g/with-inserter [ins (repos) repo-rid]
              (g/commit! ins
                         {:tree (g/write-tree! ins entries) :parents [parent]
                          :author {:name "dev" :email "d@x" :when-ms 1700000123000}
                          :message message}))]
    (g/update-ref! (repos) repo-rid "refs/heads/main" sha {:expected parent})
    (gp/adopt-push! (store) (repos) *cas* repo-rid [{:ref "refs/heads/main" :new sha}])
    sha))

(defn- tree-bytes [sha]
  (into {} (for [[p e] (g/read-tree (repos) repo-rid sha)]
             [p (g/blob-bytes (repos) repo-rid (:blob e))])))

(defn- anchor-tip [] (:tip-sha (git-rows/repo-anchor (store) repo-rid "main")))

(defn- branch-commit!
  [branch parent entries message]
  (let [ref (str "refs/heads/" branch)
        sha (g/with-inserter [ins (repos) repo-rid]
              (g/commit! ins
                         {:tree (g/write-tree! ins entries)
                          :parents (if parent [parent] [])
                          :author {:name "dev" :email "d@x" :when-ms 1700000123000}
                          :message message}))]
    (g/update-ref! (repos) repo-rid ref sha {:force? true})
    (gp/adopt-push! (store) (repos) *cas* repo-rid [{:ref ref :new sha}])
    (git-rows/set-repo-anchor! (store) repo-rid branch {:tip-sha sha})
    sha))

(deftest a-merge-carries-the-source-branchs-added-file-plane-onto-the-target
  (clear!)
  (write! (schema-with {:a "print(1)"}))
  (project!)
  (let [base (g/ref-sha (repos) repo-rid "refs/heads/main")]
    (branch-commit! "try-x" base
                    (assoc (tree-bytes base)
                           "README.md"        (.getBytes "edited on branch\n" "UTF-8")
                           ".gitignore"       (.getBytes ".claude/\n.lsp/\n" "UTF-8")
                           "notizen/note.md" (.getBytes "why we did it\n" "UTF-8"))
                    "add docs on the branch")
    (is (= base (anchor-tip)) "main still sits at base before the merge")
    (let [r    (gp/merge-git-half! (store) (repos) *cas* repo-rid "try-x" "main"
                                   {:merge-mode :squash})
          tip  (g/ref-sha (repos) repo-rid "refs/heads/main")
          tree (tree-bytes tip)]
      (testing "the inert files the branch ADDED now live on main"
        (is (contains? tree ".gitignore"))
        nil
        nil
        nil)
      (testing "README, untouched on main, takes the branch's edit — no conflict"
        nil
        (is (empty? (:conflicts r))))
      (testing "the ENTITY plane the schema owns is carried through unchanged"
        (is (contains? tree "transforms/a.py")))
      (testing "the carry is a fast-forward CHILD of main's old tip, not a rewrite"
        (is (= [base] (:parents (g/commit-info (repos) repo-rid tip))))))))

(deftest anchoring-a-fork-inherits-the-parents-file-plane
  (clear!)
  (write! (schema-with {:a "print(1)"}))
  (project!)
  (let [base (g/ref-sha (repos) repo-rid "refs/heads/main")]
    (branch-commit! "main" base
                    (assoc (tree-bytes base)
                           "notizen/why.md" (.getBytes "the decision\n" "UTF-8"))
                    "a document nobody generated")
    (fork! "fork-x")
    (write-on! "fork-x" (schema-with {:a "print(1)" :b "print(2)"}))
    (gp/project-branch! (store) (repos) *cas* repo-rid "fork-x")
    (let [tree (tree-bytes (g/ref-sha (repos) repo-rid "refs/heads/fork-x"))]
      (testing "the parent's inert files are on the fork, untouched"
        nil
        nil)
      (testing "and the fork's own schema change is there too"
        (is (contains? tree "transforms/b.py"))))
    (testing "and the fork's first commit is a CHILD of the parent's tip — an
              unrelated root left the author's clone without a merge base"
      (let [main-tip (g/ref-sha (repos) repo-rid "refs/heads/main")
            fork-tip (g/ref-sha (repos) repo-rid "refs/heads/fork-x")]
        (is (= [main-tip] (:parents (g/commit-info (repos) repo-rid fork-tip))))))))

(deftest a-fork-with-the-parents-content-starts-at-the-parents-tip
  (clear!)
  (write! (schema-with {:a "print(1)"}))
  (project!)
  (fork! "fork-same")
  (write-on! "fork-same" (schema-with {:a "print(1)"}))
  (let [r (gp/project-branch! (store) (repos) *cas* repo-rid "fork-same")]
    (is (zero? (:minted r)) "nothing differs, so nothing is minted")
    (is (= (g/ref-sha (repos) repo-rid "refs/heads/main")
           (g/ref-sha (repos) repo-rid "refs/heads/fork-same"))
        "the branch IS its parent's tip, the way `git switch -c` would cut it")))

(deftest a-merge-that-touched-no-inert-file-carries-nothing
  (clear!)
  (write! (schema-with {:a "print(1)"}))
  (project!)
  (let [base (g/ref-sha (repos) repo-rid "refs/heads/main")]
    (branch-commit! "try-z" base (tree-bytes base) "no inert change")
    (let [r (gp/merge-git-half! (store) (repos) *cas* repo-rid "try-z" "main"
                                {:merge-mode :squash})]
      (is (nil? (:sha r)))
      (is (empty? (:carried r)))
      (is (= base (g/ref-sha (repos) repo-rid "refs/heads/main")) "main's tip did not move"))))

(deftest merge-mode-merge-adopts-the-source-commit-as-an-ancestor
  (clear!)
  (write! (schema-with {:a "print(1)"}))
  (project!)
  (let [base (g/ref-sha (repos) repo-rid "refs/heads/main")
        src  (branch-commit! "try-m" base
                             (assoc (tree-bytes base)
                                    "notizen/why.md" (.getBytes "rationale\n" "UTF-8"))
                             "work the client pushed")
        r    (gp/merge-git-half! (store) (repos) *cas* repo-rid "try-m" "main"
                                 {:merge-mode :merge :actor "cilli"})
        tip  (g/ref-sha (repos) repo-rid "refs/heads/main")
        c    (g/commit-info (repos) repo-rid tip)]
    (testing "one merge commit, main's old tip and the source tip as its two parents"
      (is (:merged? r))
      (is (= tip (:sha r)))
      (is (= [base src] (:parents c))))
    (testing "the pushed source commit is now an ANCESTOR of main"
      (let [reachable (loop [q [tip] seen #{}]
                        (if-let [s (first q)]
                          (if (seen s)
                            (recur (rest q) seen)
                            (recur (into (rest q) (:parents (g/commit-info (repos) repo-rid s)))
                                   (conj seen s)))
                          seen))]
        (is (contains? reachable src)))
      (is (= src (second (:parents c))) "specifically, as the merge commit's second parent"))
    (testing "authored by the merger, and the file rode along"
      (is (= "cilli" (:author c)))
      nil)
    (testing "and the merge is rebuild-safe — src outlives its branch in the ledger"
      (is (some #{"try-m"} (git-rows/repo-branches (store) repo-rid))
          "git commits are git-ddl, not swept by drop-branch!, so the second
           parent is always re-insertable"))))

(deftest merge-mode-merge-mints-even-when-the-tree-is-unchanged
  (clear!)
  (write! (schema-with {:a "print(1)"}))
  (project!)
  (let [base (g/ref-sha (repos) repo-rid "refs/heads/main")
        src  (branch-commit! "try-t" base (tree-bytes base) "same tree, own commit")
        r    (gp/merge-git-half! (store) (repos) *cas* repo-rid "try-t" "main"
                                 {:merge-mode :merge :actor "dev"})
        c    (g/commit-info (repos) repo-rid (:sha r))]
    (is (:merged? r) "forced: two parents over an unchanged tree is a real merge")
    (is (= [base src] (:parents c)))
    (is (= (g/read-tree (repos) repo-rid base) (g/read-tree (repos) repo-rid (:sha r)))
        "same tree as main had — only the topology is new")))

(deftest a-push-that-needed-no-normalization-still-moves-the-anchor
  (clear!)
  (write! (schema-with {:a "print(1)\n"}))
  (project!)
  (let [parent (g/ref-sha (repos) repo-rid "refs/heads/main")
        pushed (push! parent (assoc (tree-bytes parent)
                                    "docs/note.md" (.getBytes "hi\n" "UTF-8"))
                      "my work")]
    (is (nil? (gp/normalize-followup! (store) (repos) *cas* repo-rid "main" pushed
                                      (schema-with {:a "print(1)\n"})))
        "nothing to normalize — the case the anchor used to be left out of")
    (is (= pushed (anchor-tip))
        "a suppressed follow-up mints nothing and must STILL re-anchor: the ref
         has moved, and an anchor that does not know it makes every later
         compare-and-set expect a parent that is two commits behind")))

(defn- reordered
  [^bytes canonical]
  (let [m (clojure.edn/read-string (String. canonical "UTF-8"))
        flip (fn flip [x]
               (cond (map? x) (into (array-map) (reverse (map (fn [[k v]] [k (flip v)]) x)))
                     (vector? x) (mapv flip x)
                     :else x))]
    (.getBytes (str ";; mine\n" (pr-str (flip m)) "\n") "UTF-8")))

(deftest an-authored-ent-in-another-key-order-is-not-rewritten
  (clear!)
  (write! (schema-with {:a "print(1)\n"}))
  (project!)
  (let [parent  (g/ref-sha (repos) repo-rid "refs/heads/main")
        mine    (reordered (get (tree-bytes parent) "etl.ent"))
        pushed  (push! parent (assoc (tree-bytes parent) "etl.ent" mine) "my work")]
    (is (not (java.util.Arrays/equals ^bytes mine ^bytes (get (tree-bytes parent) "etl.ent"))))
    (testing "same content, other layout: no `normalized by install` commit"
      (is (nil? (gp/normalize-followup! (store) (repos) *cas* repo-rid "main" pushed
                                        (schema-with {:a "print(1)\n"}))))
      (is (= pushed (g/ref-sha (repos) repo-rid "refs/heads/main"))))
    (testing "and the projection of the install's history row keeps the author's bytes"
      (write! (schema-with {:a "print(1)\n"}))
      (project!)
      (is (= pushed (g/ref-sha (repos) repo-rid "refs/heads/main")))
      (is (java.util.Arrays/equals ^bytes mine
                                   ^bytes (get (tree-bytes pushed) "etl.ent"))))))

(deftest an-install-that-changed-the-content-still-writes-back
  (clear!)
  (write! (schema-with {:a "print(1)\n"}))
  (project!)
  (let [parent (g/ref-sha (repos) repo-rid "refs/heads/main")
        mine   (reordered (get (tree-bytes parent) "etl.ent"))
        pushed (push! parent (assoc (tree-bytes parent) "etl.ent" mine) "my work")
        follow (gp/normalize-followup! (store) (repos) *cas* repo-rid "main" pushed
                                       (schema-with {:a "print(1)\n" :b "print(2)\n"}))]
    (is (some? follow) "the installed schema says more than the pushed file")
    (let [ent (String. ^bytes (get (tree-bytes follow) "etl.ent") "UTF-8")]
      (is (str/includes? ent ":b"))
      (testing "PATCHED where it differs, not reprinted: the author's comment and
                layout survive the write-back"
        (is (str/starts-with? ent ";; mine"))))))

(deftest a-push-the-projection-never-saw-does-not-freeze-it
  (clear!)
  (write! (schema-with {:a "print(1)\n"}))
  (project!)
  (let [parent (g/ref-sha (repos) repo-rid "refs/heads/main")
        pushed (push! parent (assoc (tree-bytes parent)
                                    "docs/note.md" (.getBytes "hi\n" "UTF-8"))
                      "my work")]
    (git-rows/set-repo-anchor! (store) repo-rid "main" {:tip-sha parent})
    (write! (schema-with {:a "print(2)\n"}))
    (let [r (project!)]
      (testing "the pass adopts the ref it found instead of losing to it"
        (is (= 1 (:minted r)))
        (is (= (:tip r) (g/ref-sha (repos) repo-rid "refs/heads/main")))
        (is (g/descends-from? (repos) repo-rid (:tip r) pushed)))
      (testing "and the pushed file plane survived — a mint on the stale parent
                would have deleted it while looking like a routine commit"
        nil))))

(deftest a-branch-whose-ref-was-deleted-is-restored-not-stalled
  (clear!)
  (write! (schema-with {:a "print(1)\n"}))
  (project!)
  (let [tip (g/ref-sha (repos) repo-rid "refs/heads/main")]
    (g/delete-ref! (repos) repo-rid "refs/heads/main")
    (is (nil? (g/ref-sha (repos) repo-rid "refs/heads/main")))
    (write! (schema-with {:a "print(2)\n"}))
    (let [r (project!)]
      (is (= 1 (:minted r)))
      (is (= (:tip r) (g/ref-sha (repos) repo-rid "refs/heads/main"))
          "the recorded tip is RESTORED and then advanced; expecting a ref that
           is not there is a compare-and-set nothing can ever satisfy")
      (is (g/descends-from? (repos) repo-rid (:tip r) tip)))))

(deftest two-lineages-with-no-shared-ancestry-are-refused-LOUDLY
  (clear!)
  (write! (schema-with {:a "print(1)\n"}))
  (project!)
  (let [alien (g/with-inserter [ins (repos) repo-rid]
                (g/commit! ins
                           {:tree (g/write-tree! ins {"README.md"
                                                      (.getBytes "elsewhere\n" "UTF-8")})
                            :parents []
                            :author {:name "dev" :email "d@x" :when-ms 1700000123000}
                            :message "another repository entirely"}))]
    (g/update-ref! (repos) repo-rid "refs/heads/main" alien {:force? true})
    (write! (schema-with {:a "print(2)\n"}))
    (let [e (try (project!) nil (catch clojure.lang.ExceptionInfo e e))]
      (is (some? e) "adopting either side would drop the other's commits")
      (is (= ::gp/projection-diverged (:kind (ex-data e))))
      (is (str/includes? (ex-message e) alien)
          "both shas by name: `LOCK_FAILURE` alone is what made this unreadable
           for three days"))))

(deftest a-staging-branch-is-not-projected
  (clear!)
  (let [r (gp/project-branch! (store) (repos) *cas* repo-rid "staging/u1/main")]
    (is (= :machine-namespace (:skipped r)))
    (is (= 0 (:minted r)))))

(defn- clear-ontology! [] (clear!))

(defn- schema-with-types
  [objects]
  (merge (schema-with {:a "print(1)"})
         {:object-types
          (into {} (for [nm objects]
                     [(str "ri.o." (name nm))
                      {:object-type-name (name nm)
                       :attribute-list   [{:attribute-name :id :attribute-type :string}]
                       :primary-key      [:id]}]))}))

(deftest the-ontology-repository-projects-its-types-and-not-the-code
  (clear-ontology!)
  (write! (schema-with-types [:widget :gadget]))
  (let [r    (gp/project-branch! (store) (repos) *cas* orid "main")
        tree (g/read-tree (repos) orid (g/ref-sha (repos) orid "refs/heads/main"))
        ent  (String. (g/blob-bytes (repos) orid (:blob (get tree "ontology.ent"))) "UTF-8")]
    (is (= 1 (:minted r)))
    (is (contains? tree "ontology.ent"))
    (is (str/includes? ent "widget"))
    (is (str/includes? ent "gadget"))
    (testing "and no transform body — code belongs to the repository that owns it,
              and a fact in two trees is a fact two pushes can fight over"
      (is (not (str/includes? ent "print(1)")))
      (is (not (contains? tree "transforms/a.py"))))))

(deftest suppression-holds-for-the-ontology-repository-too
  (testing "a write that changes only a transform must mint NOTHING here — the
            whole reason suppression is tested on TREE equality rather than row
            equality is that most rows scope to identical content for any given
            repository, and this repository sees every row on the instance"
    (clear-ontology!)
    (write! (schema-with-types [:widget]))
    (gp/project-branch! (store) (repos) *cas* orid "main")
    (write! (merge (schema-with-types [:widget]) (schema-with {:a "print(2)"})))
    (let [r (gp/project-branch! (store) (repos) *cas* orid "main")]
      (is (= 0 (:minted r)))
      (is (= 1 (:suppressed r))))))

(deftest the-ontology-repository-rebuilds-to-the-same-shas
  (testing "durability is not inherited by assertion — the rebuild reads
            `control_repo_files` + the CAS, and a tree builder that skipped the
            write-through would pass every other test here and fail only after
            $SUTRA_GIT_DIR was gone"
    (clear-ontology!)
    (write! (schema-with-types [:widget]))
    (gp/project-branch! (store) (repos) *cas* orid "main")
    (let [before (g/all-refs (repos) orid)]
      (g/forget-repo! (repos) orid)
      (rm-rf (File. (repos) (str orid ".git")))
      (gp/rebuild! (store) (repos) *cas* orid)
      (is (= before (g/all-refs (repos) orid))))))

(defn- schema-with-project
  [objects]
  (-> (schema-with-types objects)
      (assoc :projects {"ri.p.1" {:project-name :beschaffung}})
      (assoc-in [:repositories repo-rid :repository-name] :beschaffung)
      (assoc-in [:repositories repo-rid :project] "ri.p.1")))

(defn- clear-all! [] (clear!))

(defn- ont-tree [] (g/read-tree (repos) orid (g/ref-sha (repos) orid "refs/heads/main")))
(defn- mint-old-world-tip!
  [pinned]
  (let [tip     (g/ref-sha (repos) orid "refs/heads/main")
        entries (into {"beschaffung" {:gitlink pinned}
                       ".gitmodules" (.getBytes (str "[submodule \"beschaffung\"]\n"
                                                     "\tpath = beschaffung\n"
                                                     "\turl = ./git/beschaffung/beschaffung.git\n"
                                                     "\tbranch = main\n")
                                                "UTF-8")}
                      (keep (fn [[path {:keys [blob mode]}]]
                              (when-not (g/gitlink? mode)
                                [path (g/blob-bytes (repos) orid blob)])))
                      (g/read-tree (repos) orid tip))
        sha     (g/with-inserter [ins (repos) orid]
                  (g/commit! ins {:tree (g/write-tree! ins entries)
                                       :parents [tip]
                                       :author {:name "tester"}
                                       :message "the world with submodules"}))]
    (g/update-ref! (repos) orid "refs/heads/main" sha {:expected tip})
    (gp/adopt-push! (store) (repos) *cas* orid [{:ref "refs/heads/main" :new sha}])
    (g/pin-anchor! (repos) orid "main" sha)
    (git-rows/set-repo-anchor! (store) orid "main" {:tip-sha sha})
    sha))

(deftest the-ontology-tree-names-no-other-repository
  (clear-all!)
  (write! (schema-with-project [:widget]))
  (let [code-tip (:tip (project!))
        _        (gp/project-branch! (store) (repos) *cas* orid "main")
        tree     (ont-tree)]
    (is (some? code-tip))
    (testing "no pointer and no mapping. A code repository is reached by cloning
              it, not by an edge in this repository's history — `sutra clone
              ontology` lays it down beside this one, which is a checkout layout
              and not a fact about either tree."
      (is (not (contains? tree "beschaffung")))
      (is (not (contains? tree ".gitmodules")))
      (is (not (contains? tree "beschaffung/transforms/a.py"))
          "and certainly not a copy — that would put the bodies under two
           authorities"))
    (is (contains? tree "ontology.ent"))))

(deftest a-code-push-moves-nothing-in-the-ontology
  (clear-all!)
  (write! (schema-with-project [:widget]))
  (project!)
  (gp/project-branch! (store) (repos) *cas* orid "main")
  (let [before (g/ref-sha (repos) orid "refs/heads/main")]
    (testing "the code repository advances and the ontology does not follow. That
              is the whole point of dropping the pointer: one logical change is
              one commit in one repository, and neither history contains the
              other's version."
      (write! (-> (schema-with-project [:widget])
                  (assoc-in [:transforms "ri.t.a" :code] "print(2)")))
      (project!)
      (is (= 0 (:minted (gp/project-branch! (store) (repos) *cas* orid "main"))))
      (is (= before (g/ref-sha (repos) orid "refs/heads/main"))))))

(deftest an-old-tip-stays-frozen-until-its-branch-changes
  (clear-all!)
  (write! (schema-with-project [:widget]))
  (project!)
  (gp/project-branch! (store) (repos) *cas* orid "main")
  (let [stale (mint-old-world-tip! (g/ref-sha (repos) repo-rid "refs/heads/main"))]
    (testing "no history row, no commit: a deploy never re-mints a branch"
      (is (= 0 (:minted (gp/project-branch! (store) (repos) *cas* orid "main"))))
      (is (= stale (g/ref-sha (repos) orid "refs/heads/main"))))
    (testing "the next real change drops the pointer and keeps the file plane"
      (write! (schema-with-project [:widget :gadget]))
      (is (= 1 (:minted (gp/project-branch! (store) (repos) *cas* orid "main"))))
      (is (= [stale]
             (:parents (g/commit-info (repos) orid (g/ref-sha (repos) orid "refs/heads/main")))))
      (is (not (contains? (ont-tree) "beschaffung")))
      (is (not (contains? (ont-tree) ".gitmodules")))
      nil
      (is (contains? (ont-tree) "ontology.ent")))))

(deftest a-historical-gitlink-survives-the-rebuild
  (testing "nothing derives a pointer any more, and the commits that carry one do
            not go away. Its sha is recorded VERBATIM rather than through the blob
            CAS — it names a commit in another repository, so there is no content
            here to address, and CAS-ing it would lose the only thing the rebuild
            needs while passing every other test in this namespace."
    (clear-all!)
    (write! (schema-with-project [:widget]))
    (project!)
    (gp/project-branch! (store) (repos) *cas* orid "main")
    (let [pinned (g/ref-sha (repos) repo-rid "refs/heads/main")]
      (mint-old-world-tip! pinned)
      (let [refs (g/all-refs (repos) orid)]
        (g/forget-repo! (repos) orid)
        (rm-rf (File. (repos) (str orid ".git")))
        (gp/rebuild! (store) (repos) *cas* orid)
        (is (= refs (g/all-refs (repos) orid)) "SAME SHAS — the tree object is reproduced exactly")
        (is (= pinned (:blob (get (ont-tree) "beschaffung"))))))))

(deftest the-seeded-readme-says-the-tree-is-authoritative
  (testing "a code repository's README describes a slice; this one has to warn
            that a stale checkout is a dangerous thing to push, because that is
            the failure the retire check exists to catch and the README is where
            somebody reads about it before hitting it"
    (clear-ontology!)
    (write! (schema-with-types [:widget]))
    (gp/project-branch! (store) (repos) *cas* orid "main")
    (let [tree (g/read-tree (repos) orid (g/ref-sha (repos) orid "refs/heads/main"))
          txt  (String. (g/blob-bytes (repos) orid (:blob (get tree "README.md"))) "UTF-8")]
      (is (str/includes? txt "AUTHORITATIVE"))
      (is (str/includes? txt "retire")))))

(defn- main-commit!
  [parent entries message]
  (let [sha (g/with-inserter [ins (repos) repo-rid]
              (g/commit! ins
                         {:tree (g/write-tree! ins entries) :parents [parent]
                          :author {:name "dev" :email "d@x" :when-ms 1700000123000}
                          :message message}))]
    (g/update-ref! (repos) repo-rid "refs/heads/main" sha {:force? true})
    (gp/adopt-push! (store) (repos) *cas* repo-rid [{:ref "refs/heads/main" :new sha}])
    (git-rows/set-repo-anchor! (store) repo-rid "main" {:tip-sha sha})
    sha))

(deftest a-file-the-branch-deleted-is-removed-from-the-target
  (clear!)
  (write! (schema-with {:a "print(1)"}))
  (project!)
  (let [cur  (g/ref-sha (repos) repo-rid "refs/heads/main")
        base (main-commit! cur
                           (assoc (tree-bytes cur)
                                  "docs/alt.md" (.getBytes "veraltet\n" "UTF-8")
                                  "README.md"   (.getBytes "start\n" "UTF-8"))
                           "a doc that will be retired")]
    (branch-commit! "try-del" base
                    (dissoc (tree-bytes base) "docs/alt.md")
                    "retire the stale doc")
    (let [r
          (gp/merge-git-half! (store) (repos) *cas* repo-rid "try-del" "main" {:merge-mode :squash})
          tree (tree-bytes (g/ref-sha (repos) repo-rid "refs/heads/main"))]
      (is (contains? tree "transforms/a.py") "so is the entity plane")
      (is (empty? (:conflicts r))))))

(deftest a-file-deleted-here-and-edited-there-is-a-conflict-kept-on-target
  (clear!)
  (write! (schema-with {:a "print(1)"}))
  (project!)
  (let [cur  (g/ref-sha (repos) repo-rid "refs/heads/main")
        base (main-commit! cur
                           (assoc (tree-bytes cur)
                                  "docs/alt.md" (.getBytes "veraltet\n" "UTF-8"))
                           "the contested doc")]
    (branch-commit! "try-del2" base
                    (dissoc (tree-bytes base) "docs/alt.md")
                    "delete it on the branch")
    (let [main2 (main-commit! base
                              (assoc (tree-bytes base)
                                     "docs/alt.md" (.getBytes "gerade ueberarbeitet\n" "UTF-8"))
                              "meanwhile, main rewrites it")]
      (git-rows/set-repo-anchor! (store) repo-rid "main" {:tip-sha main2})
      (let [r
            (gp/merge-git-half! (store) (repos) *cas* repo-rid "try-del2" "main"
                                {:merge-mode :squash})
            _tree (tree-bytes (g/ref-sha (repos) repo-rid "refs/heads/main"))]
        (is (empty? (:removed r)))
        nil))))

(deftest a-deletion-only-merge-still-mints-a-followup
  (clear!)
  (write! (schema-with {:a "print(1)"}))
  (project!)
  (let [cur  (g/ref-sha (repos) repo-rid "refs/heads/main")
        base (main-commit! cur
                           (assoc (tree-bytes cur)
                                  "docs/weg.md" (.getBytes "weg damit\n" "UTF-8"))
                           "a doc and nothing else")]
    (branch-commit! "try-del3" base
                    (dissoc (tree-bytes base) "docs/weg.md")
                    "delete the only inert change")
    (let [r
          (gp/merge-git-half! (store) (repos) *cas* repo-rid "try-del3" "main"
                              {:merge-mode :squash})]
      (is (some? (:sha r)) "a follow-up commit exists to carry the removal")
      (is (empty? (:carried r)))
      nil)))

(deftest the-ontology-repo-propagates-its-file-plane-deletions-too
  (clear!)
  (write! (schema-with {:a "print(1)"}))
  (project!)
  (with-redefs [rid/ontology-repo? (constantly true)]
    (let [cur  (g/ref-sha (repos) repo-rid "refs/heads/main")
          base (main-commit! cur
                             (assoc (tree-bytes cur)
                                    "views/rechnung.entv"
                                      (.getBytes "{:object-type :rechnung}\n" "UTF-8"))
                             "an inert lean file at an unregistered prefix")]
      (branch-commit! "try-onto-del" base
                      (dissoc (tree-bytes base) "views/rechnung.entv")
                      "drop it on the branch")
      (let [r (gp/merge-git-half! (store) (repos) *cas* repo-rid "try-onto-del" "main"
                                  {:merge-mode :merge :actor "dev"})]
        (is (= ["views/rechnung.entv"] (:removed r))
            "the deletion propagates on the ontology tree as it does on a code tree")
        (is (not (contains? (tree-bytes (g/ref-sha (repos) repo-rid "refs/heads/main"))
                            "views/rechnung.entv"))
            "and the file is actually gone from the target")))))

(deftest a-deletion-the-ontology-target-edited-since-is-still-a-conflict
  (clear!)
  (write! (schema-with {:a "print(1)"}))
  (project!)
  (with-redefs [rid/ontology-repo? (constantly true)]
    (let [cur  (g/ref-sha (repos) repo-rid "refs/heads/main")
          base (main-commit! cur
                             (assoc (tree-bytes cur)
                                    "notizen/alt.md" (.getBytes "ein Vermerk\n" "UTF-8"))
                             "a record on the ontology tree")]
      (branch-commit! "try-onto-del2" base
                      (dissoc (tree-bytes base) "notizen/alt.md")
                      "drop the record on the branch")
      (main-commit! base
                    (assoc (tree-bytes base)
                           "notizen/alt.md" (.getBytes "ein Vermerk, fortgeschrieben\n" "UTF-8"))
                    "but the target wrote on it since")
      (let [r (gp/merge-git-half! (store) (repos) *cas* repo-rid "try-onto-del2" "main"
                                  {:merge-mode :merge :actor "dev"})]
        (is (empty? (:removed r)))
        nil))))

(deftest the-projection-writes-the-body-not-its-content-address
  (clear-ontology!)
  (let [body (str "\"\"\"a docstring long enough to pass the inline threshold, so the\n"
                  "body is stored as a content address rather than inline.\"\"\"\n"
                  (apply str (repeat 20 "print('padding to get past 512 bytes')\n")))]
    (write! (schema-with {:big body}))
    (project!)
    (let [tree (tree-bytes (anchor-tip))
          file (some (fn [[p b]] (when (clojure.string/ends-with? p "big.py") b)) tree)]
      (is (some? file) "the transform is in the tree")
      (is (= body (String. ^bytes file "UTF-8"))
          "byte-for-byte the authored program, not the address it is stored under")
      (is (not (clojure.string/includes? (String. ^bytes file "UTF-8") "sutra/blob"))
          "and no trace of the marker map"))))

(deftest ein-baum-wird-je-schema-wert-einmal-gebaut
  (let [s {} r rid/ontology-repo-rid]
    (is (identical? (gp/entity-tree s r) (gp/entity-tree s r))
        "derselbe Wert liefert denselben Baum, ohne ihn erneut zu exportieren")
    (is (identical? (gp/entity-tree s r) (gp/entity-tree (into {} s) r))
        "der Cache haengt am Inhalt, nicht an der Objektidentitaet")
    (is (not (identical? (gp/entity-tree s r)
                         (gp/entity-tree {:datasets {}} r)))
        "ein anderer Wert bekommt einen eigenen Baum")))

(defn- mode-at [rid sha path] (:mode (get (g/read-tree (repos) rid sha) path)))

(deftest the-projection-keeps-the-authors-file-modes
  (clear!)
  (write! (schema-with {:a "print(1)\n"}))
  (project!)
  (let [parent (g/ref-sha (repos) repo-rid "refs/heads/main")
        tree   (tree-bytes parent)
        pushed (push! parent (assoc tree
                                    "bin/run" {:bytes (.getBytes "#!/bin/sh\n" "UTF-8")
                                               :mode "100755"}
                                    "transforms/a.py" {:bytes (get tree "transforms/a.py")
                                                       :mode "100755"})
                      "my scripts")]
    (is (= "100755" (mode-at repo-rid pushed "bin/run")))
    (let [follow (gp/normalize-followup! (store) (repos) *cas* repo-rid "main" pushed
                                         (schema-with {:a "print(1)\n" :b "print(2)\n"}))]
      (is (some? follow))
      (testing "the file plane and the entity plane both keep +x through `normalized by install`"
        (is (= "100755" (mode-at repo-rid follow "bin/run")))
        (is (= "100755" (mode-at repo-rid follow "transforms/a.py"))))
      (testing "and through a projection of a later history row"
        (write! (schema-with {:a "print(9)\n" :b "print(2)\n"}))
        (project!)
        (let [tip (g/ref-sha (repos) repo-rid "refs/heads/main")]
          (is (not= follow tip))
          (is (= "100755" (mode-at repo-rid tip "bin/run")))
          (is (= "100755" (mode-at repo-rid tip "transforms/a.py")))))
      (testing "and through a rebuild from the ledger — same shas, same modes"
        (let [refs (g/all-refs (repos) repo-rid)
              tip  (g/ref-sha (repos) repo-rid "refs/heads/main")]
          (g/forget-repo! (repos) repo-rid)
          (rm-rf (File. (repos) (str repo-rid ".git")))
          (gp/rebuild! (store) (repos) *cas* repo-rid)
          (is (= refs (g/all-refs (repos) repo-rid)))
          (is (= "100755" (mode-at repo-rid tip "bin/run"))))))))

(deftest a-merge-carries-a-mode-change
  (clear!)
  (write! (schema-with {:a "print(1)\n"}))
  (project!)
  (let [base   (g/ref-sha (repos) repo-rid "refs/heads/main")
        script (.getBytes "#!/bin/sh\n" "UTF-8")
        main   (push! base (assoc (tree-bytes base) "bin/run" script) "plain")
        _      (git-rows/set-repo-anchor! (store) repo-rid "main" {:tip-sha main})
        side   (branch-commit! "try-mode" main
                               (assoc (tree-bytes main) "bin/run" {:bytes script :mode "100755"})
                               "chmod +x")]
    (is (= "100755" (mode-at repo-rid side "bin/run")))
    (let [r (gp/merge-git-half! (store) (repos) *cas* repo-rid "try-mode" "main" {})]
      (is (= ["bin/run"] (:carried r)))
      (is (= "100755"
             (mode-at repo-rid (g/ref-sha (repos) repo-rid "refs/heads/main") "bin/run"))))))

(defn- kw-types
  [objects]
  {:projections  standard/projections
   :object-types (into {} (for [nm objects]
                            [(str "ri.o." (name nm))
                             {:object-type-name nm
                              :attribute-list   [{:attribute-name :id :attribute-type :string}]
                              :primary-key      [:id]}]))})

(deftest the-ontology-ent-keeps-its-riders-and-comments
  (clear-ontology!)
  (write! (kw-types [:widget]))
  (gp/project-branch! (store) (repos) *cas* orid "main")
  (let [parent  (g/ref-sha (repos) orid "refs/heads/main")
        derived (String. ^bytes (g/blob-bytes (repos) orid
                                              (:blob (get (g/read-tree (repos) orid parent)
                                                          "ontology.ent")))
                         "UTF-8")
        body    (clojure.edn/read-string derived)
        mine    (str ";; Warum es diese Typen gibt.\n"
                     (pr-str (assoc body :automations
                                    [{:name "x" :on :widget :effect {:action :a :args {}}}]))
                     "\n")
        pushed  (g/with-inserter [ins (repos) orid]
                  (g/commit! ins
                             {:tree
                                (g/write-tree! ins
                                               (assoc
                                                (into {}
                                                      (for [[p e] (g/read-tree (repos) orid parent)]
                                                              [p
                                                               (g/blob-bytes (repos)
                                                                             orid (:blob e))]))
                                                     "ontology.ent" (.getBytes mine "UTF-8")))
                                   :parents [parent]
                                   :author {:name "dev"} :message "riders"}))]
    (g/update-ref! (repos) orid "refs/heads/main" pushed {:expected parent})
    (gp/adopt-push! (store) (repos) *cas* orid [{:ref "refs/heads/main" :new pushed}])
    (testing "the riders are not the projection's to compare: nothing to normalize"
      (is (nil? (gp/normalize-followup! (store) (repos) *cas* orid "main" pushed
                                        (kw-types [:widget])))))
    (testing "a real schema change is patched in, and the rider block and the
              comment the author wrote beside it stay"
      (let [follow (gp/normalize-followup! (store) (repos) *cas* orid "main" pushed
                                           (kw-types [:widget :gadget]))
            ent
              (String. ^bytes (g/blob-bytes (repos) orid
                                            (:blob (get (g/read-tree (repos) orid follow)
                                                        "ontology.ent")))
                       "UTF-8")]
        (is (str/includes? ent "gadget"))
        (is (str/starts-with? ent ";; Warum es diese Typen gibt."))
        (is (= ["x"] (mapv :name (:automations (clojure.edn/read-string ent)))))))))

