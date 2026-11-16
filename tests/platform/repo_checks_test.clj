(ns ^:unit platform.repo-checks-test
  (:require [clojure.set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.repos.checks :as repo-checks]
            [sutra.parser.authoring.standard :as standard]
            [sutra.parser.project-export :as pe]
            [sutra.repos.records :as git-rows]
            [sutra.repos.objects :as g]
            [sutra.types.rid :as rid])
  (:import (java.io File)))

(def ^:private rid "ri.r.c")
(def ^:private orid rid/ontology-repo-rid)
(def ^:private ^:dynamic *store* nil)
(def ^:private ^:dynamic *repos* nil)

(def ^:private live {:projections standard/projections})

(defn- ontology [body] (pr-str {:schema body}))

(defn- rm-rf [^File f]
  (when (.isDirectory f) (run! rm-rf (.listFiles f)))
  (.delete f))

(use-fixtures
  :each
  (fn [t]
    (let [d (File/createTempFile "sutra-checks-test" "")]
      (.delete d) (.mkdirs d)
      (binding [*repos* d
                *store*    {:sutra.repos.checks/schema live}]
        (try (t)
             (finally (g/forget-repo! *repos* rid) (g/forget-repo! *repos* orid) (rm-rf d)))))))

(defn- commit-in! [r entries]
  (g/with-inserter [ins *repos* r]
    (let [t (g/write-tree! ins (into {} (map (fn [[k v]] [k (.getBytes ^String v "UTF-8")]))
                                     entries))]
      (g/commit! ins {:tree t :parents []
                      :author {:name "t" :email "t@s" :when-ms 1700000000000}
                      :message "m"}))))

(defn- commit! [entries] (commit-in! rid entries))

(deftest an-ontology-section-fails-and-is-named
  (let [sha (commit! {"etl.ent" (pr-str {:schema {:transforms {:a {:language :python}}
                                                  :objects    {:widget {}}}})})
        r   (repo-checks/scope-check *store* *repos* rid sha)]
    (is (= :failed (:status r)))
    (testing "the SECTION is named — 'out of scope' without a noun is a message
              nobody can act on"
      (is (str/includes? (:detail r) ":objects")))
    (testing "and the message says where types actually live — which changed when
              they got a repository of their own. 'imported, not committed' was
              true while there was nowhere to commit them TO; now the refusal has
              somewhere to send you, and a refusal that names the wrong remedy is
              worse than one that names none."
      (is (str/includes? (:detail r) "ontology repository"))
      (is (str/includes? (:detail r) "instance root")))))

(deftest the-repo-owned-sections-pass
  (doseq [section [:transforms :functions :applications :repositories :projects]]
    (let [sha (commit! {"etl.ent" (pr-str {:schema {section {}}})})]
      (is (= :passed (:status (repo-checks/scope-check *store* *repos* rid sha)))
          (str section " is a section a code repository owns")))))

(deftest ontology-types-are-not-in-a-CODE-repository
  (testing "every type-shaped section is refused, not just :objects"
    (doseq [section [:objects :links :interfaces :shared-properties :value-types
                     :object-type-groups :datasets :agents :actions]]
      (let [sha (commit! {"etl.ent" (pr-str {:schema {section {}}})})]
        (is (= :failed (:status (repo-checks/scope-check *store* *repos* rid sha)))
            (str section " must not be committable to a code repository"))))))

(deftest the-two-trees-partition-the-sections
  (testing "no section is owned by both, which is what stops two authorities
            racing to state one fact — the loser's push lands green and is
            reverted by the next projection of the winner"
    (is (empty? (clojure.set/intersection pe/code-sections pe/type-sections))))
  (testing "and `repo-sections` still holds exactly what it held before it became
            a derivation — the partition is a restatement, not a change"
    (is (= #{:transforms :functions :applications :pipelines :templates
             :repositories :projects}
           pe/repo-sections)))
  (testing "the containers are in BOTH, and that is not a partition violation:
            they are emitted so refs resolve and are never the subject of a push"
    (is (every? pe/repo-sections pe/container-sections))
    (is (every? pe/ontology-sections pe/container-sections))))

(deftest the-ontology-repository-owns-exactly-the-complement
  (testing "every section a CODE repository refuses is one the ontology
            repository accepts, and vice versa — otherwise a section is refused
            everywhere and there is nowhere to declare it"
    (doseq [section [:objects :links :interfaces :shared-properties :value-types
                     :object-type-groups :datasets :agents :sources :actions]]
      (let [sha (commit-in! orid {"ontology.ent" (pr-str {:schema {section {}}})})]
        (is (= :passed (:status (repo-checks/scope-check *store* *repos* orid sha)))
            (str section " is the ontology repository's to declare")))))
  (doseq [section [:transforms :functions :applications]]
    (let [sha (commit-in! orid {"ontology.ent" (pr-str {:schema {section {}}})})
          r   (repo-checks/scope-check *store* *repos* orid sha)]
      (is (= :failed (:status r)) (str section " is code, and code lives in a repository"))
      nil)))

(deftest sources-and-actions-belong-to-the-ontology-repository
  (testing "both were once refused EVERYWHERE (a source carried a credential, an
            action could not be told apart from a derived one). Both reasons are
            gone, and the section that used to have no home now has exactly one —
            a code repository still refuses them, and says which remote does not."
    (doseq [section [:actions :sources]]
      (let [sha (commit-in! rid {"x.ent" (pr-str {:schema {section {}}})})
            r   (repo-checks/scope-check *store* *repos* rid sha)]
        (is (= :failed (:status r)) (str section " is not a code repository's"))
        (is (str/includes? (:detail r) "ontology repository")
            "and the refusal names the remote that DOES accept it")))))

(deftest a-credential-never-enters-a-git-tree
  (testing "the whole point of publishing sources: the CONNECTOR is reviewable,
            the secret is not. Plaintext `:secrets` would be a password in a
            commit; a sealed `:secret-cipher` would be a line the exporter omits,
            so the projection would delete it under the author's feet."
    (doseq [[k v] [[:secrets {:password "hunter2"}]
                   [:secret-cipher "BASE64SEALEDBLOB"]]]
      (let [sha (commit-in! orid {"ontology.ent"
                                  (pr-str {:schema {:sources {:crm (merge {:type :postgres}
                                                                          {k v})}}})})
            r   (repo-checks/scope-check *store* *repos* orid sha)]
        (is (= :failed (:status r)) (str k " must not be pushable"))
        (is (str/includes? (:detail r) "crm") "the SOURCE is named, not counted")
        (is (str/includes? (:detail r) (name k)) "and so is the offending key")
        (is (str/includes? (:detail r) "source secret")
            "the message hands over the in-platform way to set it"))))
  (testing "a source WITHOUT a credential is exactly what a tree should carry"
    (let [sha (commit-in! orid
                          {"ontology.ent"
                           (pr-str {:schema {:sources {:crm {:type :postgres
                                                             :jdbc-url "jdbc:postgresql://h:5432/db"
                                                             :egress ["h:5432"]}}}})})]
      (is (= :passed (:status (repo-checks/scope-check *store* *repos* orid sha)))))))

(deftest the-ontology-repository-reads-ontology-and-transform-ent-as-one
  (let [sha (commit-in! orid
                        {"ontology.ent"  (ontology {:objects {:widget {}}})
                         "transform.ent" (pr-str {:schema {:datasets {:widget_raw {}}
                                                           :sources  {:erp {:type :postgres}}}})})]
    (is (= :passed (:status (repo-checks/scope-check *store* *repos* orid sha))))
    (is (= #{:objects :datasets :sources}
           (set (keys (:schema (repo-checks/commit-ent *store* *repos* orid sha))))))))

(deftest a-section-split-across-both-files-merges-and-a-contradiction-is-named
  (let [ok  (commit-in! orid {"ontology.ent"  (ontology {:datasets {:a {}}})
                              "transform.ent" (pr-str {:schema {:datasets {:b {}}}})})
        bad (commit-in! orid {"ontology.ent"  (ontology {:datasets {:a {:title "x"}}})
                              "transform.ent" (pr-str {:schema {:datasets {:a {:title "y"}}}})})]
    (is (= #{:a
             :b}
           (set (keys (:datasets (:schema (repo-checks/commit-ent *store* *repos* orid ok)))))))
    (let [e (:sutra.repos.checks/unreadable (repo-checks/commit-ent *store*
                                                                                 *repos* orid bad))]
      (is (str/includes? e "ontology.ent and transform.ent"))
      (is (str/includes? e ":datasets")))))

(deftest a-root-ent-no-declaration-reads-is-refused-by-name
  (let [store  (assoc *store* :sutra.repos.checks/schema
                   {:projections {:ontologie (update (:ontologie standard/projections) :filename
                                                     select-keys [:zentral :transform])}})
        sha (commit-in! orid {"ontology.ent" (pr-str {:schema {:objects {:auftrag {}}}})
                              "extra.ent"    (pr-str {:schema {:datasets {:extra_raw {}}}})})
        r   (repo-checks/scope-check store *repos* orid sha)]
    (is (= :failed (:status r)))
    (is (str/includes? (:detail r) "extra.ent"))
    (is (str/includes? (:detail r) ":filename"))))

(deftest variant-1-a-root-file-that-declares-its-project
  (let [sha (commit-in! orid
                        {"ontology.ent"                  (ontology {:objects {:kunde {}}})
                         "konstruktions-performance.ent"
                           (pr-str {:schema {:projects {:konstruktions-performance {}}
                                             :objects  {:zeichnung {}}
                                             :links
                                               [[:zeichnung-kunde :zeichnung :kunde
                                                 :many-to-one]]}})})
        ent (:schema (repo-checks/commit-ent *store* *repos* orid sha))]
    (is (= :passed (:status (repo-checks/scope-check *store* *repos* orid sha))))
    (is (= :konstruktions-performance (get-in ent [:objects :zeichnung :project])))
    (is (nil? (get-in ent [:objects :kunde :project])))))

(deftest variant-2-the-file-name-says-the-project
  (let [sha (commit-in! orid
                        {"ontology.ent"                  (ontology {})
                         "konstruktions-performance.ent"
                           (pr-str {:schema
                                      {:objects {:zeichnung {:attributes [[:datei :string]]}}}})})
        ent (:schema (repo-checks/commit-ent *store* *repos* orid sha))]
    (is (= :passed (:status (repo-checks/scope-check *store* *repos* orid sha))))
    (is (= :konstruktions-performance (get-in ent [:objects :zeichnung :project])))
    (is (contains? (:projects ent) :konstruktions-performance)
        "the project exists because the file is named for it")))

(deftest variant-3-no-schema-envelope
  (let [sha (commit-in! orid {"ontology.ent"                  (ontology {})
                              "konstruktions-performance.ent"
                                (pr-str {:objects {:zeichnung {:attributes [[:datei :string]]}}})})
        ent (:schema (repo-checks/commit-ent *store* *repos* orid sha))]
    (is (= :passed (:status (repo-checks/scope-check *store* *repos* orid sha))))
    (is (= [[:datei :string]] (get-in ent [:objects :zeichnung :attributes])))
    (is (= :konstruktions-performance (get-in ent [:objects :zeichnung :project])))))

(deftest variant-4-yaml
  (let [sha (commit-in! orid {"ontology.ent" (ontology {:objects {:auftrag {}}})
                              "konstruktions-performance.ent.yaml"
                              (str "objects:\n"
                                   "  zeichnung:\n"
                                   "    title: datei\n"
                                   "    description: Eine Zeichnung aus dokv\n"
                                   "    attributes:\n"
                                   "      - [datei, string, {required: true}]\n"
                                   "links:\n"
                                   "  - [zeichnung-auftrag, zeichnung, auftrag, many-to-one]\n")})
        ent (:schema (repo-checks/commit-ent *store* *repos* orid sha))]
    (is (= :passed (:status (repo-checks/scope-check *store* *repos* orid sha))))
    (is (= {:title :datei :description "Eine Zeichnung aus dokv"
            :attributes [[:datei :string {:required true}]] :project :konstruktions-performance}
           (get-in ent [:objects :zeichnung])))
    (is (= [:zeichnung-auftrag :zeichnung :auftrag :many-to-one]
           (subvec (first (:links ent)) 0 4)))))

(deftest facet-files-split-one-type-by-what-each-author-is-responsible-for
  (let [sha (commit-in! orid
                        {"ontology.ent"                              (ontology {})
                         "konstruktions-performance.objektmodell.ent"
                           (pr-str {:objects {:zeichnung {:attributes [[:datei :string]]}}})
                         "konstruktions-performance.objects.ent"
                           (pr-str {:schema {:objects {:zeichnung {:description "aus dokv"}}}})})
        ent (:schema (repo-checks/commit-ent *store* *repos* orid sha))]
    (is (= :passed (:status (repo-checks/scope-check *store* *repos* orid sha))))
    (is (= {:attributes [[:datei :string]] :description "aus dokv"
            :project :konstruktions-performance}
           (get-in ent [:objects :zeichnung])))))

(deftest a-text-kind-scopes-what-its-texts-may-hold
  (let [store  (assoc-in *store*
                      [:sutra.repos.checks/schema :projections :ontologie :filename
                       :modell]
                      {:language :edn :envelope [] :name ["*.modell.ent"] :facet "modell"
                       :owner [:facet] :sections [:objects :links] :order 0})
        sha (commit-in! orid {"ontology.ent" (ontology {})
                              "kp.modell.ent" (pr-str {:datasets {:roh {}}})})
        r   (repo-checks/scope-check store *repos* orid sha)]
    (is (= :failed (:status r)))
    (is (str/includes? (:detail r) "kp.modell.ent"))
    (is (str/includes? (:detail r) ":datasets"))))

(deftest two-files-saying-different-things-about-one-field-are-refused-naming-both
  (let [sha (commit-in! orid {"ontology.ent" (ontology {:objects {:auftrag {:description "a"}}})
                              "transform.ent"
                                (pr-str {:schema {:objects {:auftrag {:description "b"}}}})})
        r   (repo-checks/scope-check *store* *repos* orid sha)]
    (is (= :failed (:status r)))
    (is (str/includes? (:detail r) "ontology.ent and transform.ent"))
    (is (str/includes? (:detail r) ":description"))))

(deftest a-source-repository-still-declares-one-root-ent
  (let [sha (commit! {"etl.ent" (pr-str {:schema {:transforms {}}})
                      "transform.ent" (pr-str {:schema {:transforms {}}})})]
    (is (= :failed (:status (repo-checks/scope-check *store* *repos* rid sha))))))

(deftest retire-sits-before-the-expensive-gates
  (testing "a stale-clone push is well-formed and would pass everything after it,
            so failing it late means paying for a full staging walk to reach a
            verdict the tree already held"
    (let [order (vec repo-checks/check-order)]
      (is (< (.indexOf order :retire) (.indexOf order :lint)))
      (is (< (.indexOf order :scope) (.indexOf order :retire))))))

(deftest a-commit-with-no-entity-file-passes-and-publishes-nothing
  (testing "a README-only commit is a legitimate commit — row 8 depends on it"
    (let [sha (commit! {"README.md" "hi\n" "tests/test_x.py" "assert True\n"})
          r   (repo-checks/scope-check *store* *repos* rid sha)]
      (is (= :passed (:status r)))
      (is (str/includes? (:detail r) "no entity file")))))

(deftest two-entity-files-are-an-error-not-a-choice
  (testing "a repository with two root `.ent`s has two answers to 'what does this
            repository declare', and picking one silently is the failure mode
            this check exists to prevent"
    (let [sha (commit! {"a.ent" "{:schema {}}" "b.ent" "{:schema {}}"})
          r   (repo-checks/scope-check *store* *repos* rid sha)]
      (is (= :failed (:status r)))
      (is (str/includes? (:detail r) "exactly one")))))

(deftest an-unparseable-ent-fails-with-the-parse-error
  (let [r (repo-checks/scope-check *store* *repos* rid (commit! {"etl.ent" "{:schema {"}))]
    (is (= :failed (:status r)))
    (is (str/includes? (:detail r) "does not parse"))))

(deftest a-traversal-path-never-becomes-a-commit-at-all
  (testing "JGit refuses `..` when the TREE is built, so a pushed pack carrying
            one is rejected before there is anything to check. `scope-check`'s
            path guard is the second layer, for a tree that arrived some other
            way — a rebuild from a hand-edited manifest, a future importer."
    (is (thrown? Exception (commit! {"../escape" "x"})))))

(deftest a-dotted-filename-is-not-a-traversal
  (testing "`a..b/x.py` is a legal filename; a substring test for `..` would fail
            a legitimate push while claiming an attack"
    (let [r
          (repo-checks/scope-check *store* *repos* rid
                                   (commit! {"transforms/a..b.py" "print(1)\n"}))]
      (is (= :passed (:status r))))))

(deftest changed-paths-is-the-first-parent-diff
  (let [a (commit! {"a" "1" "b" "1"})
        b (g/with-inserter [ins *repos* rid]
            (let [t (g/write-tree! ins {"a" (.getBytes "1" "UTF-8")
                                        "b" (.getBytes "2" "UTF-8")
                                        "c" (.getBytes "3" "UTF-8")})]
              (g/commit! ins {:tree t :parents [a]
                              :author {:name "t" :email "t@s" :when-ms 1700000001000}
                              :message "second"})))]
    (is (= #{"a" "b"} (repo-checks/changed-paths *repos* rid a))
        "a root commit's whole tree is changed")
    (is (= #{"b" "c"} (repo-checks/changed-paths *repos* rid b)))))

(deftest publish-is-last-and-runs-only-when-everything-passed
  (testing "the ordering is what makes 'the ontology never holds an unvalidated
            state' true without the transport having to know anything"
    (is (= :ci/sutra-publish (last repo-checks/check-order)))
    (is (= :scope (first repo-checks/check-order))
        "scope is the security boundary and must gate the gates")))

(deftest side-band-names-the-failure-not-the-transport
  (testing "a user whose push failed `plan` must read the problem, not
            `pre-receive hook declined` — that phrase is what a transport-level
            gate would have given them, and avoiding it is why the gate moved"
    (let [lines (repo-checks/side-band-lines
                 {:status :failed
                  :checks [{:check "scope" :status :passed}
                           {:check "plan"  :status :failed
                            :detail "widget is live and produced by [\"t\"]"}]})]
      (is (some #(str/includes? % "plan") lines))
      (is (some #(str/includes? % "widget is live") lines))
      (is (not-any? #(str/includes? % "scope") lines)
          "a passing check is not noise on a failed push"))))

(deftest a-pushed-code-file-is-inlined-from-the-tree
  (let [body "def compute(df): return df"
        sha  (commit! {"etl.ent" (pr-str {:schema {:transforms
                                                   {:parse {:language :python
                                                            :code-file "transforms/parse.py"}}}})
                       "transforms/parse.py" body})
        tx   (get-in (repo-checks/commit-ent *store* *repos* rid sha) [:schema :transforms :parse])]
    (is (= body (:code tx))
        "the pushed body reaches the gates as inline :code")
    (is (nil? (:code-file tx))
        "and the ref is consumed, exactly as the CLI's resolve-code-files does")))

(deftest an-app-source-file-and-a-binary-template-inline-too
  (let [src "(fn [d] [:div])"
        sha (commit!
             {"apps.ent" (pr-str {:schema {:applications
                                           {:board {:source-file "apps/board.cljs"
                                                    :layout {:export {:template-file
                                                                      "exports/board.ods"}}}}}})
              "apps/board.cljs"   src
              "exports/board.ods" "PKbinary"})
        app (get-in (repo-checks/commit-ent *store* *repos* rid sha)
                    [:schema :applications :board])]
    (is (= src (:source app)) "an app's cljs view inlines like a transform's python")
    (is (nil? (:source-file app)))
    (testing "a binary asset inlines base64 — one map carries BOTH refs, so the
              inlining must not short-circuit on the first"
      (is (some? (get-in app [:layout :export :template-b64])))
      (is (nil? (get-in app [:layout :export :template-file]))))))

(deftest a-ref-the-tree-does-not-hold-is-left-alone
  (testing "not inlined as nil and not thrown on — the gates downstream refuse a
            bodiless entity in their own phrasing, which says more than this could"
    (let [sha (commit! {"etl.ent" (pr-str {:schema {:transforms
                                                    {:parse {:language :python
                                                             :code-file "transforms/gone.py"}}}})})
          tx  (get-in (repo-checks/commit-ent *store* *repos* rid sha)
                      [:schema :transforms :parse])]
      (is (= "transforms/gone.py" (:code-file tx)))
      (is (nil? (:code tx))))))

(deftest inlining-is-why-a-push-cannot-blank-a-transform
  (let [sha (commit! {"etl.ent" (pr-str {:schema {:transforms
                                                  {:parse {:language :python
                                                           :code-file "transforms/parse.py"}}}})
                      "transforms/parse.py" "print(1)"})
        tx  (get-in (repo-checks/commit-ent *store* *repos* rid sha) [:schema :transforms :parse])]
    (is (contains? tx :code)
        "a transform reaching stage-schema! WITH :language and WITHOUT :code is
         the shape that wipes it — inlining is what keeps that unreachable")))

(deftest the-pushed-ent-reaches-the-gates-lowered
  (let [sha (commit! {"types.ent"
                      (pr-str {:schema
                               {:objects
                                {:zeitbuchung
                                 {:attributes
                                  [[:buchung-id :string]
                                   [:wo-art-id :integer
                                    {:derived {:rollup {:agg :arg-max
                                                        :over :zeitbuchung-arbeitsauftrag
                                                        :by :wo-nr
                                                        :pick :wo-art-id}}}]]}}
                                :transforms {:parse {:language :pg-sql :query "select 1"
                                                     :incremental true}}}})})
        sch (:schema (repo-checks/commit-ent *store* *repos* rid sha))
        opts (get-in sch [:objects :zeitbuchung :attributes 1 2])]
    (is (= [:connected-arg-max :zeitbuchung-arbeitsauftrag :wo-nr :wo-art-id]
           (get-in opts [:derived :expression]))
        "`:rollup` is author sugar — only `:derived :expression` is read
         (sutra.objects.views/derived-attrs), so an un-lowered push installs a
         property that reads back null forever, with no error anywhere")
    (is (nil? (get-in opts [:derived :rollup])))
    (is (= {:read-mode :added :write-mode :modify}
           (get-in sch [:transforms :parse :incremental]))
        "every lowering of `lower-schema` runs here, not only the rollup one")))

(deftest a-review-is-about-the-commit-the-author-pushed
  (let [rows [{:commit_sha "a1" :source "pushed"}
              {:commit_sha "b2" :source "minted"}
              {:commit_sha "c3" :source "minted"}]]
    (is (= "a1" (:commit_sha (repo-checks/reviewable-tip rows)))
        "the server's follow-ups advance the ref but carry no authored change —
         gating on them asks for a check on a tree nobody pushed, and checks run
         on PUSH, so no check could ever exist")))

(deftest a-later-push-wins-over-an-earlier-one
  (let [rows [{:commit_sha "a1" :source "pushed"}
              {:commit_sha "b2" :source "minted"}
              {:commit_sha "c3" :source "pushed"}
              {:commit_sha "d4" :source "minted"}]]
    (is (= "c3" (:commit_sha (repo-checks/reviewable-tip rows)))
        "stepping over minted rows must never reach PAST newer authored content")))

(deftest a-server-authored-branch-keeps-the-newest-commit
  (let [rows [{:commit_sha "a1" :source "minted"}
              {:commit_sha "b2" :source "minted"}]]
    (is (= "b2" (:commit_sha (repo-checks/reviewable-tip rows)))
        "a branch written entirely through the UI or the CLI mint path has no
         author's tip to prefer — there the minted commit IS the content")))

(deftest no-commits-is-nil-not-a-throw
  (is (nil? (repo-checks/reviewable-tip []))))

(deftest the-ontology-repository-is-a-candidate
  (let [asked (atom [])]
    (with-redefs [git-rows/commits-of (fn [_st r _branch]
                                        (swap! asked conj r)
                                        [{:commit_sha "s" :source "pushed"}])]
      (let [repos (repo-checks/repos-on-branch nil {:repositories {rid {}}} "b")]
        (is (some #{orid} @asked)
            "a candidate list built from (keys (:repositories schema)) alone never
             offers the ontology repository to the ledger — the row does not exist")
        (is (= orid (first repos))
            "listed first, so a refusal names the tip an author pushed and can
             look up with `sutra checks`, not whichever code repo rode along")
        (is (some #{rid} repos)
            "and the code repositories are still measured — this widens the
             candidate list, it does not swap one blind spot for another")))))

(deftest a-repository-with-no-commits-on-the-branch-is-not-a-candidate
  (with-redefs [git-rows/commits-of (fn [_st r _branch]
                                      (when (= r rid) [{:commit_sha "s" :source "pushed"}]))]
    (is (= [rid] (repo-checks/repos-on-branch nil {:repositories {rid {}}} "b"))
        "the ontology repo is OFFERED on every branch but only survives where it
         actually has commits — otherwise every proposal would drag an empty
         repository, and `checks-block` reads an empty check list as a refusal")))

(deftest a-gate-asks-only-about-pushed-content
  (let [rows [{:commit_sha "a1" :source "pushed"}
              {:commit_sha "b2" :source "minted"}]]
    (is (= "a1" (:commit_sha (repo-checks/pushed-tip rows)))
        "the authored tip, exactly as the review sees it")))

(deftest a-branch-authored-in-platform-has-no-tip-for-a-gate
  (let [rows [{:commit_sha "a1" :source "minted"}
              {:commit_sha "b2" :source "minted"}]]
    (is (nil? (repo-checks/pushed-tip rows))
        "nil is what makes `checks-block` a no-op here — a checks gate that
         cannot be satisfied is not a stricter gate, it is a wall")
    (is (some? (repo-checks/reviewable-tip rows))
        "while the REVIEW still names the content, which is a different question")))

(deftest revert-runs-with-the-cheap-structural-gates
  (let [pos (fn [c] (.indexOf ^java.util.List repo-checks/check-order c))]
    (is (< (pos :retire) (pos :revert)) "after the drop half of the same accident")
    (is (< (pos :revert) (pos :lint))
        "and before anything that pays for a staging walk to reach a verdict the
         tree already answered")))

(def ^:private own-path "konstruktion.objects.ent")

(deftest a-project-s-own-text-is-read-with-the-root-texts-and-owns-what-it-declares
  (let [sha (commit-in! orid
                        {"ontology.ent" (ontology {:objects {:auftrag {}}})
                         own-path
                           (pr-str {:schema {:projects {:konstruktion {:imports [:controlling]}}
                                                 :objects  {:zeichnung {}}
                                                 :links
                                                   [[:zeichnung-auftrag :zeichnung :auftrag
                                                     :many-to-one]]}})
                         "notizen/konstruktion.md" "# Frage"})
        ent (:schema (repo-checks/commit-ent *store* *repos* orid sha))]
    (is (= :passed (:status (repo-checks/scope-check *store* *repos* orid sha))))
    (is (= #{:auftrag :zeichnung} (set (keys (:objects ent)))))
    (is (= :konstruktion (get-in ent [:objects :zeichnung :project]))
        "the file states the project once; every resource in it carries it")
    (is (nil? (get-in ent [:objects :auftrag :project])) "the root file is untouched")))

(deftest a-project-s-text-refusal-names-the-text
  (let [sha (commit-in! orid
                        {"ontology.ent" (ontology {:objects {}})
                         own-path   (pr-str {:schema {:projects    {:konstruktion {}}
                                                          :value-types {:mm {:type :double}}}})})
        r   (repo-checks/scope-check *store* *repos* orid sha)]
    (is (= :failed (:status r)))
    (is (str/includes? (:detail r) own-path))
    (is (str/includes? (:detail r) ":value-types"))))

(deftest a-code-repository-cannot-carry-a-project-s-own-text
  (let [sha (commit! {"etl.ent"    (pr-str {:schema {:transforms {}}})
                      own-path (pr-str {:schema {:projects {:konstruktion {}}}})})
        r   (repo-checks/scope-check *store* *repos* rid sha)]
    (is (= :failed (:status r)))
    (is (str/includes? (:detail r) own-path))))

(deftest a-facet-holds-only-its-sections
  (let [sha (commit-in! orid
                        {"ontology.ent"            (ontology {:objects {}})
                         "konstruktion.apps.ent"   (pr-str {:schema {:actions {:einlesen {}}}})})
        r   (repo-checks/scope-check *store* *repos* orid sha)]
    (is (= :failed (:status r)))
    (is (str/includes? (:detail r) ":actions")
        "an action belongs in the objects facet, beside its types")))

(deftest a-push-is-read-with-the-kinds-it-declares
  (let [kinds (assoc-in (:filename (:ontologie standard/projections)) [:modell]
                        {:language :edn :envelope [:schema] :name ["*.modell.ent"] :facet "modell"
                         :owner [:facet] :sections [:objects] :home-for [:objects] :order 0})
        sha   (commit-in! orid
                          {"ontology.ent"
                             (ontology {:projections {:ontologie {:filename kinds}}})
                           "konstruktion.modell.ent" (pr-str {:schema {:objects {:zeichnung {}}}})})
        r     (repo-checks/scope-check *store* *repos* orid sha)]
    (is (str/includes? (:detail r) "kind") (:detail r))
    (is (= :failed (:status r))
        "two project homes for :objects (objekte and modell) — the kinds it declares are judged, not the live ones")))

(deftest a-project-extends-an-imported-type-with-attributes
  (let [sha (commit-in! orid
                        {"ontology.ent" (ontology {:objects {:auftrag {:status :active
                                                                              :attributes
                                                                                [[:auf-nr
                                                                                  :string]]}}})
                         own-path
                           (pr-str {:schema {:projects {:konstruktion {:imports [:controlling]}}
                                                 :objects
                                                   {:auftrag
                                                      {:attributes
                                                         [[:konstruktion-stunden :double]]}}}})})
        ent (:schema (repo-checks/commit-ent *store* *repos* orid sha))]
    (is (= :passed (:status (repo-checks/scope-check *store* *repos* orid sha))))
    (is (= :active (get-in ent [:objects :auftrag :status]))
        "the declaration is the root file's, untouched")
    (is (nil? (get-in ent [:objects :auftrag :project])))
    (is (= 2 (count (get-in ent [:objects :auftrag :attributes]))))))

(deftest a-view-is-the-projections-own-text-and-does-not-make-the-tree-foreign
  (testing "an Object View reads as the :applications section — because an Object View IS
            one — and `object-views/*.entv` is a kind the ONTOLOGY projection declares
            itself. Ownership was judged over a read of the WHOLE tree, so a single view
            made :applications stray and refused every push into that tree, a commit that
            only adds a markdown file included. Ownership is about what a text DECLARES,
            so it is judged over the declaring texts; where a lean file may sit is policed
            by name (misplaced-lean-files, foreign-files) instead."
    (let [view {:object-type :raum
                :panel       {:cards [{:widget :attributes}]}}
          sha  (commit-in! orid {"ontology.ent"           (ontology {:objects {:raum {}}})
                                 "object-views/raum.entv" (pr-str view)
                                 "concerns/plan.md"       "# nur eine Datei\n"})
          r    (repo-checks/scope-check *store* *repos* orid sha)]
      (is (= :passed (:status r))
          (str "a view beside a clean root text is not out of scope — " (:detail r)))))
  (testing "and the line it draws stays where it was: an app DECLARED in a root text is
            still code in the wrong tree"
    (let [sha (commit-in! orid {"ontology.ent" (ontology {:applications {:a {}}})})
          r   (repo-checks/scope-check *store* *repos* orid sha)]
      (is (= :failed (:status r)))
      (is (str/includes? (:detail r) ":applications")))))

(deftest a-python-transform-carries-the-repository-modules-it-imports
  (let [sha (commit! {"etl.ent"
                      (pr-str {:schema
                               {:transforms
                                {:haupt {:code-file "transforms/haupt.py"
                                         :language  :python
                                         :inputs    {}
                                         :output    {:kind :dataset :dataset :d}}}}})
                      "transforms/haupt.py"  "from kern import f\nimport os\n"
                      "transforms/kern.py"   "from kern2 import g\n"
                      "transforms/kern2.py"  "def g():\n    return 1\n"
                      "transforms/fremd.py"  "x = 1\n"})
        t   (get-in (repo-checks/commit-ent *store* *repos* rid sha) [:schema :transforms])]
    (is (= {"kern" "from kern2 import g\n" "kern2" "def g():\n    return 1\n"}
           (some :modules (vals t)))
        "imported siblings travel along, transitively; a module nobody imports does not")))

(deftest a-projection-push-reads-a-new-code-file-from-the-repository-tree
  (let [sha    (commit! {"transforms/f.py"    "from kern import g\n"
                         "transforms/kern.py" "def g():\n    return 1\n"})
        _      (g/update-ref! *repos* rid "refs/heads/zweig" sha {})
        schema {:repositories {rid {:repository-name :etl}}}
        body   {:functions {:f  {:code-file "transforms/f.py" :repository :etl}
                            :g  {:code "x" :code-file "transforms/f.py" :repository :etl}
                            :h  {:code-file "transforms/fehlt.py" :repository :etl}}}
        out    (repo-checks/resolve-code-files schema *repos* body "zweig")]
    (is (= "from kern import g\n" (get-in out [:functions :f :code])))
    (is (= {"kern" "def g():\n    return 1\n"} (get-in out [:functions :f :modules])))
    (is (= "x" (get-in out [:functions :g :code])) "a body the text carries is kept")
    (is (= "transforms/fehlt.py" (get-in out [:functions :h :code-file]))
        "a file the tree lacks stays named, for staging to refuse")
    (is (= {"kern" "def g():\n    return 1\n"}
           (get-in (repo-checks/resolve-code-files
                    schema *repos* {:functions {:f {:code-file "transforms/f.py"}}} "zweig" "etl")
                   [:functions :f :modules]))
        "a node of a repository's own text names no :repository — the text does")
    (is (= {"kern" "def g():\n    return 1\n"}
           (get-in (repo-checks/resolve-code-files
                    schema *repos* {:functions {:i {:code "from kern import g\n"
                                                    :language :python}}} "zweig" "etl")
                   [:functions :i :modules]))
        "inline Python code gets the repository modules it imports")))
