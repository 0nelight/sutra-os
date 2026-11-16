(ns ^:unit grammar.canonical-ent-test
  (:require [sutra.parser.authoring.registry :as registry]
            [sutra.parser.authoring.standard :as standard]
            [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [sutra.parser.canonical-print :as cp]
            [sutra.parser.ent :as ent]
            [sutra.parser.project-export :as pe]
            [sutra.parser.project-emit :as pemit]
            [sutra.types.shape :as tshape]))

(defn- rand-key [i]
  (rand-nth [(keyword (str "k" i)) (str "s" i) i]))

(defn- rand-value [depth i]
  (if (or (zero? depth) (< 0.5 (rand)))
    (rand-nth [i (str "v" i) (keyword (str "kw" i)) true nil 1.5])
    (rand-nth [(into {} (for [j (range (+ 9 (rand-int 6)))]
                          [(rand-key j) (rand-value (dec depth) j)]))
               (vec (for [j (range 3)] (rand-value (dec depth) j)))
               (set (for [j (range 5)] (keyword (str "e" j))))])))

(defn- rand-schema []
  {:schema (into {} (for [i (range (+ 12 (rand-int 8)))]
                      [(rand-key i) (rand-value 3 i)]))})

(deftest same-value-different-insertion-order-writes-identical-bytes
  (testing "the property, over generated schema-shaped maps"
    (dotimes [_ 40]
      (let [m (rand-schema)
            shuffled (into {} (shuffle (vec (:schema m))))]
        (is (= (cp/canonical-ent-str m)
               (cp/canonical-ent-str {:schema shuffled}))
            (str "canonical writer is order-dependent for " (pr-str (keys (:schema m)))))))))

(deftest print-settings-are-pinned-not-inherited
  (testing "a caller's *print-length* cannot elide the ontology into `...`"
    (let [m {:schema {:xs (vec (range 40))}}]
      (is (= (cp/canonical-ent-str m)
             (binding [*print-length* 3] (cp/canonical-ent-str m))))
      (is (not (str/includes? (binding [*print-length* 3] (cp/canonical-ent-str m)) "...")))))
  (testing "and *print-namespace-maps* cannot flip #:a{:b 1} vs {:a/b 1}"
    (let [m {:schema {:a/b 1 :a/c 2}}]
      (is (= (cp/canonical-ent-str m)
             (binding [*print-namespace-maps* true] (cp/canonical-ent-str m)))))))

(deftest bytes-are-lf-terminated-and-carry-no-banner
  (let [s (cp/canonical-ent-str {:schema {:a 1}})]
    (testing "exactly one trailing newline, no CR"
      (is (str/ends-with? s "\n"))
      (is (not (str/ends-with? s "\n\n")))
      (is (not (str/includes? s "\r"))))
    (testing "no provenance banner — a skip COUNT is not a function of the content,
              so a banner would rewrite the file when the ontology did not"
      (is (not (str/starts-with? s ";;"))))))

(deftest nothing-is-lost-to-the-sort
  (testing "every key of every nested map survives the canonical rendering"
    (dotimes [_ 20]
      (let [m (rand-schema)
            s (cp/canonical-ent-str m)
            n (fn count-keys [v]
                (cond (map? v) (+ (count v) (reduce + 0 (map count-keys (vals v))))
                      (vector? v) (reduce + 0 (map count-keys v))
                      :else 0))]
        (is (= (n m) (n (read-string s)))
            (str "the canonical text parses back to a different key count: " s))))))

(def ^:private repo-formats
  (registry/texts standard/standard :code-repository))

(def ^:private schema-with-a-type
  {:projections    standard/projections
   :projects     {"ri.p.1" {:project-name "controlling"}}
   :repositories {"ri.r.1" {:repository-name :etl :type :transforms :project "ri.p.1"}}
   :transforms   {"ri.t.a" {:transform-name :a :language :python
                            :code "print(1)" :repository "ri.r.1"}}
   :object-types {"ri.o.9" {:object-type-name :triebwerk :attribute-list [] :plural "Triebwerke"}}})

(deftest a-repo-tree-carries-no-ontology-types
  (testing "row 16: object types are IMPORTED, not committed. `export`'s
            `:repository` option narrows only the CODE-bearing sections — every
            other section still comes out under the project scope, and with no
            project that means the UNASSIGNED entities. An unscoped object-type
            therefore lands in the repository's `.ent` unless the tree filters to
            `repo-sections`, and it did."
    (let [ent (read-string (:content (get (pe/repo-tree schema-with-a-type "etl") "etl.ent")))]
      (is (nil? (:objects (:schema ent))))
      (is (= #{:transforms :repositories :projects} (set (keys (:schema ent))))))))

(deftest an-unrelated-ontology-write-does-not-change-the-repo-tree
  (testing "this is what makes SUPPRESSION work. If an unrelated object-type
            rewrote the repository's `.ent`, every ontology write anywhere would
            mint a commit in every repository, and `git log` would be unreadable
            within a week — while the projection reported success throughout."
    (let [before (pe/repo-tree schema-with-a-type "etl")
          after  (pe/repo-tree (assoc-in schema-with-a-type [:object-types "ri.o.9"]
                                         {:object-type-name :triebwerk
                                          :attribute-list [{:attribute-name :n
                                                            :attribute-type :string}]})
                               "etl")]
      (is (= (:content (get before "etl.ent")) (:content (get after "etl.ent")))))))

(deftest a-repo-tree-classifies-every-path-as-entity-plane
  (testing "asserted by `repo-tree` itself, because an emitter that grows a new
            output directory without teaching `plane-of` about it produces files
            the pre-receive hook calls inert — a transform body that silently
            stops publishing"
    (doseq [p (keys (pe/repo-tree schema-with-a-type "etl"))]
      (is (= :entity (pe/plane-of repo-formats p)) p))))

(def ^:private two-repos-one-project
  {:projections    standard/projections
   :projects     {"ri.p.1" {:project-name "controlling"}}
   :repositories {"ri.r.1" {:repository-name :etl  :type :transforms   :project "ri.p.1"}
                  "ri.r.2" {:repository-name :apps :type :applications :project "ri.p.1"}}
   :transforms   {"ri.t.a" {:transform-name :a :language :python
                            :code "print(1)" :repository "ri.r.1"}}
   :applications {"ri.a.1" {:application-name :mine   :source "(fn [] [:div])"
                            :project "ri.p.1" :repository "ri.r.2"}
                  "ri.a.2" {:application-name :theirs :source "(fn [] [:span])"
                            :project "ri.p.1" :repository "ri.r.1"}}})

(deftest an-app-belongs-to-its-repository-not-to-its-project
  (testing "while apps were project-scoped, EVERY repo in a project carried every
            one of that project's apps — and nothing in the tree declared them,
            so a `.cljs` shipped with no entry to publish an edit back through.
            The push went green and the edit vanished."
    (let [tree (pe/repo-tree two-repos-one-project "apps")]
      (is (contains? tree "apps/mine.cljs")
          "its own member's view is in the tree")
      (is (not (contains? tree "apps/theirs.cljs"))
          "another repo's app is NOT — this is the leak, and it was in :files")
      (is (= ["apps/mine.cljs" "apps/mine.enta"]
             (sort (filter #(str/starts-with? % "apps/") (keys tree))))
          "and the tree carries exactly the member's file AND its declaration"))))

(deftest an-app-in-a-repo-tree-can-be-published-back
  (testing "the file and its DECLARATION must both be present, or the entity
            plane carries a body with nothing that says whose body it is.

            The declaration is the app's own `apps/<name>.enta` — nav apps left
            the `.ent`'s `:applications` section when the per-app file landed,
            and that is what keeps the guarantee rather than losing it: the
            importer reads the `.enta` back out of the tree, so an edited
            `.cljs` still has a sibling that names it."
    (let [tree (pe/repo-tree two-repos-one-project "apps")
          app  (read-string (:content (get tree "apps/mine.enta")))]
      (is (= :mine (:name app)) "the .enta says WHOSE body the .cljs is")
      (is (= "apps/mine.cljs" (:source-file app))
          "declared as a sibling ref, like a transform's :code-file")
      (is (= :entity (pe/plane-of repo-formats (:source-file app))))
      (is (= :entity (pe/plane-of repo-formats "apps/mine.enta"))
          "and the declaration itself is publishable — a file-plane .enta would
           be inert to the pre-receive hook"))))

(deftest sci-view-cljs-carries-a-migrate-banner-stripped-on-import
  (testing "the emitted .cljs opens with the DEPRECATED banner, author code follows"
    (let [cljs (:content (get (pe/repo-tree two-repos-one-project "apps") "apps/mine.cljs"))]
      (is (str/starts-with? cljs pemit/migrate-banner))
      (is (str/includes? cljs "(fn [] [:div])") "the author's SCI code follows the banner")))
  (testing "with/strip is an exact round-trip — the STORED :source stays banner-free
            (else a phantom install would re-plan the app on the banner every time)"
    (let [src "(fn [] [:div \"hi\"])"]
      (is (= src (pemit/strip-migrate-banner (pemit/with-migrate-banner src)))
          "emit→import identity")
      (is (str/starts-with? (pemit/with-migrate-banner src) pemit/migrate-banner))
      (is (= src (pemit/strip-migrate-banner src)) "strip is a no-op on un-bannered source")
      (is (= (pemit/with-migrate-banner src)
             (pemit/with-migrate-banner (pemit/with-migrate-banner src)))
          "with is idempotent"))))

(deftest an-unrelated-apps-edit-does-not-touch-another-repos-tree
  (testing "the suppression property, for apps: without repo scoping, editing any
            app in the project rewrote every repo's `.ent` and minted a commit in
            each"
    (let [before (pe/repo-tree two-repos-one-project "apps")
          after  (pe/repo-tree (assoc-in two-repos-one-project
                                         [:applications "ri.a.2" :source] "(fn [] [:p])")
                               "apps")]
      (is (= before after)))))

(defn- b64 [^String s] (.encodeToString (java.util.Base64/getEncoder) (.getBytes s "UTF-8")))

(deftest extract-app-template-round-trips-the-binary
  (testing "no template → layout untouched, no file"
    (let [{:keys [layout file]} (pemit/extract-app-template :kostenverlauf {:kind :chart})]
      (is (= {:kind :chart} layout))
      (is (nil? file))))
  (testing "inline :template-b64 → exports/<app>.ods + :template-file, b64 dropped"
    (let [{:keys [layout file]}
          (pemit/extract-app-template :kostenverlauf {:export {:template-b64 (b64 "ODS-BYTES")
                                                            :filename "Wartungskosten.ods"}})]
      (is (= "exports/kostenverlauf.ods" (:path file)))
      (is (= "ODS-BYTES" (String. ^bytes (:bytes file) "UTF-8")) "decoded verbatim")
      (is (= "exports/kostenverlauf.ods" (get-in layout [:export :template-file])))
      (is (nil? (get-in layout [:export :template-b64])) "b64 removed from the layout")))
  (testing "extension mirrors the app's output :filename (xlsx template)"
    (let [{:keys [file]}
          (pemit/extract-app-template :report {:export {:template-b64 (b64 "x")
                                                        :filename "R.xlsx"}})]
      (is (= "exports/report.xlsx" (:path file))))))

(deftest a-python-body-that-declares-nothing-keeps-its-wiring
  (testing "no @transform decorator (no :spec-source) ⇒ the EDN is the ONLY record
            of :inputs, so the entry must carry them — prod's python QUERY
            FUNCTIONS are exactly this shape and came out bare"
    (let [[_ e] (:entry (pemit/emit-transform
                         {:transform-name :vorhaben-kandidaten
                          :function-name  :vorhaben-kandidaten
                          :language :python :code "def f(): pass"
                          :inputs {:arbeitsauftrag {:kind :object-type
                                                     :object-type :arbeitsauftrag}}}
                         {:inline? false}))]
      (is (= "transforms/vorhaben_kandidaten.py" (:code-file e)))
      (is (= {:arbeitsauftrag {:kind :object-type :object-type :arbeitsauftrag}}
             (:inputs e))
          "the export must be re-installable on a FRESH instance")))
  (testing "a SELF-DECLARING body (@transform ⇒ :spec-source :code) stays terse —
            a second copy in the EDN could go stale against the .py that outranks it"
    (let [[_ e] (:entry (pemit/emit-transform
                         {:transform-name :parse-lohn :language :python :code "x"
                          :spec-source :code
                          :inputs {:a {:kind :dataset :dataset :d}}
                          :output {:kind :dataset :dataset :o}}
                         {:inline? false}))]
      (is (= #{:language :code-file} (set (keys e)))))))

(deftest an-app-file-lifts-every-key-the-fold-folded-in
  (testing ":functions — the app's query-Function whitelist — is folded INTO
            :layout by enta->schema, so emit must lift it back OUT or the tree
            disagrees with the commit after every install"
    (let [emit pemit/app-unit
          out  (emit {:application-name :vorhaben :status :published
                      :layout {:kind :rows :items []
                               :functions [:vorhaben-kandidaten]
                               :object-types [:arbeitsauftrag]}}
                     {:inline? false})
          m    (:doc out)]
      (is (= [:vorhaben-kandidaten] (:functions m)) "at FILE level, like :object-types")
      (is (nil? (get-in m [:layout :functions])) "and not left inside :layout")
      (is (= [:arbeitsauftrag] (:object-types m))))))

(deftest a-query-functions-timeout-survives-install-and-read-back
  (testing "a python Function's :timeout-ms (its timeout, per version) is
            kept where install turns the Function into its transform, and printed back
            — else the push's read-back reports it dropped and the next pull loses it"
    (let [spec {:transform-name :positionen-extrahieren
                :function-name  :positionen-extrahieren
                :language :python :code "def f(): pass" :timeout-ms 300000}]
      (is (= 300000 (:timeout-ms (tshape/->author :transforms spec {:name :f})))
          "install/plan.clj tx->wire-args projects a query Function through :transforms")
      (is (= 300000 (:timeout-ms (second (:entry (pemit/emit-transform spec {:inline? false})))))
          "the projection renders it")
      (is (nil? (ent/explain-schema
                 {:functions {:positionen-extrahieren
                              {:language :python :code-file "transforms/p.py"
                               :timeout-ms 300000}}}))
          "the grammar admits it on a query Function"))))
