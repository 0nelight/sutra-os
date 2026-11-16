(ns ^:unit platform.projection-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.parser.authoring :as authoring]
            [sutra.parser.authoring.standard :as standard]
            [sutra.parser.checkout :as co]
            [sutra.parser.project-export :as pe]
            [sutra.parser.projection :as proj]
            [sutra.parser.sugar :as sugar]))

(def ^:private berechtigungen
  {:filename {:haupt {:language :edn :envelope [:schema] :name ["berechtigungen.ent"]
                      :home-for [:projects]}}
   :select   {:sections [:projects] :fields {:projects [:imports :synonyms :grants]}}
   :edits    [:update]
   :description "Die Beziehungen der Projekte zueinander, sonst nichts."})

(def ^:private konstruktion
  {:filename {:yaml {:language :yaml :name ["konstruktion.ent.yaml"] :home-for [:*] :order 0}}
   :select   {:project :konstruktion :sections [:objects :links]}})

(def ^:private schema
  {:projections  (assoc standard/projections :konstruktion konstruktion
                        :berechtigungen berechtigungen)
   :projects     {"ri.sutra.main.project.p1" {:project-name :controlling :description "Zahlen"}
                  "ri.sutra.main.project.p2" {:project-name :konstruktion :description "Zeichnungen"
                                              :imports ["ri.sutra.main.project.p1"]}}
   :object-types {"ri.sutra.main.object-type.o1"
                  {:object-type-name :auftrag
                   :project          "ri.sutra.main.project.p1"
                   :attribute-list   [{:attribute-name :auf-nr :attribute-type :string}]
                   :primary-key      :auf-nr}
                  "ri.sutra.main.object-type.o2"
                  {:object-type-name :zeichnung
                   :project          "ri.sutra.main.project.p2"
                   :attribute-list   [{:attribute-name :datei :attribute-type :string}]
                   :primary-key      :datei}}})

(defn- body [text] (:schema (edn/read-string text)))

(defn- push
  ([s nm texts] (push s nm texts {}))
  ([s nm texts {:keys [pulled] :as opts}]
   (proj/absorb s nm texts
                (merge (select-keys (proj/view (or pulled s) nm) [:base :shape])
                       (dissoc opts :pulled)))))

(deftest the-standard-projection-is-the-ontology-as-authored
  (is (= (update-vals (pe/ontology-tree schema) :content) (proj/render schema :ontologie))
      "the lens the ontology repository was projected through, now declared")
  (testing "a kind of text another projection declares stays out of it"
    (is (= ["controlling.objects.ent" "konstruktion.objects.ent" "ontology.ent"]
           (keys (proj/render schema :ontologie)))
        "each project's types in its objects facet, the rest centrally")))

(deftest a-projection-shows-what-it-selects-in-the-texts-it-declares
  (let [texts (proj/render schema :konstruktion)]
    (is (= ["konstruktion.ent.yaml"] (keys texts)))
    (is (str/includes? (get texts "konstruktion.ent.yaml") "zeichnung"))
    (is (not (str/includes? (get texts "konstruktion.ent.yaml") "auftrag"))
        "another project's type is not in it")
    (is (= #{"zeichnung"} (get (proj/shown schema :konstruktion) :objects)))))

(deftest absorbing-what-was-rendered-says-the-same-nodes
  (let [{:keys [schema opts result]} (push schema :ontologie (proj/render schema :ontologie))]
    (is (= #{"auftrag" "zeichnung"} (set (map name (keys (:objects result))))))
    (is (= {} schema) "nothing changed, so nothing is installed — no node is restated")
    (is (:strict? opts))
    (is (= {} @(:strict-projected opts)) "and nothing deleted")))

(deftest a-push-needs-the-base-of-its-pull
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"needs the base of the pull"
                        (proj/absorb schema :ontologie (proj/render schema :ontologie) {})))
  (testing "and a base taken under another cut of the elements is not compared, it is refused"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"cuts its elements otherwise"
                          (proj/absorb schema :ontologie (proj/render schema :ontologie)
                                       {:base (:base (proj/view schema :ontologie))
                                        :shape "an-old-shape"})))))

(defn- with-description [s ot d]
  (update s :object-types
          (fn [m]
            (into {}
                  (for [[k v] m]
                    [k (cond-> v (= ot (:object-type-name v)) (assoc :description d))])))))

(deftest a-push-is-merged-per-element-over-its-base
  (let [texts  (proj/render schema :ontologie)
        facet  (body (get texts "konstruktion.objects.ent"))
        mine   (assoc texts "konstruktion.objects.ent"
                      (pr-str {:schema
                                 (assoc-in facet [:objects :zeichnung :description] "meins")}))]
    (testing "what only others changed since the pull is kept, not undone"
      (let [now (with-description schema :auftrag "von anderen")
            r   (push now :ontologie mine {:pulled schema})
            out (:result r)]
        (is (= "meins" (get-in out [:objects :zeichnung :description])))
        (is (not (contains? (:objects (:schema r)) :auftrag))
            "a node only others changed is not even read, so nothing restates it")))
    (testing "a node others created since the pull is not deleted by a folder that never saw it"
      (let [now (assoc-in schema [:object-types "ri.sutra.main.object-type.o3"]
                          {:object-type-name :pruefung :project "ri.sutra.main.project.p1"
                           :attribute-list [{:attribute-name :nr :attribute-type :string}]
                           :primary-key :nr})
            r   (push now :ontologie mine {:pulled schema})]
        (is (not (contains? (:objects @(get-in r [:opts :strict-projected])) "pruefung")))))
    (testing "a node the pull showed and the folder dropped is deleted"
      (let [gone (assoc texts
                        "konstruktion.objects.ent"
                          (pr-str {:schema (update facet :objects dissoc :zeichnung)}))]
        (is (not (contains? (set (map name
                                      (keys (:objects (:result (push schema :ontologie gone))))))
                            "zeichnung")))))
    (testing "the same element changed on both sides is a conflict, named, decided only on request"
      (let [now (with-description schema :zeichnung "von anderen")]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"objects zeichnung :description"
                              (push now :ontologie mine {:pulled schema})))
        (is (= "meins" (get-in (push now :ontologie mine {:pulled schema :resolve :mine})
                               [:result :objects :zeichnung :description])))
        (is (= "von anderen" (get-in (push now :ontologie mine {:pulled schema :resolve :theirs})
                                     [:result :objects :zeichnung :description])))))
    (testing "the next base is what the push leaves"
      (let [r (push schema :ontologie mine)]
        (is (= (co/fingerprint "meins")
               (get-in r
                       [:base "konstruktion.objects.ent" [:objects "zeichnung"] :description])))))))

(deftest a-move-changes-the-memory-alone
  (let [texts (proj/render schema :ontologie)
        facet (body (get texts "konstruktion.objects.ent"))
        moved (-> texts
                  (assoc "konstruktion.objects.ent" (pr-str {:schema (dissoc facet :objects)}))
                  (assoc "konstruktion.ent"
                         (pr-str {:schema {:objects (select-keys (:objects facet) [:zeichnung])}})))
        r     (push schema :ontologie moved)
        again (proj/render schema :ontologie (co/memory-merge nil (:said r)))]
    (is (= "konstruktion.ent" (get-in r [:said :placement [:objects "zeichnung"]])))
    (is (= [[[:objects "zeichnung"] "konstruktion.objects.ent" "konstruktion.ent"]]
           (filter #(= [:objects "zeichnung"] (first %)) (:moved r))))
    (is (nil? (get-in r [:schema :projections :ontologie :layout])) "the schema holds no placement")
    (is (contains? again "konstruktion.ent") "rendered back where the author put it")
    (is (not (str/includes? (get again "konstruktion.objects.ent") "zeichnung")))
    (testing "without a memory every node is at its home again"
      (is (not (contains? (proj/render schema :ontologie) "konstruktion.ent"))))))

(deftest a-checkout-is-the-texts-one-pull-named
  (let [one   (proj/view schema :ontologie nil {:texts ["konstruktion.objects.ent"]})
        facet (body (get-in one [:files "konstruktion.objects.ent"]))]
    (is (= ["konstruktion.objects.ent"] (keys (:files one))) "one text, not the projection")
    (is (= #{[:objects "zeichnung"] [:projects "konstruktion"]}
           (set (keys (get-in one [:base "konstruktion.objects.ent"]))))
        "its base: the nodes that text holds, nothing else")
    (testing "pushing that one text changes what it holds and nothing else"
      (let [r (proj/absorb schema :ontologie
                           {"konstruktion.objects.ent"
                              (pr-str {:schema
                                         (assoc-in facet [:objects :zeichnung :description]
                                                   "neu")})}
                           {:base (:base one) :shape (:shape one)})]
        (is (= "neu" (get-in r [:schema :objects :zeichnung :description])))
        (is (= #{"zeichnung"} (set (map name (keys (get-in r [:schema :objects])))))
            "only what it changed is installed")
        (is (not (contains? (set (map name (keys (get-in r [:result :objects])))) "auftrag"))
            "a node of a text the checkout does not carry is not even read")
        (is (= ["konstruktion.objects.ent"] (keys (:base r))))))
    (testing "dropping its text deletes what it held"
      (let [r (proj/absorb schema :ontologie {"konstruktion.objects.ent" nil}
                           {:base (:base one) :shape (:shape one)})]
        (is (not (contains? (set (map name (keys (get-in r [:result :objects])))) "zeichnung")))
        (is (= #{"zeichnung"} (:objects @(get-in r [:opts :strict-projected])))
            "exactly that is deleted")))
    (testing "a node pasted in from a text the checkout does not carry is refused, naming that text"
      (let [central (proj/view schema :ontologie nil {:texts ["ontology.ent"]})
            auftrag (get-in (body (get (proj/render schema :ontologie) "controlling.objects.ent"))
                            [:objects :auftrag])]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"objects auftrag lives in controlling.objects.ent"
                              (proj/absorb schema :ontologie
                                           {"ontology.ent"
                                              (pr-str
                                               {:schema
                                                  {:objects
                                                     {:auftrag
                                                        (assoc auftrag :project :controlling)}}})}
                                           {:base (:base central) :shape (:shape central)})))))
    (testing "with both texts in one checkout the same paste is a move"
      (let [both (proj/view schema :ontologie nil
                            {:texts ["ontology.ent" "controlling.objects.ent"]})
            ctl  (body (get-in both [:files "controlling.objects.ent"]))
            r    (proj/absorb schema :ontologie
                              {"ontology.ent"
                                 (pr-str
                                  {:schema
                                     {:objects
                                        {:auftrag
                                           (assoc (get-in ctl [:objects :auftrag])
                                                  :project :controlling)}}})
                               "controlling.objects.ent"
                                 (pr-str {:schema (update ctl :objects dissoc :auftrag)})}
                              {:base (:base both) :shape (:shape both)})]
        (is (some #(= [[:objects "auftrag"] "controlling.objects.ent" "ontology.ent"] %)
                  (:moved r)))))
    (testing "a checkout that still holds a node moved since its pull is refused, not doubled"
      (let [moved-memory {:placement {[:objects "zeichnung"] "ontology.ent"}}]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"objects zeichnung moved to ontology.ent"
                              (proj/absorb schema :ontologie
                                           {"konstruktion.objects.ent" (pr-str {:schema facet})}
                                           {:base (:base one) :shape (:shape one)
                                            :memory moved-memory})))))
    (testing "a project's checkout is that project's texts"
      (is (= ["controlling.objects.ent"]
             (keys (:files (proj/view schema :ontologie nil {:project "controlling"}))))))
    (testing "a named text that holds nothing yet comes empty, to write into"
      (is (= {}
             (get-in (proj/view schema :ontologie nil {:texts ["konstruktion.apps.ent"]})
                     [:base "konstruktion.apps.ent"]))))))

(deftest a-text-of-a-project-is-pushed-without-the-text-that-declares-it
  (let [s     (assoc schema :datasets
                     {"ri.sutra.main.dataset.d1"
                      {:dataset-name :auftrag_rohdaten :kind :standalone
                       :project "ri.sutra.main.project.p1"
                       :columns [{:attribute-name :auf_nr :attribute-type :string}]}})
        texts ["ontology.ent" "controlling.ingest.ent"]
        pull  (proj/view s :ontologie nil {:texts texts})]
    (is (not (contains? (get-in pull [:base "controlling.ingest.ent"]) [:projects "controlling"]))
        "the ingest text lives in the project; it does not hold it")
    (testing "pushing it back, without controlling.objects.ent, is no move of the project"
      (let [r (proj/absorb s :ontologie (:files pull) {:base (:base pull) :shape (:shape pull)})]
        (is (empty? (:moved r)))
        (is (not (contains? (:placement (:said r)) [:projects "controlling"])))))))

(deftest what-the-render-shows-is-shown
  (let [s     (assoc schema
                     :applications {"ri.a.1" {:application-name :zeichnung-ansicht
                                              :kind :object-view
                                             :object-type :zeichnung :form-factor :panel
                                              :layout {:cards []}}})
        _     (assert (get-in s [:object-types "ri.sutra.main.object-type.o2"]))
        pull  (proj/view s :ontologie)
        views (filter #(re-find #"\.entv$" %) (keys (:files pull)))]
    (is (seq views) "the object view is one of the texts")
    (is (map? (:schema (proj/absorb s :ontologie (:files pull)
                                    {:base (:base pull) :shape (:shape pull)})))
        "a section only a unit kind of text produces (an object view) is not refused as unshown")))

(deftest a-permissions-projection-shows-only-the-relations-of-the-projects
  (let [texts (proj/render schema :berechtigungen)
        shown (body (get texts "berechtigungen.ent"))]
    (is (= ["berechtigungen.ent"] (keys texts)))
    (is (= #{:projects} (set (keys shown))))
    (is (= [:controlling] (get-in shown [:projects :konstruktion :imports])))
    (is (not (str/includes? (get texts "berechtigungen.ent") "Zeichnungen"))
        "a field it does not select is not in the text")
    (testing "a field it shows is written back, the fields it hides keep their value"
      (let [edited (pr-str {:schema (assoc-in shown [:projects :controlling :imports]
                                              [:konstruktion])})
            {:keys [schema]} (push schema :berechtigungen {"berechtigungen.ent" edited})]
        (is (= [:konstruktion] (get-in schema [:projects :controlling :imports])))
        (is (= "Zahlen" (get-in schema [:projects :controlling :description])))))
    (testing "it may change a project, not create or delete one"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"may not delete"
                            (push schema :berechtigungen
                                  {"berechtigungen.ent"
                                     (pr-str
                                      {:schema (update shown :projects dissoc :controlling)})})))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"may not create"
                            (push schema :berechtigungen
                                  {"berechtigungen.ent"
                                     (pr-str {:schema (assoc-in shown [:projects :neu] {})})}))))))

(deftest an-attribute-another-project-adds-is-that-project-s
  (let [texts (assoc (proj/render schema :ontologie)
                     "konstruktion.ent" (pr-str {:schema {:projects {:konstruktion {}}
                                                          :objects
                                                            {:auftrag
                                                               {:attributes
                                                                  [[:zeichnung-nr :string]]}}}}))
        {:keys [schema]} (push schema :ontologie texts)
        added (some #(when (= "zeichnung-nr" (name (first %))) %)
                    (get-in schema [:objects :auftrag :attributes]))]
    (is (= :konstruktion (:project (get added 2))) "the text's project owns what it added")))

(deftest a-projection-refuses-what-it-cannot-say
  (testing "a text none of its kinds reads"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"reads no text"
                          (push schema :konstruktion {"ontology.ent" "{:schema {}}"}))))
  (testing "a section it does not show"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"does not show"
                          (push schema :konstruktion
                                {"konstruktion.ent.yaml"
                                   "actions:\n  x:\n    display-name: X\n"}))))
  (testing "an undeclared projection"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no projection"
                          (proj/render schema :gibt-es-nicht)))))

(deftest the-standard-is-adopted-entry-by-entry
  (let [all (standard/entry-ids standard/standard)]
    (is (contains? all [:projections "ontologie"]))
    (is (= {} (standard/missing standard/standard #{})) "a world that has them lacks nothing")
    (is (= {:projections (dissoc standard/projections :code-repository)}
           (standard/missing {:projections (select-keys standard/projections [:code-repository])}
                             #{}))
        "one it lacks is added")
    (is (= {} (standard/missing {} all)) "and one it deleted after adopting stays deleted")))

(deftest an-entry-an-earlier-standard-wrote-is-upgraded
  (let [old
          (edn/read-string
           (slurp (io/resource "support/fixtures/standard/ontologie-c1ed94e0.edn")))]
    (is (= {:projections {:ontologie (:ontologie standard/projections)}}
           (standard/missing {:projections (assoc standard/projections :ontologie old)}
                             (standard/entry-ids standard/standard)))
        "held exactly as the standard once wrote it, so never changed: today's replaces it")
    (is (= {:projections {:ontologie (:ontologie standard/projections)}}
           (standard/missing {:projections
                                (assoc standard/projections
                                       :ontologie (assoc old :layout {:placement {}}))}
                             (standard/entry-ids standard/standard)))
        "a memory an old ontology attached does not make it its own")
    (is (= {}
           (standard/missing {:projections
                                (assoc standard/projections
                                       :ontologie (assoc old :description "unsere"))}
                             (standard/entry-ids standard/standard)))
        "one the ontology changed is its own and stays")))

(deftest a-concern-and-a-layout-are-dropped
  (let [old {:authoring   {:alt {:language :edn :match ["x.ent"]}}
             :projections (assoc-in standard/projections [:ontologie :layout] {:placement {}})
             :projects    {"ri.p.2" {:project-name :konstruktion :concern "2026_10_16__KONSTRUKTION"
                                     :stage :ontology}}}
        m   (standard/migrate old (standard/entry-ids standard/standard))]
    (is (nil? (:authoring m)) "the old declarations section is gone")
    (is (nil? (get-in m [:projects "ri.p.2" :concern])) "a project names no folder")
    (is (nil? (get-in m [:projects "ri.p.2" :stage]))
        "and has no stage: its texts' facets are the stages")
    (is (nil? (get-in m [:projections :ontologie :layout]))
        "a projection's memory is not the schema's")))

(def ^:private code
  {:projections  standard/projections
   :projects     {"ri.p.1" {:project-name :post}}
   :repositories {"ri.r.1" {:repository-name :mail :project "ri.p.1"}
                  "ri.r.2" {:repository-name :finanzen :project "ri.p.1"}}
   :actions      {"ri.a.1" {:action-name :auftragszeile-anlegen}}
   :transforms   {"ri.t.1" {:transform-name :shape-mail :language :pg-sql :query "select 1"
                            :output {:kind :dataset :dataset "ri.ds.mail"}
                            :project "ri.p.1" :repository "ri.r.1"}
                  "ri.t.2" {:transform-name :shape-mail-lesung :language :pg-sql :query "select 2"
                            :output {:kind :dataset :dataset "ri.ds.lesung"}
                            :project "ri.p.1" :repository "ri.r.2"}
                  "ri.t.3" {:transform-name :frei :language :pg-sql :query "select 3"
                            :output {:kind :dataset :dataset "ri.ds.frei"}}}})

(deftest a-code-repository-is-pulled-repository-by-repository
  (is (empty? (pe/kinds-problems (:formats (proj/declared code :code-repository)))))
  (let [{:keys [files]} (proj/view code :code-repository)]
    (is (= ["finanzen.ent" "mail.ent" "ohne-repository.ent"] (keys files))
        "each repository in its own root text, what none owns centrally — and no action")
    (is (= {:shape-mail {:inputs {} :language :pg-sql :project :post :query "select 1"
                        :output {:kind :dataset :dataset "ri.ds.mail"}}}
           (:transforms (body (get files "mail.ent"))))
        "a repository's text shows only its own transforms, its name saying the repository")
    (is (empty? (:schema (push code :code-repository files)))
        "pushed back unchanged, nothing changes")
    (is (= :mail
           (get-in (push code :code-repository
                         (update files "mail.ent" str/replace "select 1" "select 11"))
                   [:schema :transforms :shape-mail :repository]))
        "read back, a transform in mail.ent is mail's"))
  (is (= ["finanzen.ent"]
         (keys (proj/render (assoc-in code [:projections :code-repository :select :repository]
                                      :finanzen) :code-repository)))
      ":select :repository shows one repository")
  (is (= {:transforms {:shape-mail {:inputs {} :language :pg-sql :project :post :query "select 1"
                                     :output {:kind :dataset :dataset "ri.ds.mail"}
                                     :repository :mail}}}
         (select-keys (body (get-in (pe/repo-tree code :mail) ["mail.ent" :content]))
                      [:transforms]))
      "the git tree still names the repository in every entry"))

(deftest a-text-the-grammar-refuses-is-read-around-and-can-be-repaired
  (let [read-file authoring/read-file
        refused   "konstruktion.objects.ent"
        fixed?    #(str/includes? (str %) ";; repariert")]
    (with-redefs [authoring/read-file
                  (fn [formats path text]
                    (if (and (= refused path) (not (fixed? text)))
                      (throw (ex-info "Invalid .ent" {:path path}))
                      (read-file formats path text)))]
      (is (= ["controlling.objects.ent"]
             (keys (:files (proj/view schema :ontologie nil
                                      {:texts ["controlling.objects.ent"]}))))
          "another project's text pulls, though one stored text no longer reads")
      (let [{:keys [files base]} (proj/view schema :ontologie nil {:texts [refused]})]
        (is (str/includes? (get files refused) "zeichnung")
            "the refused text itself comes out as stored, to be repaired")
        (is (= {} (get base refused)) "without a base: its parts could not be read")
        (let [{:keys [opts result]}
              (proj/absorb schema :ontologie {refused (str (get files refused) "\n;; repariert\n")}
                           {:base base :shape (:shape (proj/view schema :ontologie))})]
          (is (contains? (set (map name (keys (:objects result)))) "zeichnung")
              "the repaired text says its node again")
          (is (not (contains? (set (map name (keys (:objects result)))) "auftrag"))
              "the text the push did not carry is not read")
          (is (= {} @(:strict-projected opts)) "and nothing is deleted"))))))

(deftest an-eval-suite-is-placed-in-its-projects-evals-text
  (let [s     (assoc schema :eval-suites
                     {:rechnungskopf {:project    "ri.sutra.main.project.p1"
                                      :function   :kopf-lesen
                                      :evaluators [{:name :netto :kind :exact-match
                                                    :field :netto}]}
                      :lose          {:agent      :assistent
                                      :evaluators [{:name :a :kind :contains :field :answer}]}})
        files (:files (proj/view s :ontologie))]
    (is (= [:rechnungskopf] (keys (:eval-suites (body (get files "controlling.evals.ent"))))))
    (is (contains? (:eval-suites (body (get files "ontology.ent"))) :lose)
        "a suite of no project goes where everything else without a home goes")))

(deftest an-installed-log-type-differing-only-in-placement-is-not-stale
  (let [body   {:actions {:auftrag-anlegen
                          {:project :controlling :display-name "Anlegen"
                           :action-log {:project :controlling}
                           :params {:auf-nr {:type :string}}
                           :rules [{:op :create :object-type :auftrag
                                    :values {:auf-nr [:get :auf-nr]}}]}}
                :objects {:auftrag {:attributes [[:auf-nr :string]] :primary-key :auf-nr
                                    :plural "Aufträge" :project :controlling}}}
        gen    (get-in (sugar/lower-schema body) [:objects :auftrag-anlegen-log])
        stored (-> gen
                   (dissoc :attributes :title)
                   (assoc :object-type-name :auftrag-anlegen-log
                          :title-key (:title gen)
                          :project "ri.sutra.main.project.p1"
                          :attribute-list (mapv (fn [[n t o]]
                                                  (cond-> {:attribute-name n :attribute-type t}
                                                    o (assoc :opts o)))
                                                (:attributes gen))))
        stale  (fn [extra]
                 (#'proj/stale-generators
                  (pe/exports (assoc-in schema [:object-types "ri.sutra.main.object-type.o9"]
                                        (merge stored extra))
                              {:all-projects? true})
                  body))]
    (is (empty? (stale {})))
    (is (empty? (stale {:status :active}))
        "a status the install set is placement: comparing it restated every such action")
    (is (= [[:actions "auftrag-anlegen"]] (stale {:plural "anders"}))
        "a generated field that differs is stale")))

(deftest a-pushed-text-the-grammar-refuses-is-refused-by-its-path-before-it-is-read
  (let [texts (proj/render schema :ontologie)
        path  "konstruktion.objects.ent"
        bad   (pr-str {:schema (assoc (body (get texts path)) :links
                                      [[:zeichnung-auftrag :zeichnung :stray :auftrag
                                        :many-to-one {}]])})
        data  (try (push schema :ontologie {path bad}) nil
                   (catch clojure.lang.ExceptionInfo e (ex-data e)))]
    (is (= :refusal.parser/invalid-text (:refusal/code data))
        "a link tuple with a stray element: the grammar's refusal, not a reader's cast")
    (is (= path (:source data)))))
