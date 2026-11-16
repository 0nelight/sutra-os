(ns ^:unit platform.project-text-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.parser.authoring :as authoring]
            [sutra.parser.authoring.registry :as registry]
            [sutra.parser.authoring.standard :as standard]
            [sutra.parser.ent :as ent]
            [sutra.parser.project-export :as pe]
            [sutra.parser.projection :as proj]))

(def ^:private own "konstruktion.objects.ent")
(def ^:private ingest "konstruktion.ingest.ent")
(def ^:private apps "konstruktion.apps.ent")

(def ^:private layout
  {:placement {[:projects "konstruktion"] own}
   :files     {own {:format :objekte :owner "konstruktion" :rule :facet}}})

(def ^:private schema
  {:projections  (assoc-in standard/projections [:ontologie :layout] layout)
   :projects     {"ri.sutra.main.project.p1" {:project-name :controlling}
                  "ri.sutra.main.project.p2" {:project-name :konstruktion
                                              :imports      ["ri.sutra.main.project.p1"]}}
   :object-types {"ri.sutra.main.object-type.o1"
                  {:object-type-name :auftrag
                   :project          "ri.sutra.main.project.p1"
                   :attribute-list   [{:attribute-name :auf-nr :attribute-type :string}]
                   :primary-key      :auf-nr
                   :plural "Aufträge"}
                  "ri.sutra.main.object-type.o2"
                  {:object-type-name :zeichnung
                   :project          "ri.sutra.main.project.p2"
                   :attribute-list   [{:attribute-name :datei :attribute-type :string}
                                      {:attribute-name :auf-nr :attribute-type :string}]
                   :primary-key      :datei
                   :plural "Zeichnungen"}}
   :link-types   {"ri.sutra.main.link-type.l1"
                  {:link-type-name   :zeichnung-auftrag
                   :from-object-type :zeichnung
                   :to-object-type   :auftrag
                   :cardinality      :many-to-one
                   :project          "ri.sutra.main.project.p2"}}
   :datasets     {"ri.sutra.main.dataset.d1"
                  {:dataset-name :zeichnung_uploads :kind :standalone
                   :project "ri.sutra.main.project.p2"}}
   :actions      {:zeichnung-einlesen {:project :konstruktion :display-name "Einlesen"}
                  :auftrag-anlegen    {:project :controlling :display-name "Anlegen"}
                  :frei               {:display-name "Ohne Projekt"}}})

(defn- read-at [texts path]
  (:schema (edn/read-string (get texts path))))

(defn- by-name [m nm]
  (some (fn [[k v]] (when (= nm (name k)) v)) m))

(deftest a-project-lays-out-into-its-facets
  (let [texts (proj/render schema :ontologie)
        mine  (read-at texts own)
        ont   (read-at texts pe/ontology-ent-path)]
    (is (= #{"zeichnung"} (set (map name (keys (:objects mine))))))
    (is (= ["zeichnung-auftrag"] (mapv (comp name first) (:links mine))))
    (is (= #{"zeichnung_uploads"} (set (map name (keys (:datasets (read-at texts ingest))))))
        "a project's datasets go to its ingest facet, not into transform.ent")
    (is (nil? (get texts pe/transform-ent-path)))
    (is (= #{"zeichnung-einlesen"} (set (map name (keys (:actions mine)))))
        "its actions to its objects facet, beside the types they act on")
    (is (nil? (get texts apps)) "a project with nothing for the apps facet has no apps text")
    (testing "a project without a text so far gets its facets too"
      (is (= #{"auftrag"}
             (set (map name (keys (:objects (read-at texts "controlling.objects.ent")))))))
      (is (= #{"auftrag-anlegen"}
             (set (map name (keys (:actions (read-at texts "controlling.objects.ent"))))))))
    (testing "and nothing it declares is declared a second time centrally"
      (is (nil? (:objects ont)))
      (is (empty? (:links ont)))
      (is (= #{"frei"} (set (map name (keys (:actions ont)))))
          "an action no project owns stays central"))
    (testing "the project is said once, by its objects facet; the others say it by name"
      (is (= #{"konstruktion"} (set (map name (keys (:projects mine))))))
      (doseq [p [own ingest]]
        (is (not (str/includes? (get texts p) ":project ")) p)))))

(deftest reading-a-project-s-facets-restores-what-the-lay-out-left-implicit
  (let [texts (proj/render schema :ontologie)
        back  (pe/owned-body own (read-at texts own))]
    (is (= :konstruktion (:project (by-name (:objects back) "zeichnung"))))
    (is (= :konstruktion (get-in back [:links 0 4 :project])))
    (is (= :konstruktion (:project (by-name (:actions back) "zeichnung-einlesen"))))))

(def ^:private onto
  (registry/texts standard/standard :ontologie))

(deftest a-project-s-facets-are-schema-and-the-rest-is-content
  (doseq [p [own ingest apps]]
    (is (= :entity (pe/plane-of onto p)) p))
  (is (= :file (pe/plane-of onto "konstruktion/README.md")))
  (is (= :file (pe/plane-of onto "konstruktion/konstruktion.objects.ent"))
      "only a text at the top level is read"))

(deftest a-facet-holds-only-its-sections
  (let [read #(pe/read-texts onto [%1] {%1 (pr-str {:schema %2})})]
    (is (some? (read own
                     {:objects {:zeichnung {}}
                      :actions {:zeichnung-einlesen {:display-name "Einlesen"}}}))
        "an action lives with the types it acts on")
    (is (some? (read apps {:workspaces {:zeichnungen {}}})))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #":actions"
                          (read apps {:actions {:x {}}}))
        "an action in the apps facet is refused, naming the section")
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #":objects"
                          (read ingest {:objects {:zeichnung {}}}))
        "and the generic *.ent kinds do not read it instead: the name decides the kind")))

(deftest the-section-a-project-already-keeps-somewhere-stays-there
  (let [modell "konstruktion.modell.ent"
        memory {:placement {[:objects "zeichnung"] modell [:projects "konstruktion"] modell}
                :files     {modell {:format :ent-dateiname :owner "konstruktion" :rule :facet}}}
        s      (assoc-in schema [:object-types "ri.sutra.main.object-type.o3"]
                         {:object-type-name :bauteil :project "ri.sutra.main.project.p2"
                          :attribute-list [{:attribute-name :nr :attribute-type :string}]})
        texts  (update-vals (pe/lay-out s
                                        {:scope {:all-projects? true} :sections pe/ontology-sections
                                         :formats onto :memory memory})
                            :content)]
    (is (= #{"bauteil" "zeichnung"} (set (map name (keys (:objects (read-at texts modell))))))
        "a new type joins the text where the project keeps its types")
    (is (nil? (:objects (read-at texts own))) "no second text for its types beside it")
    (is (seq (:links (read-at texts own))) "links it keeps nowhere yet go to their declared home")
    (is (contains? (:actions (read-at texts own)) :zeichnung-einlesen)
        "a section it keeps nowhere goes to its declared home")))

(deftest a-placement-its-kind-now-refuses-is-re-homed
  (let [memory {:placement {[:actions "zeichnung-einlesen"] apps}
                :files     {apps {:format :apps :owner "konstruktion" :rule :facet}}}
        texts  (update-vals (pe/lay-out schema
                                        {:scope {:all-projects? true} :sections pe/ontology-sections
                                         :formats onto :memory memory})
                            :content)]
    (is (nil? (get texts apps)) "the apps facet never gets the action written into it")
    (is (contains? (:actions (read-at texts own)) :zeichnung-einlesen))
    (testing "and the laid-out texts read back"
      (is (some? (pe/read-texts onto (keys texts) texts))))))

(deftest a-tree-that-does-not-read-as-a-whole-keeps-the-placement-of-its-other-texts
  (let [prior
          {"ontology.ent"
             (pr-str {:schema {:objects {:auftrag {:attributes [[:auf-nr :string]]
                                                   :plural "Aufträge"}}}})
           own
             (pr-str {:schema {:objects {:zeichnung {:attributes [[:datei :string]]
                                                     :plural "Zeichnungen"}}}})
           apps            (pr-str {:schema {:objects {:zeichnung {}}}})}]
    (is (thrown? clojure.lang.ExceptionInfo (pe/read-texts onto (keys prior) prior)))
    (is (= {[:objects "auftrag"] "ontology.ent" [:objects "zeichnung"] own}
           (select-keys (:placement (pe/placement-of onto prior))
                        [[:objects "auftrag"] [:objects "zeichnung"]])))))

(deftest a-tree-is-read-with-the-kinds-its-central-text-declares
  (let [kinds (assoc (:filename (:ontologie standard/projections))
                     :modell {:language :edn :envelope [:schema] :name ["*.modell.ent"]
                              :facet "modell"
                              :owner [:facet] :sections [:objects] :order 0})
        tree  {"ontology.ent" (pr-str {:schema {:projections {:ontologie {:filename kinds}}}})}]
    (is (contains? (pe/tree-formats standard/standard :ontologie (keys tree) tree) :modell))
    (is (not (contains? (pe/tree-formats standard/standard :ontologie ["x.ent"] {"x.ent" "{}"})
                        :modell))
        "a tree that declares none is read with the live kinds")))

(deftest a-projection-s-kinds-are-checked-as-a-whole
  (is (empty? (pe/kinds-problems onto)) "the standard is sound")
  (let [bad (assoc onto
                   :zweites  {:name ["*.model.ent"] :owner [:facet] :facet "model"
                              :sections [:objects] :home-for [:objects]}
                   :anders   {:name ["*.objects.ent"] :owner [:facet] :facet "objects"
                              :sections [:objects]}
                   :wertlos  {:name ["*.werte.ent"] :owner [:facet] :facet "werte"
                              :sections [:value-types]}
                   :daneben  {:name ["*.x.ent"] :owner [:declared] :sections [:objects]
                              :home-for [:links]})
        ps  (str/join "\n" (pe/kinds-problems (registry/formats bad)))]
    (is (str/includes? ps "section objects has 2 project homes"))
    (is (str/includes? ps "pattern *.objects.ent"))
    (is (str/includes? ps "kind wertlos: a project's text cannot hold [:value-types]"))
    (is (str/includes? ps "kind daneben: :home-for [:links] is not among its :sections"))
    (is (str/includes? ps "kind daneben: a project home does not let the name say the project"))))

(deftest a-project-home-is-the-kind-s-pattern-with-the-project-in-it
  (is (= "controlling.objects.ent" (authoring/project-home onto :controlling :actions)))
  (is (= "controlling.apps.ent" (authoring/project-home onto :controlling :workspaces)))
  (is (= "controlling.objects.ent" (authoring/project-home onto :controlling :projects)))
  (is (nil? (authoring/project-home onto :controlling :value-types)))
  (is (= "ontology.ent" (authoring/home onto :value-types))))

(defn- refusal [schema]
  (try (pe/owned-body own schema) nil
       (catch clojure.lang.ExceptionInfo e (ex-message e))))

(deftest what-a-project-s-text-may-not-say
  (testing "an unowned section would be laid out back into ontology.ent"
    (is (str/includes? (refusal {:projects {:k {}} :value-types {:v {}}}) ":value-types")))
  (testing "the text IS one project"
    (is (str/includes? (refusal {:objects {:x {}}}) "exactly one project"))
    (is (str/includes? (refusal {:projects {:k {} :l {}}}) "exactly one project")))
  (testing "a resource of another project belongs in that project's text"
    (is (str/includes? (refusal {:projects {:k {}} :objects {:x {:project :controlling}}})
                       "controlling"))))

(deftest a-malformed-link-tuple-is-left-to-the-grammar
  (let [bad  [:l :a :b :many-to-one :stray {:x 1}]
        body (pe/owned-body own {:projects {:k {}} :links [bad]})]
    (is (= [bad] (:links body)) "kept as written, not cast")
    (is (= :refusal.parser/invalid-text
           (try (ent/validate-schema! (dissoc body :projects) "t.ent") nil
                (catch clojure.lang.ExceptionInfo e (:refusal/code (ex-data e))))))))

(deftest an-attribute-another-project-added-lays-out-into-that-project-s-text
  (let [s     (assoc-in schema [:object-types "ri.sutra.main.object-type.o1" :attribute-list]
                        [{:attribute-name :auf-nr :attribute-type :string}
                         {:attribute-name :konstruktion-stunden :attribute-type :double
                          :opts {:project :konstruktion :name "Konstruktionsstunden"}}])
        texts (proj/render s :ontologie)
        added (by-name (:objects (read-at texts own)) "auftrag")]
    (is (= {:attributes [[:konstruktion-stunden :double {:name "Konstruktionsstunden"}]]} added)
        "an entry that ONLY adds attributes — the type stays declared where it is")
    (is (= ["auf-nr"]
           (mapv (comp name first)
                 (:attributes (by-name (:objects (read-at texts "controlling.objects.ent"))
                                       "auftrag")))))
    (testing "and read back, the attribute is that project's"
      (let [back (:result (proj/absorb s :ontologie texts
                                       (select-keys (proj/view s :ontologie) [:base :shape])))
            a    (some #(when (= "konstruktion-stunden" (name (first %))) %)
                       (get-in back [:objects :auftrag :attributes]))]
        (is (= :konstruktion (:project (get a 2))))))))

(deftest a-checkout-of-one-text-may-hold-an-extension-of-another-project-s-type
  (let [s     (assoc-in schema [:object-types "ri.sutra.main.object-type.o1" :attribute-list]
                        [{:attribute-name :auf-nr :attribute-type :string}
                         {:attribute-name :konstruktion-stunden :attribute-type :double
                          :opts {:project :konstruktion}}])
        pull  (proj/view s :ontologie nil {:texts [own]})
        text  (get-in pull [:files own])
        r     (proj/absorb s :ontologie
                           {own (str/replace text ":konstruktion-stunden :double"
                                             ":konstruktion-stunden :long")}
                           {:base (:base pull) :shape (:shape pull)})]
    (is (some #(and (= "konstruktion-stunden" (name (first %))) (= :long (second %)))
              (get-in r [:result :objects :auftrag :attributes]))
        "the extension is pushed from its own text, the type stays where it is defined")))

(deftest a-project-writes-its-own-names-and-the-platform-reads-the-real-ones
  (let [s     (-> schema
                  (assoc-in [:projects "ri.sutra.main.project.p2" :synonyms] {:ba :auftrag})
                  (assoc-in [:object-types "ri.sutra.main.object-type.o1" :attribute-list]
                            [{:attribute-name :auf-nr :attribute-type :string}
                             {:attribute-name :h :attribute-type :double
                              :opts {:project :konstruktion}}]))
        mine  (read-at (proj/render s :ontologie) own)]
    (testing "the text speaks the project's language"
      (is (= [:zeichnung-auftrag :zeichnung :ba] (subvec (first (:links mine)) 0 3)))
      (is (= {:attributes [[:h :double]]} (by-name (:objects mine) "ba")))
      (is (= {:ba :auftrag} (:synonyms (val (first (:projects mine)))))))
    (testing "and reading it back resolves to the declared names"
      (let [back (pe/owned-body own mine)]
        (is (= [:zeichnung-auftrag :zeichnung :auftrag] (subvec (first (:links back)) 0 3)))
        (is (some? (by-name (:objects back) "auftrag")))))
    (testing "a synonym cannot shadow a type the project itself declares"
      (is (str/includes? (refusal {:projects {:k {:synonyms {:zeichnung :auftrag}}}
                                   :objects
                                     {:zeichnung {:title :datei :attributes [[:datei :string]]
                                                  :plural "Zeichnungen"}}})
                         "synonym zeichnung")))))
