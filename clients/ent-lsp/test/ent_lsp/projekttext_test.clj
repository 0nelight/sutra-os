(ns ent-lsp.projekttext-test
  (:require [babashka.fs]
            [sutra.parser.authoring.registry]
            [sutra.parser.authoring.standard]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [ent-lsp.features :as feat]
            [ent-lsp.spec :as spec]
            [ent-lsp.workspace :as ws]))

(def ^:private frag "konstruktion.objects.ent")

(def ^:private base
  {"ontology.ent"
   (pr-str {:schema {:projects {:controlling {}}
                     :objects  {:auftrag {:project :controlling :title :auf-nr
                                          :attributes [[:auf-nr :string]]}}}})})

(defn- analyzed [files]
  (ws/analyze (reduce-kv ws/set-text {:root "/ws" :spec (spec/load-spec) :files {}}
                         (merge base files))))

(defn- found [w rule]
  (set (for [[file fs] (:findings w) f fs :when (= rule (:rule f))]
         [file (get-in f [:vars :name])])))

(def uri-of #(str "file:///ws/" %))

(defn- actions [w rule]
  (let [dgs (for [f (get-in w [:findings frag]) :when (= rule (:rule f))] (feat/diagnostic f))]
    (feat/code-actions w uri-of frag dgs)))

(deftest only-a-project-s-texts-at-the-top-level-are-indexed
  (let [w {:formats
             (sutra.parser.authoring.registry/texts
              sutra.parser.authoring.standard/standard :ontologie)}]
    (is (ws/in-scope? w frag))
    (is (ws/in-scope? w "konstruktion.apps.ent"))
    (is (not (ws/in-scope? w "konstruktion/entwurf.ent")))
    (is (not (ws/in-scope? w "konstruktion/konstruktion.objects.ent")))))

(deftest a-new-type-makes-the-project-text-its-authoring-place
  (let [w (analyzed {frag (pr-str {:schema {:projects {:konstruktion {:imports [:controlling]}}
                                            :objects
                                              {:zeichnung
                                                 {:title :datei
                                                  :attributes [[:datei :string]]}}}})})]
    (is (= #{[frag ":zeichnung"]} (found w :projekttext-autorenort)))
    (testing "the file's one project owns it — no :project on the entry, no layout complaint"
      (is (empty? (found w :ohne-besitzer)))
      (is (empty? (found w :falscher-ort)))
      (is (= :konstruktion (:owner (first (get-in w [:index :by-key [:object :zeichnung]]))))))))

(deftest a-type-that-exists-is-imported-not-redeclared
  (let [w (analyzed {frag (pr-str {:schema {:projects {:konstruktion {}}
                                            :objects
                                              {:auftrag
                                                 {:title :auf-nr
                                                  :attributes [[:auf-nr :string]]}}}})})]
    (is (= #{[frag ":auftrag"]} (found w :projekttext-gibt-es-schon)))
    (is (= #{["ontology.ent" ":auftrag"]} (found w :doppelte-definition))
        "the established declaration is still told about the conflict")
    (let [[a] (actions w :projekttext-gibt-es-schon)
          edits (get-in a [:edit :changes (uri-of frag)])]
      (is (str/includes? (:title a) "importiert controlling"))
      (is (= 2 (count edits)) "drop the declaration, add the import")
      (is (some #(= "" (:newText %)) edits))
      (is (some #(str/includes? (:newText %) ":imports [:controlling]") edits)))))

(deftest an-existing-type-used-without-import-offers-the-import
  (let [w (analyzed {frag (pr-str {:schema {:projects {:konstruktion {}}
                                            :objects
                                              {:zeichnung
                                                 {:title :datei :attributes [[:datei :string]]}}
                                            :links
                                              [[:zeichnung-auftrag :zeichnung :auftrag
                                                :many-to-one]]}})})]
    (is (= #{[frag ":auftrag"]} (found w :import-fehlt)))))

(deftest an-unknown-type-is-created-in-the-project-text
  (let [w (analyzed {frag (pr-str {:schema {:projects {:konstruktion {}}
                                            :objects
                                              {:zeichnung
                                                 {:title :datei :attributes [[:datei :string]]}}
                                            :links
                                              [[:zeichnung-bauteil :bauteil :zeichnung
                                                :many-to-one]]}})})
        [a] (actions w :unbekannt)
        [e] (get-in a [:edit :changes (uri-of frag)])]
    (is (str/includes? (:title a) "Autorenort"))
    (is (str/includes? (:newText e) ":bauteil {:title :name"))))

(deftest a-project-text-extends-an-imported-type-with-attributes
  (let [ext (fn [attrs imports]
              (analyzed {frag
                           (pr-str {:schema
                                      {:projects
                                         {:konstruktion
                                            (cond-> {} imports (assoc :imports imports))}
                                       :objects  {:auftrag {:attributes attrs}}}})}))
        w   (ext [[:konstruktion-stunden :double]] [:controlling])]
    (is (empty? (found w :doppelte-definition)) "a contribution, not a second declaration")
    (is (empty? (found w :projekttext-gibt-es-schon)))
    (is (empty? (found w :projekttext-autorenort)))
    (is (= :controlling (:owner (first (get-in w [:index :by-key [:object :auftrag]])))))
    (is (seq (get-in w [:index :props [:auftrag :konstruktion-stunden]]))
        "the added attribute is a property of the type, for :title, :set and :fk")
    (testing "it needs the owner imported"
      (is (= #{[frag ":auftrag"]}
             (found (ext [[:konstruktion-stunden :double]] nil) :import-fehlt))))
    (testing "and only adds"
      (is (= #{[frag nil]}
             (set (map (fn [[f _]] [f nil])
                       (found (ext [[:auf-nr :string]] [:controlling])
                              :projekttext-attribut-doppelt))))))))

(deftest a-project-text-may-call-a-type-by-its-own-name
  (let [w (analyzed {frag
                       (pr-str {:schema
                                  {:projects
                                     {:konstruktion
                                        {:imports [:controlling] :synonyms {:ba :auftrag}}}
                                   :objects
                                     {:zeichnung {:title :datei :attributes [[:datei :string]]}
                                      :ba        {:attributes [[:konstruktion-stunden :double]]}}
                                   :links    [[:zeichnung-ba :zeichnung :ba :many-to-one]]}})})]
    (is (empty? (found w :unbekannt)) "the synonym resolves, in links and in extensions")
    (is (= #{[frag ":zeichnung"]} (found w :projekttext-autorenort))
        ":ba extends :auftrag, it declares nothing")
    (is (seq (get-in w [:index :props [:auftrag :konstruktion-stunden]])))))

(deftest sutra-suggests-and-the-project-text-adopts-a-synonym
  (let [asked (atom nil)
        w (-> (analyzed
               {frag (pr-str {:schema {:projects {:konstruktion {}}
                                       :objects
                                         {:zeichnung {:title :datei :attributes [[:datei :string]]}}
                                       :links
                                         [[:zeichnung-ba :zeichnung :betriebsauftraege
                                           :many-to-one]]}})})
              (assoc :suggest (fn [nm kind] (reset! asked [nm kind])
                                [{:name :auftrag :owner :controlling :why :plural :score 0.9}])))
        as (actions w :unbekannt)
        syn (first (filter #(str/includes? (:title %) "Synonym") as))
        edits (get-in syn [:edit :changes (uri-of frag)])]
    (is (= [:betriebsauftraege :object] @asked))
    (is (str/includes? (:title syn) ":auftrag (controlling)"))
    (is (some #(str/includes? (:newText %) ":synonyms {:betriebsauftraege :auftrag}") edits))
    (is (some #(str/includes? (:newText %) ":imports [:controlling]") edits)
        "the owner is imported in the same step")
    (is (some #(str/includes? (:title %) "Autorenort") as) "creating it here stays on offer")))

(deftest without-an-instance-nothing-is-suggested-and-nothing-waits
  (let [w (analyzed {frag (pr-str {:schema {:projects {:konstruktion {}}
                                            :links [[:x :zeichnung :y :many-to-one]]}})})]
    (is (every? #(not (str/includes? (:title %) "Synonym")) (actions w :unbekannt)))))

(deftest a-project-text-hears-that-its-new-type-reads-like-an-existing-one
  (let [w (analyzed {frag (pr-str {:schema {:projects {:konstruktion {}}
                                            :objects
                                              {:auftraege {:title :x :attributes [[:x :string]]}
                                               :zeichnung
                                                 {:title :datei
                                                  :attributes [[:datei :string]]}}}})})
        f (first (filter #(= :projekttext-aehnlich (:rule %)) (get-in w [:findings frag])))]
    (is (= ":auftraege" (get-in f [:vars :name])))
    (is (str/includes? (:message f) ":auftrag (:controlling, plural)"))
    (is (= :information (:severity f)))
    (is (= 1 (count (filter #(= :projekttext-aehnlich (:rule %)) (get-in w [:findings frag]))))
        ":zeichnung resembles nothing")))

(def ^:private varianten
  {:zentral       {:language :edn :envelope [:schema] :name ["ontology.ent"] :home-for [:*]
                   :order 0}
   :ent-projekt   {:language :edn :envelope [:schema] :name ["*.ent"] :owner [:declared]}
   :ent-dateiname {:language :edn :envelope [:schema] :name ["*.ent"] :owner [:facet :file-name]}
   :ent-kompakt   {:language :edn :name ["*.ent"] :owner [:declared :facet :file-name] :order 10}
   :yaml          {:language :yaml :name ["*.ent.yaml"] :owner [:declared :facet :file-name]}})

(defn- kunden-base [authoring]
  {"ontology.ent"
   (pr-str {:schema (cond-> {:projects {:controlling {}}
                             :objects  {:kunde {:project :controlling :title :name
                                                :attributes [[:kunde-id :string] [:name :string]]}}}
                      authoring (assoc :projections {:ontologie {:filename authoring}}))})})

(defn- analyzed-on
  ([files] (analyzed-on varianten files))
  ([authoring files]
   (ws/analyze (reduce-kv ws/set-text {:root "/ws" :spec (spec/load-spec) :files {}}
                          (merge (kunden-base authoring) files)))))

(defn- findings-in [w file rule]
  (for [f (get-in w [:findings file]) :when (= rule (:rule f))] f))

(deftest an-empty-project-text-file-gets-the-skeleton-its-declaration-says
  (let [file "konstruktions-performance.ent"
        w    (analyzed-on {file ""})
        t    (feat/projekt-template w file)]
    (is (str/includes? t ":objects {}"))
    (is (not (str/includes? t ":schema")) "the compact declaration has no envelope")
    (is (not (str/includes? t ":projects")) "the name says the project")
    (testing "the skeleton reads, and is clean"
      (let [w (analyzed-on {file t})]
        (is (empty? (get-in w [:findings file])))
        (is (= :ent-kompakt (get-in w [:files file :authored :format])))))
    (testing "offered as a code action on the empty file"
      (is (= ["Projekt-Gerüst einfügen"] (map :title (feat/code-actions w uri-of file [])))))
    (testing "no skeleton for ontology.ent, nor once the file has text"
      (is (nil? (feat/projekt-template w "ontology.ent")))
      (is (empty? (feat/code-actions (analyzed-on {file t}) uri-of file []))))))

(deftest a-facet-skeleton-holds-what-the-facet-authors
  (let [w (analyzed-on (assoc varianten
                              :ingest (get-in sutra.parser.authoring.standard/projections
                                              [:ontologie :filename :ingest]))
                       {"konstruktions-performance.ingest.ent" ""})
        t (feat/projekt-template w "konstruktions-performance.ingest.ent")]
    (is (not (str/includes? t ":projects")))
    (is (str/includes? t ":datasets"))
    (is (str/includes? t ":sources"))
    (is (not (str/includes? t ":objects")) "only what the ingest kind may hold")
    (is (str/includes? t "Quellen, Datasets") "headed by the kind's own description")))

(deftest a-file-no-kind-of-text-reads-is-reported
  (let [file "konstruktions-performance.ent"
        w    (analyzed-on (select-keys varianten [:zentral])
                          {file (pr-str {:schema {:objects {:zeichnung {}}}})})]
    (is (seq (findings-in w file :nicht-deklariert)))
    (is (nil? (feat/projekt-template w file)) "no kind of text, no skeleton")))

(deftest a-type-that-exists-is-imported-not-declared-again-in-a-root-project-text
  (let [file "konstruktions-performance.ent"
        w    (analyzed-on
              {file (pr-str {:schema {:projects {:konstruktions-performance {}}
                                      :objects
                                        {:kunde {:title :name
                                                 :attributes [[:kunde-id :string]]}}}})})
        [f]  (findings-in w file :projekttext-gibt-es-schon)
        [a]  (feat/code-actions w uri-of file [(feat/diagnostic f)])
        es   (get-in a [:edit :changes (uri-of file)])]
    (is (some? f))
    (is (str/includes? (:title a) "konstruktions-performance importiert controlling"))
    (is (some #(= "" (:newText %)) es) "the declaration goes")
    (is (some #(str/includes? (:newText %) ":imports [:controlling]") es)
        "the import comes, in the same file")))

(deftest when-only-the-name-says-the-project-the-import-declares-it
  (let [file "konstruktions-performance.ent"
        w
          (analyzed-on {file
                          (pr-str {:objects
                                     {:kunde {:title :name :attributes [[:kunde-id :string]]}}})})
        [f]  (findings-in w file :projekttext-gibt-es-schon)
        [a]  (feat/code-actions w uri-of file [(feat/diagnostic f)])
        es   (get-in a [:edit :changes (uri-of file)])]
    (is (some? f))
    (is (some #(str/includes? (:newText %)
                              ":projects {:konstruktions-performance {:imports [:controlling]}}")
              es))))

(deftest a-yaml-project-text-is-indexed-like-any-other
  (let [file "konstruktions-performance.ent.yaml"
        w
          (analyzed-on
           {file
              "objects:\n  zeichnung:\n    title: datei\n    attributes:\n      - [datei, string]\n"})
        [d]  (get-in w [:index :by-key [:object :zeichnung]])]
    (is (= :yaml (get-in w [:files file :authored :format])))
    (is (= :konstruktions-performance (:owner d)))
    (is (empty? (findings-in w file :nicht-deklariert)))))

(deftest facets-of-one-project-text-split-a-type-and-only-a-contradiction-is-reported
  (let [modell "konstruktions-performance.objektmodell.ent"
        ingest "konstruktions-performance.objects.ent"
        files  (fn [desc]
                 {modell (pr-str {:schema {:projects {:konstruktions-performance {}}
                                           :objects
                                             {:zeichnung {:title :datei :description "Zeichnung"
                                                          :attributes [[:datei :string]]}}}})
                  ingest (pr-str {:objects {:zeichnung {:description desc}}})})
        ok     (analyzed-on (files "Zeichnung"))
        bad    (analyzed-on (files "CAD-Datei"))]
    (is (empty? (findings-in ok modell :doppelte-definition)))
    (is (empty? (findings-in ok ingest :doppelte-definition)))
    (is (empty? (findings-in ok ingest :feld-widerspruch)))
    (is (= :konstruktions-performance
           (:owner (first (get-in ok [:index :by-key [:object :zeichnung]])))))
    (is (seq (findings-in bad ingest :feld-widerspruch)))
    (is (str/includes? (get-in (first (findings-in bad ingest :feld-widerspruch)) [:vars :fields])
                       ":description"))))

(deftest a-pulled-projection-folder-is-read-with-the-projection-s-kinds-of-text
  (let [root (str (babashka.fs/create-temp-dir))]
    (spit (str root "/" ws/marker)
          (pr-str {:projection "werkstatt"
                   :filename   {:werkstatt {:language :yaml :name ["werkstatt.yaml"]
                                            :home-for [:*]}}}))
    (spit (str root "/werkstatt.yaml")
          "objects:\n  maschine:\n    attributes:\n      - [m-nr, string]\n")
    (spit (str root "/README.md") "nicht gelesen")
    (let [w (ws/open-workspace (spec/load-spec) root)]
      (is (= #{"werkstatt.yaml"} (set (keys (:files w))))
          "what the projection reads, and nothing else")
      (is (= #{:werkstatt} (set (keys (:formats w))))))))

(deftest the-importer-extends-a-type-both-only-attribute
  (let [texts (fn [imports]
                {"ontology.ent"     (pr-str {:schema {:projects {:controlling {}}
                                                      :objects  {:auftrag {:project :controlling
                                                                           :attributes
                                                                             [[:auf-nr
                                                                               :string]]}}}})
                 "konstruktion.ent" (pr-str {:schema {:projects {:konstruktion {:imports imports}}
                                                      :objects
                                                        {:auftrag
                                                           {:attributes [[:stunden :double]]}}}})})
        dupes (fn [w] (concat (findings-in w "konstruktion.ent" :projekttext-gibt-es-schon)
                              (findings-in w "konstruktion.ent" :doppelte-definition)))]
    (is (empty? (dupes (analyzed-on varianten (texts [:controlling]))))
        "it imports the owner: an extension, not a second declaration")
    (is (seq (dupes (analyzed-on varianten (texts []))))
        "no import: a second owner")))
