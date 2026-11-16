(ns ^:unit platform.ontology-graph-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.parser.authoring :as authoring]
            [sutra.parser.authoring.registry :as registry]
            [sutra.parser.ent-graph :as graph]
            [sutra.parser.project-export :as pe]))

(def ^:private varianten
  {:zentral       {:language :edn :envelope [:schema] :name ["ontology.ent"] :home-for [:*]
                   :order 0}
   :ent-projekt   {:language :edn :envelope [:schema] :name ["*.ent"] :owner [:declared]}
   :ent-dateiname {:language :edn :envelope [:schema] :name ["*.ent"] :owner [:facet :file-name]}
   :ent-kompakt   {:language :edn :name ["*.ent"] :owner [:declared :facet :file-name]}
   :yaml          {:language :yaml :name ["*.ent.yaml"] :owner [:declared :facet :file-name]}})

(def ^:private formats (registry/formats varianten))

(def ^:private schema
  {:projections  {:ontologie {:filename varianten}}
   :projects     {"ri.sutra.main.project.p1" {:project-name :controlling}
                  "ri.sutra.main.project.p2" {:project-name :konstruktion}}
   :object-types {"ri.sutra.main.object-type.o1"
                  {:object-type-name :auftrag :project "ri.sutra.main.project.p1"
                   :attribute-list [{:attribute-name "auf-nr" :attribute-type "string"}]
                   :primary-key ["auf-nr"]}
                  "ri.sutra.main.object-type.o2"
                  {:object-type-name :zeichnung :project "ri.sutra.main.project.p2"
                   :description "Eine Zeichnung"
                   :attribute-list [{:attribute-name "datei" :attribute-type "string"}
                                    {:attribute-name "auf-nr" :attribute-type "string"}]
                   :primary-key ["datei"]}}
   :link-types   {"ri.sutra.main.link-type.l1"
                  {:link-type-name :zeichnung-auftrag :from-object-type :zeichnung
                   :to-object-type :auftrag :cardinality :many-to-one
                   :project "ri.sutra.main.project.p2"}}})

(def ^:private modell "konstruktion.objektmodell.ent")
(def ^:private prosa "konstruktion.beschreibung.ent.yaml")

(def ^:private prior
  {"ontology.ent" (pr-str {:schema {:projects  {:controlling {}}
                                    :objects
                                      {:auftrag
                                         {:project :controlling :attributes [[:auf-nr :string]]}}}})
   modell         (pr-str {:objects {:zeichnung {:attributes [[:datei :string] [:auf-nr :string]]}}
                           :links   [[:zeichnung-auftrag :zeichnung :auftrag :many-to-one]]})
   prosa          "objects:\n  zeichnung:\n    description: Eine Zeichnung\n"
   "README.md"    "# nicht gelesen"})

(defn- read-back [tree]
  (pe/read-texts formats (keys tree) #(:content (get tree %))))

(deftest one-resource-may-sit-in-several-files
  (let [{:keys [schema placement fields attributes]}
        (graph/merge-bodies [["a.ent" {:objects {:x {:attributes [[:id :string]]}}}]
                             ["b.ent" {:objects {:x {:description "X"}}}]])]
    (is (= {:attributes [[:id :string]] :description "X"} (get-in schema [:objects :x])))
    (is (= "a.ent" (placement [:objects "x"])) "the part with the attributes is the primary")
    (is (= "b.ent" (fields [[:objects "x"] :description])))
    (is (empty? attributes))))

(deftest attributes-merge-by-name-and-a-contradiction-names-both-files
  (let [{:keys [schema attributes]}
        (graph/merge-bodies [["a.ent"
                              {:objects {:x {:description "X" :attributes [[:id :string]]}}}]
                             ["b.ent" {:objects {:x {:attributes [[:id :string] [:h :double]]}}}]])]
    (is (= [[:id :string] [:h :double]] (get-in schema [:objects :x :attributes])))
    (is (= "b.ent" (attributes [[:objects "x"] "h"]))))
  (let [e (try (graph/merge-bodies [["a.ent" {:objects {:x {:description "X"}}}]
                                    ["b.ent" {:objects {:x {:description "Y"}}}]])
               nil
               (catch clojure.lang.ExceptionInfo e (ex-message e)))]
    (is (str/includes? e "a.ent and b.ent"))
    (is (str/includes? e ":description")))
  (testing "a link is one part: two different links under one name collide"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"a.ent and b.ent both declare :links l"
                          (graph/merge-bodies [["a.ent" {:links [[:l :x :y :many-to-one]]}]
                                               ["b.ent" {:links [[:l :x :z :many-to-one]]}]])))))

(deftest distribute-writes-every-part-back-where-it-was-read
  (let [files [["a.ent" {:objects {:x {:attributes [[:id :string]]}}}]
               ["b.ent" {:objects {:x {:description "X" :attributes [[:h :double]]}}}]]
        read  (graph/merge-bodies files)
        back  (graph/distribute (:schema read) (fn [node _] (get-in read [:placement node])) read)]
    (is (= (into {} files) back))))

(deftest each-text-is-read-by-the-kind-it-is
  (let [{:keys [files schema]} (pe/read-texts formats (keys prior) prior)]
    (is (= :zentral (get-in files ["ontology.ent" :format])))
    (is (= :ent-kompakt (get-in files [modell :format])))
    (is (= :yaml (get-in files [prosa :format])))
    (is (not (contains? files "README.md")))
    (is (= {:attributes [[:datei :string] [:auf-nr :string]] :description "Eine Zeichnung"
            :project :konstruktion}
           (get-in schema [:objects :zeichnung])))
    (is (contains? (:projects schema) :konstruktion) "named by the files, declared nowhere")))

(deftest the-projection-keeps-every-file-in-its-format
  (let [tree (pe/ontology-tree schema prior)]
    (is (= #{"ontology.ent" modell prosa} (set (keys tree))))
    (testing "the compact file stays compact: no :schema, no :projects, no :project"
      (let [body (edn/read-string (:content (get tree modell)))]
        (is (not (contains? body :schema)))
        (is (not (contains? body :projects)))
        (is (nil? (get-in body [:objects :zeichnung :project])))
        (is (= ["zeichnung-auftrag"] (mapv (comp name first) (:links body))))))
    (testing "the YAML file stays YAML and holds the description"
      (is (str/includes? (:content (get tree prosa)) "description: Eine Zeichnung"))
      (is (not (str/includes? (:content (get tree prosa)) "{:"))))
    (testing "the projection itself is laid out into ontology.ent"
      (is (str/includes? (:content (get tree "ontology.ent")) ":projections")))
    (testing "and the projection reads back to the same graph"
      (let [back (read-back tree)]
        (is (= "Eine Zeichnung" (get-in back [:schema :objects :zeichnung :description])))
        (is (= :konstruktion (get-in back [:schema :objects :zeichnung :project])))
        (is (= prosa (get-in back [:fields [[:objects "zeichnung"] :description]])))))))

(deftest a-new-resource-goes-to-the-file-of-its-project
  (let [s    (assoc-in schema [:object-types "ri.sutra.main.object-type.o3"]
                       {:object-type-name :bauteil :project "ri.sutra.main.project.p2"
                        :attribute-list [{:attribute-name "nr" :attribute-type "string"}]})
        tree (pe/ontology-tree s prior)]
    (is (contains? (:objects (edn/read-string (:content (get tree modell)))) :bauteil))))

(deftest without-a-prior-tree-the-projection-is-the-single-file
  (is (= ["ontology.ent"] (keys (pe/ontology-tree schema)))))

(deftest owner-rules
  (let [f formats]
    (is (= {:format :ent-dateiname :owner "konstruktions-performance" :rule :file-name}
           (select-keys (authoring/read-file f "konstruktions-performance.ent"
                                             "{:schema {:objects {}}}")
                        [:format :owner :rule])))
    (is (= :facet (:rule (authoring/read-file f "kp.ingest.ent" "{:objects {}}"))))
    (is (= {:format :ent-projekt :owner "anders"}
           (select-keys (authoring/read-file f "kp.ent" "{:schema {:projects {:anders {}}}}")
                        [:format :owner])))
    (is (= :zentral
           (:format (authoring/read-file f "ontology.ent" "{:schema {:projects {:a {} :b {}}}}"))))
    (is (nil? (authoring/read-file f "README.md" "x")))
    (testing "a facet file naming another project than its name is refused"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"named for project kp"
                            (pe/read-texts f ["kp.ingest.ent"]
                                           {"kp.ingest.ent"
                                              (pr-str {:schema {:projects {:anders {}}}})}))))))

(defn- two-projects [konstruktion-project]
  {"ontology.ent"     (pr-str {:schema {:projects {:controlling {}}
                                        :objects
                                          {:auftrag
                                             {:project :controlling
                                              :attributes [[:auf-nr :string]]}}}})
   "konstruktion.ent" (pr-str {:schema {:projects {:konstruktion konstruktion-project}
                                        :objects  {:auftrag {:attributes [[:stunden :double]]}}}})})

(deftest the-project-that-defines-a-type-owns-it-and-an-importer-extends-it
  (testing "both parts hold only attributes: the import says which one extends"
    (let [texts (two-projects {:imports [:controlling]})
          a     (get-in (pe/read-texts formats (keys texts) texts) [:schema :objects :auftrag])]
      (is (= :controlling (:project a)) "the owner is the project that defines it")
      (is (= [[:auf-nr :string] [:stunden :double {:project :konstruktion}]] (:attributes a))
          "the importer's attribute is its own")))
  (testing "neither importing the other, two projects declare one type — refused"
    (let [texts (two-projects {})]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"imports it instead"
                            (pe/read-texts formats (keys texts) texts))))))
