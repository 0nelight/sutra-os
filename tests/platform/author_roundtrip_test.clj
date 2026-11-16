(ns ^:unit platform.author-roundtrip-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.cli.verbs.constructs]
            [sutra.parser.ent :as ent]
            [sutra.parser.sugar :as sugar]
            [sutra.authoring.http.resource-text :as rtext]
            [sutra.types.shape :as tshape]
            [clojure.edn]))

(def ^:private live-type
  {:object-type-name :rechnung
   :display-name     "Ausgangsrechnung"
   :description      "eine Rechnung"
   :attribute-list   [{:attribute-name :nr :attribute-type :string}]})

(deftest a-door-prints-the-display-name-the-grammar-reads
  (testing "the entity table says which sections write it :name"
    (is (= #{:object-types :shared-properties} sugar/name-sugar-sections)))
  (let [printed (sugar/raise-name-sugar
                 :object-types
                 (tshape/->author :object-types live-type {:attributes [[:nr :string]]}))]
    (testing "the printed node carries :name, and the grammar takes it"
      (is (= "Ausgangsrechnung" (:name printed)))
      (is (not (contains? printed :display-name)))
      (is (nil? (ent/explain-schema {:objects {:rechnung printed}}))))
    (testing "what the printer used to emit is refused at the same door — the
              round-trip is the test, not the field name"
      (is (seq (ent/explain-schema
                {:objects {:rechnung (-> printed (dissoc :name)
                                         (assoc :display-name "Ausgangsrechnung"))}}))))))

(deftest a-section-without-the-sugar-keeps-its-field
  (let [printed (sugar/raise-name-sugar :actions {:display-name "Rechnung ablegen" :rules []})]
    (is (= "Rechnung ablegen" (:display-name printed)))
    (is (nil? (ent/explain-schema {:actions {:rechnung-ablegen printed}})))))

(deftest the-resource-editor-prints-what-it-saves
  (let [schema {:shared-properties
                {"sp_1" {:shared-property-name :iban
                         :display-name "IBAN"
                         :type :string}}}
        text   (rtext/text schema :shared-properties "sp_1"
                           (get-in schema [:shared-properties "sp_1"]))
        node   (clojure.edn/read-string text)]
    (is (= "IBAN" (:name node)))
    (is (nil? (ent/explain-schema {:shared-properties {:iban node}})))))

(def ^:private live-body
  {:projects     {"ri.sutra.main.project.9" {:project-name :finanzen}}
   :repositories {"ri.sutra.main.repository.7" {:repository-name :finanzen-code}}
   :datasets     {"ri.sutra.main.dataset.5" {:dataset-name :offene_posten}}
   :object-types {"ri.sutra.main.object-type.8" {:object-type-name :rechnung :plural "Rechnungen"}}
   :transforms
   {"ri.sutra.main.transform.1"
    {:transform-name :offene-posten
     :function-name  :offene-posten
     :language       :clojure
     :code           "(fn [in] [])"
     :inputs         {:rechnungen {:kind :object-type :object-type "ri.sutra.main.object-type.8"}}
     :project        "ri.sutra.main.project.9"
     :repository     "ri.sutra.main.repository.7"}
    "ri.sutra.main.transform.2"
    {:transform-name :posten-bauen
     :language       :clojure
     :code           "(fn [in] [])"
     :inputs         {:rechnungen {:kind :object-type :object-type "ri.sutra.main.object-type.8"}}
     :output         {:kind :dataset :dataset "ri.sutra.main.dataset.5"}
     :project        "ri.sutra.main.project.9"
     :repository     "ri.sutra.main.repository.7"}}})

(defn- rids [m] (filter #(and (string? %) (str/starts-with? % "ri.sutra.")) (tree-seq coll? seq m)))

(deftest a-body-edit-sends-the-body-with-the-shape-that-carries-it
  (let [entry  (#'sutra.cli.verbs.constructs/editable-entry live-body :transform "posten-bauen")
        staged ((:writer entry) "(fn [in] [{}])")]
    (testing "a compute body is written back with its declaration — a transform
              whose wiring stands in the text loses it if the write omits it"
      (is (= "clj" (:ext entry)))
      (is (= {:transforms
              {:posten-bauen {:language :clojure
                              :code "(fn [in] [{}])"
                              :inputs {:rechnungen {:kind :object-type :object-type :rechnung}}
                              :output {:kind :dataset :dataset :offene_posten}
                              :project :finanzen
                              :repository :finanzen-code}}}
             staged)))
    (testing "and every reference is a NAME — the installed form keys by rid, no
              authored install reads one"
      (is (empty? (rids staged))))
    (testing "what it sends, the grammar takes"
      (is (nil? (ent/explain-schema staged))))))

(deftest a-function-body-is-staged-where-a-function-lives
  (testing "a Function is a Function because of its section and its :function-name;
            staged under :transforms it would be a build transform without an
            output, which the install refuses (output-mutex)"
    (let [entry  (#'sutra.cli.verbs.constructs/editable-entry live-body :transform "offene-posten")
          staged ((:writer entry) "(fn [in] [{}])")]
      (is (= {:functions
              {:offene-posten {:language :clojure
                               :code "(fn [in] [{}])"
                               :inputs {:rechnungen {:kind :object-type :object-type :rechnung}}
                               :project :finanzen
                               :repository :finanzen-code}}}
             staged))
      (is (nil? (ent/explain-schema staged))))))

(deftest a-function-on-a-transform-is-edited-where-its-body-is
  (testing "a :clojure Function lives under :transforms live; `function edit` finds
            it instead of saying the name has no editable body"
    (let [entry (#'sutra.cli.verbs.constructs/editable-entry live-body :function "offene-posten")]
      (is (= "(fn [in] [])" (:text entry)))
      (is (= {:functions {:offene-posten {:language :clojure
                                          :code "x"
                                          :inputs
                                          {:rechnungen
                                           {:kind :object-type :object-type :rechnung}}
                                          :project :finanzen
                                          :repository :finanzen-code}}}
             ((:writer entry) "x"))))))

(def ^:private live-schema
  {:projects   {"ri.sutra.main.project.9" {:project-name :beschaffung}}
   :interfaces {"ri.sutra.main.interface.8" {:interface-name :beleg}}
   :object-types
   {"ri.sutra.main.object-type.7"
    {:object-type-name :rechnung
     :display-name     "Ausgangsrechnung"
     :project          "ri.sutra.main.project.9"
     :implements       ["ri.sutra.main.interface.8"]
     :attribute-list   [{:attribute-name :nr :attribute-type :string}]
     :plural "Ausgangsrechnungen"}}})

(deftest the-printed-object-type-names-what-the-installed-form-keys-by-rid
  (let [node (clojure.edn/read-string
              (:text (#'sutra.cli.verbs.constructs/editable-entry
                      live-schema :object-type "rechnung")))]
    (is (= :beschaffung (:project node)))
    (is (= [:beleg] (:implements node)))
    (is (= "Ausgangsrechnung" (:name node)))
    (is (nil? (ent/explain-schema {:objects {:rechnung node}})))))
