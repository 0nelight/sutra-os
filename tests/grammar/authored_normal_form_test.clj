(ns ^:unit grammar.authored-normal-form-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.parser.authoring.registry :as registry]
            [sutra.parser.authoring.standard :as standard]
            [sutra.parser.project-export :as pe]
            [clojure.string]))

(def ^:private onto (registry/texts standard/standard :ontologie))

(defn- b [s] (.getBytes ^String s "UTF-8"))

(def ^:private mit-zucker
  ";; Das Luftfahrzeug kennt seinen Halter -- einmal deklariert.
{:schema
 {:objects
  {:halter    {:attributes [[:halter-id :string]]}
   :luftfahrzeug {:attributes [[:kennzeichen :string]
                           [:halter-id :string
                            {:link {:to :halter :as :halter :reverse :luftfahrzeuge}}]]}}}}")

(def ^:private ausgeschrieben
  "{:schema
 {:links [[:luftfahrzeug-halter :luftfahrzeug :halter :many-to-one
           {:fk {:attribute :halter-id :on-null :skip :on-unresolved :skip}
            :forward-api-name :halter :reverse-api-name :luftfahrzeuge}]]
  :objects
  {:halter    {:attributes [[:halter-id :string]]}
   :luftfahrzeug {:attributes [[:kennzeichen :string] [:halter-id :string]]}}}}")

(deftest zucker-und-ausgeschrieben-sind-dieselbe-normalform
  (testing "die Projektion behält den Autorentext, wenn beide gleich lowern"
    (is (pe/same-authored-content? onto "ontology.ent" (b mit-zucker) (b ausgeschrieben)))))

(deftest status-experimental-ist-der-default
  (testing "der Export schreibt :status :experimental aus, der Autor darf es weglassen"
    (is
     (pe/same-authored-content? onto
      "ontology.ent"
      (b "{:schema {:objects {:halter {:attributes [[:halter-id :string]]}}}}")
      (b
       "{:schema {:objects {:halter {:status :experimental :attributes [[:halter-id :string]]}}}}"))))
  (testing ":active ist kein Default"
    (is
     (not
      (pe/same-authored-content? onto
       "ontology.ent"
       (b "{:schema {:objects {:halter {:attributes [[:halter-id :string]]}}}}")
       (b
        "{:schema {:objects {:halter {:status :active :attributes [[:halter-id :string]]}}}}"))))))

(deftest eine-echte-aenderung-bleibt-eine
  (is (not (pe/same-authored-content? onto
            "ontology.ent" (b mit-zucker)
            (b (clojure.string/replace ausgeschrieben ":luftfahrzeuge" ":flotte"))))))
