(ns ^:unit grammar.authoring-find-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.cli.projection :as cli]
            [sutra.cli.view :as view]
            [sutra.cli.widgets :as widgets]
            [sutra.parser.authoring.standard :as standard]
            [sutra.parser.projection :as proj]))

(def ^:private central
  "{:schema
 {:actions
  {:auftrag-zuordnen
   {:description
    \"Ein Block des Belegs\",
    :params
    {:abschnitt
     {:name \"Postengruppe\"}}}}}}\n")

(def ^:private view-text
  "{:full
 {:items
  [{:label \"Andere…\",
    :widget :button}]},
 :project :beschaffung}\n")

(deftest a-hit-names-its-line-and-the-node-around-it
  (is (= [{:line 5 :node "auftrag-zuordnen" :snippet "\"Ein Block des Belegs\","}]
         (cli/text-hits central "block DES" ["auftrag-zuordnen"])))
  (testing "a nested key still answers with the node"
    (is (= "auftrag-zuordnen"
           (:node (first (cli/text-hits central "Postengruppe" ["auftrag-zuordnen"]))))))
  (testing "a text whose nodes no key names answers with the outermost key"
    (is (= [{:line 3 :node ":full" :snippet "[{:label \"Andere…\","}]
           (cli/text-hits view-text "Andere" ["eingangsdokument-view-full"]))))
  (testing "no hit, nothing"
    (is (= [] (cli/text-hits view-text "gibt es nicht" [])))))

(deftest a-long-line-is-cut-around-the-hit
  (let [line (str (str/join (repeat 100 "a")) "TREFFER" (str/join (repeat 100 "b")))
        {:keys [snippet]} (first (cli/text-hits line "treffer" []))]
    (is (str/includes? snippet "TREFFER"))
    (is (< (count snippet) 130))))

(def ^:private live
  {:object-types {"ri.ot.1" {:object-type-name :rechnung}}
   :applications {"ri.app.1" {:name :rechnung-view-full :form-factor :full
                              :object-type "ri.ot.1"}
                  "ri.app.2" {:name :rechnung-view-panel :form-factor :panel
                              :object-type :rechnung}
                  "ri.app.3" {:name :rechnungs-app :object-type :rechnung}
                  "ri.app.4" {:name :anderes-view-full :form-factor :full
                              :object-type :anderes}}})

(deftest a-type-names-its-views
  (is (= ["rechnung-view-full" "rechnung-view-panel"] (view/object-views live :rechnung)))
  (is (= [] (view/object-views live :anderes-typ))))

(def ^:private project-schema
  {:projections  standard/projections
   :projects     {"ri.sutra.main.project.p1" {:project-name :beschaffung}
                  "ri.sutra.main.project.p2" {:project-name :controlling}}
   :object-types {"ri.sutra.main.object-type.o1"
                  {:object-type-name :rechnung
                   :project          "ri.sutra.main.project.p1"
                   :attribute-list   [{:attribute-name "nr" :attribute-type "string"}]
                   :primary-key      "nr"
                   :plural "Rechnungen"}}
   :applications {"ri.sutra.main.application.a1"
                  {:name :rechnung-view-full :form-factor :full
                   :object-type      "ri.sutra.main.object-type.o1"
                   :project          "ri.sutra.main.project.p1"
                   :status           :published
                   :layout           {:kind :rows :items []}}}})

(deftest a-project-pull-carries-the-texts-of-its-views
  (let [paths (fn [p] (keys (:files (proj/view project-schema "ontologie" nil {:project p}))))]
    (is (some #(str/ends-with? % "rechnung.entv") (paths "beschaffung")))
    (is (not-any? #(str/ends-with? % ".entv") (paths "controlling")))))

(deftest the-catalog-answers-for-a-widget
  (testing "every kind is listed, and the reference forms close the answer"
    (let [{:keys [lines]} (widgets/answer nil "REFS")]
      (is (some #(str/includes? % "combo-input") lines))
      (is (some #(str/includes? % "loop") lines))
      (is (= "REFS" (last lines)))))
  (testing "one widget names what the renderer takes"
    (let [text (str/join "\n" (:lines (widgets/answer :combo-input "REFS")))]
      (is (str/includes? text ":value-param"))
      (is (str/includes? text ":params"))
      (is (str/includes? text ":action"))
      (is (str/includes? text ":sort enum :asc | :desc"))))
  (testing "a layout kind names its children"
    (is (some #(str/includes? % ":body") (:lines (widgets/answer :loop "REFS")))))
  (testing "an unknown name says where to look"
    (is (:error (widgets/answer :gibt-es-nicht "REFS")))))
