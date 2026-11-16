(ns ^:unit grammar.app-reference-door-test
  (:require [clojure.test :refer [deftest testing is]]
            [sutra.capps.catalog :as cat]
            [sutra.capps.variables :as vars]
            [sutra.parser.ent :as ent]))

(defn- tuer
  [layout]
  (try (ent/parse-enta {:name :probe :layout layout} "probe.enta")
       :angenommen
       (catch Exception e (.getMessage e))))

(deftest eine-variable-die-niemand-deklariert-hat-kommt-nicht-durch
  (testing "der deklarierte Name geht durch"
    (is (= :angenommen
           (tuer {:kind :rows
                  :vars {:zeilen {:source :mangel}}
                  :items [{:widget :object-table :rows {:val :zeilen}}]}))))
  (testing "der undeklarierte nicht — und die Meldung nennt ihn"
    (let [msg (tuer {:kind :rows
                     :vars {:zeilen {:source :mangel}}
                     :items [{:widget :object-table :rows {:val :zeiln}}]})]
      (is (string? msg))
      (is (re-find #"zeiln" msg) "die Meldung muss den Namen nennen, nicht nur die Stelle")))
  (testing "auch ein `:sel`-Griff auf eine Variable, die es nicht gibt"
    (let [msg (tuer {:kind :rows
                     :vars {:q {:as :string :default ""}}
                     :items [{:widget :text-input :sel {:var :suche}}]})]
      (is (string? msg))
      (is (re-find #"suche" msg)))))

(deftest ein-innerer-vars-bereich-legt-sich-ueber-den-aeusseren
  (testing "eine innen deklarierte Variable gilt in ihrem Teilbaum"
    (is (= :angenommen
           (tuer {:kind :rows
                  :vars {:aussen {:source :mangel}}
                  :items [{:kind :rows
                           :vars {:innen {:from :aussen}}
                           :items [{:widget :object-table :rows {:val :innen}}]}]}))))
  (testing "und die äussere gilt drinnen weiter"
    (is (= :angenommen
           (tuer {:kind :rows
                  :vars {:aussen {:source :mangel}}
                  :items [{:kind :rows
                           :vars {:innen {:from :aussen}}
                           :items [{:widget :object-table :rows {:val :aussen}}]}]})))))

(deftest from-bedeutet-jetzt-ueberall-dasselbe
  (testing "in einem :vars-Eintrag"
    (is (= :angenommen
           (tuer {:kind :rows
                  :vars {:plaene {:source :hangarplan}
                         :plan   {:from :plaene :where [[:id := "x"]]}}
                  :items [{:widget :object-table :rows {:val :plan}}]}))))
  (testing "und auf einem Baustein — beides geht durch"
    (is (= :angenommen
           (tuer {:kind :rows
                  :vars {:plaene   {:source :hangarplan}
                         :gewaehlt {:as :string :default ""}}
                  :items [{:widget :select-input
                           :sel {:var :gewaehlt}
                           :items {:from :plaene}}]})))))

(deftest ein-blatt-from-liest-die-variable-und-nicht-ins-leere
  (let [rows    [{:id "a" :values {:stellplatz "h1"}} {:id "b" :values {:stellplatz "h2"}}]
        data    {:plaene rows :hangarplan rows}
        derived {:plaene {:source :hangarplan}}
        lies    (fn [ref] (vars/resolve-ref ref {} derived data))]
    (is (= 2 (count (lies {:from :plaene}))) "der Variablenname löst auf")
    (is (= 2 (count (lies {:val :plaene}))) "wie die andere Schreibweise auch")
    (is (= 2 (count (lies {:from :hangarplan}))) "und ein Objekttyp weiterhin ebenso")
    (is (nil? (lies {:from :gibt-es-nicht}))
        "ein Name, den es nirgends gibt, bleibt nil — nicht eine leere Menge")))

(deftest die-tuer-nennt-ALLE-befunde-nicht-nur-den-ersten
  (let [msg (tuer {:kind :rows
                   :vars {:a {:source :mangel}}
                   :items [{:widget :object-table :rows {:val :x}}
                           {:widget :text-input :sel {:var :y}}]})]
    (is (string? msg))
    (is (re-find #"x" msg))
    (is (re-find #"y" msg) "auch der zweite Befund muss in derselben Absage stehen")))

(deftest ein-skalar-steht-nicht-da-wo-eine-objektmenge-hingehoert
  (testing "eine Objektmenge dort, wo eine hingehört"
    (is (= :angenommen
           (tuer {:kind :rows :vars {:z {:source :mangel}}
                  :items [{:widget :object-table :rows {:val :z}}]}))))
  (testing "eine Summe dort nicht — und die Meldung sagt, was sie hält"
    (let [msg (tuer {:kind :rows
                     :vars {:z {:source :mangel} :summe {:agg [:sum :kosten] :of :z}}
                     :items [{:widget :object-table :rows {:val :summe}}]})]
      (is (string? msg))
      (is (re-find #"summe" msg))
      (is (re-find #"Objektmenge" msg))))
  (testing "wo der Typ sich nicht bestimmen lässt, schweigt die Tür"
    (is (= :angenommen
           (tuer {:kind :rows
                  :vars {:z {:source :mangel} :m {:agg [:max :gemeldet-am] :of :z}}
                  :items [{:widget :object-table :rows {:val :m}}]}))))
  (testing "eine Aggregation MIT :group-by hält Zeilen und darf dort stehen"
    (is (= :angenommen
           (tuer {:kind :rows
                  :vars {:a {:aggregate {:find :arbeitsauftrag :group-by [:monat]
                                         :aggs [{:name :summe :fn :sum :attr :kosten}]}}}
                  :items [{:widget :object-table :rows {:val :a}}]})))))

(deftest ueber-objekttypen-sagt-die-tuer-nichts
  (testing "{:from <objekttyp>} auf einem Baustein bleibt unbeanstandet"
    (is (= :angenommen
           (tuer {:kind :rows
                  :items [{:widget :object-table :rows {:from :mangel}}]}))))
  (testing "auch wenn der Typ nirgends aufgezählt ist"
    (is (= :angenommen
           (tuer {:kind :rows
                  :vars {:q {:as :string :default ""}}
                  :items [{:widget :object-table :rows {:from :voellig-unbekannt}}]})))))

(deftest die-regel-liegt-an-einer-stelle
  (testing "ein sauberes Layout hat keine Befunde"
    (is (empty? (cat/reference-problems
                 {:kind :rows :vars {:a {:source :mangel}}
                  :items [{:widget :object-table :rows {:val :a}}]}))))
  (testing "ein Befund nennt Pfad, Referenz und Grund"
    (let [[p] (cat/reference-problems
               {:kind :rows :vars {:a {:source :mangel}}
                :items [{:widget :object-table :rows {:val :b}}]})]
      (is (= {:val :b} (:ref p)))
      (is (seq (:path p)) "ohne Pfad müsste der Autor den Namen selbst suchen")
      nil)))
