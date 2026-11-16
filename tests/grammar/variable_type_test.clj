(ns ^:unit grammar.variable-type-test
  (:require [clojure.test :refer [deftest testing is]]
            [sutra.capps.variables :as vars]
            [sutra.parser.ent :as ent]))

(defn- tuer [vars]
  (try (ent/parse-enta {:name :probe :layout {:kind :rows :vars vars :items []}} "p.enta")
       :angenommen
       (catch Exception e (.getMessage e))))

(deftest der-erklaerte-typ-gilt-vor-jeder-ableitung
  (is (= :string (vars/var-type {:type :string :default :ein-keyword}))
      "was dasteht, wird nicht ueberstimmt"))

(deftest wo-die-form-den-typ-hergibt-braucht-es-keine-angabe
  (testing "Objektmengen"
    (is (= :object-set (vars/var-type {:source :rechnung})))
    (is (= :object-set (vars/var-type {:from :zeilen :where []})))
    (is (= :object-set (vars/var-type {:linked :position}))))
  (testing "eine Funktion rechnet Zeilen aus — keine Menge, die der Store beantwortet"
    (is (= :array (vars/var-type {:function :kandidaten})))
    (is (= ":type :object-set widerspricht der Form, die :array ergibt"
           (vars/type-contradiction {:function :kandidaten :type :object-set}))))
  (testing "Zahlen"
    (is (= :number (vars/var-type {:expr [:+ 1 2]})))
    (is (= :number (vars/var-type {:agg [:count] :of :zeilen})))
    (is (= :number (vars/var-type {:aggregate {:find :auftrag}}))))
  (testing "eine Aggregation MIT :group-by haelt Eimer-Zeilen, keine Zahl"
    (is (= :array (vars/var-type {:aggregate {:find :auftrag} :group-by [:monat]}))
        "daneben")
    (is (= :array (vars/var-type {:aggregate {:find :auftrag :group-by [:monat]}}))
        "und innerhalb — die Schreibweise, die jede App tatsaechlich benutzt"))
  (testing "Zustandswerte lesen ihren Standardwert"
    (is (= :keyword (vars/var-type {:default :maschinen})))
    (is (= :string  (vars/var-type {:default "eng"})))
    (is (= :boolean (vars/var-type {:default true})))
    (is (= :number  (vars/var-type {:default 2026})))
    (is (= :array   (vars/var-type {:default []})))))

(deftest wo-die-form-schweigt-schweigt-die-ableitung-auch
  (is (nil? (vars/var-type {:agg [:max :datum] :of :zeilen}))
      ":max ueber einer Datums-Eigenschaft haelt ein Datum, ueber einem Betrag eine Zahl")
  (is (nil? (vars/var-type {:coalesce [{:val :a} {:val :b}]}))
      "eine Kette haelt, was ihre erste nicht-leere Alternative haelt")
  (is (nil? (vars/var-type {:default nil}))
      "nil sagt nichts ueber den Typ")
  (is (= :date (vars/var-type {:type :date :agg [:max :datum] :of :zeilen}))
      "…und genau dort ist die Angabe der Gewinn"))

(deftest ein-typ-der-der-form-widerspricht-kommt-nicht-durch
  (is (re-find #"widerspricht" (tuer {:x {:type :object-set :default "a"}})))
  (is (re-find #"ist kein Variablen-Typ" (tuer {:x {:type :quatsch :default "a"}})))
  (testing "hier haelt keine Variable einen Filter: aus einer Menge entsteht keiner"
    (is (re-find #"object-set-filter entsteht nicht"
                 (tuer {:x {:type :object-set-filter :source :rechnung}}))))
  (testing "aber eine Form, die den Typ NICHT hergibt, widerspricht ihm nicht"
    (is (= :angenommen (tuer {:y {:source :r}
                              :x {:type :date :agg [:max :datum] :of :y}}))))
  (testing "und Menge und Reihe vertreten einander NICHT"
    (is (re-find #"widerspricht" (tuer {:x {:type :array :source :rechnung}})))))

(deftest der-typ-beantwortet-die-url-frage
  (testing "…und ersetzt damit die :as-Pflicht"
    (is (= :angenommen
           (tuer {:seite {:type :keyword :default :maschinen :external-id :seite
                          :in-url :always}}))))
  (testing "der abgeleitete Codec"
    (is (= :keyword (vars/coercion-of {:type :keyword :default :maschinen})))
    (is (= :int     (vars/coercion-of {:type :number  :default 2026})))
    (is (= :bool    (vars/coercion-of {:type :boolean :default true})))
    (is (= :edn     (vars/coercion-of {:type :array   :default []})))
    (is (= :string  (vars/coercion-of {:type :date    :default nil}))
        "ein Datum faehrt als Text — ISO ist schon die Schreibweise"))
  (testing ":as schlaegt den Typ, wenn beide dastehen"
    (is (= :string (vars/coercion-of {:as :string :type :keyword :default :x}))))
  (testing "ohne beides zaehlt der Standardwert"
    (is (= :keyword (vars/coercion-of {:default :maschinen})))))

(deftest der-typ-schraenkt-ein-woraus-die-variable-entsteht
  (testing "die Definitionsart, wie die Form sie ausdrueckt"
    (is (= :object-set-definition (vars/definition-kind {:source :rechnung})))
    (is (= :aggregation           (vars/definition-kind {:aggregate {:find :x}})))
    (is (= :aggregation           (vars/definition-kind {:default-agg [:max :jahr :x]})))
    (is (= :transformation        (vars/definition-kind {:agg [:sum :netto] :of :z})))
    (is (= :function              (vars/definition-kind {:function :kandidaten})))
    (is (= :static                (vars/definition-kind {:default "x"})))
    (is (nil? (vars/definition-kind {}))))
  (testing "eine Objektmenge entsteht nicht aus einem statischen Wert"
    (let [msg (tuer {:x {:type :object-set :default nil}})]
      (is (string? msg))
      (is (re-find #"entsteht nicht aus" msg))))
  (testing "…und nicht aus einer Umformung"
    (is (re-find #"entsteht nicht aus"
                 (tuer {:z {:source :r} :x {:type :object-set :agg [:max :a] :of :z}}))))
  (testing "was zusammenpasst, geht durch"
    (is (= :angenommen (tuer {:x {:type :object-set :source :rechnung}})))
    (is (= :angenommen (tuer {:x {:type :string :default "eng"}})))
    (is (= :angenommen (tuer {:x {:type :number :aggregate {:find :auftrag}}})))
    (is (= :angenommen (tuer {:x {:type :array :default []}})))))

(deftest die-eigenschaft-des-gewaehlten-objekts-ist-eine-variable
  (let [data    {:alle [{:id "a" :values {:bezeichnung "CNC SCM" :baujahr 2009}}
                        {:id "b" :values {:bezeichnung "Weber" :baujahr 1998}}]}
        derived {:alle {:source :bm}
                 :sel  {:from :alle :where [[:bezeichnung := "Weber"]]}
                 :leer {:from :alle :where [[:bezeichnung := "gibtsnicht"]]}
                 :name {:type :string :object-property :bezeichnung :of :sel}
                 :jahr {:type :number :object-property :baujahr    :of :sel}
                 :nix  {:type :string :object-property :bezeichnung :of :leer}}
        lies    (fn [k] (vars/resolve-ref {:val k} {} derived data))]
    (testing "sie liest die Eigenschaft der ersten Zeile"
      (is (= "Weber" (lies :name)))
      (is (= 1998 (lies :jahr))))
    (testing "eine leere Auswahl ergibt nil, keinen Fehler"
      (is (nil? (lies :nix))))
    (testing "sie ist eine eigene Definitionsart"
      (is (= :object-property (vars/definition-kind (:name derived)))))
    (testing "…die ein Skalar haben darf, eine Objektmenge nicht"
      (is (nil? (vars/definition-problem {:type :string :object-property :x :of :y})))
      (is (re-find #"entsteht nicht aus"
                   (vars/definition-problem {:type :object-set :object-property :x :of :y}))))
    (testing "und ihr Typ ist NICHT ableitbar — welche Eigenschaft, das weiss nur der Autor"
      (is (nil? (vars/var-type {:object-property :bezeichnung :of :sel}))))))
