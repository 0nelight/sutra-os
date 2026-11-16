(ns ^:unit grammar.variable-value-test
  (:require [clojure.test :refer [deftest testing is]]
            [sutra.capps.variables :as vars]))

(def ^:private zeile
  {:id              "792fdbdc-e2d1-5902-8423-f4a1855d9981"
   :object-type-ref "ri.sutra.main.object-type.70fc78b9"
   :values          {:bezeichnung "AOG - Hydraulik" :prioritaet 75 :status-id 7
                     :status-satz "strich"}})

(defn- wert [derived data ref] (vars/resolve-ref ref {} derived data))

(deftest object-property-liest-eine-gespeicherte-eigenschaft
  (let [data    {:st75 [zeile]}
        derived {:st75-chance {:object-property :prioritaet :of :st75 :type :number}}]
    (is (= 75 (wert derived data {:val :st75-chance}))
        "eine Eigenschaft aus :values war nie das Problem")))

(deftest object-property-id-findet-die-id-der-zeile
  (testing "die :id ist die Angabe, mit der eine Action ein Objekt anspricht"
    (let [data    {:st75 [zeile]}
          derived {:st75-id {:object-property :id :of :st75 :type :string}}]
      (is (= "792fdbdc-e2d1-5902-8423-f4a1855d9981"
             (wert derived data {:val :st75-id}))
          "sonst bleibt jeder Knopf, der ein Objekt übergibt, auf ewig gesperrt"))))

(deftest object-property-auf-leerer-menge-bleibt-nil
  (let [derived {:st75-id {:object-property :id :of :st75 :type :string}}]
    (is (nil? (wert derived {:st75 []} {:val :st75-id}))
        "keine Zeile, kein Wert — und KEIN Ausweichen auf irgendetwas anderes")))

(deftest eine-eigenschaft-die-es-nicht-gibt-bleibt-nil
  (let [data    {:st75 [zeile]}
        derived {:x {:object-property :gibtsnicht :of :st75 :type :string}}]
    (is (nil? (wert derived data {:val :x})))))

(deftest values-schlaegt-die-huelle
  (testing "trägt eine Zeile die Eigenschaft BEIDE Male, gilt die gespeicherte"
    (let [data    {:m [{:id "aussen" :values {:id "innen"}}]}
          derived {:mid {:object-property :id :of :m :type :string}}]
      (is (= "innen" (wert derived data {:val :mid}))
          "die Hülle ist der Ausweg, nicht der Vorrang"))))

(def ^:private arbeitsauftraege
  [{:id "a" :values {:auftrag-nr "WA26.0261" :bezeichnung "LDG - Bremse links" :status-id 13}}
   {:id "b" :values {:auftrag-nr "WA26.0099" :bezeichnung "GAL - Kueche vorn" :status-id 3}}])

(defn- treffer
  [where]
  (mapv #(get-in % [:values :auftrag-nr])
        (wert {:t {:from :alle :where where}} {:alle arbeitsauftraege} {:val :t})))

(deftest suche-ueber-eine-liste-von-eigenschaften
  (is (= ["WA26.0261"] (treffer [[[:auftrag-nr :bezeichnung] :contains "LDG"]]))
      "die Bezeichnung zaehlt mit — genau der Fall, der vorher leer blieb")
  (is (= ["WA26.0099"] (treffer [[[:auftrag-nr :bezeichnung] :contains "WA26.00"]]))
      "und die Nummernsuche bleibt unveraendert"))

(deftest suche-ueber-jeden-wert-der-zeile
  (testing ":* ist die Stichwortsuche des Object Workspace"
    (is (= ["WA26.0099"] (treffer [[:* :contains "Kueche"]])))
    (is (= ["WA26.0261"] (treffer [[:* :contains "0261"]]))
        "auch ueber eine Spalte, die niemand benannt hat")))

(deftest eine-einzelne-eigenschaft-verhaelt-sich-wie-bisher
  (is (= [] (treffer [[:auftrag-nr :contains "LDG"]]))
      "der alte Weg bleibt der alte Weg — nichts still umdeuten")
  (is (= ["WA26.0261"] (treffer [[:auftrag-nr :contains "0261"]]))))

(deftest leeres-suchfeld-filtert-nicht
  (testing "BLANK-WIDENS gilt auch fuer die Liste und fuer :*"
    (is (= ["WA26.0261" "WA26.0099"] (treffer [[[:auftrag-nr :bezeichnung] :contains ""]])))
    (is (= ["WA26.0261" "WA26.0099"] (treffer [[:* :contains ""]])))))

(deftest die-liste-wirkt-auch-auf-anderen-ops
  (testing "die Liste heisst IRGENDEINE davon — nicht nur fuer :contains"
    (is (= ["WA26.0261"] (treffer [[[:status-id :prioritaet] := 13]])))))

(deftest datumsrechnung-wie-apphost
  (let [data (vars/with-today {} "2026-11-03")
        ex   #(vars/eval-expr % (fn [r] (vars/deref* (vars/resolve-ref r {} {} data))))]
    (testing "Current date: der Tag, den der Rand hereingibt — keine Systemuhr"
      (is (= "2026-11-03" (ex [:today])))
      (is (nil? (vars/eval-expr [:today] #(vars/resolve-ref % {} {} {}))))
      (is (= :date (vars/var-type {:expr [:today]}))))
    (testing "Relative date: Tage, Wochen, Monate (am Monatsende geklemmt), Jahre"
      (is (= "2026-11-03" (ex [:relative-date "2026-10-31" 3 :days])))
      (is (= "2026-10-17" (ex [:relative-date "2026-10-31" -2 :weeks])))
      (is (= "2027-02-28" (ex [:relative-date "2027-01-31" 1 :months])))
      (is (= "2025-02-28" (ex [:relative-date "2024-02-29" 1 :years])))
      (is (= "2026-10-30" (ex [:relative-date "2026-10-31T23:10:00Z" -1 :days]))))
    (testing "Relative time behält die Uhrzeit"
      (is (= "2026-10-31T12:00:00Z" (ex [:relative-time "2026-10-31T10:00:00Z" 2 :hours]))))
    (testing "Between dates / times: b − a in ganzen Einheiten"
      (is (= 30 (ex [:between-dates "2026-10-01" "2026-10-31" :days])))
      (is (= 0 (ex [:between-dates "2026-01-31" "2026-02-28" :months])))
      (is (= 1 (ex [:between-dates "2026-01-31" "2026-03-01" :months])))
      (is (= 90 (ex [:between-times "2026-10-31T10:00:00Z" "2026-10-31T11:30:00Z" :minutes]))))
    (testing "Is on or after: die Vergleiche lesen Datum und Zeit"
      (is (true? (ex [:>= [:relative-time [:today] 8 :hours] [:today]])))
      (is (false? (ex [:>= [:relative-date [:today] -1 :days] [:today]])))
      (is (nil? (ex [:relative-date nil 1 :days]))))))

(deftest min-max-ueber-zeitpunkte
  (let [rows [{:values {:t "2026-10-30T08:00:00Z"}} {:values {:t "2026-10-29T17:30:00Z"}}
              {:values {:t nil}}]]
    (is (= "2026-10-29T17:30:00Z" ((:min vars/agg-fns) :t rows)))
    (is (= "2026-10-30T08:00:00Z" ((:max vars/agg-fns) :t rows)))
    (is (nil? ((:min vars/agg-fns) :t [])))))

(deftest coalesce-aus-inline-specs-nennt-ihre-mengen
  (is (= #{:zeige-neu :naechstes-neu :x}
         (vars/var-deps {:coalesce [{:object-property :dokument-id, :of :zeige-neu}
                                    {:object-property :dokument-id, :of :naechstes-neu}
                                    {:val :x}]}))
      "sonst fragt die App-Tür die Mengen nie, aus denen der Wert entsteht"))
