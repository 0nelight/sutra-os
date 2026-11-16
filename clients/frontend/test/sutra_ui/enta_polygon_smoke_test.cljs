(ns sutra-ui.enta-polygon-smoke-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [sutra.capps.polygon :as poly]))

(defn- draw [& ps]
  (reduce (fn [poly p] (poly/tap poly p {:tolerance 5})) (poly/state) ps))

(def ^:private quadrat (draw [0 0] [100 0] [100 100] [0 100]))

(deftest ein-tipp-setzt-einen-punkt
  (is (= [[0.0 0.0]] (:points (draw [0 0]))))
  (is (= 4 (count (:points quadrat))))
  (testing "ein Tipp auf den letzten Punkt zaehlt nicht"
    (is (= :zu-nah (:ignored (poly/tap (draw [0 0]) [2 2] {:tolerance 5}))))
    (is (= 1 (count (:points (poly/tap (draw [0 0]) [2 2] {:tolerance 5})))))))

(deftest eine-kante-die-sich-selbst-kreuzt-zaehlt-nicht
  (let [poly (draw [0 0] [100 0] [100 100])
        nx (poly/tap poly [50 -50] {:tolerance 5})]
    (is (= :kreuzt (:ignored nx)))
    (is (= (:points poly) (:points nx)) "der Umriss bleibt, wie er war")))

(deftest der-erste-punkt-schliesst-ab-drei-punkten
  (testing "unter drei Punkten ist der erste Punkt kein Ziel"
    (let [poly (poly/tap (draw [0 0] [100 0]) [1 1] {:tolerance 5})]
      (is (false? (:closed? poly)))
      (is (= :zu-nah (:ignored poly))
          "ein Tipp auf einen vorhandenen Punkt legt keinen zweiten dorthin")
      (is (= 2 (count (:points poly))))))
  (testing "ab drei Punkten schliesst er den Umriss"
    (let [poly (poly/tap quadrat [2 2] {:tolerance 5})]
      (is (true? (:closed? poly)))
      (is (poly/ring? poly))
      (is (= 4 (count (:points poly))) "der erste Punkt wird nicht verdoppelt")))
  (testing "auch die schliessende Kante darf sich nicht kreuzen"
    (let [poly (draw [0 0] [100 0] [50 50] [150 50])]
      (is (= :kreuzt (:ignored (poly/tap poly [1 1] {:tolerance 5}))))
      (is (false? (:closed? (poly/tap poly [1 1] {:tolerance 5})))))))

(deftest punkt-zurueck-und-verwerfen
  (is (= 3 (count (:points (poly/undo quadrat)))))
  (is (empty? (:points (poly/undo (draw [0 0])))) "der letzte Punkt fuehrt in den leeren Zustand")
  (is (= :leer (:ignored (poly/undo (poly/state)))))
  (testing "auf einem stehenden Umriss oeffnet punkt-zurueck den Ring"
    (let [ring (poly/tap quadrat [2 2] {:tolerance 5})]
      (is (false? (:closed? (poly/undo ring))))
      (is (= 4 (count (:points (poly/undo ring)))))))
  (is (empty? (:points (poly/clear quadrat)))))

(deftest ein-stehender-umriss-nimmt-keinen-punkt-mehr
  (let [ring (poly/tap quadrat [2 2] {:tolerance 5})]
    (is (= :geschlossen (:ignored (poly/tap ring [50 50] {:tolerance 5}))))
    (is (= 4 (count (:points (poly/tap ring [50 50] {:tolerance 5})))))))

(deftest nachbessern-schiebt-dreht-und-zieht
  (let [ring (poly/tap quadrat [2 2] {:tolerance 5})]
    (is (= [[10.0 -10.0] [110.0 -10.0] [110.0 90.0] [10.0 90.0]]
           (:points (poly/translate ring 10 -10))))
    (is (= [[0.0 0.0] [100.0 0.0] [100.0 100.0] [60.0 60.0]]
           (:points (poly/move-point ring 3 [60 60]))))
    (is (= (:points ring) (:points (poly/move-point ring 9 [60 60])))
        "ein Index ausserhalb laesst den Umriss unberuehrt")
    (testing "eine Vierteldrehung um den Schwerpunkt"
      (let [ps (:points (poly/rotate ring (/ Math/PI 2)))]
        (is (every? (fn [[a b]] (< (Math/abs (- a b)) 1e-9))
                    (map vector (flatten ps) [100.0 0.0 100.0 100.0 0.0 100.0 0.0 0.0])))))))

(deftest der-ring-weiss-wo-innen-ist
  (let [ring (poly/tap quadrat [2 2] {:tolerance 5})]
    (is (true? (poly/inside? ring [50 50])))
    (is (false? (poly/inside? ring [200 50])))
    (is (nil? (poly/inside? quadrat [50 50])) "eine offene Kette hat kein Innen")
    (is (= 1 (poly/vertex-at ring [103 2] 5)))
    (is (nil? (poly/vertex-at ring [50 50] 5)))))

(deftest der-umriss-faehrt-als-zeichenkette-hin-und-zurueck
  (let [ring (poly/tap quadrat [2 2] {:tolerance 5})]
    (is (= "0,0 100,0 100,100 0,100" (poly/->str ring :points)))
    (is (= "{\"type\":\"Polygon\",\"coordinates\":[[[0,0],[100,0],[100,100],[0,100],[0,0]]]}"
           (poly/->str ring :geojson)))
    (is (nil? (poly/->str (poly/state) :points)) "ein leerer Umriss ist keine Zeichenkette")
    (testing "beide Formen lesen sich als geschlossener Ring zurueck"
      (doseq [as [:points :geojson]]
        (let [back (poly/parse (poly/->str ring as))]
          (is (poly/ring? back))
          (is (= (:points ring) (:points back))
              "der GeoJSON-Schlusspunkt wird nicht zum fuenften Punkt"))))
    (is (= [] (:points (poly/parse nil))))
    (is (= 3 (count (:points (poly/parse [[1 2] [3 4] [5 6]])))))))

(deftest ein-kreuzender-ring-sagt-es
  (is (poly/simple? (poly/tap quadrat [2 2] {:tolerance 5})))
  (is (not (poly/simple? (poly/state [[0 0] [100 100] [100 0] [0 100]] true)))))

(def ^:private inkscape-plan
  (str "<?xml version=\"1.0\"?>\n"
       "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"210mm\" height=\"297mm\""
       " viewBox=\"10 20 210 297\">"
       "<defs><marker id=\"m\"><path d=\"M 0,0 L 5,5 L 0,10 Z\"/></marker></defs>"
       "<g inkscape:label=\"Ebene 1\" transform=\"translate(10,5)\">"
       "<path inkscape:label=\"Hangar Nord\" d=\"m 20,30 h 60 v 40 h -60 z\"/>"
       "<rect inkscape:label=\"Büro\" x=\"100\" y=\"30\" width=\"40\" height=\"25\"/>"
       "<g transform=\"rotate(90,50,50) scale(2)\">"
       "<polygon inkscape:label=\"Teilelager\" points=\"10,10 30,10 30,20 10,20\"/>"
       "</g></g></svg>"))

(defn- gerundet [ps] (mapv (fn [p] (mapv #(Math/round (double %)) p)) ps))

(defn- ring-named [plan nm]
  (first (filter #(= nm (:label %)) (:rings plan))))

(deftest ein-inkscape-plan-kommt-mit-seinen-namen-herein
  (let [plan (poly/svg->plan inkscape-plan)]
    (testing "die Ausdehnung ist die viewBox, nicht die Millimeter-Angabe"
      (is (= {:w 210.0 :h 297.0} (:extent plan))))
    (testing "jeder Umriss traegt seinen inkscape:label"
      (is (= #{"Hangar Nord" "Büro" "Teilelager"} (set (map :label (:rings plan))))))
    (testing "der viewBox-Versatz und der Gruppen-transform sind eingerechnet"
      (is (= [[20 15] [80 15] [80 55] [20 55]]
             (gerundet (:points (ring-named plan "Hangar Nord"))))))
    (testing "ein relativer Pfad, ein rect und ein gedrehter polygon lesen gleich weit"
      (is (= [[100 15] [140 15] [140 40] [100 40]]
             (gerundet (:points (ring-named plan "Büro")))))
      (is (= 4 (count (:points (ring-named plan "Teilelager"))))))
    (testing "was in <defs> steht, ist kein Raum"
      (is (= 3 (count (:rings plan)))))))

(deftest ein-plan-faehrt-als-svg-hinaus-und-wieder-herein
  (let [plan {:extent {:w 200 :h 100}
              :rings  [{:label "Hangar 1" :points [[10 10] [90 10] [90 60] [10 60]]}
                       {:label "Lackierhalle" :points [[100 10] [190 10] [190 40]]}]}
        back (poly/svg->plan (poly/plan->svg plan))]
    (is (= {:w 200.0 :h 100.0} (:extent back)))
    (is (= ["Hangar 1" "Lackierhalle"] (mapv :label (:rings back))))
    (is (= (gerundet (:points (first (:rings plan))))
           (gerundet (:points (first (:rings back))))))))

(deftest ein-umriss-unter-drei-punkten-ist-kein-raum
  (let [plan (poly/svg->plan
              (str "<svg viewBox=\"0 0 10 10\"><polygon id=\"kante\" points=\"0,0 5,5\"/>"
                   "<polygon id=\"raum\" points=\"0,0 5,0 5,5\"/></svg>"))]
    (is (= ["raum"] (mapv :label (:rings plan))))))

(deftest der-gelesene-plan-faehrt-als-json-in-die-aktion
  (let [j (poly/plan->json {:extent {:w 200 :h 100}
                            :rings [{:label "Hangar 1"
                                     :points [[10 10] [90 10] [90 60]]}]})]
    (is
     (= "{\"extent\":{\"w\":200,\"h\":100},\"rings\":[{\"label\":\"Hangar 1\",\"points\":\"10,10 90,10 90,60\"}]}"
        j))))

(deftest viele-waende-fahren-als-multipolygon-in-eine-property
  (let [waende [[[0 0] [10 0] [10 1] [0 1]]
                [[0 0] [1 0] [1 8] [0 8]]
                [[3 3] [4 3] [4 4] [3 4]]]
        s      (poly/rings->str waende)]
    (is (= (str "{\"type\":\"MultiPolygon\",\"coordinates\":"
                "[[[[0,0],[10,0],[10,1],[0,1],[0,0]]],"
                "[[[0,0],[1,0],[1,8],[0,8],[0,0]]],"
                "[[[3,3],[4,3],[4,4],[3,4],[3,3]]]]}")
           s))
    (testing "jede Wand kommt als EIGENER Ring zurueck"
      (let [back (poly/parse-rings s)]
        (is (= 3 (count back)))
        (is (every? poly/ring? back))
        (is (= (mapv #(mapv (fn [[x y]] [(double x) (double y)]) %) waende)
               (mapv :points back)))))
    (testing "parse liest den ERSTEN Ring — die Leinwand bearbeitet einen"
      (is (= [[0.0 0.0] [10.0 0.0] [10.0 1.0] [0.0 1.0]] (:points (poly/parse s)))))
    (testing "ein Ring unter drei Punkten wird nicht geschrieben"
      (is (= "{\"type\":\"MultiPolygon\",\"coordinates\":[]}"
             (poly/rings->str [[[0 0] [1 1]]]))))))

(deftest ein-einzelnes-polygon-bleibt-ein-ring
  (testing "die Regex-Lesung haette alle Paare zu EINEM Ring verschmolzen"
    (is (= 1 (count (poly/parse-rings
                     "{\"type\":\"Polygon\",\"coordinates\":[[[0,0],[2,0],[2,2],[0,0]]]}"))))
    (is (= 2 (count (poly/parse-rings
                     (str "{\"type\":\"Polygon\",\"coordinates\":"
                          "[[[0,0],[9,0],[9,9],[0,9]],[[3,3],[4,3],[4,4],[3,4]]]}"))))
        "Aussenring und Loch sind zwei Ringe"))
  (testing "das Punkt-Format bleibt, wie es war"
    (is (= [[36.0 7.75] [41.75 7.75] [41.75 9.0]]
           (:points (poly/parse "36.00,7.75 41.75,7.75 41.75,9.00"))))))

(deftest ein-gelesener-plan-findet-die-raeume-ueber-ihren-namen
  (let [formen [{:key [:r1 0] :label "Werkbank"}
                {:key [:r2 0] :label "Lackierhalle"}]
        ringe  [{:label "Werkbank"   :points [[0 0] [4 0] [4 4]]}
                {:label "Lackierhalle" :points [[5 5] [9 5] [9 9]]}]
        {:keys [matched ambiguous unmatched]} (poly/match-rings ringe formen)]
    (is (= 2 (count matched)))
    (is (= [:r1 0] (ffirst matched)))
    (is (zero? ambiguous))
    (is (empty? unmatched))))

(deftest ein-name-auf-zwei-formen-ordnet-nichts-zu
  (let [formen [{:key [:a 0] :label "REIFENLAGER"}
                {:key [:b 0] :label "REIFENLAGER"}
                {:key [:c 0] :label "Werkbank"}]
        ringe  [{:label "reifenlager" :points [[0 0] [4 0] [4 4]]}
                {:label "Werkbank"   :points [[5 5] [9 5] [9 9]]}]
        {:keys [matched ambiguous unmatched]} (poly/match-rings ringe formen)]
    (is (= 1 (count matched)) "nur der eindeutige Name ordnet zu")
    (is (= [:c 0] (ffirst matched)))
    (is (= 1 ambiguous) "der doppelte wird gezaehlt, nicht geraten")
    (is (empty? unmatched) "und er ist auch keine NEUE Form")))

(deftest was-nicht-bearbeitbar-ist-kommt-nicht-als-neue-form-zurueck
  (let [formen [{:key [:r1 0] :label "Werkbank"}
                {:label "Triebwerksprüfstand"}]
        ringe  [{:label "Triebwerksprüfstand" :points [[0 0] [4 0] [4 4]]}
                {:label "Neuer Raum"           :points [[5 5] [9 5] [9 9]]}]
        {:keys [matched unmatched]} (poly/match-rings ringe formen)]
    (is (empty? matched) "eine Ebene ohne :key wird nicht bearbeitet")
    (is (= ["Neuer Raum"] (mapv :label unmatched))
        "und nur der wirklich unbekannte Ring will gezeichnet werden")))

(deftest ein-ring-ohne-namen-ist-eine-neue-form
  (let [{:keys [matched unmatched]}
        (poly/match-rings [{:label nil :points [[0 0] [4 0] [4 4]]}]
                          [{:key [:r1 0] :label "Werkbank"}])]
    (is (empty? matched))
    (is (= 1 (count unmatched)))))

(deftest eine-kante-traegt-die-stelle-an-der-ein-punkt-dazukommt
  (let [quadrat (poly/state [[0 0] [10 0] [10 10] [0 10]] true)
        mitten  (poly/edge-midpoints quadrat)]
    (is (= 4 (count mitten)) "vier Kanten, vier Griffe")
    (is (= [[1 [5.0 0.0]] [2 [10.0 5.0]] [3 [5.0 10.0]] [4 [0.0 5.0]]] mitten))
    (is (nil? (poly/edge-midpoints (poly/state [[0 0] [10 0]] false)))
        "ein offener Zug hat keine Schlusskante und daher keine Griffe")))

(deftest ein-punkt-kommt-genau-in-seine-kante
  (let [quadrat (poly/state [[0 0] [10 0] [10 10] [0 10]] true)]
    (is (= [[0.0 0.0] [5.0 0.0] [10.0 0.0] [10.0 10.0] [0.0 10.0]]
           (:points (poly/insert-point quadrat 1 [5 0])))
        "die Kante zwischen Punkt 0 und 1 nimmt ihn an Stelle 1 auf")
    (is (= 5 (count (:points (poly/insert-point quadrat 4 [0 5]))))
        "und die Schlusskante haengt ihn hinten an")
    (is (= (:points quadrat) (:points (poly/insert-point quadrat 9 [1 1])))
        "ein Index, den es nicht gibt, veraendert nichts")))

(deftest ein-dreieck-verliert-keinen-punkt-mehr
  (let [quadrat  (poly/state [[0 0] [10 0] [10 10] [0 10]] true)
        dreieck  (poly/drop-point quadrat 1)]
    (is (= [[0.0 0.0] [10.0 10.0] [0.0 10.0]] (:points dreieck)))
    (is (poly/ring? dreieck))
    (let [zu-klein (poly/drop-point dreieck 0)]
      (is (= (:points dreieck) (:points zu-klein)) "das Dreieck bleibt unberuehrt")
      (is (= :zu-wenig (:ignored zu-klein)) "und sagt, warum es nicht ging"))))
