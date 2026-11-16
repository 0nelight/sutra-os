(ns sutra-e2e.walkthrough.befund
  (:require [clojure.string :as str]))

(def heuristik
  {1 "Sichtbarkeit des Systemstatus (Selbstbeschreibungsfähigkeit)"
   2 "Sprache der Nutzerin (Erwartungskonformität)"
   3 "Kontrolle und Freiheit (Steuerbarkeit)"
   4 "Konsistenz und Standards (Erwartungskonformität)"
   5 "Fehlervermeidung (Robustheit gegen Benutzungsfehler)"
   6 "Erkennen statt Erinnern (Erlernbarkeit)"
   7 "Flexibilität und Effizienz (Aufgabenangemessenheit)"
   8 "Ästhetik und Minimalismus (Aufgabenangemessenheit)"
   9 "Fehler erkennen und beheben (Robustheit gegen Benutzungsfehler)"
   10 "Hilfe und Dokumentation (Erlernbarkeit)"})

(def fragen-text
  ["Will sie an dieser Stelle genau diese Wirkung erreichen?"
   "Bemerkt sie, dass die richtige Aktion verfügbar ist?"
   "Verbindet sie die Aktion mit der Wirkung, die sie will?"
   "Sieht sie nach der Aktion, dass sie vorankommt?"])

(def frage-heuristik
  [nil 6 2 1])

(defn- norm [x] (-> (str x) str/lower-case (str/replace #"\s+" " ") str/trim))
(defn- neu [a b] (remove (set a) b))
(defn- f [i auto beleg] {:frage (fragen-text i) :auto auto :beleg beleg :urteil nil})

(defn- q2 [s z]
  (cond
    (nil? z)       (f 1 "n/a" (if (:goto s) "Sprung per URL — kein Bedienelement"
                                  "Aktion ohne Zielelement"))
    (:fehlt z)     (f 1 "nein" "Bedienelement nicht gefunden")
    (:gesperrt z)  (f 1 "nein" (str "„" (:name z) "“ ist gesperrt"))
    (not (:imBild z)) (f 1 "unklar" (str "„" (:name z) "“ außerhalb des Bildes (y=" (:y z)
                                         ") — sie müsste scrollen"))
    :else          (f 1 "ja" (str "„" (:name z) "“ sichtbar, " (:breite z) "×" (:hoehe z) " px"))))

(defn- q3 [s z vor]
  (let [w (norm (:sucht s))]
    (cond
      (or (nil? z) (:fehlt z)) (f 2 "n/a" "")
      (str/blank? w) (f 2 "unklar" "kein „sucht“ im Drehbuch — ihr eigenes Wort fehlt")
      (str/includes? (norm (:name z)) w)
      (f 2 "ja" (str "sucht „" (:sucht s) "“, Element heißt „" (:name z) "“"))
      (pos? (get-in vor [:treffer :imBild] 0))
      (f 2 "unklar" (str "„" (:sucht s) "“ steht im Bild, aber das Element heißt „" (:name z) "“"))
      :else (f 2 "nein" (str "sucht „" (:sucht s) "“ — nirgends im Bild; Element heißt „"
                             (:name z) "“")))))

(defn- q4 [s rec vor nach]
  (let [wechsel (concat
                 (when (and vor (not= (:url vor) (:url nach))) [(str "Adresse → " (:url nach))])
                 (map #(str "neu: " %) (take 3 (neu (:titel vor) (:titel nach))))
                 (map #(str "Meldung: " %) (take 2 (neu (:live vor) (:live nach))))
                 (map #(str "Dialog: " (subs % 0 (min 60 (count %))))
                      (take 1 (neu (:modal vor) (:modal nach)))))
        belegt (str/join "; " wechsel)
        tr (:treffer nach)]
    (cond
      (:abbruch rec) (f 3 "nein" (:abbruch rec))
      (:erwartet s)
      (cond
        (not (pos? (:irgendwo tr 0)))
        (f 3 "nein" (str "erwartet „" (:erwartet s) "“ — nicht auf der Seite. " belegt))
        (not (pos? (:imBild tr 0)))
        (f 3 "unklar" (str "„" (:erwartet s) "“ steht nur außerhalb des Bildes"))
        :else (f 3 "ja" (str "„" (:erwartet s) "“ sichtbar. " belegt)))
      (some s [:tippe :fuelle :waehle]) (f 3 "ja" "Eingabe steht im Feld")
      (seq wechsel) (f 3 "ja" belegt)
      (not= (:text vor) (:text nach))
      (f 3 "unklar" "nur der Text hat sich geändert — keine Meldung, kein neuer Titel")
      :else (f 3 "nein" "keine sichtbare Veränderung"))))

(defn fragen
  [s rec vor nach]
  (let [seite (or (not-empty (str/join " · " (take 4 (:titel vor)))) "–")
        a (f 0 "manuell" (str (or (:ziel s) "kein ziel im Drehbuch") " — Seite zeigt: " seite))]
    (if (:sieh s)
      [a (f 1 "n/a" "nur schauen") (f 2 "n/a" "") (f 3 "n/a" "")]
      [a (q2 s (:ziel rec)) (q3 s (:ziel rec) vor) (q4 s rec vor nach)])))

(def ^:private schwere-axe {"critical" 3 "serious" 3 "moderate" 2 "minor" 1})

(defn- liste [xs n] (str/join " · " (take n xs)))

(defn befunde
  [{:keys [nr schritt ms lade layout a11y abbruch fehler erlaubt ziel]} nach]
  (let [sek (.toFixed (/ ms 1000) 1)
        l layout
        b (fn [h s art beleg] {:heuristik h :schwere s :art art :beleg beleg})]
    (cond-> []
      abbruch (conj (b 9 4 "Schritt nicht ausführbar" abbruch))
      (and (:goto schritt) (> nr 1))
      (conj (b "Methode" nil "Sprung per URL"
               (str (:goto schritt) " — der Weg dorthin ist unbelegt")))
      (seq fehler) (conj (b 9 3 "Fehler im Hintergrund" (str/join " | " (take 3 fehler))))
      (> ms 10000) (conj (b 1 3 "Antwort dauert über 10 s" (str sek " s")))
      (and (< 1000 ms 10001) (zero? lade))
      (conj (b 1 2 "Wartezeit ohne Ladeanzeige" (str sek " s")))
      (and (seq erlaubt) (not (re-find #"(?i)rückgängig|widerruf|storn|undo" (str (:text nach)))))
      (conj (b 3 2 "Schreibt ohne sichtbare Rücknahme" (str/join ", " erlaubt)))
      (and ziel (not (:fehlt ziel)) (or (< (:breite ziel) 24) (< (:hoehe ziel) 24)))
      (conj (b "WCAG 2.5.8" 1 "Ziel dieses Schritts kleiner als 24 px" (:name ziel)))
      (seq (:abgeschnitten l))
      (conj (b 8 2 "Text abgeschnitten, kein Tooltip" (liste (:abgeschnitten l) 5)))
      (seq (:ueberlappung l)) (conj (b 8 2 "Bedienelemente überlappen" (liste (:ueberlappung l) 3)))
      (seq (:fremdeFormate l))
      (conj (b 2 2 "Zahl/Datum nicht im österreichischen Format" (liste (:fremdeFormate l) 5)))
      (seq (:klein l))
      (conj (b "WCAG 2.5.8" 1 "Bedienziele kleiner als 24 px" (liste (:klein l) 5)))
      (pos? (:querscroll l 0)) (conj (b 8 1 "Seite scrollt waagrecht" (str (:querscroll l) " px")))
      (seq (:kleineSchrift l)) (conj (b 8 1 "Schrift unter 12 px" (liste (:kleineSchrift l) 5)))
      (> (:bedienelemente l 0) 60)
      (conj (b 8 1 "Sehr viele Wahlmöglichkeiten zugleich"
               (str (:bedienelemente l) " Bedienelemente im Bild")))
      (> (count (:hauptaktionen l)) 1)
      (conj (b 4 1 "Mehrere Hauptaktionen konkurrieren" (liste (:hauptaktionen l) 4)))
      true (into (for [v a11y]
                   (b (str "WCAG " (str/join ", " (:wcag v))) (schwere-axe (:impact v) 1)
                      (str "axe: " (:id v))
                      (str (:hilfe v) " — " (:knoten v) "×, z. B. " (:beispiel v))))))))

(def ^:private op
  {\K 0.28 \P 1.1 \B 0.1 \H 0.4 \M 1.35})

(defn klm
  [s tastatur?]
  (let [text (second (or (:tippe s) (:fuelle s)))
        ops (cond
              (:goto s) nil
              (:sieh s) "M"
              (some s [:klick :knopf :zeile :option :maus]) "MPBB"
              (some? text) (apply str "MPBBH" (repeat (count (str text)) \K))
              (:waehle s) "MPBBPBB"
              (:taste s) "MK"
              (:rad s) (apply str "MP" (repeat (js/Math.ceil (/ (abs (nth (:rad s) 2)) 100)) \K))
              (:ziehe s) "MPBPB"
              :else "")
        ops (if (and ops tastatur? (str/includes? ops "P")) (str "H" ops) ops)]
    {:ops (when ops (->> (partition-by identity ops)
                         (map #(if (> (count %) 1) (str (count %) (first %)) (str (first %))))
                         (str/join " ")))
     :sek (when ops (/ (js/Math.round (* 100 (reduce + 0 (map op ops)))) 100))
     :sprung (boolean (:goto s))
     :tastatur (if (:sieh s) tastatur? (boolean (or (some? text) (:taste s))))}))
