(ns sutra-ui.rtf-smoke-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [clojure.string :as str]
            [reagent.core :as r]
            [reagent.dom.server :as rds]
            [sutra.capps.rtf :as rtf]
            [sutra-ui.apphost.widgets.rtf :refer [rtf-text]]))

(def ^:private dokumente
  ["{\\rtf1\\ansi\\ansicpg1252\\deff0\\deflang1031{\\fonttbl{\\f0\\fswiss\\fcharset0 SegoeUI;}{\\f1\\fnil\\fcharset0 Arial;}}\r\n\\viewkind4\\uc1\\pard\\lang3079\\f0\\fs20 Lieferzeit (ab Datum Freigabe Werkpl\\'e4ne) 8 Wochen.\\par\r\nIm Falle einer Vergabe und Planfreigabe bis 22.12.2023 bitten wir\\par\r\nauch um Ber\\'fccksichtigung der 2-w\\'f6chigen Betriebsurlaube zum\\par\r\nJahreswechsel, die zur genannten Lieferzeit nochmal dazu\\par\r\ngerechnet werden m\\'fcssen.\\f1\\par\r\n\\par\r\n\\lang1031 Alle Preise \\lang3079 nur g\\'fcltig bei Abnahme der angebotenen Menge und\\lang1031  einschlie\\'dfich Lieferung und Montage an normalen Arbeitstagen. \\par\r\n\\par\r\nF\\'fcr weitere Fragen und Ausk\\'fcnfte stehen wir Ihnen gerne zur Verf\\'fcgung!\\par\r\n}"
   "{\\rtf1\\ansi\\ansicpg1252\\deff0\\deflang1031{\\fonttbl{\\f0\\fnil\\fcharset0 Arial;}}\r\n\\viewkind4\\uc1\\pard\\fs20 Leistungszeitraum: 37. - 38. KW 2022\\par\r\n\\par\r\n\\lang3079 Projekt: Planung Nischenm\\'f6bel Damensauna\\par\r\n\\par\r\nEntwurfsplanung M\\'f6belbau mit der Darstellung der beiden besprochenen Varianten, 1x Korrektur lt. Besprechungsergebnis. \\par\r\n\\lang1031\\par\r\n}"])

(deftest unveraendert-ist-zeichengleich
  (doseq [d dokumente]
    (is (rtf/rtf? d))
    (is (= d (rtf/patch d (rtf/edit-text d)))
        "ein unberührtes Dokument muss Zeichen für Zeichen wiederkommen")))

(deftest zerlegung-verliert-nichts
  (doseq [d dokumente]
    (is (= d (apply str (map :s (rtf/segments d))))
        "die Stücke aneinandergehängt sind wieder das Dokument")))

(deftest bearbeiten-erhaelt-das-dokument
  (doseq [d dokumente]
    (let [t  (rtf/edit-text d)
          t2 (str t "
Ein neuer Absatz mit Umlauten: äöü ß €.")
          d2 (rtf/patch d t2)]
      (is (= t2 (rtf/edit-text d2)) "der geänderte Text muss zurücklesbar sein")
      (is (str/includes? d2 "fonttbl") "die Schrifttabelle überlebt eine Änderung")
      (is (str/includes? d2 "\\'e4") "ein Umlaut geht als Codepage-Byte hinaus"))))

(deftest text-zeigt-keine-auszeichnung
  (doseq [d dokumente]
    (let [t (rtf/text d)]
      (is (not (str/includes? t "\rtf")) "der Lesetext trägt keine Marken")
      (is (not (str/includes? t "fonttbl"))))))

(deftest widget-zeigt-text-und-schreibt-dokument
  (testing "gezeigt wird der Fliesstext, nicht die Auszeichnung"
    (doseq [d dokumente]
      (let [sel  (r/atom d)
            html (rds/render-to-string [rtf-text sel {:rows 6}])]
        (is (str/includes? html "textarea"))
        (is (not (str/includes? html "fonttbl"))
            "die Schrifttabelle darf nicht im Feld stehen")
        (is (not (str/includes? html "\\rtf1"))
            "der RTF-Kopf darf nicht im Feld stehen"))))

  (testing "geschrieben wird das geflickte Dokument"
    (doseq [d dokumente]
      (let [sel (r/atom d)]
        (reset! sel (rtf/patch @sel (rtf/edit-text @sel)))
        (is (= d @sel))
        (let [neu (str (rtf/edit-text d) "\nNachtrag")]
          (reset! sel (rtf/patch @sel neu))
          (is (= neu (rtf/edit-text @sel)))
          (is (str/includes? @sel "fonttbl")
              "die Schrifttabelle ueberlebt das Schreiben"))))))

(deftest leerraum-vor-dem-ersten-textlauf-ist-kein-textlauf
  (let [rtf-doc (str "{\\rtf1\\ansi\\ansicpg1252\\deff0\\deflang2055"
                  "{\\fonttbl{\\f0\\fnil\\fcharset0 Arial;}}\r\n"
                  "\\viewkind4\\uc1\\pard\\fs20 Vielen Dank.\\par}")
        eins (str/replace-first (rtf/edit-text rtf-doc) "Dank" "Donk")
        aus  (rtf/patch rtf-doc eins)]
    (is (= eins (rtf/edit-text aus)))
    (is (= (count rtf-doc) (count aus))
        "eine Ein-Zeichen-Aenderung aendert genau ein Zeichen")
    (is (str/includes? aus "\\pard\\fs20 Vielen Donk.")
        "der Text bleibt HINTER seinen Formatangaben — ein \\r\\n zwischen
         Kopfgruppe und Steuerwort ist bedeutungsloser Leerraum, kein Textlauf,
         und wer ihn fuer einen haelt, legt den neuen Text davor")))
