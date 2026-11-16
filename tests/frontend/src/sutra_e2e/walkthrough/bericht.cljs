(ns sutra-e2e.walkthrough.bericht
  (:require [clojure.string :as str]
            [sutra-e2e.walkthrough.befund :as befund]))

(defn- urteil [q] (or (:urteil q) (:auto q)))
(def ^:private zeichen {"ja" "✓" "nein" "✗" "unklar" "?" "manuell" "·" "n/a" "–"})
(defn- esc [x] (-> (str x) (str/replace "|" "\\|") (str/replace #"\s+" " ")))
(defn- heur [h] (if (number? h) (str "H" h " " (befund/heuristik h)) (str h)))
(defn- datei [pfad] (str/replace pfad #"^.*/" ""))
(defn- sek [x] (.toFixed x 0))

(def ^:private aktionen
  [:goto :klick :knopf :zeile :tippe :option :waehle :fuelle :taste :rad :ziehe :maus :sieh])

(defn- aktion [s]
  (str/join " " (for [k aktionen :when (contains? s k)]
                  (str (name k) (when-not (boolean? (s k)) (str " " (pr-str (s k))))))))

(defn- kriterium [ok text] (str "- " (case ok nil "○" true "✓" false "✗") " " text))

(defn- gruppiert
  [befunde]
  (->> (group-by (juxt :heuristik :art) befunde)
       vals
       (map (fn [bs] (assoc (first bs)
                            :schwere (apply max -1 (keep :schwere bs))
                            :schritte (distinct (map :nr bs)))))
       (sort-by :schwere >)
       (map (fn [g] (str "- **[" (if (neg? (:schwere g)) "–" (str "S" (:schwere g))) "] "
                         (esc (:art g)) "** · " (heur (:heuristik g))
                         " · Schritt " (str/join ", " (:schritte g))
                         "\n  " (esc (:beleg g)))))))

(defn bericht
  [{:keys [schritte persona vorher aufgabe benutzer axe start manuell]}]
  (let [qs (for [r schritte [i q] (map-indexed vector (:fragen r))] {:nr (:nr r) :i i :q q})
        nein (filter #(= "nein" (urteil (:q %))) qs)
        offen (filter #(#{"unklar" "manuell"} (urteil (:q %))) qs)
        cw (for [{:keys [nr i q]} nein]
             {:nr nr :heuristik (or (befund/frage-heuristik i) "CW") :schwere 3
              :art (str "CW: " (:frage q)) :beleg (:beleg q)})
        alle (concat (for [r schritte b (:befunde r)] (assoc b :nr (:nr r))) cw manuell)
        abbr (filter :abbruch schritte)
        spruenge (filter #(and (get-in % [:klm :sprung]) (> (:nr %) 1)) schritte)
        schwer (filter #(>= (or (:schwere %) 0) 3) alle)
        rot (cond-> []
              (seq abbr) (conj (str (count abbr) " Schritt(e) nicht ausführbar"))
              (seq nein) (conj (str (count nein) "× eine Walkthrough-Frage mit „nein“"))
              (seq schwer) (conj (str (count schwer) " Befund(e) mit Schwere ≥ 3")))
        gelb (cond-> []
               (seq offen) (conj (str (count offen) " Frage(n) noch ohne Urteil"))
               (seq spruenge) (conj (str (count spruenge) " Sprung/Sprünge per URL"
                                         " — Teile des Wegs unbelegt"))
               (some #(= 2 (:schwere %)) alle) (conj "Befunde mit Schwere 2"))
        ampel (cond (seq rot) "ROT" (seq gelb) "GELB" :else "GRÜN")
        klm-sek (reduce + (keep #(get-in % [:klm :sek]) schritte))
        sys-sek (/ (reduce + (map :ms schritte)) 1000)
        gesamt (+ klm-sek sys-sek)
        nutzer-schritte (count (remove #(or (get-in % [:klm :sprung]) (get-in % [:schritt :sieh]))
                                       schritte))
        {:keys [name rolle vorwissen vokabular]} persona]
    {:ampel ampel
     :md
     (str/join
      "\n"
      (concat
       [(str "# Walkthrough: " (or aufgabe "(keine Aufgabe im Drehbuch)"))
        ""
        (str "**Persona:** " (or name benutzer) " — " (or rolle "?") ". Vorwissen: "
             (or vorwissen "?") ". Ihr Vokabular: " (or (not-empty (str/join ", " vokabular)) "?"))
        (str "**Methoden:** Cognitive Walkthrough (4 Fragen je Schritt) · Heuristische Evaluation"
             " (Nielsen / ISO 9241-110, Schwere 0–4) · WCAG 2.2 AA (axe-core"
             (when-not axe " — FEHLT") ") · Layoutprüfung · KLM-Zeitschätzung. Lauf " start ".")
        ""
        (str "## Ergebnis: " ampel)]
       (map #(str "- " %) (concat rot gelb))
       [""
        "## Kann das den bisherigen Ablauf ersetzen?"
        (kriterium (empty? abbr) "Jeder Schritt ist ausführbar")
        (kriterium (empty? spruenge)
                   "Der ganze Weg ist in der Oberfläche begehbar (kein Sprung per URL)")
        (kriterium (empty? nein) "Keine Walkthrough-Frage mit „nein“")
        (kriterium (empty? offen) "Jede Frage hat ein Urteil")
        (kriterium (empty? schwer) "Kein Befund mit Schwere ≥ 3")
        (kriterium (when (:sekunden vorher) (<= gesamt (:sekunden vorher)))
                   (str "Zeit: " (sek gesamt) " s (KLM " (sek klm-sek) " s + System "
                        (sek sys-sek) " s) gegen bisher " (or (:sekunden vorher) "?") " s"
                        (when (:quelle vorher) (str " (" (:quelle vorher) ")"))))
        (kriterium (when (:schritte vorher) (<= nutzer-schritte (:schritte vorher)))
                   (str "Schritte: " nutzer-schritte " gegen bisher " (or (:schritte vorher) "?")))
        (str "- ○ Effektivität mit echten Fällen und Zufriedenheit (SUS) misst nur ein Test"
             " mit ihr selbst")
        ""
        "## Schritte"
        "| # | Ziel | Aktion | Q1 | Q2 | Q3 | Q4 | KLM | System | Bild |"
        "|---|---|---|---|---|---|---|---|---|---|"]
       (for [r schritte]
         (str "| " (:nr r) " | " (esc (get-in r [:schritt :ziel])) " | " (esc (aktion (:schritt r)))
              " | " (str/join " | " (map #(zeichen (urteil %) (urteil %)) (:fragen r)))
              " | " (or (get-in r [:klm :sek]) "–") " s " (esc (get-in r [:klm :ops]))
              " | " (.toFixed (/ (:ms r) 1000) 1) " s"
              " | [" (datei (:bild r)) "](" (datei (:bild r)) ") |"))
       ["" "### Belege zu allem, was nicht „ja“ ist"]
       (for [{:keys [nr q]} qs :when (not (#{"ja" "n/a"} (urteil q)))]
         (str "- **" nr "** " (:frage q) " → " (urteil q) ": " (esc (:beleg q))))
       (for [r schritte :when (:notiz r)] (str "- **" (:nr r) "** Notiz: " (esc (:notiz r))))
       ["" "## Befunde nach Schwere"]
       (or (seq (gruppiert alle)) ["- keine"])
       [""]))}))
