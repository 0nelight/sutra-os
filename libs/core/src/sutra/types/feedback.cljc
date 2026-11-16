(ns sutra.types.feedback
  (:require [clojure.string :as str]
            [sutra.kit.decisions :as decisions]
            #?@(:clj [[malli.core :as m]
                      [malli.error :as me]
                      [malli.transform :as mt]])))

(def gefuehle
  [:leicht :geht :schwer])

(def steuertasten
  #{"Enter" "Escape" "Tab" "ArrowUp" "ArrowDown" "PageUp" "PageDown"})

(def ^:private laengen {:id 64 :pfad 300 :app 120 :name 120 :bereich 120 :titel 200
                        :aufgabe 300 :text 4000 :selektor 400 :fehler 500
                        :entscheidungen 20000})

(defn- text [k] [:string {:max (laengen k)}])

(def Id [:string {:min 1 :max (laengen :id)}])
(def Punkt [:tuple :int :int])
(def Rechteck [:tuple :int :int :int :int])

(def Objekt
  [:map
   [:typ (text :name)]
   [:id (text :name)]
   [:titel {:optional true} (text :name)]])

(def ^:private objekte-max 6)

(def Ort
  [:map
   [:pfad (text :pfad)]
   [:app {:optional true} (text :app)]
   [:fenster {:optional true} Punkt]
   [:scroll {:optional true} Punkt]
   [:zustand {:optional true} [:map-of :keyword (text :name)]]
   [:objekte {:optional true} [:vector {:max objekte-max} Objekt]]
   [:dialog {:optional true} (text :bereich)]])

(def Ziel
  [:map
   [:rolle :keyword]
   [:name (text :name)]
   [:objekt {:optional true} Objekt]
   [:wahl {:optional true} (text :name)]
   [:stumm {:optional true} :boolean]
   [:bereich {:optional true} (text :bereich)]
   [:rechteck {:optional true} Rechteck]])

(def Stelle
  [:map
   [:datei (text :pfad)]
   [:zeile pos-int?]
   [:fn {:optional true} (text :name)]])

(def Rahmen
  [:map [:skript (text :name)] [:zeile pos-int?] [:spalte pos-int?]])

(def ^:private spur-max 30)
(def ^:private stellen-max 8)

(defn- stellen [] [:vector {:max stellen-max} Stelle])

(def ^:private referenzen-max 9)

(def Knoten
  [:map
   [:app {:optional true} (text :app)]
   [:pfad {:optional true} (text :pfad)]
   [:art {:optional true} (text :name)]
   [:aktion {:optional true} (text :name)]
   [:param {:optional true} (text :name)]
   [:titel {:optional true} (text :titel)]])

(def Ausloeser
  [:map
   [:knoten {:optional true} Knoten]
   [:selektor {:optional true} (text :selektor)]])

(def ^:private klassen-max 12)

(def Klassen
  [:vector {:max klassen-max} (text :name)])

(def traege-ms
  100)

(def Verzoegerung
  [:map [:tasten nat-int?] [:traege nat-int?] [:max-ms nat-int?]])

(defn verzoegerung
  [tasten dauern]
  {:tasten (max tasten (count dauern))
   :traege (count (filter #(> % traege-ms) dauern))
   :max-ms (long (Math/round (double (reduce max 0 dauern))))})

(def ^:private gesendet-max 40)

(def Gesendet
  [:map-of {:max gesendet-max} (text :name)
   [:or [:enum :leer :zahl :ja-nein :objekt :wert]
    [:map {:closed true} [:id (text :name)]]
    [:map {:closed true} [:laenge nat-int?]]
    [:map {:closed true} [:anzahl nat-int?]]]])

(def ^:private basistypen
  #{:string :integer :long :short :byte :decimal :double :float :boolean :date :timestamp
    :uuid :json})

(defn- objekt-param? [spec]
  (let [t (:type spec)]
    (boolean (and t (not (sequential? t)) (not (basistypen (keyword (name t))))))))

(defn gesendet
  [param-specs args]
  (into (sorted-map)
        (for [[k v] (take gesendet-max args)
              :let [spec (get param-specs (keyword (name k)))]]
          [(name k)
           (cond
             (or (nil? v) (and (string? v) (str/blank? v))) :leer
             (and (objekt-param? spec) (or (string? v) (uuid? v) (number? v)))
             {:id (let [s (str v)] (subs s 0 (min (count s) (laengen :name))))}
             (string? v)     {:laenge (count v)}
             (number? v)     :zahl
             (boolean? v)    :ja-nein
             (sequential? v) {:anzahl (count v)}
             (set? v)        {:anzahl (count v)}
             (map? v)        :objekt
             :else           :wert)])))

(def Absage
  [:map
   [:methode (text :name)]
   [:pfad (text :pfad)]
   [:status :int]
   [:aktion {:optional true} (text :name)]
   [:fehlt {:optional true} [:vector {:max 20} (text :name)]]
   [:ref-art {:optional true} (text :name)]
   [:gesendet {:optional true} Gesendet]
   [:zuordnung {:optional true} [:enum :text :zeit]]
   [:ausloeser {:optional true} Ausloeser]
   [:spur {:optional true} [:vector {:max spur-max} Rahmen]]
   [:browser {:optional true} (stellen)]
   [:server {:optional true} (stellen)]])

(def Referenz
  [:map
   [:nr [:int {:min 1 :max referenzen-max}]]
   [:knoten {:optional true} Knoten]
   [:selektor (text :selektor)]
   [:klassen {:optional true} Klassen]
   [:renderer {:optional true} (stellen)]
   [:ziel Ziel]])

(def ladearten
  {:navigate "geladen" :reload "neu geladen" :back-forward "aus dem Verlauf geladen"
   :prerender "vorab geladen"})

(def Laden
  [:map
   [:antwort {:optional true} nat-int?]
   [:sichtbar {:optional true} nat-int?]
   [:skript nat-int?]
   [:bereit {:optional true} nat-int?]])

(def Paket
  [:vector {:max 8} [:map [:pfad (text :pfad)] [:cache? :boolean] [:ms nat-int?]]])

(def ursache-ms
  2000)

(def schliessarten
  {:befehl "die App schloss es"
   :zustand "sein Offen-Stand ging verloren, ohne Schließbefehl"
   :abgebaut "es wurde offen abgebaut: die Ansicht darum entstand neu"})

(def Ursache
  [:map
   [:art [:enum :zeiger :taste]]
   [:vor-ms nat-int?]
   [:taste {:optional true} (into [:enum] (sort steuertasten))]
   [:punkt {:optional true} Punkt]
   [:ziel {:optional true} Ziel]
   [:selektor {:optional true} (text :selektor)]
   [:klassen {:optional true} Klassen]])

(defn- art [k & felder]
  (into [:map [:id Id] [:art [:= k]] [:t nat-int?]] felder))

(def Ereignis
  [:multi {:dispatch #(some-> % :art keyword)}
   [:klick   (art :klick [:ort Ort] [:punkt Punkt] [:ziel Ziel]
                  [:knoten {:optional true} Knoten]
                  [:selektor {:optional true} (text :selektor)]
                  [:klassen {:optional true} Klassen]
                  [:renderer {:optional true} (stellen)]
                  [:antwort-ms {:optional true} nat-int?]
                  [:entscheidungen {:optional true} (text :entscheidungen)]
                  [:revision {:optional true} (text :name)])]
   [:eingabe (art :eingabe [:ort Ort] [:ziel Ziel] [:laenge nat-int?]
                  [:verzoegerung {:optional true} Verzoegerung])]
   [:taste   (art :taste [:ort Ort] [:taste (into [:enum] (sort steuertasten))]
                  [:ziel {:optional true} Ziel])]
   [:kopie   (art :kopie [:ort Ort] [:ziel Ziel] [:laenge nat-int?])]
   [:einfuegen (art :einfuegen [:ort Ort] [:ziel Ziel] [:laenge nat-int?])]
   [:seite   (art :seite [:ort Ort] [:titel {:optional true} (text :titel)])]
   [:laden   (art :laden [:ort Ort] [:typ (into [:enum] (keys ladearten))] [:ms Laden]
                  [:paket {:optional true} Paket])]
   [:fenster (art :fenster [:ort Ort] [:wechsel [:enum :auf :zu]]
                  [:name {:optional true} (text :bereich)]
                  [:objekt {:optional true} Objekt]
                  [:ursache {:optional true} Ursache]
                  [:wie {:optional true} (into [:enum] (keys schliessarten))]
                  [:spur {:optional true} [:vector {:max spur-max} Rahmen]]
                  [:browser {:optional true} (stellen)]
                  [:revision {:optional true} (text :name)])]
   [:fehler  (art :fehler [:ort Ort] [:text (text :fehler)] [:knoten {:optional true} Knoten]
                  [:selektor {:optional true} (text :selektor)]
                  [:klassen {:optional true} Klassen]
                  [:renderer {:optional true} (stellen)]
                  [:absage {:optional true} Absage]
                  [:revision {:optional true} (text :name)])]
   [:meldung [:and
              (art :meldung [:ort {:optional true} Ort] [:text {:optional true} (text :text)]
                   [:gefuehl {:optional true} (into [:enum] gefuehle)]
                   [:bezug {:optional true} Id]
                   [:revision {:optional true} (text :name)]
                   [:referenzen {:optional true} [:vector {:max referenzen-max} Referenz]])
              [:fn {:error/message "eine Meldung braucht :text oder :gefuehl"}
               #(or (:text %) (:gefuehl %))]]]])

(def Fazit
  [:map
   [:sus [:vector {:min 10 :max 10} [:int {:min 1 :max 5}]]]
   [:text {:optional true} (text :text)]])

(def Sitzung
  [:map
   [:id Id]
   [:benutzer :string]
   [:aufgabe {:optional true} (text :aufgabe)]
   [:begonnen inst?]
   [:beendet {:optional true} [:maybe inst?]]
   [:fazit {:optional true} [:maybe Fazit]]
   [:erledigt {:optional true} [:maybe inst?]]
   [:erledigt-von {:optional true} [:maybe :string]]])

(defn entscheidungen
  [records]
  (let [cap    (laengen :entscheidungen)
        neuste (->> (group-by :kind records) vals (map #(last (sort-by :at %))))
        passt  (reduce (fn [acc r]
                         (let [n (conj acc r)]
                           (if (<= (count (decisions/text n)) cap) n acc)))
                       []
                       (sort-by #(count (decisions/text [%])) neuste))]
    (when (seq passt) (decisions/text (vec (sort-by :at passt))))))

(defn- kappe [k s]
  (when-let [s (some-> s str str/trim (str/replace #"\s+" " ") not-empty)]
    (subs s 0 (min (count s) (laengen k)))))

(defn- runde [v] (mapv #(long (Math/round (double %))) v))

(def ^:private rolle-des-tags
  {"button" :button "a" :link "select" :combobox "textarea" :textbox "summary" :button
   "tr" :row "td" :cell "th" :columnheader "li" :listitem "img" :img "option" :option
   "h1" :heading "h2" :heading "h3" :heading "label" :label})

(def ^:private rolle-des-inputs
  {"checkbox" :checkbox "radio" :radio "button" :button "submit" :button "reset" :button
   "range" :slider "search" :searchbox})

(defn- bedienend
  [r]
  (not (contains? #{:generic :label :heading :img :cell :presentation :none :paragraph} r)))

(defn- objekt [{:keys [typ id titel]}]
  (let [t (kappe :name typ) i (kappe :name id)]
    (when (and t i)
      (cond-> {:typ t :id i}
        (kappe :name titel) (assoc :titel (kappe :name titel))))))

(defn ziel
  [{:keys [tag typ rolle aria beschriftung text titel platzhalter wahl klickbar? bereich
           rechteck] o :objekt}]
  (let [tag (some-> tag str/lower-case)
        r (or (some-> rolle not-empty keyword)
              (when (= "input" tag) (rolle-des-inputs typ :textbox))
              (rolle-des-tags tag)
              :generic)]
    (cond-> {:rolle r
             :name (or (some #(kappe :name %) [aria beschriftung text titel platzhalter]) "")}
      (kappe :name wahl)                        (assoc :wahl (kappe :name wahl))
      (and klickbar? (not (bedienend r)))       (assoc :stumm true)
      (kappe :bereich bereich)                  (assoc :bereich (kappe :bereich bereich))
      (objekt o)                                (assoc :objekt (objekt o))
      rechteck                                  (assoc :rechteck (runde rechteck)))))

(defn- wert-text
  [v]
  (when-not (or (nil? v) (and (coll? v) (empty? v)) (and (string? v) (str/blank? v)))
    (kappe :name (if (string? v) v (pr-str v)))))

(defn ort
  [{:keys [pfad app fenster scroll zustand objekte dialog]}]
  (let [z (into {} (keep (fn [[k v]] (when-let [t (wert-text v)] [(keyword k) t]))) zustand)
        os (into [] (comp (keep objekt) (distinct) (take objekte-max)) objekte)]
    (cond-> {:pfad (or (kappe :pfad pfad) "/")}
      (kappe :app app)          (assoc :app (kappe :app app))
      fenster                   (assoc :fenster (runde fenster))
      scroll                    (assoc :scroll (runde scroll))
      (seq z)                   (assoc :zustand z)
      (seq os)                  (assoc :objekte os)
      (kappe :bereich dialog)   (assoc :dialog (kappe :bereich dialog)))))

(defn knoten* [k]
  (not-empty (into {} (keep (fn [f] (some->> (kappe (if (= f :pfad) :pfad :name) (get k f))
                                             (vector f))))
                   [:app :pfad :art :aktion :param])))

(defn meldung
  [id t {:keys [text gefuehl bezug ort referenzen]}]
  (let [rs (into [] (comp (filter #(and (:nr %) (kappe :selektor (:selektor %)) (:ziel %)))
                          (map #(update % :selektor (partial kappe :selektor)))
                          (map #(let [k (knoten* (:knoten %))]
                                  (cond-> (dissoc % :knoten) k (assoc :knoten k))))
                          (take referenzen-max))
                 referenzen)]
    (cond-> {:id id :art :meldung :t t}
      (kappe :text text) (assoc :text (kappe :text text))
      gefuehl            (assoc :gefuehl gefuehl)
      bezug              (assoc :bezug bezug)
      ort                (assoc :ort ort)
      (seq rs)           (assoc :referenzen rs))))

(defn fenster
  [{:keys [ort wechsel ursache wie spur] n :name o :objekt}]
  (cond-> {:art :fenster :ort ort :wechsel wechsel}
    (kappe :bereich n) (assoc :name (kappe :bereich n))
    (objekt o)         (assoc :objekt (objekt o))
    ursache            (assoc :ursache (cond-> (dissoc ursache :selektor)
                                         (kappe :selektor (:selektor ursache))
                                         (assoc :selektor (kappe :selektor (:selektor ursache)))))
    wie                (assoc :wie wie)
    (seq spur)         (assoc :spur (vec spur))))

(defn fehler
  [{:keys [text werte knoten selektor absage] :as m}]
  (let [ohne (->> werte (filter #(<= 3 (count %))) (sort-by (comp - count))
                  (reduce #(str/replace %1 %2 "…") (str text)))
        k (knoten* knoten)]
    (when-let [t (kappe :fehler ohne)]
      (cond-> (assoc (select-keys m [:ort]) :art :fehler :text t)
        k (assoc :knoten k)
        (kappe :selektor selektor) (assoc :selektor (kappe :selektor selektor))
        absage (assoc :absage absage)))))

(defn spur
  [stapel]
  (into []
        (comp (map (fn [[_ s z sp]]
                     {:skript s :zeile (parse-long z) :spalte (parse-long sp)}))
              (take spur-max))
        (re-seq #"([^/\s()]+\.js)(?:\?[^:\s]*)?:(\d+):(\d+)" (str stapel))))

(defn klassen
  [selektor]
  (reverse (map second (re-seq #"\.((?:[\w-]|\\.)+)" (str selektor)))))

(defn bedienbar?
  [z]
  (or (:stumm z) (bedienend (:rolle z))))

(defn sitzung
  [id benutzer aufgabe jetzt]
  (cond-> {:id id :benutzer benutzer :begonnen jetzt}
    (kappe :aufgabe aufgabe) (assoc :aufgabe (kappe :aufgabe aufgabe))))

#?(:clj
   (do
     (def ^:private stapel-max 500)

     (def ^:private leitung
       (mt/transformer mt/strip-extra-keys-transformer mt/json-transformer))

     (defn- pruefer [schema]
       (let [lesen (m/decoder schema leitung) gut? (m/validator schema) warum (m/explainer schema)]
         (fn [roh]
           (let [v (lesen roh)]
             (if (gut? v) {:wert v} {:fehler (me/humanize (warum v))})))))

     (def ^:private ereignis* (pruefer Ereignis))
     (def ^:private fazit* (pruefer Fazit))

     (defn stapel
       [rohe]
       (let [rs (for [roh (take stapel-max rohe)] (assoc (ereignis* roh) :id (:id roh)))]
         {:angenommen (vec (keep :wert rs))
          :abgelehnt (cond-> (mapv #(select-keys % [:id :fehler]) (filter :fehler rs))
                       (> (count rohe) stapel-max)
                       (conj {:id nil :fehler (str "mehr als " stapel-max " in einem Stapel")}))}))

     (defn fazit
       [roh]
       (fazit* roh))))

(def sus-fragen
  ["Ich denke, dass ich das System gerne häufig benutzen würde."
   "Ich fand das System unnötig komplex."
   "Ich fand das System einfach zu benutzen."
   (str "Ich glaube, ich würde die Hilfe einer technisch versierten Person benötigen, um das "
        "System benutzen zu können.")
   "Ich fand, die verschiedenen Funktionen in diesem System waren gut integriert."
   "Ich denke, das System enthielt zu viele Inkonsistenzen."
   (str "Ich kann mir vorstellen, dass die meisten Menschen den Umgang mit diesem System sehr "
        "schnell lernen.")
   "Ich fand das System sehr umständlich zu nutzen."
   "Ich fühlte mich bei der Benutzung des Systems sehr sicher."
   "Ich musste eine Menge lernen, bevor ich anfangen konnte, mit dem System zu arbeiten."])

(defn sus-wert
  [antworten]
  (when (and (= 10 (count antworten)) (every? #(and (int? %) (<= 1 % 5)) antworten))
    (* 2.5 (reduce + (map-indexed (fn [i a] (if (even? i) (dec a) (- 5 a))) antworten)))))

(defn meldung? [e] (= :meldung (:art e)))
(defn fehler? [e] (= :fehler (:art e)))

(def ^:private rollen-wort
  {:button "Knopf" :link "Link" :textbox "Feld" :searchbox "Suchfeld" :combobox "Auswahl"
   :checkbox "Kästchen" :radio "Option" :slider "Regler" :row "Zeile" :cell "Zelle"
   :columnheader "Spaltenkopf" :tab "Reiter" :option "Eintrag" :menuitem "Menüpunkt"
   :listitem "Eintrag" :img "Bild" :heading "Überschrift" :label "Beschriftung"
   :dialog "Dialog" :generic "Fläche" :presentation "Text" :none "Text" :paragraph "Absatz"})

(defn- objekt-wort [{:keys [typ id titel]}]
  (str typ (when titel (str " „" titel "“")) " [" id "]"))

(defn ziel-wort [{:keys [rolle bereich wahl stumm objekt] n :name}]
  (str (rollen-wort rolle (some-> rolle name)) (when (seq n) (str " „" n "“"))
       (when wahl (str " = " wahl))
       (when stumm " (klickbar, ohne Rolle)")
       (when bereich (str " · " bereich))
       (when objekt (str " · in " (objekt-wort objekt)))))

(defn stelle-wort [{:keys [datei zeile] f :fn}]
  (str datei ":" zeile (when f (str " " f))))

(defn- stellen-wort [ss] (str/join " ← " (map stelle-wort ss)))

(defn knoten-wort [{:keys [app pfad art aktion param titel]}]
  (str/join " " (remove nil? [(when app (str "App " app))
                              pfad
                              (when art (str "(" art (when titel (str " „" titel "“")) ")"))
                              (when aktion (str "· Aktion " aktion
                                                (when param (str " / " param))))])))

(defn referenz-wort
  [{:keys [nr selektor knoten renderer] z :ziel}]
  (str "/" nr " = " (ziel-wort z)
       (when knoten (str " · " (knoten-wort knoten)))
       " ⟨" selektor "⟩"
       (when (seq renderer) (str " → " (stellen-wort renderer)))))

(defn herkunft
  [{:keys [knoten selektor renderer absage revision]}]
  (let [{:keys [methode pfad status aktion fehlt ref-art zuordnung ausloeser browser server]}
        absage
        k (first (filter #(and % (some % [:pfad :aktion :param]))
                         [knoten (:knoten ausloeser)]))]
    (not-empty
     (cond-> []
       aktion (conj (str "Aktion " aktion
                         (when (seq fehlt) (str " — fehlt: " (str/join ", " fehlt)))
                         (when ref-art (str " (" ref-art ")"))))
       k (conj (str "Knoten " (knoten-wort k)))
       (:selektor ausloeser) (conj (str "Auslöser ⟨" (:selektor ausloeser) "⟩"))
       selektor (conj (str "Anzeige ⟨" selektor "⟩"))
       (seq renderer) (conj (str "Renderer " (stellen-wort renderer)))
       (seq browser) (conj (str "Browser " (stellen-wort browser)))
       absage (conj (str "Anfrage " methode " " pfad " → " status
                         (when (= :zeit (some-> zuordnung keyword))
                           " (nur der Zeit nach zugeordnet)")))
       (seq server) (conj (str "Server " (stellen-wort server)))
       (and revision (or (seq renderer) (seq browser) (seq server)))
       (conj (str "Stand " revision))))))

(defn stand
  [{:keys [dialog objekte zustand]}]
  (some->> (concat (when dialog [(str "Dialog „" dialog "“")])
                   (map objekt-wort objekte)
                   (for [[k v] (sort-by key zustand)] (str (name k) "=" v)))
           seq
           (str/join " · ")))

(defn- laden-satz [typ {:keys [sichtbar skript bereit]} paket pfad]
  (let [cache (filter :cache? paket)]
    (str "Seite " (ladearten typ) ": " pfad
         (some->> [(when sichtbar (str "erstes Bild nach " sichtbar " ms"))
                   (str "Paket läuft nach " skript " ms")
                   (when bereit (str "bereit nach " bereit " ms"))
                   (when (seq cache)
                     (str "Paket aus dem Browser-Cache ("
                          (str/join ", " (map :pfad cache)) ")"))]
                  (remove nil?) seq (str/join ", ") (str " · ")))))

(defn- ursache-wort [{:keys [art taste ziel vor-ms]}]
  (str (if (= :taste art)
         (str "Taste " (or taste "(Zeichen)"))
         (str "Druck auf " (if ziel (ziel-wort ziel) "die Seite")))
       " " vor-ms " ms davor"))

(defn- fenster-satz [{:keys [wechsel objekt ursache wie browser] n :name}]
  (let [zu? (= :zu wechsel)]
    (str "Fenster " (if (str/blank? n) "ohne Titel" (str "„" n "“"))
         (when objekt (str " (" (objekt-wort objekt) ")"))
         (if zu? " geschlossen" " geöffnet")
         (cond ursache (str " · nach " (ursache-wort ursache))
               zu?     (str " · OHNE ZUTUN (kein Druck, keine Taste in den "
                            ursache-ms " ms davor)"))
         (when wie (str " · " (schliessarten wie)))
         (when (seq browser) (str " · aufgerufen von " (stellen-wort browser))))))

(defn satz
  [{:keys [art ziel laenge taste titel ort text gefuehl referenzen knoten verzoegerung
           antwort-ms typ ms paket] :as e}]
  (case art
    :klick   (str "Klick auf " (ziel-wort ziel)
                  (when-not (bedienbar? ziel) " (antwortet nicht auf Klicks)")
                  (when (> (or antwort-ms 0) traege-ms)
                    (str " (" antwort-ms " ms bis zum nächsten Bild)")))
    :laden   (laden-satz typ ms paket (:pfad ort))
    :eingabe (if (:wahl ziel)
               (str "Wahl in " (ziel-wort ziel))
               (str "Eingabe in " (ziel-wort ziel) " (" laenge " Zeichen"
                    (when-let [{:keys [tasten traege max-ms]} verzoegerung]
                      (when (> max-ms traege-ms)
                        (str "; bis " max-ms " ms je Taste, " traege " von " tasten " träge")))
                    ")"))
    :taste   (str "Taste " taste (when ziel (str " in " (ziel-wort ziel))))
    :kopie   (str "Kopiert aus " (ziel-wort ziel) " (" laenge " Zeichen)")
    :einfuegen (str "Eingefügt in " (ziel-wort ziel) " (" laenge " Zeichen)")
    :seite   (str "Wechsel zu " (or titel (:pfad ort)))
    :fenster (fenster-satz e)
    :fehler  (str "Fehler: " text
                  (when knoten (str " · " (str/replace (knoten-wort knoten) #"^· " ""))))
    :meldung (str (str/join " — " (remove nil? [(some-> gefuehl name) text]))
                  (when (seq referenzen)
                    (str " · " (str/join " · " (map referenz-wort referenzen)))))))

(defn uebertragungen
  ([ereignisse] (uebertragungen ereignisse 120000))
  ([ereignisse ms]
   (first (reduce (fn [[paare kopie] e]
                    (case (:art e)
                      :kopie [paare e]
                      :einfuegen [(cond-> paare
                                    (and kopie (<= (- (:t e) (:t kopie)) ms)) (conj [kopie e]))
                                  kopie]
                      [paare kopie]))
                  [[] nil]
                  (sort-by :t ereignisse)))))

(defn- mit-ort
  [es]
  (first (reduce (fn [[acc letzter] e]
                   (cond
                     (not (meldung? e)) [(conj acc e) (or (:ort e) letzter)]
                     (or (:ort e) (nil? letzter)) [(conj acc e) letzter]
                     :else [(conj acc (assoc e :ort letzter)) letzter]))
                 [[] nil]
                 es)))

(defn verlauf
  [ereignisse]
  (let [es (mit-ort (sort-by (juxt :t :id) ereignisse))
        ids (into #{} (comp (remove meldung?) (map :id)) es)
        angehaengt? #(and (meldung? %) (ids (:bezug %)))
        quelle (into {} (map (fn [[k e]] [(:id e) k])) (uebertragungen es))
        mit-satz #(cond-> (assoc % :satz (satz %))
                    (and (fehler? %) (herkunft %)) (assoc :herkunft (herkunft %))
                    (quelle (:id %))
                    (update :satz str " ← aus " (ziel-wort (:ziel (quelle (:id %)))))
                    (and (meldung? %) (stand (:ort %))) (assoc :stand (stand (:ort %))))
        an (group-by :bezug (filter angehaengt? es))]
    (into []
          (comp (remove angehaengt?)
                (map #(cond-> (mit-satz %)
                        (an (:id %)) (assoc :meldungen (mapv mit-satz (an (:id %)))))))
          es)))

(defn relativ
  [{[x y] :punkt {[b h] :fenster} :ort}]
  (when (and x b h (pos? b) (pos? h))
    [(min 1 (max 0 (/ x b))) (min 1 (max 0 (/ y h)))]))

(defn- abstand [[x1 y1] [x2 y2]]
  (Math/sqrt (+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2)))))

(defn mehrfachklicks
  ([ereignisse] (mehrfachklicks ereignisse {:n 3 :ms 1500 :px 30}))
  ([ereignisse {:keys [n ms px]}]
   (let [nah? #(and (<= (- (:t %2) (:t %1)) ms) (<= (abstand (:punkt %1) (:punkt %2)) px))]
     (->> (filter #(= :klick (:art %)) ereignisse)
          (sort-by :t)
          (reduce (fn [serien e]
                    (let [s (peek serien)]
                      (if (and s (nah? (peek s) e))
                        (conj (pop serien) (conj s e))
                        (conj serien [e]))))
                  [])
          (filterv #(>= (count %) n))))))

(defn zusammenfassung
  [s ereignisse]
  (let [arten (frequencies (map :art ereignisse))
        ms (filter meldung? ereignisse)]
    {:dauer-s (quot (reduce max 0 (map :t ereignisse)) 1000)
     :klicks (arten :klick 0) :eingaben (arten :eingabe 0) :seiten (arten :seite 0)
     :meldungen (count ms)
     :fehler (into [] (comp (filter fehler?) (map :text) (distinct)) ereignisse)
     :gefuehle (frequencies (keep :gefuehl ms))
     :schwer (into [] (comp (filter #(= :schwer (:gefuehl %))) (map #(or (:bezug %) (:id %)))) ms)
     :kopien (arten :kopie 0)
     :ladungen (into [] (comp (filter #(= :laden (:art %))) (map :id)) ereignisse)
     :langsame-klicks (into [] (comp (filter #(> (:antwort-ms % 0) traege-ms)) (map :id))
                            ereignisse)
     :uebertragungen (mapv #(mapv :id %) (uebertragungen ereignisse))
     :mehrfachklicks (mapv #(mapv :id %) (mehrfachklicks ereignisse))
     :stumm (into [] (comp (keep :ziel) (filter :stumm) (map :name) (distinct)) ereignisse)
     :traege (into [] (comp (filter #(> (get-in % [:verzoegerung :max-ms] 0) traege-ms))
                            (map :id))
                   ereignisse)
     :sus (sus-wert (get-in s [:fazit :sus]))}))
