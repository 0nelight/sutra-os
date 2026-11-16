(ns sutra.capps.events
  (:require [sutra.kit.coll :as coll]
            [clojure.string :as str]
            [sutra.capps.bind :as bind]))

(def value-refs
  (mapv #(select-keys % [:id :key :label :hint]) bind/effect-forms))

(def ^:private value-ref-keys
  (into #{} (keep :key) value-refs))

(def effects
  [{:do :set :label "Variable setzen" :group :variable
    :hint "Setzt eine Zustandsvariable auf einen Wert — App Studio's „Set variable value“."
    :fields [{:key :var :kind :handle :label "Variable" :required true
              :hint "Die Zustandsvariable, die geschrieben wird."}
             {:key :to :kind :value :label "Wert"
              :hint "Ohne Wert wird die Variable geleert (nil) — ein „Filter zurücksetzen“."}]}
   {:do :navigate :label "Seite wechseln" :group :navigation
    :hint "Schreibt die Auswahl-Variable eines :pages/:tabs-Containers — der Sprung auf eine Seite."
    :fields [{:key :var :kind :handle :label "Seiten-Variable" :required true
              :hint "Dieselbe Variable, die der :pages/:tabs-Container als :select bindet."}
             {:key :to :kind :page :label "Seite" :required true
              :hint "Die :id der Seite bzw. des Reiters."}]}
   {:do :open-app :label "Andere App öffnen" :group :navigation
    :hint (str "Öffnet eine andere Anwendung und reicht ihr Werte durch deren"
               " SCHNITTSTELLE — App Studio's „Open App Studio module“. Die Ziel-App"
               " bestimmt selbst, was sie annimmt: die :external-id ihrer Variablen."
               " Darauf wird hier abgebildet, statt eine URL von Hand zu bauen — ein"
               " handgeschriebener Link kennt die Schnittstelle nicht und merkt nicht,"
               " wenn sie sich ändert.")
    :fields [{:key :app :kind :app-name :label "Anwendung" :required true
              :hint "Der Name der Ziel-Anwendung."}
             {:key :with :kind :params :label "Schnittstelle"
              :hint (str "Map <external-id der Ziel-App> → Wert aus dieser App."
                         " Referenzen ({:val …}/{:row …}) sind erlaubt.")}]}
   {:do :close-app :label "App schließen" :group :navigation
    :hint (str "Schließt die App und kehrt dorthin zurück, wo sie geöffnet wurde: auf die"
               " Startseite ihres Arbeitsplatzes oder ins Portal — auch wenn die App selbst"
               " die Startseite ist. Der Rückweg einer App, die ihren Modulkopf verbirgt.")
    :fields []}
   {:do :open-object :label "Objekt-Seite öffnen" :group :navigation
    :hint "Verlässt die App und öffnet die volle Objekt-Ansicht — App Studio's „Open Object view“."
    :fields [{:key :type :kind :type :label "Objekttyp"
              :hint "Leer = der Typ der geklickten Zeile bzw. der Overlay-Nutzlast."}
             {:key :id :kind :value :label "Objekt-Id"
              :hint "Leer = die :id der geklickten Zeile."}
             {:key :object :kind :value :label "Objekt"
              :hint (str "Eine Objekt-Variable ({:val v}): öffnet ihr (erstes) Objekt, Typ und"
                         " Id aus ihm — statt :type/:id von Hand.")}
             {:key :new-tab? :kind :bool :label "In neuem Tab"
              :hint "Die App bleibt offen, wie sie ist; die Objekt-Seite öffnet daneben."}]}
   {:do :open-explorer :label "Explorer öffnen" :group :navigation
    :hint (str "Öffnet eine Objektmenge im Explorer, in einem neuen Tab —"
               " App Studio's „Open Explorer“. Die Menge ist ein Typ mit"
               " Gleichheitsfiltern (:where) oder eine Liste von Objekt-Ids (:ids);"
               " sie reist als temporäres Objekt-Set, nicht als handgebaute URL.")
    :fields [{:key :type :kind :type :label "Objekttyp"
              :hint "Leer = der Typ der geklickten Zeile."}
             {:key :where :kind :params :label "Filter"
              :hint (str "Map <Eigenschaft> → Wert, jeweils Gleichheit; Referenzen"
                         " ({:val …}/{:row …}) sind erlaubt, ein leerer Wert filtert nicht.")}
             {:key :ids :kind :value :label "Objekt-Ids"
              :hint "Eine Liste von Ids (oder Zeilen), z. B. {:val auswahl} — statt :where."}]}
   {:do :open-document :label "Notebook-Dokument öffnen" :group :navigation
    :hint (str "Öffnet ein Notebook-Dokument zum Lesen in einem neuen Tab — App Studio's"
               " „Open Notebook document“.")
    :fields [{:key :document :kind :value :label "Dokument" :required true
              :hint "Id oder Name des Dokuments; Referenzen sind erlaubt."}]}
   {:do :open :label "Overlay öffnen" :group :overlay
    :hint (str "Öffnet ein benanntes Modal/Drawer und übergibt ihm eine Nutzlast."
               " Bindet der Container :visible an eine setzbare Variable, landet"
               " die Nutzlast DORT — dann trägt eine einzige Variable, ob die"
               " Klappe offen ist und wofür.")
    :fields [{:key :overlay :kind :overlay-key :label "Overlay" :required true
              :hint "Der :overlay-Schlüssel des Containers, der geöffnet wird."}
             {:key :arg :kind :value :label "Nutzlast"
              :hint "Füllt die {:arg <prop>}-Referenzen im Body des Overlays."}]}
   {:do :document :label "Beleg erzeugen" :group :document
    :hint (str "Rendert eine Vorlage (.entt) zu einem Dokument und legt dessen"
               " PDF in eine Variable — was ein :pdf-Baustein daneben zeigt."
               " Die App darf nur die Vorlagen, die sie unter :documents nennt."
               " Gespeichert wird NICHTS, ausser :save? (das Dokument, an :link)"
               " oder :media (die PDF-Bytes, fuer eine Action, die sie an ein"
               " Objekt schreibt).")
    :fields [{:key :template :kind :template-key :label "Vorlage" :required true
              :hint "Der Name der Vorlage — eine der unter :documents erlaubten."}
             {:key :values :kind :params :label "Parameter"
              :hint "Die Werte der Vorlage; Referenzen ({:val …}/{:row …}) sind erlaubt."}
             {:key :var :kind :handle :label "Ziel-Variable"
              :hint "Bekommt die URL des erzeugten PDFs — Bindung fuer :pdf/:media."}
             {:key :link :kind :value :label "Objekt"
              :hint "Id des Objekts, an dem das Dokument haengen soll."}
             {:key :type :kind :type :label "Objekttyp"
              :hint "Typ zu :link — leer = der Typ der geklickten Zeile."}
             {:key :name :kind :value :label "Dokumentname"
              :hint "Leer = der Titel der Vorlage."}
             {:key :save? :kind :bool :label "Als Dokument speichern"
              :hint "Legt das gerenderte Dokument ab (an :link). Ohne: nur das PDF."}
             {:key :media :kind :handle :label "Media-Referenz"
              :hint (str "Bekommt den sha256 des PDFs im Blob-Speicher — den Wert"
                         " einer :media-reference, die eine Action ans Objekt schreibt.")}]}
   {:do :mail :label "E-Mail-Entwurf öffnen" :group :document
    :hint (str "Öffnet im Mailprogramm dieses Rechners ein Entwurfsfenster mit"
               " dem PDF als Anhang — nur im Desktop-Client (clients/sutra-desktop),"
               " denn eine Webseite kann kein Programm starten.")
    :fields [{:key :to :kind :value :label "An" :required true}
             {:key :subject :kind :value :label "Betreff"}
             {:key :text :kind :value :label "Text" :multiline true}
             {:key :from :kind :value :label "Absender"
              :hint "Leer = die Standard-Identität des Mailprogramms."}
             {:key :attach :kind :value :label "Anhang"
              :hint "Die PDF-URL, die ein {:do :document} in seine Variable legte."}
             {:key :filename :kind :value :label "Dateiname"}
             {:key :template :kind :template-key :label "Vorlage"
              :hint "Ist :attach leer, wird das PDF aus dieser Vorlage erst erzeugt."}
             {:key :values :kind :params :label "Parameter"}
             {:key :var :kind :handle :label "PDF-Variable"
              :hint "Bekommt die URL des dabei erzeugten PDFs."}
             {:key :link :kind :value :label "Objekt"}
             {:key :type :kind :type :label "Objekttyp"}
             {:key :name :kind :value :label "Dokumentname"}]}
   {:do :close :label "Overlay schließen" :group :overlay
    :hint (str "Schließt ein benanntes Overlay und leert dessen :visible-Variable,"
               " sofern sie setzbar ist. Eine ABGELEITETE Sichtbarkeit kann nur"
               " :on-close zurücknehmen — indem es setzt, woraus sie sich ableitet.")
    :fields [{:key :overlay :kind :overlay-key :label "Overlay" :required true}]}
   {:do :refresh-data :label "Daten neu laden" :group :data
    :hint (str "Lädt alle Daten der App neu — App Studio's „Refresh data in module“."
               " Variablen und Auswahl bleiben, wie sie sind.")
    :fields []}
   {:do :recompute :label "Variable neu rechnen" :group :data
    :hint (str "Rechnet eine Kennzahl oder einen Funktionsaufruf neu — App Studio's"
               " „Recompute {variable}“. Für Variablen, die mit :recompute nicht"
               " automatisch rechnen.")
    :fields [{:key :var :kind :handle :label "Variable" :required true}]}
   {:do :enable-auto-refresh :label "Auto-Refresh einschalten" :group :data
    :hint (str "Gemeldete Änderungen wirken wieder; was während der Pause kam, wird"
               " sofort nachgeladen.")
    :fields []}
   {:do :disable-auto-refresh :label "Auto-Refresh pausieren" :group :data
    :hint (str "Gemeldete Änderungen wirken in dieser Sitzung nicht mehr, bis"
               " Auto-Refresh wieder eingeschaltet wird.")
    :fields []}])

(def dos
  (mapv :do effects))

(def ^:private by-do (coll/index-by :do effects))

(defn spec
  [d]
  (get by-do d))

(defn fields
  [d]
  (vec (:fields (spec d))))

(defn effect-keys
  [d]
  (into #{:do} (map :key) (fields d)))

(defn blank
  [d]
  (into {:do d} (for [{:keys [key required]} (fields d) :when required] [key nil])))

(defn as-vector
  [event]
  (cond (nil? event)    []
        (map? event)    [event]
        (vector? event) event
        (seq? event)    (vec event)
        :else           [event]))

(defn as-event
  [effs]
  (let [v (vec (remove nil? effs))]
    (case (count v) 0 nil 1 (first v) v)))

(defn value-mode
  [x]
  (or (when (map? x)
        (some (fn [{:keys [id key]}] (when (and key (contains? x key)) id)) value-refs))
      :literal))

(defn value-of
  [x]
  (if-let [k (and (map? x) (some #(when (contains? x %) %) value-ref-keys))]
    (get x k)
    x))

(defn- problem [i d key error]
  (cond-> {:index i :error error} d (assoc :do d) key (assoc :key key)))

(defn- handle-problem
  [i d k v]
  (when-not (and (map? v) (= 1 (count v)) (contains? v :var) (keyword? (:var v)))
    (problem i d k (str (str k) " muss die Variablen-Referenz {:var <name>} sein"
                        " — ein blanker Name wird nie zu einem Handle aufgelöst"
                        " und der Klick tut dann still nichts"))))

(defn- value-problem
  [i d k v]
  (when (and (map? v)
             (not (and (= 1 (count v)) (some value-ref-keys (keys v)))))
    (problem i d k (str "unbekannte Wertform bei " (str k) ": "
                        (str/join ", " (sort (map #(str %) (keys v))))
                        " — erlaubt sind ein fester Wert oder genau eine Referenz "
                        (str/join "/" (sort (map #(str "{" % " …}") value-ref-keys)))))))

(defn- effect-problems
  [i e]
  (let [d (:do e)]
    (cond
      (not (map? e))
      [(problem i nil nil (str "Effekt #" (inc i) " ist keine Map — ein Ereignis ist"
                               " {:do …} oder ein Vektor davon"))]

      (not (contains? by-do d))
      [(problem i d :do (str "unbekanntes :do " (pr-str d) " — erlaubt sind "
                             (str/join ", " (map #(str %) dos))))]

      :else
      (let [allowed (effect-keys d)]
        (vec
         (concat
          (for [k (sort (remove allowed (keys e)))]
            (problem i d k (str "unerlaubter Schlüssel " (str k) " bei :do " (str d)
                                " — " (str d) " nimmt "
                                (str/join ", " (sort (map #(str %) (disj allowed :do)))))))
          (for [{:keys [key required]} (fields d)
                :when (and required (nil? (get e key)))]
            (problem i d key (str (str key) " fehlt — " (str d) " braucht es")))
          (keep identity
                (for [{:keys [key kind]} (fields d)
                      :when (contains? e key)]
                  (case kind
                    :handle      (handle-problem i d key (get e key))
                    :value       (value-problem i d key (get e key))
                    :type        (when-let [t (get e key)]
                                   (when-not (keyword? t)
                                     (problem i d key
                                              (str (str key) " ist der NAME des Objekttyps"
                                                   " — ein Schlüsselwort wie :projekt, kein "
                                                   (if (string? t) "Text" "Wert")))))
                    :app-name    (when-not (keyword? (get e key))
                                   (problem i d key
                                            (str (str key) " ist der NAME der Ziel-Anwendung"
                                                 " — ein Schlüsselwort wie :leitwarte, kein "
                                                 (if (string? (get e key)) "Text" "Wert"))))
                    :template-key (when-not (keyword? (get e key))
                                    (problem i d key
                                             (str (str key) " ist der NAME einer Vorlage"
                                                  " — ein Schlüsselwort wie :angebot, kein "
                                                  (if (string? (get e key)) "Text" "Wert"))))
                    :params      (when-not (map? (get e key))
                                   (problem i d key
                                            (str (str key) " ist eine Map"
                                                 " <Parameter> → <Wert>")))
                    :overlay-key (when-not (keyword? (get e key))
                                   (problem i d key
                                            (str (str key)
                                                 " ist der Schlüssel eines :overlay-Containers"
                                                 " — ein Schlüsselwort, kein "
                                                 (if (string? (get e key)) "Text" "Wert"))))
                    nil)))))))))

(defn explain
  [event]
  (if (and (some? event) (not (map? event)) (not (sequential? event)))
    [(problem 0 nil nil "ein Ereignis ist eine Effekt-Map oder ein Vektor von Effekt-Maps")]
    (vec (mapcat effect-problems (range) (as-vector event)))))

(defn sound? [event] (empty? (explain event)))

(defn message
  [event]
  (str/join "; " (for [{:keys [index error]} (explain event)]
                   (str "Effekt #" (inc index) ": " error))))
