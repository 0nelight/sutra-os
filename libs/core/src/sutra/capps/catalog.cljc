(ns sutra.capps.catalog
  (:require [sutra.kit.coll :as coll]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.kit.tree :as tree]
            [clojure.string :as str]
            [sutra.capps.table :as tbl]
            [sutra.capps.bind :as bind]
            [sutra.capps.walk :as walk]
            [sutra.capps.variables :as vars]
            [sutra.capps.catalog.data :as data]
            [sutra.capps.catalog.display :as display]
            [sutra.capps.catalog.input :as input]
            [sutra.capps.catalog.action :as action]))

(def param-kinds
  {:text       {:label "Text" :binds :any}
   :long-text  {:label "Langtext" :binds :any}
   :number     {:label "Zahl" :binds :any}
   :bool       {:label "Ja/Nein" :binds :any}
   :enum       {:label "Auswahl" :of :options :binds :any}
   :fmt        {:label "Format" :of :format :binds :any}
   :object-set {:label "Objektmenge" :of :ref :binds bind/variable-keys
                :needs "{:source …} oder {:val …}"}
   :value      {:label "Wert" :of :ref :binds :any}
   :handle     {:label "Variable" :of :var :binds #{:var} :needs "eine Variable {:var …}"}
   :var-name   {:label "Variablenname" :of :var :binds :any}
   :prop       {:label "Eigenschaft" :binds :any}
   :action     {:label "Action" :binds :any}
   :object-type {:label "Objekttyp" :binds :any}
   :resource   {:label "Ressource" :binds :any}
   :cols       {:label "Spalten" :many true :binds :any}
   :items      {:label "Einträge" :many true :binds :any}
   :params     {:label "Parameter" :of :map :binds :any}
   :event      {:label "Ereignis" :of :raw :binds :any}
   :edn        {:label "EDN" :of :raw :binds :any}})

(defn param-binds
  [p]
  (or (:binds p) (:binds (get param-kinds (:kind p)))))

(def format-options (vec (sort (keys tbl/formats))))

(def widget-specs
  (into [] cat [data/widgets display/widgets input/widgets action/widgets]))

(def container-specs
  [{:kind :rows :label "Zeilen" :group :struktur
    :children [{:key :items :many true}]
    :params [{:key :title :kind :text} {:key :scroll? :kind :bool}]}
   {:kind :columns :label "Spalten" :group :struktur
    :children [{:key :items :many true}]
    :params [{:key :title :kind :text}
             {:key :detail :kind :handle}]}
   {:kind :flow :label "Fluss" :group :struktur
    :children [{:key :items :many true}]
    :params [{:key :title :kind :text}]}
   {:kind :toolbar :label "Werkzeugleiste" :group :struktur
    :children [{:key :items :many true}]
    :params []}
   {:kind :section :label "Abschnitt" :group :struktur
    :children [{:key :body} {:key :tag}]
    :params [{:key :title :kind :text}
             {:key :icon :kind :text}
             {:key :header-style :kind :enum :options [:block :contained :floating]}
             {:key :bg :kind :enum :options [:1 :2 :3 :4 :5]}
             {:key :padding :kind :edn}
             {:key :collapse :kind :handle :binds #{:var :boolean}}]}
   {:kind :tabs :label "Reiter" :group :struktur
    :children [{:key :tabs :panes true}]
    :params [{:key :select :kind :handle :required true}]}
   {:kind :pages :label "Seiten" :group :struktur
    :children [{:key :pages :panes true}]
    :params [{:key :select :kind :handle :required true}
             {:key :header :kind :edn}]}
   {:kind :profiles :label "Profile" :group :struktur
    :children [{:key :variants :panes true}]
    :params [{:key :select :kind :handle :required true}
             {:key :default :kind :text}]}
   {:kind :loop :label "Schleife" :group :struktur
    :children [{:key :body} {:key :empty} {:key :group-head}]
    :params [{:key :items :kind :object-set :required true}
             {:key :key :kind :prop}
             {:key :selected :kind :handle}
             {:key :arrange :kind :enum :options [:rows :columns :flow :grid] :default :rows}
             {:key :max-columns :kind :number}
             {:key :min-card-width :kind :number}
             {:key :sort :kind :edn}
             {:key :limit :kind :number}
             {:key :paging :kind :enum :options [:limit :paged] :default :limit}
             {:key :per-page :kind :number}
             {:key :padding :kind :edn}
             {:key :group-by :kind :prop}
             {:key :group-label :kind :fmt}]}
   {:kind :overlay :label "Überlagerung" :group :struktur
    :children [{:key :body}]
    :params [{:key :overlay :kind :text :required true}
             {:key :visible :kind :handle}
             {:key :as :kind :enum :options [:modal :drawer] :default :modal}
             {:key :side :kind :enum :options [:left :right]}
             {:key :size :kind :enum :options [:s :m :l]}
             {:key :backdrop? :kind :bool :default true}
             {:key :close-on-backdrop? :kind :bool :default true}
             {:key :header? :kind :bool :default true}
             {:key :icon :kind :text}
             {:key :dock :kind :enum :options [:mittel :breit :weit]}
             {:key :title :kind :text}
             {:key :on-close :kind :event}]}])

(def common-params
  [{:key :when :kind :edn :advanced true}
   {:key :hidden? :kind :bool :advanced true}
   {:key :min-size :kind :enum :options [:mittel :breit :weit] :advanced true}
   {:key :bg :kind :text :advanced true}
   {:key :class :kind :text :advanced true}
   {:key :box :kind :edn :advanced true}
   {:key :mount :kind :enum :options [:default :on-screen :eager] :advanced true}
   {:key :unmount :kind :enum :options [:default :off-screen :never] :advanced true}
   {:key :scenario :kind :handle :advanced true}])

(def root-params
  [{:key :title :kind :text}
   {:key :subtitle :kind :text}
   {:key :fill? :kind :bool}
   {:key :device :kind :enum :options [:desktop :mobile] :default :desktop}
   {:key :padding :kind :edn}
   {:key :header :kind :edn}
   {:key :view-as :kind :edn}])

(def rider-params
  [{:key :vars :kind :edn}
   {:key :title :kind :text}])

(def param-labels
  {:rows "Zeilen (Objektmenge)" :cols "Spalten" :items "Einträge" :cards "Karten"
   :sort "Sortierung" :limit "Höchstzahl" :row-key "Zeilenschlüssel"
   :on-row-click "Klick auf Zeile" :on-click "Klick"
   :url-type "URL-Art" :property "Eigenschaft mit der URL"
   :hide-if-null? "ohne Wert ausblenden" :url "Feste URL"
   :template "URL-Vorlage ({eigenschaft})" :open-in "Öffnen in"
   :wrap? "Text umbrechen" :lines "Zeilen je Eintrag" :frozen-cols "feste Spalten"
   :fit-columns? "Spalten auf die Breite verteilen"
   :active "Aktives Objekt (Ausgang)" :auto-active? "erste Zeile vorwählen"
   :detail "Detailspalte (am Telefon eine eigene Seite)"
   :header-style "Kopfzeile (Block, im Rahmen, schwebend)"
   :device "Gerät (mobil: wie am Telefon, in jeder Breite)"
   :close-on-backdrop? "Klick daneben schließt" :header? "Kopfzeile zeigen"
   :dock "neben der Seite ab Größe" :min-size "sichtbar ab Größe"
   :on-done "nach erfolgreicher Action"
   :output "angelegte Objekte (Variable)"
   :defaults "Parameter-Vorgaben" :options "Auswahlmengen je Parameter"
   :cancel-label "Abbrechen-Beschriftung" :on-cancel "Abbrechen"
   :row "Zeilen-Gruppierung" :col "Spalten-Gruppierung" :value "Wert"
   :corner "Ecke" :totals? "Summenzeile" :padding "Innenabstand"
   :x "X-Achse" :y "Y-Achse" :group "Gruppierung" :chart-kind "Diagrammart"
   :x-bucket "Zeitraum der X-Achse"
   :missing "leere Zeiträume (Lücke/Null/verbinden)"
   :stacked? "gestapelt" :legend? "Legende" :x-title "X-Titel" :y-title "Y-Titel"
   :y-fmt "Y-Format" :series "Reihen (Ebenen)" :axes "Werteachsen"
   :axis "Achse (links/rechts)" :kind "Art" :name "Name"
   :height "Höhe (px)" :layout "Plotly-Layout"
   :lat "Breitengrad" :lon "Längengrad" :map-kind "Kartenart" :zoom "Zoom"
   :color "Farbe (Feld)"
   :annotations "Markierungen" :on-select "Klick auf Markierung"
   :color-by "Farbe je Wert" :caption "Beschriftung oben rechts (Eigenschaft oder Liste)"
   :on-area "Bereich aufgezogen" :area-label "Knopf: Rahmen ziehen"
   :draw "Zeichnen an (von der App gesteuert)" :collect? "Rahmen sammeln"
   :area-kind "Art des gezogenen Rahmens"
   :value-param "Parameter für den Wert"
   :area-page "Seite des Bereichs"
   :area-boxes "Rahmen des Bereichs"
   :search "Suchwort (markiert)" :prefill? "jede Zeile absenden"
   :multi-select? "Mehrfachauswahl" :selected "Ausgewählte Objekte"
   :object "Objekt (Menge, erstes zählt)" :form-factor "Formfaktor"
   :panel-behavior "Panel: ein Objekt, Menge oder je nach Anzahl"
   :hide-header? "Kopfzeile verbergen"
   :hide-actions? "Aktionen der Standardansicht verbergen"
   :rev "neu laden, wenn sich dieser Wert ändert"
   :owner-type "Besitzer-Typ" :children-conn "Link zu den Zeilen" :other-type "Spalten-Typ"
   :only-prominent? "nur hervorgehobene"
   :exclude "ausgenommen" :label-position "Wert neben/unter der Bezeichnung"
   :columns "Spalten (Anzahl)" :hide-null? "leere ausblenden"
   :interface "Variablen-Zuordnung (Modul → Ansicht)"
   :hide-tabs? "Reiter verbergen" :initial-tab "Erster Reiter"
   :initial-tab-on-switch? "bei Objektwechsel zum ersten Reiter"
   :label "Beschriftung" :fmt "Format" :sub "Unterzeile" :accent "Akzent"
   :tone "Färbung" :text "Text" :icon "Symbol" :title "Titel" :hint "Hinweis"
   :sel "Variable" :select "Variable" :all-label "Alle-Beschriftung"
   :steps "Schritte" :current "aktueller Schritt"
   :placeholder "Platzhalter" :type "Art" :step "Schrittweite"
   :min "Minimum" :max "Maximum"
   :item-value "Wert-Feld" :item-label "Anzeige-Feld" :item-search "Such-Felder"
   :item-match "Zuordnungs-Feld"
   :strict? "nur Auswahl"
   :selection-required? "Auswahl erforderlich" :on-change "Bei Auswahl"
   :fields "Filterfelder" :action "Action" :params "Parameter"
   :key-param "Action-Parameter: Schlüssel" :submit-label "Beschriftung Absenden"
   :busy-label "Beschriftung während" :error-var "Fehler-Variable"
   :disabled? "gesperrt" :filename "Dateiname" :mime "MIME-Typ"
   :accept "erlaubte Dateien" :meta "Metadaten"
   :multiple "mehrere Dateien" :routes "Metadaten je Dateiname" :drop-label "Text der Ablagefläche"
   :param "Zielparameter"
   :empty "leer-Zustand" :new-label "Neu-Beschriftung" :allow-drop? "löschbar"
   :allow-apply? "anwendbar" :apply-label "Anwenden-Beschriftung"
   :src "Quelle" :html "HTML-Inhalt" :media-kind "Medienart" :alt "Alternativtext" :allow "allow"
   :pick "Ausgabeform" :shapes "weitere Umrisse" :shape-points "Umriss-Feld"
   :shape-label "Beschriftungs-Feld" :readonly? "nur ansehen"
   :shape-labels? "Beschriftungen zeigen" :shape-tooltips? "Tooltips zeigen"
   :labels? "Beschriftungen zeigen" :tooltips? "Tooltips zeigen"
   :extent "Plan-Ausdehnung" :layers "weitere Ebenen" :points "Umriss-Feld"
   :except "ausgenommene Zeile (Wert)" :except-key "Schlüsselfeld der Ausnahme"
   :paging "Blättern (Höchstzahl oder Seiten)" :per-page "Einträge je Seite"
   :view "Plattform-Ansicht (Schlüssel)"
   :stack "Pläne untereinander" :stack-key "Plan-Schlüssel (Plan und Form)"
   :stack-order "Reihenfolge-Feld" :stack-label "Plan-Beschriftung"
   :stack-w "Plan-Breite-Feld" :stack-h "Plan-Tiefe-Feld"
   :coordinate-system "Koordinatensystem" :unit "Einheit"
   :shape-actions "Form als Objekt anlegen" :role "Rolle auf dem Plan"
   :on-shape-click "beim Antippen eines Umrisses"
   :shape-param "Action-Parameter: Objekt" :points-param "Action-Parameter: Umriss"
   :label-param "Action-Parameter: Beschriftung" :shape-id "Schlüsselfeld des Umrisses"
   :remove-action "Action: Form entfernen"
   :object-param "Action-Parameter: Objekt" :boxes-param "Action-Parameter: Rahmen"
   :row-class "Zeilen-Klasse" :row-style "Zeilen-Stil" :foot "Fußzeile"
   :compare "Szenario-Vergleich (Variable)" :scenario "Szenario (Variable)"
   :agg "Zusammenfassung" :row-label "Zeilen-Beschriftung" :col-label "Spalten-Beschriftung"
   :cell "Zellen-Darstellung"
   :names "Reihen-Namen (Feld)" :colors "Farben" :sort? "nach X sortieren"
   :mode "Plotly-Modus" :fill? "Fläche füllen"
   :lng "Längengrad (Alternative)" :z "Gewicht (Feld)"
   :radius "Streuradius (px)" :style "Kartenstil" :center "Mittelpunkt"
   :multi? "Mehrfachauswahl" :poster "Vorschaubild" :sandbox "sandbox"
   :import "gelesener Plan" :on-import "nach dem SVG-Import"
   :on-close "Umriss geschlossen"
   :app "Anwendung" :inputs "Eingaben" :on-output "Ausgaben"
   :props "Eigenschaften"
   :scroll? "scrollbar" :bg "Hintergrund" :collapse "einklappbar"
   :header "Kopfzeile" :default "Standard"
   :intent "Absicht (Farbe)" :display "Darstellung (Karte/Tag)"
   :max-columns "höchstens Spalten" :min-card-width "Kartenbreite mindestens (px)"
   :tag "Tag im Kopf"
   :key "Schlüssel" :arrange "Anordnung" :group-by "gruppieren nach"
   :group-label "Gruppen-Format" :group-head "Gruppen-Kopf"
   :overlay "Name" :as "Darstellung" :side "Seite"
   :visible "Sichtbarkeit (Variable)"
   :size "Größe" :backdrop? "Hintergrund dahinter"
   :object-type "Objekttyp" :sources "Quellen" :resource "Ressource"
   :override-name "Anzeigename"
   :when "Sichtbar wenn" :hidden? "ausgeblendet" :class "CSS-Klasse" :box "Rahmen"
   :no-value "Text ohne Wert" :narrow-headers? "schmale Kopfzeilen"
   :whole-cell? "ganze Zelle klickbar" :user-sort? "Sortieren durch Benutzer"
   :sort-column "Sortierspalte" :visible-cols "sichtbare Spalten (Variable)"
   :column-config? "Spalten einstellbar" :hide-column-config? "Spalteneinstellung ausblenden"
   :separate-types? "Typen getrennt" :icon-prop "Eigenschaft mit dem Symbol"
   :export-csv? "CSV-Export" :export-excel? "Excel-Export"
   :custom-export-names? "eigene Exportnamen" :export-name "Exportname"
   :context-menu "Kontextmenü" :right-clicked "Rechtsklick-Zeile (Variable)"
   :edit-mode "Bearbeitungsmodus" :edit-label "Beschriftung Bearbeiten"
   :confirm-submit? "Absenden bestätigen" :show-count? "Anzahl zeigen" :grid "Raster"
   :property-style "Darstellung der Eigenschaften" :sort-props "sortierbare Eigenschaften"
   :media "Medien" :reorder "Umordnen" :links "Verknüpfungen" :expand "Aufklapp-Tiefe"
   :explore? "Erkunden erlaubt" :on-explore "Beim Erkunden" :open-view? "Ansicht öffnen"
   :preview? "Vorschau" :single? "einzeln" :show-icon? "Symbol zeigen" :drag? "Ziehen erlaubt"
   :render-empty? "auch leer zeigen" :empty-type "Objekttyp wenn leer" :subtitle "Untertitel"
   :row-groups "Zeilengruppen" :col-interval "Spalten-Intervall" :col-sort "Spalten-Sortierung"
   :aggs "Kennzahlen" :placement "Platzierung" :row-totals? "Zeilensummen"
   :col-totals? "Spaltensummen" :selected-rows "gewählte Zeilen (Variable)"
   :group-rows "Gruppenzeilen" :group-col "Gruppenspalte" :value-fields "Wertfelder"
   :expandable "aufklappbar" :stacked-label "Beschriftung gestapelt" :fill-width? "volle Breite"
   :table-style "Tabellenstil" :color-mode "Farbmodus" :orientation "Ausrichtung"
   :order "Reihenfolge" :gap-tooltip? "Hinweis zur Lücke" :highlight? "hervorheben"
   :status "Eigenschaft mit dem Status" :at "Eigenschaft mit dem Zeitpunkt" :statuses "Status"
   :objects "Objekte" :logs "Protokolle" :selection? "Auswahl erlaubt" :on-selection "Bei Auswahl"
   :show-now? "Jetzt-Linie" :time-zone "Zeitzone" :grid-unit "Rastereinheit"
   :start-month "erster Monat" :start-day "erster Tag" :start-hour "erste Stunde"
   :stacking "Stapelung" :row-ordering "Zeilenreihenfolge" :group-width "Gruppenbreite"
   :start "Beginn" :end "Ende" :spec "Spezifikation" :grammar "Grammatik" :theme "Thema"
   :selections "Auswahlen" :empty-header "Überschrift wenn leer"
   :empty-description "Text wenn leer" :path-saving? "Pfad speichern"
   :copy-notebook? "nach Notebook kopieren" :widget-key "Baustein-Schlüssel"
   :selectable "auswählbar" :event-sets "Ereignismengen" :new-plot-canvas "neue Zeichenflächen"
   :plot-types "Diagrammarten" :event-set-types "Ereignisarten" :overlay-y? "Y-Achsen überlagern"
   :collapse-y? "Y-Achsen zusammenlegen" :boundaries? "Grenzen zeigen" :tooltip? "Hinweistext"
   :digits "Nachkommastellen" :utc? "UTC" :sync-x? "X-Achsen gekoppelt" :view-range "Zeitraum"
   :view-start "Beginn des Zeitraums" :view-end "Ende des Zeitraums"
   :view-relative-ms "relativer Zeitraum (ms)" :saving? "Speichern erlaubt"
   :output-rid "gespeicherte RID (Variable)" :autoload "automatisch laden"
   :keep-on-load? "beim Laden behalten" :canvases "Zeichenflächen" :segments "Abschnitte"
   :ontology-colors? "Farben der Ontologie" :legend-position "Lage der Legende"
   :x-fmt "Format der X-Achse" :sort-by-x "nach X sortieren" :bar-mode "Balkenart"
   :selection-filter "Auswahlfilter (Variable)" :export? "Export" :overlays "Überlagerungen"
   :base-picker? "Kartenwahl" :draw-tools "Zeichenwerkzeuge" :draw-color "Zeichenfarbe"
   :draw-opacity "Deckkraft beim Zeichnen" :shape-output "Form-Ausgabe"
   :drawn "Gezeichnetes (Variable)" :on-drawn "Nach dem Zeichnen" :single-draw? "nur eine Form"
   :clear-after-op? "nach Aktion leeren" :antimeridian? "über die Datumsgrenze"
   :shape-select? "Formen wählbar" :shape-search "Formsuche (Variable)"
   :shape-edit? "Formen bearbeitbar" :measure-perimeter? "Umfang messen"
   :measure-area? "Fläche messen" :measure-lines? "Linien messen"
   :measure-segments? "Teilstrecken messen" :selected-shapes "gewählte Formen (Variable)"
   :timeline? "Zeitleiste" :time-editable? "Zeit änderbar" :live-toggle? "Live-Schalter"
   :timeline-open? "Zeitleiste offen" :selected-time "gewählte Zeit (Variable)"
   :time-start "Zeitbeginn (Variable)" :time-end "Zeitende (Variable)" :time-format "Zeitformat"
   :playing "Wiedergabe läuft (Variable)" :playback-position "Wiedergabeposition (Variable)"
   :auto-pause "automatisch anhalten" :legend-collapsed? "Legende eingeklappt"
   :legend-size "Größe der Legende" :selection-tools? "Auswahlwerkzeuge"
   :selection-panel? "Auswahlleiste" :auto-zoom? "automatisch zoomen" :zoom-to "Zoomen auf"
   :zoom-outside-only? "nur zoomen wenn außerhalb" :bounds "Kartenausschnitt (Variable)"
   :follow "Folgen" :locate? "eigener Standort" :direction "Richtung" :description "Beschreibung"
   :conditional-format "bedingte Formatierung" :secondary "Zweitwert" :sparkline "Kleindiagramm"
   :mode-toggle? "Modus-Schalter" :inline-actions "Aktionen in der Zeile" :prop "Eigenschaft"
   :page-var "Seite (Variable)" :selected-text "gewählter Text (Variable)"
   :downloads? "Herunterladen erlaubt" :create "Anlegen" :resource-kind "Ressourcenart"
   :content "Inhalt" :projects "Projekte" :folders "Ordner" :tags "Schlagwörter" :format "Format"
   :horizontal? "waagrecht" :selected-object-type "gewählter Objekttyp (Variable)"
   :selected-object-set "gewählte Objektmenge (Variable)" :heading "Überschrift"
   :hide-upload? "Hochladen ausblenden" :hide-link? "Verknüpfen ausblenden"
   :media-param "Parameter für das Medium" :name-param "Parameter für den Namen"
   :actions "Aktionen" :box-prop "Eigenschaft mit dem Rahmen"
   :color-prop "Eigenschaft mit der Farbe" :on-create "Beim Anlegen"
   :monospace? "Festbreitenschrift" :breaks? "Zeilenumbrüche übernehmen" :align "Ausrichtung"
   :selection-start "Auswahlbeginn (Variable)" :selection-end "Auswahlende (Variable)"
   :ref-types "Bezugstypen" :selection-behavior "Auswahlverhalten"
   :on-ref-select "Bei Wahl eines Bezugs" :design "Gestaltung" :minimal? "schlicht"
   :width "Breite" :active-color "Farbe der aktiven Stufe" :alignment "Ausrichtung"
   :set "Objektmenge (Variable)" :filters "Filter" :editable? "bearbeitbar"
   :prevent-operator-change? "Operator gesperrt" :show-object-type? "Objekttyp zeigen"
   :available-properties "verfügbare Eigenschaften" :available-links "verfügbare Verknüpfungen"
   :autocomplete? "Vorschläge beim Tippen" :keyword? "Stichwortsuche"
   :show-clear? "Leeren anbieten" :show-help? "Hilfe zeigen" :hide-empty? "Leeres ausblenden"
   :terms "Begriffe" :allow-none? "keine Auswahl erlaubt" :title-prop "Eigenschaft mit dem Titel"
   :sort-by-prop "Sortiereigenschaft" :sort-dir "Sortierrichtung" :multiple? "Mehrfachauswahl"
   :clearable? "leerbar" :creatable? "neue Werte erlaubt" :allow-clear? "Leeren erlaubt"
   :groups "Gruppen" :date-format "Datumsformat" :precision "Genauigkeit"
   :timezone-editable? "Zeitzone wählbar" :default-timezone "Standard-Zeitzone"
   :show-number? "Nummer zeigen" :completed-color "Farbe erledigter Stufen"
   :on-enter "Bei Eingabetaste" :auto-size? "Größe automatisch" :grouping? "Tausendertrennung"
   :reset? "Zurücksetzen anbieten" :prefix "Präfix" :suffix "Suffix" :percent? "Prozent"
   :range? "Zeitspanne" :sel-to "Ende der Spanne (Variable)" :sort-by "sortieren nach"
   :max-files "höchstens Dateien" :left-icon "Symbol links" :right-icon "Symbol rechts"
   :capture "Aufnahme" :destination "Ziel" :destination-var "Ziel (Variable)"
   :media-column "Spalte für das Medium" :branch "Zweig" :rid-output "RID (Variable)"
   :filename-output "Dateiname (Variable)" :file-param "Parameter für die Datei"
   :users-param "Parameter für die Benutzer" :notify? "benachrichtigen"
   :kinds "Änderungsarten" :editors "Bearbeiter"
   :mention-users? "Benutzer erwähnen" :references "Bezüge" :action-log? "Aktionsprotokoll"
   :button-type "Knopfart" :tag? "als Etikett" :large? "groß" :buttons "Knöpfe"
   :layout-switch? "Layout-Schalter" :row-management? "Zeilen verwalten"
   :file-upload? "Datei hochladen" :invalid-state "Ungültiger Zustand"
   :hide-text? "Text ausblenden" :prompt? "Aufforderung zeigen"
   :hide-display? "Anzeige ausblenden" :display-name "Anzeigename" :shared "Geteilte Werte"
   :auto-open "automatisch öffnen" :toasts? "Hinweise zeigen" :pause-toggle? "Pause-Schalter"
   :enrich? "anreichern" :scope "Geltungsbereich" :include-pipeline? "Pipeline einbeziehen"
   :seek "Sprungmarke" :key-frames "Schlüsselbilder" :transcript "Transkript"
   :widget-set "Widget Set" :widget-name "Widget"
   :embedded? "eingebettet" :url-params "URL-Parameter" :input-params "Eingaben"
   :output-params "Ausgaben" :variables "Variablen" :events "Ereignisse" :static "feste Szenarien"
   :dynamic "Szenarien (Variable)" :candidates "Kandidaten" :dropdown? "als Auswahlliste"
   :agent "Agent" :version "Version" :show-reasoning? "Begründung zeigen" :state "Zustand"
   :textbox "Eingabefeld (Variable)" :answer "Antwort (Variable)" :logic "Logik" :prompt "Prompt"
   :prompt-function "Prompt-Funktion" :clear-on-change? "bei Änderung leeren"
   :temperature "Temperatur" :model "Modell" :column "Spalte" :as-rows? "als Zeilen"
   :button-label "Knopftext" :loading-message "Text beim Laden" :spinner? "Ladeanzeige"
   :document "Dokument" :bind "Bindung" :on-event "Bei Ereignis"
   :view-as "Ansicht wählen" :vars "Variablen" :spacing "Abstand"
   :advanced? "erweitert" :aggregate "Aggregation" :aggregation "Aggregation"
   :badge "Etikett" :body "Inhalt" :boxes "Rahmen" :cluster-px "Bündelabstand (px)"
   :collapsed? "eingeklappt" :completed "erledigt" :component "Bauteil"
   :connect-by "verbinden nach" :count "Anzahl" :edit-action "Action zum Bearbeiten"
   :edit-id-prop "Eigenschaft mit der ID" :edit-label-param "Parameter für die Beschriftung"
   :edit-object-param "Parameter für das Objekt" :edit-shape-param "Parameter für die Form"
   :end-fallback "Ende, wenn leer" :end-lat "Breitengrad Ende" :end-lon "Längengrad Ende"
   :event-type "Ereignisart" :export "Export" :family "Familie" :form? "als Formular"
   :function-export "Funktions-Export" :geojson "GeoJSON" :geometries "Geometrien"
   :geometry "Geometrie" :geopoint "Geopunkt" :xy "Planpunkt (x,y)" :hover "beim Überfahren"
   :icon-color "Symbolfarbe" :id "Kennung" :id-prop "Eigenschaft mit der ID"
   :interactions "Interaktionen" :interval "Intervall"
   :label-prop "Eigenschaft für die Beschriftung"
   :line-color "Linienfarbe" :line-width "Linienbreite" :link "Verknüpfung"
   :locked? "gesperrt" :max-label "Beschriftung Maximum" :max-sel "Maximum (Variable)"
   :media-export "Medien-Export" :min-label "Beschriftung Minimum" :min-sel "Minimum (Variable)"
   :mount "Laden" :no-end "ohne Ende" :on-start "beim Start" :opacity "Deckkraft"
   :order-by "sortieren nach" :page "Seite" :prominent? "hervorgehoben" :properties "Eigenschaften"
   :range-prop "Eigenschaft mit dem Bereich" :shape "Form" :sheet-prop "Eigenschaft mit dem Blatt"
   :show-labels? "Beschriftungen zeigen" :target-prop "Eigenschaft mit dem Ziel"
   :thumbnail "Vorschaubild" :thumbnail-position "Position des Vorschaubilds"
   :time-prop "Eigenschaft mit der Zeit" :time-series "Zeitreihe" :title-mode "Titelart"
   :toggle-label "Beschriftung des Schalters" :tooltip "Kurzinfo" :unmount "Entladen"
   :values "Werte" :var "Variable" :visible? "sichtbar" :when-false "wenn nicht erfüllt"})

(defn item-moved
  [rows i dir]
  (let [j (+ i dir)]
    (if (< -1 j (count rows))
      (assoc rows i (nth rows j) j (nth rows i))
      rows)))

(defn item-duplicated
  [rows i]
  (let [[before after] (split-at (inc i) rows)]
    (vec (concat before [(nth rows i)] after))))

(defn param-label [k]
  (or (get param-labels k)
      (str/replace (name k) "-" " ")))

(def group-labels
  {:struktur "Struktur" :daten "Daten" :kennzahl "Anzeige"
   :eingabe "Eingabe" :aktion "Aktion" :medien "Medien"})

(def widget-index (coll/index-by :widget widget-specs))
(def container-index (coll/index-by :kind container-specs))

(defn widget-spec [w] (get widget-index (->kw w)))

(defn control? [w] (not (contains? (widget-spec w) :loading)))
(defn container-spec [k] (get container-index (->kw k)))

(defn node-type [node]
  (cond
    (not (map? node))                          :unknown
    (contains? node :widget)                   :widget
    (container-spec (:kind node))              :container
    (contains? node :kind)                     :unknown
    :else                                      :unknown))

(defn node-spec [node]
  (case (node-type node)
    :widget    (widget-spec (:widget node))
    :container (container-spec (:kind node))
    nil))

(defn node-params [node]
  (let [own (vec (:params (node-spec node)))
        had (set (map :key own))]
    (into own (remove #(contains? had (:key %))) common-params)))

(defn row-unknown?
  [node k]
  (= :unknown (:row (some #(when (= k (:key %)) %) (node-params node)))))

(defn node-label [node]
  (let [spec (node-spec node)]
    (cond
      (nil? node)  "—"
      (nil? spec)  (str "? " (pr-str (or (:widget node) (:kind node))))
      :else        (let [t (or (:title node) (:label node)
                               (when (= :widget (node-type node))
                                 (some-> (:rows node) :from name)))]
                     (cond-> (:label spec) t (str " · " (str t)))))))

(defn child-slots [node] (vec (:children (node-spec node))))

(defn slot-children
  [node {:keys [key many panes]}]
  (let [v (get node key)]
    (cond
      panes (vec (map-indexed (fn [i pane] {:path [key i :body] :node (:body pane)
                                            :slot key :index i :pane pane
                                            :remove-path [key i]})
                              v))
      many  (vec (map-indexed (fn [i child] {:path [key i] :node child
                                             :slot key :index i
                                             :remove-path [key i]})
                              v))
      (some? v) [{:path [key] :node v :slot key :remove-path [key]}]
      :else     [])))

(defn children-of [node]
  (vec (mapcat #(slot-children node %) (child-slots node))))

(defn node-at [tree path] (if (seq path) (get-in tree path) tree))

(defn update-node [tree path f]
  (if (seq path) (update-in tree (vec path) f) (f tree)))

(defn set-node [tree path node]
  (if (seq path) (assoc-in tree (vec path) node) node))

(defn- at-parent
  [tree path f]
  (let [path (vec path)
        lst  (peek path)]
    (update-node tree (subvec path 0 (dec (count path))) #(f % lst))))

(defn remove-at [tree path]
  (if (empty? path)
    nil
    (at-parent tree path
               (fn [xs lst]
                 (if (integer? lst)
                   (let [xs (vec xs)]
                     (vec (concat (subvec xs 0 lst) (subvec xs (inc lst)))))
                   (dissoc xs lst))))))

(defn move-at [tree path dir]
  (if-not (integer? (peek (vec path)))
    tree
    (at-parent tree path
               (fn [xs lst]
                 (let [xs (vec xs) j (+ lst dir)]
                   (if (<= 0 j (dec (count xs)))
                     (assoc xs lst (nth xs j) j (nth xs lst))
                     xs))))))

(defn pane
  [existing node]
  (let [used (into #{} (map :id) existing)]
    {:id    (first (remove used (map #(keyword (str "pane-" %)) (range))))
     :label (str "Reiter " (inc (count existing)))
     :body  node}))

(defn add-child [tree parent-path slot node]
  (let [spec (some #(when (= slot (:key %)) %) (child-slots (node-at tree parent-path)))]
    (cond
      (nil? spec)    tree
      (:panes spec)  (update-node tree (conj (vec parent-path) slot)
                                  (fn [xs] (conj (vec xs) (pane (vec xs) node))))
      (:many spec)   (update-node tree (conj (vec parent-path) slot)
                                  (fn [xs] (conj (vec xs) node)))
      :else          (set-node tree (conj (vec parent-path) slot) node))))

(defn flatten-tree
  ([tree] (flatten-tree tree [] 0 nil))
  ([node path depth slot]
   (let [entry {:path (vec path) :depth depth :node node :slot slot
                :label (node-label node) :type (node-type node)}]
     (into [entry]
           (mapcat (fn [c]
                     (flatten-tree (:node c)
                                   (into (vec path) (:path c))
                                   (inc depth)
                                   (or (some-> (:pane c) :label str) (name (:slot c))))))
           (children-of node)))))

(defn- collect [xf tree] (vec (sort (into #{} xf (tree/nodes tree)))))

(defn declared-vars [tree]
  (collect (comp (map :vars) (filter map?) (mapcat keys)) tree))

(defn referenced-vars
  [tree]
  (let [declared (set (declared-vars tree))
        ref-keys [:val :var :from :of :source :index]
        as-name  (fn [v] (when (or (keyword? v) (string? v)) (->kw v)))]
    (into #{}
          (comp (keep as-name) (filter declared))
          (walk/visit (fn [x _] (map #(get x %) ref-keys)) tree {:references? true}))))

(defn name-kw
  [x]
  (cond (keyword? x)                            (keyword (name x))
        (symbol? x)                             (keyword (name x))
        (and (string? x) (not (str/blank? x)))  (keyword x)))

(defn referenced-types
  [tree]
  (collect (comp (mapcat (juxt :source :linked)) (keep name-kw)) tree))

(def ^:private scalars
  #{:string :keyword :number :boolean :date :timestamp})

(defn- stateful-spec?
  [spec]
  (let [kind (when (map? spec) (vars/definition-kind spec))]
    (or (nil? kind) (= :static kind))))

(defn- read-type
  [f form nm entry _scope]
  {:value (case (:names f)
            :variable entry
            :spec     form
            nm)})

(defn type-scope
  ([visible] (type-scope visible #{}))
  ([visible fills]
   (cond-> {:vars      visible
            :types     true
            :linked    true
            :specs     true
            :stateful? stateful-spec?
            :read      read-type}
     (contains? fills :item) (bind/with-fill :item ::row)
     (contains? fills :arg)  (bind/with-fill :arg ::payload))))

(defn- computed-handle-problem
  [node visible path]
  (when (= :custom-widget (->kw (:widget node)))
    (first
     (for [[param value] (:variables node)
           :let [p (when (= :var (bind/form-id value))
                     (:problem (bind/resolve value (type-scope visible))))]
           :when (= :no-state (:rule p))]
       {:path (conj (vec path) :variables param)
        :ref  value
        :error (str (name param) ": " (bind/message p))}))))

(defn- binding-problem
  [node visible path]
  (when-let [spec (widget-spec (:widget node))]
    (or
     (computed-handle-problem node visible path)
     (first
      (for [p (:params spec)
            :when (= :object-set (:kind p))
            :let [value (get node (:key p))
                  nm   (when (map? value) (->kw (or (:val value) (:var value))))
                  vs   (when nm (get visible nm))
                  t    (when vs (vars/var-type vs))]
            :when (and t (contains? scalars t))]
        {:path (vec (conj (vec path) (:key p)))
         :ref  value
         :error (str (name (:key p)) " braucht eine Objektmenge, aber "
                     (pr-str nm) " haelt " (pr-str t))})))))

(defn reference-problems
  ([layout] (reference-problems layout {}))
  ([layout outer]
   (walk/visit
    (fn [x {:keys [path scope decl?]}]
      (if (walk/leaf-ref? x)
        (let [p (when (#{:val :var} (bind/form-id x))
                  (:problem (bind/resolve x (type-scope scope))))]
          (when (#{:undeclared :no-name} (:rule p))
            [{:path path :ref x :error (bind/message p)}]))
        (when-not decl? (some-> (binding-problem x scope path) vector))))
    layout
    {:scope (walk/with-vars {} {:vars outer}) :references? true})))

(defn referenced-properties
  [tree]
  (into #{}
        (comp (remove coll?)
              (keep (fn [x]
                      (cond
                        (keyword? x)                 x
                        (and (string? x)
                             (not (str/blank? x)))   (keyword x)))))
        (tree-seq coll? seq tree)))

(defn referenced-object-types
  [tree]
  (into #{}
        (comp (mapcat (fn [m] [(:from m) (:source m) (:linked m)]))
              (keep (fn [x] (cond (keyword? x) x
                                  (and (string? x) (not (str/blank? x))) (keyword x)))))
        (tree/nodes tree)))

(defn widget-action-entries
  [node]
  (when (contains? node :widget)
    (filter #(some? (:action %))
            (concat [node] (:buttons node) (:actions node) (:shape-actions node)
                    (mapcat (fn [l] (cons {:action (:edit-action l)} (:actions l)))
                            (:layers node))))))

(defn used-actions
  [tree]
  (into #{}
        (comp (mapcat widget-action-entries)
              (keep (fn [m] (let [a (:action m)]
                              (cond (keyword? a) a
                                    (and (string? a) (not (str/blank? a))) (keyword a))))))
        (tree/nodes tree)))

(defn bind-dependencies
  [file tree known-types]
  (let [known  (set (map ->kw known-types))
        have-t (set (map ->kw (:object-types file)))
        want-t (sort (remove have-t (filter known (referenced-object-types tree))))
        have-a (set (keep #(some-> (:action %) ->kw) (:actions file)))
        want-a (sort (remove have-a (used-actions tree)))]
    (cond-> file
      (seq want-t) (assoc :object-types (into (vec (:object-types file)) want-t))
      (seq want-a) (assoc :actions (into (vec (:actions file))
                                         (map (fn [a] {:action a}))
                                         want-a)))))

(defn rename-object-type
  [tree old new]
  (let [old   (->kw old)
        vars  (into #{} (comp (filter (comp map? :vars)) (mapcat (comp keys :vars)) (map ->kw))
                    (tree/nodes tree))
        swap  (fn [x] (if (and (or (keyword? x) (string? x)) (= old (->kw x)))
                        (if (string? x) (name new) (->kw new))
                        x))
        one   (cond-> [:object-type :object-type-ref :source] (not (vars old)) (conj :from))
        lists [:object-types :aggregate-types]
        node  (fn [m]
                (as-> m m
                  (reduce (fn [m k] (cond-> m (contains? m k) (update k swap))) m one)
                  (reduce (fn [m k]
                            (let [v (get m k)]
                              (cond (set? v)        (assoc m k (into #{} (map swap) v))
                                    (sequential? v) (assoc m k (mapv swap v))
                                    :else           m)))
                          m lists)))]
    (walk/postwalk #(if (map? %) (node %) %) tree)))

(defn- blank-value? [v]
  (or (nil? v) (and (coll? v) (empty? v)) (and (string? v) (str/blank? v))))

(def ^:private literal-binds
  {:literal any? :boolean boolean?})

(defn- binds?
  [binds v]
  (let [id (bind/form-id v)]
    (or (contains? binds id)
        (and (= :literal id) (boolean (some #(some-> (literal-binds %) (apply [v])) binds))))))

(defn binding-kind-problem
  [v {:keys [key] :as p}]
  (let [binds (param-binds p)]
    (when (and (set? binds) (not (binds? binds v)))
      (str (name key) " braucht "
           (or (:needs p) (:needs (get param-kinds (:kind p)))
               (str/join " oder " (map pr-str (sort binds))))
           (when (and (contains? binds :var) (bind/reference? v)) (str ", nicht " (pr-str v)))))))

(def ^:private param-rules
  {:handle     (fn [v _ var-names]
                 (when (and (= :var (bind/form-id v)) (seq var-names)
                            (not (contains? var-names (->kw (:var v)))))
                   (str "Variable " (pr-str (:var v)) " ist nicht deklariert")))
   :fmt        (fn [v _ _]
                 (when-not (contains? tbl/formats (->kw v))
                   (str "unbekanntes Format " (pr-str v))))
   :enum       (fn [v {:keys [key options]} _]
                 (when (and options (not (contains? (set options) (->kw v))))
                   (str (name key) ": " (pr-str v) " ist keine Option")))})

(defn- excused?
  [node unless]
  (cond
    (nil? unless)        false
    (vector? unless)     (boolean (some #(excused? node %) unless))
    (map? unless)        (boolean (some (fn [[k allowed]] (allowed (->kw (get node k)))) unless))
    :else                (not (blank-value? (get node unless)))))

(defn explain-node
  ([node] (explain-node node (set (declared-vars node))))
  ([node var-names]
   (let [spec (node-spec node)]
     (cond
       (not (map? node))
       {:error "kein Baustein"}

       (nil? spec)
       {:error (str "unbekannt: " (pr-str (or (:widget node) (:kind node))))}

       :else
       (or
        (some (fn [{:keys [key kind required unless] :as p}]
                (let [v (get node key)]
                  (cond
                    (and required (blank-value? v) (not (excused? node unless)))
                    {:error (str (name key) " fehlt") :param key}

                    (blank-value? v) nil

                    :else
                    (when-let [m (or (binding-kind-problem v p)
                                     (when-let [f (get param-rules kind)] (f v p var-names)))]
                      {:error m :param key}))))
              (:params spec))
        (when (and (= :container (node-type node))
                   (some :many (child-slots node))
                   (empty? (children-of node)))
          {:error "keine Bausteine enthalten"}))))))

(defn binding-kind-problems
  [layout]
  (walk/visit (fn [node {:keys [path decl?]}]
                (when-not decl?
                  (for [p (node-params node)
                        :let [v (get node (:key p))]
                        :when (not (blank-value? v))
                        :let [m (binding-kind-problem v p)]
                        :when m]
                    {:path (conj path (:key p)) :param (:key p) :error m})))
              layout))

(defn explain-tree [tree]
  (let [vs (set (declared-vars tree))]
    (vec (keep (fn [{:keys [path node label]}]
                 (when-let [p (explain-node node vs)]
                   (assoc p :path path :label label)))
               (flatten-tree tree)))))

(defn blank-node [k]
  (let [k (->kw k)]
    (if-let [spec (container-spec k)]
      (reduce (fn [n {:keys [key many panes]}]
                (assoc n key (if (or many panes) [] nil)))
              {:kind k}
              (:children spec))
      (reduce (fn [n {:keys [key default]}] (cond-> n default (assoc key default)))
              {:widget k}
              (:params (widget-spec k))))))

(def scalar-types
  [:string :number :keyword :boolean :date :array :object-set-filter])

(def agg-fns
  [:count :distinct-count :sum :avg :min :max])

(def pred-ops
  [{:op :=        :label "ist"           :operand :value}
   {:op :not=     :label "ist nicht"     :operand :value}
   {:op :<        :label "kleiner als"   :operand :value}
   {:op :<=       :label "höchstens"     :operand :value}
   {:op :>        :label "größer als"    :operand :value}
   {:op :>=       :label "mindestens"    :operand :value}
   {:op :in       :label "ist eines von" :operand :values}
   {:op :contains :label "enthält"       :operand :value}
   {:op :truthy   :label "ist gesetzt"   :operand :none}
   {:op :falsy    :label "ist leer"      :operand :none}
   {:op :filter   :label "Filter anwenden" :operand :value}])

(def pred-op-index (coll/index-by :op pred-ops))

(defn pred-operand
  [op]
  (:operand (get pred-op-index (->kw op)) :value))

(def ^:private recompute-field
  {:key :recompute :kind :enum :label "Neu rechnen" :options :recompute-modes
   :advanced true
   :hint "Ohne Angabe automatisch: bei geänderten Eingaben und bei jedem Neuladen."})

(def var-kinds
  [{:id :state :label "Zustand (Eingabe)" :runtime :state
    :hint "Ein Wert, den ein Bedienelement schreibt."
    :blank {:default nil}
    :fields [{:key :default     :kind :edn      :label "Standard"}
             {:key :type        :kind :enum     :label "Werttyp" :options :scalar-types
              :free true}
             {:key :external-id :kind :var-name :label "Schnittstelle"
              :hint "Leer = privat: nicht einbettbar, nicht adressierbar."
              :clears [:in-url :as]}
             {:key :in-url      :kind :enum     :label "in der URL"
              :options :url-behaviors :needs :external-id}
             {:key :as          :kind :enum     :label "als"
              :options :url-coercions :needs :external-id}
             {:key :default-agg :kind :edn      :label "Vorgabe aus Aggregation"
              :hint "Ein Startwert, den der Server rechnet — etwa das jüngste Jahr."}
             {:key :input?      :kind :bool     :label "Eingabe" :advanced true}
             {:key :push?       :kind :bool     :label "zurückschreiben" :advanced true}
             {:key :coalesce    :kind :edn      :label "ersatzweise" :advanced true}]}

   {:id :source :label "Objektmenge (roh)" :runtime :derived
    :hint "Alle Objekte eines Typs aus der Nutzlast der App."
    :blank {:source nil}
    :fields [{:key :source :kind :type :label "Objekttyp"}
             {:key :where  :kind :where :label "wobei"}
             {:key :join   :kind :edn   :label "angehängt" :advanced true
              :hint "Zeilen einer anderen Menge dazuschlagen (:index/:key/:on/:as/:absent)."}]}

   {:id :filter :label "Objektmenge (gefiltert)" :runtime :derived
    :hint "Eine andere Menge, eingeschränkt durch Bedingungen."
    :blank {:from nil :where []}
    :fields [{:key :from  :kind :set-or-type :label "aus"}
             {:key :where :kind :where       :label "wobei"}
             {:key :join  :kind :edn         :label "angehängt" :advanced true}]}

   {:id :pick :label "Feld eines Objekts" :runtime :derived
    :hint "Ein Feld des ersten Objekts einer Menge — etwa die id der Auswahl."
    :blank {:object-property nil :of nil}
    :fields [{:key :object-property :kind :prop        :label "Feld"}
             {:key :of              :kind :set-or-type :label "aus"}
             {:key :type            :kind :enum :label "Werttyp" :options :scalar-types
              :free true}
             {:key :coalesce        :kind :edn :label "ersatzweise" :advanced true}]}

   {:id :agg :label "Kennzahl (im Browser gerechnet)" :runtime :derived
    :hint "Summe, Anzahl und dergleichen über eine Objektmenge."
    :blank {:agg [:sum nil] :of nil}
    :fields [{:key :agg   :kind :agg         :label "rechnet" :options :agg-fns}
             {:key :of    :kind :set-or-type :label "über"}
             {:key :where :kind :where       :label "wobei" :advanced true
              :hint "Erst einschränken, dann rechnen."}
             {:key :type  :kind :enum :label "Werttyp" :options :scalar-types :free true
              :advanced true}]}

   {:id :expr :label "Formel" :runtime :derived
    :hint "Arithmetik über andere Variablen."
    :blank {:expr [:+ 0]}
    :fields [{:key :expr :kind :edn :label "Formel"}
             {:key :type :kind :enum :label "Werttyp" :options :scalar-types :free true
              :advanced true}]}

   {:id :aggregate :label "Kennzahl (vom Server gerechnet)" :runtime :aggregate
    :hint "Eine Aggregation über den ganzen Bestand, nicht über die Nutzlast."
    :blank {:aggregate {:find nil :aggs []}}
    :fields [{:key :aggregate   :kind :edn :label "Aggregation"
              :hint ":find, :aggs [{:attr :fn :name}], :group-by, :where"}
             {:key :default-agg :kind :edn :label "Vorgabe" :advanced true}
             recompute-field
             {:key :scenario    :kind :edn :label "Szenario" :advanced true}]}

   {:id :function :label "Funktionsaufruf" :runtime :function
    :hint "Der Rückgabewert einer Ontologie-Funktion."
    :blank {:function nil :args {}}
    :fields [{:key :function :kind :edn :label "Funktion"}
             {:key :args     :kind :edn :label "Argumente"}
             recompute-field
             {:key :scenario :kind :edn :label "Szenario" :advanced true}]}

   {:id :compare :label "Vergleich zweier Szenarien" :runtime :compare
    :hint "Derselbe Wert in Basis und Fork, in einer Reihe."
    :blank {:compare nil}
    :fields [{:key :compare  :kind :edn :label "Vergleich"}
             {:key :scenario :kind :edn :label "Szenario" :advanced true}]}])

(def var-kind-index (coll/index-by :id var-kinds))

(defn var-kind
  [spec]
  (let [has? #(contains? spec %)]
    (cond
      (has? :aggregate)       :aggregate
      (has? :function)        :function
      (has? :compare)         :compare
      (has? :object-property) :pick
      (has? :agg)             :agg
      (has? :expr)            :expr
      (has? :source)          :source
      (has? :from)            :filter
      :else                   :state)))

(defn var-spec [spec] (get var-kind-index (var-kind spec)))

(defn blank-var [kind] (:blank (get var-kind-index (->kw kind)) {:default nil}))

(def var-field-options
  {:scalar-types   (mapv (juxt identity name) scalar-types)
   :agg-fns        (mapv (juxt identity name) agg-fns)
   :recompute-modes (mapv (juxt :id :label) vars/recompute-modes)
   :url-behaviors  (mapv (juxt :id :label) vars/url-behaviors)
   :url-coercions  (mapv (juxt identity name) (sort vars/url-coercions))})

(defn var-fields
  [spec]
  (let [fs    (:fields (var-spec spec))
        known (into #{} (map :key) fs)]
    (into (vec fs)
          (for [k (sort (keys spec)) :when (not (known k))]
            {:key k :kind :edn :label (name k) :unknown true}))))

(def palette-entries
  (into (vec (for [{:keys [kind label group]} container-specs]
               {:id kind :label label :group group :type :container}))
        (for [{:keys [widget label group]} widget-specs]
          {:id widget :label label :group group :type :widget})))

(defn palette
  ([] (palette nil))
  ([query]
   (let [q (some-> query str str/trim str/lower-case not-empty)
         hit? (fn [{:keys [id label group]}]
                (or (nil? q)
                    (str/includes? (str/lower-case (name id)) q)
                    (str/includes? (str/lower-case label) q)
                    (str/includes? (str/lower-case (name group)) q)))]
     (->> palette-entries
          (filter hit?)
          (group-by :group)
          (sort-by (fn [[g _]] (if (= :struktur g) "" (name g))))
          (mapv (fn [[g items]] {:group g :label (get group-labels g (name g))
                                 :items (vec items)}))))))

(def palette-common
  {:label "Häufig"
   :ids   [:object-table :markdown :kpi :button :action-button :chart
           :text-input :section :columns :rows]})

(def core-param-keys
  #{:rows :items :title :label :text :action :cols :x :y :value :sel :select
    :src :app :object :overlay :fields :steps :chart-kind :on-click :on-row-click})

(def path-key :sutra.capps.catalog/path)

(defn instrument
  ([tree] (instrument tree []))
  ([node path]
   (if-not (map? node)
     node
     (let [node (cond-> node
                  (or (contains? node :widget) (contains? node :kind))
                  (assoc path-key (vec path)))
           node (reduce (fn [n {:keys [key many panes]}]
                          (cond-> n (or many panes) (update key vec)))
                        node (child-slots node))]
       (reduce (fn [n c]
                 (let [p (:path c)]
                   (assoc-in n p (instrument (:node c) (into (vec path) p)))))
               node
               (children-of node))))))

(defn var-slug
  [words taken]
  (let [base (-> (str/lower-case (str words))
                 (str/replace #"ä" "ae") (str/replace #"ö" "oe")
                 (str/replace #"ü" "ue") (str/replace #"ß" "ss")
                 (str/replace #"[^a-z0-9]+" "-")
                 (str/replace #"^-+|-+$" ""))
        base (if (str/blank? base) "variable" base)
        taken (set (map ->kw taken))]
    (first (remove taken (cons (keyword base)
                               (map #(keyword (str base "-" %)) (iterate inc 2)))))))

(def active-words "Objekt-Tabelle aktives Objekt")

(defn list-detail
  [{:keys [type action action-label param]}]
  (let [t        (->kw type)
        filtered (keyword (str (name t) "-gefiltert"))
        active   (var-slug active-words [:suche filtered])
        detail   (cond-> []
                   action (conj (cond-> {:widget :action-button
                                         :label (or action-label (name action))
                                         :action (->kw action)}
                                  param (assoc :params {(->kw param) {:val active}})))
                   true   (conj {:widget :object-view :object {:val active}}))]
    {:kind :rows
     :vars {:suche {:default ""}
            filtered {:from t :where [[:* :contains {:var :suche}]]}
            active   {:default nil}}
     :items [{:kind :toolbar
              :items [{:widget :text-input :sel {:var :suche} :placeholder "Suchen…"}]}
             {:kind :columns :detail {:var active}
              :items [{:widget :object-table :rows {:val filtered} :active {:var active}}
                      {:kind :rows :items detail}]}]}))

(def ^:private list-detail-template
  {:id :liste-detail :label "Liste → Detail (Objekt zuerst)" :needs-type? true
   :tree (list-detail {:type :objekt})})

(defn- section
  ([title] (section title nil))
  ([title opts]
   (merge {:kind :section :title title :body {:kind :rows :items []}} opts)))

(def ^:private framed {:box {:border :bordered}})

(def ^:private apphost-templates
  [{:id :details :label "Details (App Studio)"
    :tree {:kind :columns :title "Neue App" :vars {}
           :items [(section "Details" {:box {:w 320} :bg :2})
                   {:kind :rows
                    :items [(section "Übersicht" framed)
                            {:kind :columns
                             :items [(section "Abschnitt" framed)
                                     (section "Abschnitt" framed)]}]}]}}
   {:id :grid :label "Raster (App Studio)"
    :tree {:kind :rows :title "Neue App" :vars {} :padding :compact
           :items [{:widget :markdown :text "## Titel\nUntertitel"}
                   {:kind :columns
                    :items [(section "Abschnitt" framed) (section "Abschnitt" framed)]}
                   {:kind :columns
                    :items [(section "Abschnitt" framed) (section "Abschnitt" framed)]}]}}
   {:id :inbox :label "Posteingang (App Studio)"
    :tree {:kind :columns :title "Neue App" :vars {}
           :items [(section "Liste" {:box {:w 360} :bg :2})
                   {:kind :rows :padding :large
                    :items [(section "Abschnitt" (merge framed {:header-style :floating}))
                            (section "Hauptinhalt" {:box {:border :bordered :flex 1}})]}]}}
   {:id :overview :label "Überblick (App Studio)"
    :tree {:kind :columns :title "Neue App" :vars {}
           :items [{:kind :rows :padding :regular
                    :items [{:widget :markdown :text "## Titel"}
                            (section "Abschnitt" framed)
                            (section "Abschnitt" framed)]}
                   (section "Seitenleiste" {:box {:w 320} :bg :3})]}}
   {:id :settings :label "Einstellungen (App Studio)"
    :tree {:kind :columns :title "Neue App" :vars {}
           :items [(section "Bereiche" {:box {:w 240} :bg :2})
                   {:kind :rows :padding :regular
                    :items [(section "Einstellungen" framed)
                            (section "Einstellungen" framed)]}]}}])

(def ^:private sutra-templates
  [list-detail-template
   {:id :leer :label "Leere Seite"
    :tree {:kind :rows :title "Neue App" :vars {} :items []}}
   {:id :liste :label "Liste mit Filter"
    :tree {:kind :rows :title "Neue App"
           :vars {:suche {:default ""}}
           :items [{:kind :toolbar
                    :items [{:widget :text-input :sel {:var :suche}
                             :placeholder "Suchen…"}]}
                   {:widget :object-table :rows nil :cols [{:prop nil}]}]}}
   {:id :dashboard :label "Kennzahlen-Dashboard"
    :tree {:kind :rows :title "Neue App" :fill? true :vars {}
           :items [{:widget :kpi-row :items [{:label "Kennzahl" :value nil :fmt :auto}]}
                   {:kind :columns
                    :items [{:widget :chart :rows nil :x nil :y nil :chart-kind :bar}
                            {:widget :object-table :rows nil :cols [{:prop nil}]}]}]}}
   {:id :reiter :label "Seite mit Reitern"
    :tree {:kind :rows :title "Neue App"
           :vars {:reiter {:type :keyword :default :eins
                           :external-id :reiter :in-url :when-visible}}
           :items [{:kind :tabs :select {:var :reiter}
                    :tabs [{:id :eins :label "Übersicht"
                            :body {:kind :rows :items []}}
                           {:id :zwei :label "Details"
                            :body {:kind :rows :items []}}]}]}}])

(def templates (into sutra-templates apphost-templates))

(defn template
  ([id] (template id nil))
  ([id opts]
   (let [id (->kw id)]
     (if (and (= id :liste-detail) (:type opts))
       (list-detail opts)
       (some #(when (= id (:id %)) (:tree %)) templates)))))

(def scope-paths
  {:bound-type [[:object-type] [:layout :object-types]]
   :type       [[:object-type] [:layout :object-types] [:layout :aggregate-types]]
   :link       [[:layout :link-types]]
   :function   [[:layout :functions]]})

(defn scope-tokens
  [app axis]
  (into []
        (comp (mapcat (fn [path]
                        (let [v (get-in app path)]
                          (if (sequential? v) v [v]))))
              (remove nil?))
        (get scope-paths axis)))

(defn app-references
  [app]
  (let [ns  (tree/nodes (:layout app))
        kws (fn [xs] (into #{} (keep name-kw) xs))]
    {:actions      (kws (map :action ns))
     :functions    (kws (concat (scope-tokens app :function) (map :function ns)))
     :link-types   (kws (concat (scope-tokens app :link)
                                (mapcat (juxt :link-type :link :over) ns)))
     :applications (kws (map :app ns))}))

(defn- view-row
  [app]
  (when (:form-factor app)
    (fn [ctx _ _]
      (cond-> ctx
        (and (= [:layout] (pop (:path ctx))) (not= :vars (peek (:path ctx))))
        (update :fills conj :item)))))

(defn function-vars
  [app]
  (walk/visit (fn [node {:keys [path fills]}]
                (for [[k spec] (when (map? (:vars node)) (:vars node))
                      :when (and (map? spec) (:function spec))]
                  {:var (->kw k) :function (->kw (:function spec))
                   :args (:args spec) :path (conj path :vars (->kw k)) :fills fills}))
              app
              {:down (view-row app)}))

(defn unresolved-arg-forms
  [v fills]
  (cond
    (sequential? v)       (into [] (mapcat #(unresolved-arg-forms % fills)) v)
    (bind/reference? v)   (let [p (:problem (bind/resolve v (vars/call-scope
                                                             (type-scope true fills))))]
                            (if (= :not-filled (:rule p)) [v] []))
    (map? v)              [v]
    :else                 []))
