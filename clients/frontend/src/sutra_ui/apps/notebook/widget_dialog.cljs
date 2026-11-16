(ns sutra-ui.apps.notebook.widget-dialog
  (:require [sutra-ui.ui.overlay :as overlay]
            [clojure.string :as str]
            [sutra-ui.ui.core :as ui]
            [sutra.capps.table :as tbl]
            [sutra-ui.object.card :as oc]
            [sutra-ui.ui.busy :as busy]
            [sutra-ui.ui.display :as display]
            [sutra-ui.editor.rich-text.editor :as rich]
            [sutra-ui.routing :as routing]
            [sutra-ui.feature :as feature]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.apps.notebook.state :refer
             [chain! docs editor graphs notebook pick-rows sel-widget send! wdialog]]
            [sutra-ui.apps.notebook.ui :refer [action-btn btn field-label]]))

(defn open-widget-dialog! [kind attrs pos] (send! [:notebook.widget/open kind attrs pos]))

(defn- attr! [k v] (send! [:notebook.widget/attrs {k v}]))

(defn resource-label [path]
  (if-let [[_ id] (re-matches #"/notebook/([^/?#]+).*" path)]
    (or (some #(when (= (str (:id %)) id) (:name %)) @docs) path)
    (if-let [[_ t id] (re-matches #"/object/([^/]+)/([^/?#]+).*" path)]
      (str (oc/type-label (keyword t)) " " (js/decodeURIComponent id))
      (if-let [[_ id] (re-find #"^/(?:branch/[^/]+/)?view/([^/?#]+)/" path)]
        (or (:name (get @catalog/views (js/decodeURIComponent id))) path)
        path))))

(def date-formats
  [["date" "Datum"] ["datetime" "Datum und Uhrzeit"] ["datetime-short" "Datum, Uhrzeit (kurz)"]
   ["time" "Uhrzeit"] ["iso" "ISO-Zeitpunkt"]])

(defn format-now [fmt]
  (let [d (js/Date.)]
    (case fmt
      "datetime"       (.toLocaleString d "de-AT" #js {:dateStyle "medium" :timeStyle "medium"})
      "datetime-short" (.toLocaleString d "de-AT" #js {:dateStyle "short" :timeStyle "short"})
      "time"           (.toLocaleTimeString d "de-AT" #js {:timeStyle "short"})
      "iso"            (.toISOString d)
      (.toLocaleDateString d "de-AT" #js {:dateStyle "medium"}))))

(defn object-types []
  (sort-by #(str/lower-case (oc/type-label %)) (keys (:object-types @catalog/schema))))

(defn fields-of [t]
  (mapv (comp keyword :name) (get-in @catalog/schema [:object-types (keyword t) :fields])))

(defn type-select [w]
  [:<>
   [field-label "Objekttyp"]
   [ui/html-select
    {:fill true :value (or (get-in @w [:attrs :type]) "")
     :on-change #(send! [:notebook.widget/attrs {:type (not-empty (.. % -target -value))
                                                  :id nil :property nil :properties nil}])}
    [:option {:value ""} "—"]
    (for [t (object-types)] ^{:key t} [:option {:value (name t)} (oc/type-label t)])]])

(defn- object-picker* [w t]
  (let [q     (:q @w)
        rows  (get @pick-rows t)
        tk    (get-in @catalog/schema [:object-types (keyword t) :title-key])
        lbl   #(catalog/instance-display-name % tk)
        shown (if (sequential? rows)
                (->> rows
                     (filter #(str/includes? (str/lower-case (str (lbl %) " " (:id %)))
                                             (str/lower-case (str q))))
                     (take 50))
                [])]
    [:<>
     [field-label "Objekt"]
     [ui/input-group {:type "search" :value (or q "") :placeholder "Suchen…"
                      :on-change #(send! [:notebook.widget/query
                                          (.. % -target -value)])}]
     [:div {:class "np-pick"}
      (cond
        (= :loading rows) [busy/block {:class "np-muted"} "lädt…"]
        (empty? shown)    [:div {:class "np-muted"} "Keine Objekte."]
        :else
        (for [row shown]
          ^{:key (str (:id row))}
          [:div {:class (str "np-pick__row"
                             (when (= (str (:id row)) (get-in @w [:attrs :id])) " is-active"))
                 :on-click #(attr! :id (str (:id row)))}
           (lbl row) [:span {:class "np-muted"} (str " · " (:id row))]]))]]))

(def ^:private object-picker-of
  (machine/entering notebook (fn [_ t] [:notebook.widget/rows t]) object-picker*))

(defn object-picker [w]
  (let [t (get-in @w [:attrs :type])]
    ^{:key (str t)} [object-picker-of w t]))

(defn prop-select [w k label multi?]
  (let [t  (get-in @w [:attrs :type])
        fs (fields-of t)]
    (if multi?
      (let [chosen (set (remove str/blank? (str/split (str (get-in @w [:attrs k])) #",")))]
        [:<>
         [field-label label]
         [:div {:class "np-checklist"}
          (for [f fs :let [n (name f)]]
            ^{:key n}
            [ui/checkbox
             {:class "np-check" :label (tbl/humanize n)
              :checked (contains? chosen n)
              :on-change #(attr! k
                                 (not-empty
                                  (str/join "," (filter (if (.. % -target -checked)
                                                          (conj chosen n)
                                                          (disj chosen n))
                                                        (map name fs)))))}])]
         [:div {:class "np-muted"} "Keine Auswahl = die prominenten Eigenschaften."]])
      [:<>
       [field-label label]
       [ui/html-select
        {:fill true :value (or (get-in @w [:attrs k]) "")
         :on-change #(attr! k (not-empty (.. % -target -value)))}
        [:option {:value ""} "Titel"]
        (for [f fs :let [n (name f)]] ^{:key n} [:option {:value n} (tbl/humanize n)])]])))

(defn text-field [w k label placeholder]
  [:<>
   [field-label label]
   [ui/input-group
    {:value (or (get-in @w [:attrs k]) "")
     :placeholder placeholder
     :on-change #(attr! k (not-empty (.. % -target -value)))}]])

(defn- graph-select* [w]
  [:<>
   [field-label "Graph"]
   [ui/html-select
    {:fill true :value (or (get-in @w [:attrs :graph]) "")
     :on-change #(attr! :graph (not-empty (.. % -target -value)))}
    [:option {:value ""} "— gespeicherten Graph wählen —"]
    (for [g (sort-by #(str/lower-case (str (or (:title %) (:name %)))) @graphs)]
      ^{:key (:id g)} [:option {:value (:id g)} (or (:title g) (:name g))])]
   [:div {:class "np-muted"}
    "Nur lesend; „öffnen“ führt in Data Lineage. Graphen anderer Apps verlinkt der Ressourcen-Link."]])

(def graph-select (machine/entering notebook [:notebook.widget/graphs] graph-select*))

(defn type-checklist [w k label]
  (let [chosen (set (remove str/blank? (str/split (str (get-in @w [:attrs k])) #",")))
        taken  (into #{} (comp (remove #{k})
                               (mapcat #(str/split (str (get-in @w [:attrs %])) #",")))
                     [:types :derived :use-case])]
    [:<>
     [field-label label]
     [:div {:class "np-checklist"}
      (for [t (object-types) :let [n (name t)] :when (not (taken n))]
        ^{:key n}
        [ui/checkbox
         {:class "np-check" :label (oc/type-label t)
          :checked (contains? chosen n)
          :on-change #(attr! k
                             (not-empty
                              (str/join "," (sort ((if (.. % -target -checked) conj disj)
                                                   chosen n)))))}])]]))

(defn state-property-select [w]
  (let [fs (fields-of (get-in @w [:attrs :type]))]
    [:<>
     [field-label "Zustand in der Eigenschaft"]
     [ui/html-select
      {:fill true :value (or (get-in @w [:attrs :property]) "status")
       :on-change #(attr! :property (.. % -target -value))}
      (for [n (distinct (cons "status" (map name fs)))]
        ^{:key n} [:option {:value n} (tbl/humanize n)])]
     [:div {:class "np-muted"}
      "Zustände sind die Werte, die Actions setzen; Übergänge ihre Vorbedingungen."]]))

(defn flag [w k label]
  [ui/checkbox
   {:class "np-check" :label label
    :checked (= "true" (get-in @w [:attrs k]))
    :on-change #(attr! k (when (.. % -target -checked) "true"))}])

(defn widget-fields [w]
  (let [kind (:kind @w)]
    (case kind
      "object-property"
      [:<> [type-select w] [object-picker w] [prop-select w :property "Property" false]
       [ui/checkbox
        {:class "np-check" :label "Property-Namen zeigen"
         :checked (= "true" (get-in @w [:attrs :show-name]))
         :on-change #(attr! :show-name
                            (when (.. % -target -checked) "true"))}]
       [flag w :hover "Eigenschaften beim Darüberfahren zeigen"]
       [flag w :no-link "Link entfernen (kein Sprung in die Objektansicht)"]]

      "object-card"
      [:<> [type-select w] [object-picker w]
       [prop-select w :properties "Sichtbare Properties" true]
       [flag w :no-link "Link entfernen (kein Sprung in die Objektansicht)"]]

      "markdown-property"
      [:<> [type-select w] [object-picker w] [prop-select w :property "Markdown-Property" false]]

      "object-table"
      [:<> [type-select w]
       [prop-select w :properties "Spalten" true]
       [text-field w :where "Filter" "status=offen; werk=Wien"]
       [text-field w :order-by "Sortiert nach" "Property"]
       [text-field w :limit "Höchstens" "50"]]

      "function"
      [:<>
       [field-label "Function"]
       [ui/html-select {:fill true :value (or (get-in @w [:attrs :function]) "")
                        :on-change #(attr! :function
                                           (not-empty (.. % -target -value)))}
        [:option {:value ""} "—"]
        (for [f (sort (map name (keys (:functions @catalog/schema))))]
          ^{:key f} [:option {:value f} f])]
       [text-field w :args "Eingaben (JSON)" "{\"auftrag\": \"BA26.0203\"}"]
       [field-label "Ausgabe"]
       [ui/html-select
        {:fill true :value (or (get-in @w [:attrs :output]) "markdown")
         :on-change #(attr! :output (.. % -target -value))}
        [:option {:value "markdown"} "Text (Markdown)"]
        [:option {:value "table"} "Tabelle"]]]

      "current-date"
      [:<>
       [field-label "Format"]
       [ui/html-select
        {:fill true :value (or (get-in @w [:attrs :format]) "date")
         :on-change #(attr! :format (.. % -target -value))}
        (for [[k l] date-formats] ^{:key k} [:option {:value k} l])]
       [text-field w :tz "Zeitzone" "Europe/Vienna"]
       [ui/checkbox
        {:class "np-check" :label "Beim Öffnen aktualisieren (sonst bleibt das Datum des Einfügens)"
         :checked (= "true" (get-in @w [:attrs :auto]))
         :on-change #(attr! :auto
                            (when (.. % -target -checked) "true"))}]]

      "graph" [graph-select w]

      "object-model"
      [:<>
       [type-checklist w :types "Kern (aus einer Quelle)"]
       [type-checklist w :derived "Abgeleitet (Anreicherung)"]
       [type-checklist w :use-case "Use Case (im Ablauf bearbeitet)"]]

      "lifecycle"
      [:<> [type-select w] [state-property-select w]]

      "use-case-draft"
      [:<>
       [field-label "Entwurf"]
       [ui/html-select
        {:fill true :value (or (get-in @w [:attrs :draft]) "")
         :on-change #(attr! :draft (not-empty (.. % -target -value)))}
        [:option {:value ""} "— Use-Case-Entwurf wählen —"]
        (for [d (sort-by :name (filter #(= "use-case" (some-> (:format %) name)) @docs))]
          ^{:key (:id d)} [:option {:value (str (:id d))} (:name d)])]
       [field-label "Zeichnung"]
       [ui/html-select
        {:fill true :value (or (get-in @w [:attrs :view]) "object-model")
         :on-change #(attr! :view (.. % -target -value))}
        [:option {:value "object-model"} "Objektmodell"]
        [:option {:value "statechart"} "Lebenszyklus"]
        [:option {:value "plan"} "Plan des Assistenten"]]
       (when (= "statechart" (get-in @w [:attrs :view]))
         [text-field w :chart "Lebenszyklus (:id im Entwurf)" "vorgang-status"])]

      "resource-link"
      [:<>
       [field-label "Dokument"]
       [ui/html-select
        {:fill true :value ""
         :on-change (fn [e]
                      (let [id (.. e -target -value)]
                        (when (seq id)
                          (send! [:notebook.widget/attrs
                                  {:href  (str "/notebook/" id)
                                   :label (resource-label (str "/notebook/" id))}]))))}
        [:option {:value ""} "— Notebook-Dokument wählen —"]
        (for [d (sort-by :name @docs)] ^{:key (:id d)} [:option {:value (:id d)} (:name d)])]
       [field-label "Graph"]
       [ui/html-select {:fill true :value ""
                        :on-change (fn [e]
                                     (when-let [g (get @catalog/views
                                                       (.. e -target -value))]
                                       (let [href (routing/graph-path g)]
                                         (send! [:notebook.widget/attrs
                                                 {:href  href
                                                  :label (resource-label href)}]))))}
        [:option {:value ""} "— gespeicherten Graph wählen —"]
        (for [[mode gs] (sort-by (comp str key)
                                 (group-by catalog/graph-mode (vals @catalog/views)))]
          ^{:key (str mode)}
          [:optgroup {:label (feature/app-label (feature/app-of (feature/mode->nav mode)))}
           (for [g (sort-by #(str/lower-case (str (:name %))) gs)]
             ^{:key (str (:id g))} [:option {:value (str (:id g))} (:name g)])])]
       [text-field w :href "Adresse" "/notebook/… oder https://…"]
       [text-field w :label "Anzeigetext" ""]]

      nil)))

(defn widget-ready? [{:keys [kind attrs]}]
  (case kind
    ("object-property" "object-card" "markdown-property") (and (:type attrs) (:id attrs))
    "object-table"  (some? (:type attrs))
    "function"      (some? (:function attrs))
    "resource-link" (some? (:href attrs))
    "graph"         (some? (:graph attrs))
    "object-model"  (some some? (map attrs [:types :derived :use-case]))
    "lifecycle"     (some? (:type attrs))
    "use-case-draft" (some? (:draft attrs))
    true))

(defn widget-dialog []
  (let [w     wdialog
        {:keys [kind pos]} @w
        label (some (fn [[k l]] (when (= k kind) l)) rich/widget-kinds)]
    [overlay/modal
     {:size "480px"
      :on-close #(send! [:notebook.widget/close])
      :title (str (if pos "Widget bearbeiten: " "Widget einfügen: ") label)
      :foot [:<>
             [action-btn {:label    "Abbrechen" :tone :ghost
                          :on-click #(send! [:notebook.widget/close])}]
             [action-btn
              {:label (if pos "Übernehmen" "Einfügen") :disabled? (not (widget-ready? @w))
               :on-click
               (fn []
                 (let [auto? (= "true" (get-in @w [:attrs :auto]))
                       fmt   (or (get-in @w [:attrs :format]) "date")
                       attrs (cond-> (:attrs @w)
                               (and (= kind "current-date") (not auto?))
                               (assoc :value (format-now fmt)))]
                   (when-let [ed @editor]
                     (if pos
                       (rich/set-widget-attrs! ed pos attrs)
                       (rich/insert-widget! ed attrs)))
                   (send! [:notebook.widget/close])))}]]
      :foot-end? true
      :close-on-backdrop? false}
     [widget-fields w]]))

(defn widget-panel []
  (when-let [{:keys [attrs] :as sw} @sel-widget]
    (let [kind  (:kind attrs)
          label (or (some (fn [[k l]] (when (= k kind) l)) rich/widget-kinds)
                    (when (= kind "mention") "Erwähnung")
                    kind)]
      [:<>
       [ui/h5 label]
       [display/property-list
        (for [[k v] (sort-by key (dissoc attrs :kind :snapshot :locked :locked-at :locked-by))
              :when (seq (str v))]
          {:prop k :label (tbl/humanize k) :text (str v)})
        {:label-position :beside}]
       [ui/button-group {:vertical true :fill true}
        (when-not (#{"mention"} kind)
          [btn {:label "Bearbeiten…" :tone :ghost
                :on-click #(open-widget-dialog! kind attrs (:pos sw))}])
        [btn {:label "Entfernen" :tone :danger
              :on-click #(chain! (fn [^js c] (.deleteSelection c)))}]]])))
