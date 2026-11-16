(ns sutra.cli.verbs.feedback
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [sutra.cli.dispatch :refer [step!]]
            [sutra.cli.errors :refer [req!]]
            [sutra.cli.out :refer [emit! emit-rows! paint render rpad]]
            [sutra.cli.projection :as projection]
            [sutra.types.feedback-szenario :as fz]
            [sutra.wire.api :as api]))

(defn- uhr [ms] (let [s (quot (or ms 0) 1000)] (format "%d:%02d" (quot s 60) (mod s 60))))

(defn- feedback-sitzungen! [ctx benutzer erledigt? offen?]
  (let [{:keys [sitzungen]} (req! "feedback" (api/get! :feedback
                                                       (cond-> {}
                                                         benutzer (assoc :benutzer benutzer))))
        gewollt (cond erledigt? (partial filter :erledigt)
                      offen?    (partial remove :erledigt)
                      :else     identity)]
    (emit-rows! ctx (mapv #(select-keys % [:id :benutzer :aufgabe :begonnen :beendet :erledigt])
                          (gewollt sitzungen))
                "keine Feedback-Sitzungen")))

(defn- abhaken! [ctx id erledigt?]
  (let [s (req! "feedback" (api/post! :feedback-erledigt {:sitzung id :erledigt erledigt?}))]
    (emit! ctx (assoc s :erledigt? erledigt?) ::abgehakt)))

(defmethod render ::abgehakt [_ {:keys [id erledigt erledigt-von erledigt?]}]
  (str "  Feedback-Sitzung " id
       (if erledigt? (str " erledigt (" erledigt ", " erledigt-von ")") " wieder offen")))

(defn- feedback-kopf [{:keys [id benutzer aufgabe begonnen beendet erledigt erledigt-von]}
                      {:keys [dauer-s klicks eingaben seiten meldungen gefuehle sus
                              kopien uebertragungen mehrfachklicks stumm traege]}]
  [(str (paint :bold (str "Feedback-Sitzung " id)) " · " benutzer
        (when aufgabe (str " · „" aufgabe "“")) " · " begonnen
        (if beendet (str " → " beendet) (paint :yellow " (läuft)"))
        (when erledigt (paint :green (str " · ✓ erledigt " erledigt " " erledigt-von))))
   (str "  " (uhr (* 1000 dauer-s)) " · " klicks " Klicks · " eingaben " Eingaben · "
        seiten " Seiten · " meldungen " Meldungen"
        (when (seq gefuehle)
          (str " (" (str/join ", " (for [[g n] gefuehle] (str (name g) " " n))) ")"))
        (when sus (str " · SUS " sus)))
   (str "  ⇄ von Hand übertragen " (count uebertragungen) " (Kopien " kopien ")"
        " · ↻ Klickserien " (count mehrfachklicks))
   (when (seq traege)
     (str "  ⌛ träge Eingaben (über 100 ms je Taste): " (count traege)))
   (when (seq stumm)
     (str "  ◌ klickbar ohne Rolle (Screenreader sieht kein Bedienelement): "
          (str/join " · " (map #(str "„" % "“") stumm))))])

(defn- text-namen
  [verlauf]
  (->> verlauf
       (mapcat (fn [{:keys [knoten absage art text ort]}]
                 (concat [(:aktion absage) (:aktion knoten) (:app knoten)
                          (get-in absage [:ausloeser :knoten :aktion]) (:app ort)]
                         (when (and (= "fehler" (some-> art name)) (not (:aktion absage)))
                           [(second (re-find #"^„([^“]+)“ nicht ausgeführt" (str text)))]))))
       (remove nil?)
       distinct
       vec))

(defn- projektionstexte!
  []
  (let [{status :status {:keys [projections]} :body} (api/get! :projections)]
    (when (= 200 status)
      (vec (for [nm (sort (map #(name (:name %)) projections))
                 :let [{:keys [status body]} (api/get! :projection-files {:name nm})]
                 :when (= 200 status)
                 [pfad text] (sort-by (comp name key) (:files body))]
             {:projektion nm :pfad (name pfad) :text text
              :knoten (map (fn [[_ k]] (if (keyword? k) (name k) (str k)))
                           (keys (get-in body [:base pfad])))})))))

(defn- texte-zu
  [namen]
  (when (seq namen)
    (let [texte (projektionstexte!)]
      (into {}
            (for [n namen
                  :let [treffer (for [{:keys [projektion pfad text knoten]} texte
                                      h (projection/text-hits text n knoten)]
                                  (assoc h :projektion projektion :pfad pfad))
                        eigene (first (filter #(str/ends-with? (:pfad %) (str "/" n ".enta"))
                                              texte))
                        h (or (when eigene (assoc eigene :line 1))
                              (first (filter #(= n (:node %)) treffer))
                              (first treffer))]
                  :when h]
              [n (str (:projektion h) " · " (:pfad h) ":" (:line h)
                      (when (:node h) (str " " (:node h))))])))))

(def ^:private basistypen
  #{:string :integer :long :decimal :double :boolean :date :timestamp :uuid :json :geopoint})

(defn- aktionen-der-texte
  [texte]
  (into {}
        (for [{:keys [text]} texte
              :let [s (try (:schema (edn/read-string {:default tagged-literal} text))
                           (catch Exception _ nil))]
              :when (map? (:actions s))
              e (:actions s)]
          e)))

(defn- typen-der-aktion
  [aktion]
  (->> (concat (map :type (vals (:params aktion))) (map :object-type (:rules aktion)))
       (keep #(some-> % name keyword))
       (remove basistypen)
       distinct))

(defn- beschrieben!
  [namen]
  (if (empty? namen)
    {}
    (let [{:keys [shape data]} (req! "describe"
                                     (api/get! :schema-describe
                                               {:names (str/join "," (map name namen))
                                                :no-count true}))
          je (if (= "one" (name shape)) {(:object-type data) data} (:described data))]
      (into {} (for [[k d] je :when (:object-type d)] [(keyword (name k)) d])))))

(defn- szenario!
  [ctx id ereignis to]
  (let [{:keys [verlauf]} (req! "feedback" (api/get! :feedback {:id id}))
        fehler? (some? (fz/fehler-und-klick verlauf ereignis))
        actions (when fehler? (aktionen-der-texte (projektionstexte!)))
        aname   (some-> (:fehler (fz/fehler-und-klick verlauf ereignis))
                        (fz/aktion-von actions))
        sz      (if fehler?
                  (fz/szenario {:sitzung id :verlauf verlauf :ereignis ereignis
                                :actions actions
                                :typen (beschrieben! (typen-der-aktion (get actions aname)))})
                  (fz/klick-szenario {:sitzung id :verlauf verlauf :ereignis ereignis}))]
    (if to
      (do (io/make-parents to)
          (spit to (with-out-str (pprint/pprint sz)))
          (emit! ctx {:szenario (:id sz) :aktion (:aktion sz) :datei to
                      :entscheidungen (frequencies (map :kind (:entscheidungen sz)))
                      :annahmen (:annahmen sz) :ausgelassen (:ausgelassen sz)}))
      (emit! ctx sz ::szenario))
    nil))

(defmethod render ::szenario [_ sz] (with-out-str (pprint/pprint sz)))

(defn- feedback! [ctx id]
  (let [antwort (req! "feedback" (api/get! :feedback {:id id}))]
    (emit! ctx (assoc antwort :texte (texte-zu (text-namen (:verlauf antwort)))) ::feedback))
  nil)

(defmethod render ::feedback [_ {:keys [sitzung verlauf zusammenfassung texte]}]
  (let [hand  (into #{} cat (:uebertragungen zusammenfassung))
        serie (into #{} cat (:mehrfachklicks zusammenfassung))]
    [(feedback-kopf sitzung zusammenfassung)
     (for [{:keys [id t satz punkt ort meldungen art stand herkunft] :as e} verlauf]
       [(str "  " (rpad (uhr t) 6)
             (cond (hand id) "⇄ " (serie id) "↻ " :else "  ")
             (if (= "meldung" (name art)) (paint :yellow (str "✎ " satz)) satz)
             (when punkt
               (paint :dim (str "  @" (str/join "," punkt)
                                (some->> (:fenster ort) (str/join "×")
                                         (str "/"))))))
        (when stand (str "          " (paint :dim (str "offen: " stand))))
        (for [z herkunft] (str "          " (paint :dim (str "⇠ " z))))
        (when (= "fehler" (name art))
          (for [n (text-namen [e]) :let [w (get texte n (get texte (keyword n)))] :when w]
            (str "          " (paint :dim (str "⇠ Text " n " → " w)))))
        (for [m meldungen]
          (str "          " (paint :yellow (str "↳ " (:satz m)))))])
     (when-let [f (get-in sitzung [:fazit :text])] (str "  Fazit: " f))]))

(defmethod step! :feedback [ctx [_ {:keys [id benutzer erledigt? offen? szenario? ereignis to]}]]
  (cond
    (and id szenario?) (szenario! ctx id ereignis to)
    (and id erledigt?) (abhaken! ctx id true)
    (and id offen?)    (abhaken! ctx id false)
    id                 (feedback! ctx id)
    :else              (feedback-sitzungen! ctx benutzer erledigt? offen?)))
