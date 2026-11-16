(ns sutra.change.findings
  (:require [clojure.string :as str]
            [sutra.i18n.core :refer [t]]
            [malli.core :as m]
            [malli.error :as me]
            [sutra.parser.ent :as ent]
            [sutra.parser.sugar :as sugar]
            [sutra.parser.project-export :as pe]
            [sutra.parser.resolve :as res]
            [sutra.types.advisory :as adv]
            [sutra.types.languages :as langs]
            [sutra.types.sql-traps :as traps]))

(def regeln
  {:datei/unlesbar
   {:bedingung :change/rule-datei-unlesbar-bedingung
    :hinweis   :change/rule-datei-unlesbar-hinweis}

   :grammatik/verletzt
   {:bedingung :change/rule-grammatik-verletzt-bedingung
    :hinweis   :change/rule-grammatik-verletzt-hinweis}

   :referenz/ungelöst
   {:bedingung :change/rule-referenz-ungelöst-bedingung
    :hinweis   :change/rule-referenz-ungelöst-hinweis}

   :referenz/verdächtig
   {:bedingung :change/rule-referenz-verdächtig-bedingung
    :hinweis   :change/rule-referenz-verdächtig-hinweis}

   :aktion/unbekannter-typ
   {:bedingung :change/rule-aktion-unbekannter-typ-bedingung
    :hinweis   :change/rule-aktion-unbekannter-typ-hinweis}

   :aktion/phantom-edit
   {:bedingung :change/rule-aktion-phantom-edit-bedingung
    :hinweis   :change/rule-aktion-phantom-edit-hinweis}

   :aktion/function-rule
   {:bedingung :change/rule-aktion-function-rule-bedingung
    :hinweis   :change/rule-aktion-function-rule-hinweis}

   :aktion/unbekanntes-ziel
   {:bedingung :change/rule-aktion-unbekanntes-ziel-bedingung
    :hinweis   :change/rule-aktion-unbekanntes-ziel-hinweis}

   :aktion/unbekannter-parametertyp
   {:bedingung :change/rule-aktion-unbekannter-parametertyp-bedingung
    :hinweis   :change/rule-aktion-unbekannter-parametertyp-hinweis}

   :geltung/fremde-sektion
   {:bedingung :change/rule-geltung-fremde-sektion-bedingung
    :hinweis   :change/rule-geltung-fremde-sektion-hinweis}

   :quelle/klartext-geheimnis
   {:bedingung :change/rule-quelle-klartext-geheimnis-bedingung
    :hinweis   :change/rule-quelle-klartext-geheimnis-hinweis}

   :neubau/schlüssel-migration
   {:bedingung :change/rule-neubau-schlüssel-migration-bedingung
    :hinweis   :change/rule-neubau-schlüssel-migration-hinweis}

   :neubau/logik-ohne-version
   {:bedingung :change/rule-neubau-logik-ohne-version-bedingung
    :hinweis   :change/rule-neubau-logik-ohne-version-hinweis}

   :neubau/backing-ohne-shape
   {:bedingung :change/rule-neubau-backing-ohne-shape-bedingung
    :hinweis   :change/rule-neubau-backing-ohne-shape-hinweis}

   :einreichung/check
   {:bedingung :change/rule-einreichung-check-bedingung
    :hinweis   :change/rule-einreichung-check-hinweis}

   :basis/veralteter-zweig
   {:bedingung :change/rule-veralteter-zweig-bedingung
    :hinweis   :change/rule-veralteter-zweig-hinweis}

   :app/formular-handgebaut
   {:bedingung :change/rule-formular-handgebaut-bedingung
    :hinweis   :change/rule-formular-handgebaut-hinweis}

   :sql/char-ohne-laenge
   {:bedingung :change/rule-sql-char-bedingung
    :hinweis   :change/rule-sql-char-hinweis}

   :sql/abgelehnt
   {:bedingung :change/rule-sql-abgelehnt-bedingung
    :hinweis   :change/rule-sql-abgelehnt-hinweis}

   :sql/ungeprueft
   {:bedingung :change/rule-sql-ungeprueft-bedingung
    :hinweis   :change/rule-sql-ungeprueft-hinweis}

   :ebene/ent-nicht-in-git
   {:bedingung :change/rule-ebene-ent-nicht-in-git-bedingung
    :hinweis   :change/rule-ebene-ent-nicht-in-git-hinweis}})

(defn- finding [schwere regel subjekt {:keys [ist soll behebung]}]
  (cond-> {:subjekt subjekt :regel regel :schwere schwere :behebung behebung}
    (some? ist)  (assoc :ist ist)
    (some? soll) (assoc :soll soll)))

(defn- kurz
  [v]
  (let [s (pr-str v)]
    (if (> (count s) 80) (str (subs s 0 77) "…") s)))

(defn- subjekt-aus-pfad
  [pfad]
  (let [p (if (= :schema (first pfad)) (rest pfad) pfad)
        [klasse nm] p]
    (when (keyword? klasse)
      (cond-> {:klasse klasse}
        (or (keyword? nm) (string? nm)) (assoc :name nm)
        :always (assoc :rest (vec (drop 2 p)))))))

(defn unlesbar-findings [kaputt]
  (for [[pfad grund] kaputt]
    (finding :fehler :datei/unlesbar {:klasse :datei :name pfad}
             {:ist grund
              :soll (t :change/soll-readable-edn)
              :behebung (t :change/fix-repair-file {:pfad pfad})})))

(defn- lean-subjekt
  [roh pfad]
  {:klasse :applications :name (:name roh) :rest (vec pfad)})

(defn- pfad->text
  [pfad]
  (str/join " › " (map #(if (keyword? %) (name %) (str %)) pfad)))

(defn- distanz
  [a b]
  (let [a (vec a) b (vec b)]
    (loop [i 1 row (vec (range (inc (count b))))]
      (if (> i (count a))
        (peek row)
        (recur (inc i)
               (reduce (fn [r j]
                         (conj r (min (inc (peek r))
                                      (inc (get row j))
                                      (+ (get row (dec j))
                                         (if (= (get a (dec i)) (get b (dec j))) 0 1)))))
                       [i]
                       (range 1 (inc (count b)))))))))

(def ^:private max-erlaubte
  12)

(defn- erlaubte-schlüssel
  [schema]
  (try (into [] (comp (map first) (filter keyword?)) (m/children schema))
       (catch Exception _ nil)))

(defn- naheliegend
  [k erlaubt]
  (let [kn (name k)]
    (->> erlaubt
         (map (fn [c] [(distanz kn (name c)) c]))
         (filter (fn [[d c]]
                   (let [cn (name c)]
                     (or (str/includes? kn cn)
                         (str/includes? cn kn)
                         (<= d (max 2 (quot (count kn) 3)))))))
         (sort-by (fn [[d c]] [d (count (name c))]))
         first
         second)))

(defn- schlüssel-liste
  [erlaubt]
  (let [ks (map str erlaubt)]
    (if (<= (count ks) max-erlaubte)
      (str/join ", " ks)
      (str (str/join ", " (take max-erlaubte ks))
           (t :change/more-keys {:n (- (count ks) max-erlaubte)})))))

(defn- unbekannter-schlüssel
  [err rest-pfad]
  (let [k       (last (:in err))
        erlaubt (erlaubte-schlüssel (:schema err))
        tipp    (when (and (keyword? k) (seq erlaubt)) (naheliegend k erlaubt))
        wo      (seq (butlast rest-pfad))]
    {:ist  (str k " " (kurz (:value err)))
     :soll (if tipp (t :change/soll-key {:tipp tipp}) (t :change/soll-known-key))
     :behebung (str (t :change/fix-unknown-key
                       {:key k :wo (if wo
                                     (t :change/at-path {:pfad (pfad->text wo)})
                                     (t :change/at-this-place))})
                    (if tipp (t :change/did-you-mean {:tipp tipp}) ". ")
                    (if (seq erlaubt)
                      (t :change/allowed-are {:keys (schlüssel-liste erlaubt)})
                      (t :change/strike-it)))}))

(defn- fehlender-schlüssel
  [err rest-pfad]
  (let [k  (last (:in err))
        wo (seq (butlast rest-pfad))]
    {:soll (t :change/soll-entry {:key k})
     :behebung (str (t :change/fix-missing-key {:key k})
                    (when wo (t :change/at-path {:pfad (pfad->text wo)})) ".")}))

(defn- falscher-wert
  [err rest-pfad]
  {:ist  (kurz (:value err))
   :soll (or (me/error-message err) (t :change/soll-allowed-value))
   :behebung (str (t :change/fix-correct-value)
                  (if-let [r (seq rest-pfad)]
                    (t :change/at-path {:pfad (pfad->text r)})
                    (t :change/of-the-entry))
                  ".")})

(defn grammatik-findings
  [datei data]
  (let [kind (:kind data)
        roh  (cond-> (:roh data) (= :view kind) ent/lower-view-cards)
        gram (ent/grammar-of kind)
        ent? (nil? kind)]
    (when-let [e (m/explain gram roh)]
      (for [err (:errors e)
            :let [s (or (if ent?
                          (subjekt-aus-pfad (vec (:in err)))
                          (lean-subjekt roh (:in err)))
                        {:klasse :datei :name "<wurzel>"})
                  pfad (vec (:rest s))]]
        (finding :fehler :grammatik/verletzt s
                 (case (:type err)
                   :malli.core/extra-key   (unbekannter-schlüssel err pfad)
                   :malli.core/missing-key (fehlender-schlüssel err pfad)
                   (falscher-wert err pfad)))))))

(defn referenz-findings
  [schema]
  (for [{:keys [severity where message rule]} (res/findings schema)]
    (let [fehler? (= :error severity)
          regel   rule]
      (finding (if fehler? :fehler :warnung)
               regel
               (or (subjekt-aus-pfad (vec where)) {:klasse :schema})
               {:soll message
                :behebung (t (case regel
                               :aktion/phantom-edit             :change/fix-phantom-edit
                               :aktion/unbekannter-typ          :change/fix-unknown-type
                               :aktion/unbekanntes-ziel         :change/fix-unknown-target
                               :aktion/unbekannter-parametertyp :change/fix-unknown-param-type
                               :aktion/function-rule            :change/fix-function-rule
                               (if fehler?
                                 :change/fix-dangling-reference
                                 :change/fix-suspicious-reference)))}))))

(defn geltung-findings
  [ontologie? datei schema]
  (let [erlaubt (pe/carried-sections ontologie?)]
    (for [sek (keys schema)
          :when (not (erlaubt sek))]
      (finding :fehler :geltung/fremde-sektion {:klasse sek}
               {:ist  (t :change/ist-declared-in {:datei datei})
                :soll (t :change/soll-repo-carries
                         {:sektionen (str/join ", " (sort (map name erlaubt)))})
                :behebung (t (if ontologie?
                               :change/fix-code-belongs-elsewhere
                               :change/fix-types-belong-to-ontology))}))))

(defn geheimnis-findings
  [schema]
  (for [[nm spec] (:sources schema)
        :let [k (some #{:secrets :secret-cipher} (keys spec))]
        :when k]
    (finding :fehler :quelle/klartext-geheimnis {:klasse :sources :name nm}
             {:ist  (t :change/ist-entry-carries {:key k})
              :soll (t :change/soll-connector-without-credentials)
              :behebung (t :change/fix-remove-secret {:key k :name (name nm)})})))

(defn neubau-findings
  [diff]
  (concat
   (for [[[sek nm] {:keys [art felder vorher nachher]}] diff
         :when (and (= :objects sek) (= :geändert art) (some #{:match-keys} felder))]
     (finding :warnung :neubau/schlüssel-migration {:klasse sek :name nm}
              {:ist  (kurz (:match-keys nachher))
               :soll (t :change/soll-was-match-keys {:vorher (kurz (:match-keys vorher))})
               :behebung (t :change/fix-key-migration {:name (name nm)})}))
   (for [[[sek nm] {:keys [art felder nachher]}] diff
         :when (and (#{:transforms :functions} sek) (= :geändert art)
                    (some #{:code :query :code-file} felder)
                    (not (some #{:semantic-version} felder))
                    (:incremental nachher))]
     (finding :warnung :neubau/logik-ohne-version {:klasse sek :name nm}
              {:ist  (t :change/ist-changed
                        {:felder (str/join ", "
                                           (map name (filter #{:code :query :code-file} felder)))})
               :soll (t :change/soll-semantic-version-bumped)
               :behebung (t :change/fix-bump-semantic-version {:name (name nm)})}))
   (let [erzeugt (into #{}
                       (comp (filter (fn [[[sek] {:keys [art]}]]
                                       (and (= :transforms sek) (not= :entfernt art))))
                             (mapcat (fn [[_ {:keys [nachher]}]]
                                       (or (seq (:outputs nachher)) [(:output nachher)])))
                             (keep :object-type))
                       diff)]
     (for [[[sek nm] {:keys [art nachher]}] diff
           :when (and (= :objects sek) (= :neu art)
                      (:backing nachher)
                      (not (erzeugt nm)))]
       (finding :warnung :neubau/backing-ohne-shape {:klasse sek :name nm}
                {:ist  (t :change/ist-backing-ohne-shape
                          {:dataset (kurz (get-in nachher [:backing :dataset]))})
                 :soll (t :change/soll-shape-transform {:name (name nm)})
                 :behebung (t :change/fix-backing-ohne-shape {:name (name nm)})})))))

(defn check-findings
  [checks]
  (concat
   (for [{:keys [check status detail]} checks
         :when (= :failed status)]
     (finding :fehler :einreichung/check {:klasse :einreichung :name check}
              {:ist  (or detail "failed")
               :soll "passed"
               :behebung (t :change/fix-failed-check {:check check})}))
   (for [zeile (adv/of-checks checks)
         :let [[subjekt hinweis] (str/split zeile #" — " 2)]]
     (finding :warnung :app/formular-handgebaut {:klasse :applications :name subjekt}
              {:ist  hinweis
               :soll (t :change/soll-form-from-action)
               :behebung (t :change/fix-use-action-form)}))))

(defn basis-findings
  [{:keys [hinterher zweig änderungen?]}]
  (when (and änderungen? hinterher (pos? hinterher))
    [(finding :warnung :basis/veralteter-zweig
              {:klasse :einreichung :name (or zweig "HEAD")}
              {:ist  (t :change/ist-commits-behind {:n hinterher})
               :soll (t :change/soll-branched-from-trunk {:trunk pe/trunk-branch})
               :behebung (t :change/fix-stale-base)})]))

(defn sql-transforms
  [diff]
  (into (sorted-map)
        (for [[[sek nm] {:keys [art nachher]}] diff
              :when (and (= :transforms sek) (not= :entfernt art) (map? nachher)
                         (= :postgres (langs/dialect (:language nachher)))
                         (string? (:query nachher)))]
          [nm nachher])))

(defn sql-char-findings
  [sql-tx]
  (for [[nm e] sql-tx
        {:keys [hit message]} (traps/char-cast-findings nm (:query e))]
    (finding :fehler :sql/char-ohne-laenge {:klasse :transforms :name nm}
             {:ist hit :soll message :behebung (t :change/fix-sql-char)})))

(defn sql-urteil-findings
  [{:keys [findings]}]
  (for [{:keys [rule entity message]} findings
        :when (contains? #{:sql-refused :sql-unverified} rule)]
    (if (= :sql-refused rule)
      (finding :fehler :sql/abgelehnt {:klasse :transforms :name entity}
               {:soll message :behebung (t :change/fix-sql-abgelehnt)})
      (finding :warnung :sql/ungeprueft {:klasse :transforms :name entity}
               {:soll message :behebung (t :change/fix-sql-ungeprueft)}))))

(defn sql-unbekannt-finding
  [sql-tx]
  (finding :warnung :sql/ungeprueft {:klasse :transforms :name (first (keys sql-tx))}
           {:ist (vec (keys sql-tx)) :soll (t :change/sql-nicht-kompiliert)
            :behebung (t :change/fix-sql-ungeprueft)}))

(def ^:private schwere-rang {:fehler 0 :warnung 1})

(defn sammeln
  [{:keys [ontologie? dateien kaputt diff]}]
  (->> (concat
        (unlesbar-findings kaputt)
        (mapcat (fn [[datei data]]
                  (let [schema (:schema data)]
                    (concat (grammatik-findings datei data)
                            (geltung-findings ontologie? datei schema)
                            (geheimnis-findings schema)
                            (try (referenz-findings (sugar/lower-schema schema))
                                 (catch Exception _ nil)))))
                dateien)
        (neubau-findings diff)
        (sql-char-findings (sql-transforms diff)))
       (remove nil?)
       (sort-by (fn [f] [(schwere-rang (:schwere f) 9)
                         (str (get-in f [:subjekt :klasse]))
                         (str (get-in f [:subjekt :name]))
                         (str (:regel f))]))
       vec))

(defn ebenen-findings
  [{:keys [pfade schmutzig zweig]}]
  (when-let [ps (seq (or (seq pfade) schmutzig))]
    [(finding (if (seq pfade) :fehler :warnung) :ebene/ent-nicht-in-git
              {:klasse :einreichung :name (or zweig "HEAD")}
              {:ist  (t :change/ist-ent-in-git
                        {:n (count ps)
                         :pfade (str/join ", " (sort (take 3 ps)))})
               :soll (t :change/soll-ent-ueber-cli)
               :behebung (t :change/fix-ent-ueber-cli)})]))

(defn- befund-schlüssel
  [{:keys [regel subjekt]}]
  [regel (:klasse subjekt) (:name subjekt) (:rest subjekt)])

(defn markiere-geerbt
  [befunde basis-befunde]
  (let [alt (into #{} (map befund-schlüssel) basis-befunde)]
    (mapv (fn [b] (cond-> b (contains? alt (befund-schlüssel b)) (assoc :geerbt? true)))
          befunde)))

(defn fremde-altlast?
  [bef]
  (boolean (and (:fremd? bef) (:geerbt? bef))))

(defn blockiert?
  [findings]
  (boolean (some #(and (= :fehler (:schwere %)) (not (fremde-altlast? %))) findings)))
