(ns ^:unit grammar.standard-presets-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.authoring.install :as install]
            [sutra.parser.ent-file :as ent]))

(def ^:private dir "plugins/packages/resources/presets/standard/")

(def ^:private packs
  ["1_basis.ent" "2_abgeleitet.ent" "3_privat.ent" "3_betrieb.ent" "4_einzelunternehmen.ent"])

(defn- schema [f] (:schema (ent/read-ent (str dir f))))

(defn- own-objects
  [f below]
  (let [objs (:objects (schema f))]
    (->> (apply dissoc objs (mapcat (comp keys :objects schema) below))
         (remove (fn [[_ spec]] (:action-log-of spec)))
         (remove (fn [[k _]] (re-find #"-log$" (name k))))
         (into {}))))

(deftest jedes-paket-besteht-die-install-tuer
  (doseq [f packs]
    (testing f
      (let [p (install/plan-schema (schema f) {} {})]
        (is (seq p))
        (is (not-any? #(= :failed (:outcome %)) p))))))

(deftest stufe-1-ist-vertrag-nicht-quelle
  (let [s (schema "1_basis.ent")]
    (testing "keine Quelle, kein Transform: die Basis kennt keine Herkunft"
      (is (empty? (:sources s)))
      (is (empty? (:transforms s))))
    (testing "jedes Interface wird in Stufe 2 oder 3 von mindestens einem Typ getragen"
      (let [alle  (:objects (schema "4_einzelunternehmen.ent"))
            iface (fn [i] (some (fn [[_ o]] (some #{i} (:implements o))) alle))]
        (doseq [i (keys (:interfaces s))]
          (is (iface i)
              (str i
                   " — ein Interface ohne Implementierer ist ein Vertrag, den niemand liest")))))))

(deftest stufe-2-leitet-von-stufe-1-ab
  (let [own (own-objects "2_abgeleitet.ent" ["1_basis.ent"])
        referenz #{:kategorie}]
    (doseq [[k o] own :when (not (referenz k))]
      (is (seq (:implements o))
          (str k
               " — ein abgeleiteter Typ implementiert ein Basis-Interface, sonst gehoert er in "
               "Stufe 3")))))

(deftest ausgehendes-hat-einen-idempotenten-weg-hinaus
  (let [s (schema "4_einzelunternehmen.ent")]
    (is (= #{:zahlungsdienst :x :mahnversand} (set (keys (:sources s))))
        "jeder Kanal, ueber den eine Action sendet, reist mit")
    (doseq [[k src] (:sources s)
            [wid wh] (:webhooks src)]
      (testing (str k "/" wid)
        (is (:idempotency-key wh) "eine Wiederholung darf nicht doppelt zahlen oder posten")
        (is (not (contains? src :secrets)) "Zugangsdaten nie im Paket")))))

(deftest interface-eigenschaft-als-gemeinsame-eigenschaft
  (testing "ein nackter Keyword-Eintrag in :property-list plant ohne Absturz"
    (let [p (install/plan-schema
             {:shared-properties {:bezeichnung {:type :string}}
              :interfaces        {:partei {:property-list [:bezeichnung]}}
              :objects           {:person {:implements [:partei]
                                           :attributes [[:person-id :string] :bezeichnung]
                                           :plural "Personen"}}}
             {} {})]
      (is (some #(= :person (:entity %)) p)))))

(deftest entwerfen-und-senden-kennen-dieselben-kanaele
  (testing "beitrag-entwerfen laesst genau die Kanaele zu, die beitrag-veroeffentlichen
            senden kann -- sonst entsteht ein Entwurf, der nie hinaus kann"
    (let [acts     (:actions (schema "3_privat.ent"))
          erlaubt  (get-in acts [:beitrag-entwerfen :params :kanal :constraints :one-of])
          kanal-k  (some (fn [{:keys [test pred]}]
                           (when (= [:get :beitrag :kanal] (some-> test vals first)) pred))
                         (get-in acts [:beitrag-veroeffentlichen :submission-criteria]))]
      (is (seq erlaubt) "der Parameter traegt eine Auswahlliste")
      (is (= :in (first kanal-k)) "das Sende-Kriterium prueft gegen eine Liste")
      (is (= (set erlaubt) (set (drop 2 kanal-k))))
      (is (contains? (set erlaubt) (get-in acts [:beitrag-entwerfen :params :kanal :default]))
          "die Vorgabe ist selbst erlaubt"))))

(deftest hinaus-nur-nach-freigabe
  (let [acts (:actions (schema "4_einzelunternehmen.ent"))
        stand-kriterium (fn [a] (some (fn [{:keys [test pred]}]
                                        (when (= :auftragsstand (last (first (vals test)))) pred))
                                      (:submission-criteria a)))
        sende? (fn [a] (some #(= :invoke-webhook (:op %)) (:rules a)))]
    (testing "jede Sende-Action verlangt freigegeben"
      (doseq [[k a] acts :when (sende? a)]
        (is (= [:= :s "freigegeben"] (stand-kriterium a)) (str k))))
    (testing "jeder ausgehende Typ, der als Entwurf entsteht, hat eine Freigabe aus entwurf"
      (doseq [ot [:ueberweisung :beitrag :mahnung]]
        (let [[k a] (first (filter (fn [[_ a]]
                                     (some #(and (= :update (:op %)) (= ot (:object-type %))
                                                 (= "freigegeben"
                                                    (get-in % [:values :auftragsstand])))
                                           (:rules a)))
                                   acts))]
          (is k (str ot " hat keine Freigabe-Action"))
          (when a
            (is (= [:= :s "entwurf"] (stand-kriterium a)) (str k " nur aus entwurf"))
            (is (some #(contains? (:values %) :freigegeben-am) (:rules a))
                (str k " stempelt freigegeben-am"))))))))

(defn- sende-actions
  [acts]
  (for [[k a] acts
        :let [wh (first (filter #(= :invoke-webhook (:op %)) (:rules a)))
              up (first (filter #(and (= :update (:op %))
                                      (= "gesendet" (get-in % [:values :auftragsstand])))
                                (:rules a)))]
        :when (and wh up)]
    [k (:object-type up) wh]))

(deftest jeder-freigegebene-ausgang-kann-hinaus
  (testing "ein Typ, den eine Action auf freigegeben stellt, hat auch eine Sende-Action --
            sonst bleibt die Freigabe ein Zustand, aus dem nichts mehr folgt"
    (let [acts      (:actions (schema "4_einzelunternehmen.ent"))
          freigabe  (set (for [[_ a] acts, r (:rules a)
                               :when (and (= :update (:op r))
                                          (= "freigegeben" (get-in r [:values :auftragsstand])))]
                           (:object-type r)))
          sendbar   (set (map second (sende-actions acts)))]
      (is (= #{:ueberweisung :beitrag :mahnung} freigabe))
      (doseq [ot freigabe]
        (is (contains? sendbar ot) (str ot " hat keine Sende-Action"))))))

(deftest was-gesendet-wird-setzt-der-entwurf
  (testing "jede Eigenschaft, die eine Sende-Action vom Objekt an den Webhook gibt,
            schreibt die Action, die den Entwurf anlegt -- sonst geht sie leer
            hinaus (so fehlte der Ueberweisung der Empfaengername)"
    (let [acts (:actions (schema "4_einzelunternehmen.ent"))
          entwurf-schreibt (fn [ot]
                             (into #{} (for [[_ a] acts, r (:rules a)
                                             :when (and (= :create (:op r)) (= ot (:object-type r))
                                                        (= "entwurf"
                                                           (get-in r [:values :auftragsstand])))
                                             k (keys (:values r))]
                                         k)))]
      (doseq [[k ot wh] (sende-actions acts)
              :let [gelesen (keep (fn [v] (when (and (vector? v) (= :get (first v)) (= 3 (count v)))
                                            (nth v 2)))
                                  (vals (:params wh)))]]
        (testing (str k)
          (is (seq gelesen))
          (doseq [prop gelesen]
            (is (contains? (entwurf-schreibt ot) prop)
                (str ot "." (name prop) " wird gesendet, aber kein Entwurf setzt es"))))))))
