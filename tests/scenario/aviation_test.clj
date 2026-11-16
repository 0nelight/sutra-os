(ns ^:api scenario.aviation-test
  (:require [sutra.catalog.grants :as grants]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [support.in-project :refer [install-in-project]]
            [platform-sdk.client :as client]
            [platform-sdk.objectset :as os]
            [platform-sdk.predicates :as p]
            [sutra.access.principals :as pr]
            [support.aviation :refer [ontologie]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.types.paths :as paths]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(defonce ^:private welt (atom {}))

(defn- admin-token []
  (-> (api/post! :login {:username grants/admin-username :password pr/default-admin-password})
      :body :token))

(defn- als-admin [f]
  (binding [tx/*auth-token* (admin-token)] (f)))

(use-fixtures :once pod/with-mem-pod-auth als-admin)

(defn- client []
  (or (:client @welt)
      (:client (swap! welt assoc :client
                      (client/client {:base-url (tx/current-base)
                                   :username grants/admin-username
                                   :password pr/default-admin-password})))))

(defn- menge [typ] (os/of (client) typ))

(defn- tun! [action args] (api/mutate action args {:publish? true}))

(defn- fehler [r] (str (get-in r [:body :error]) " " (pr-str (get-in r [:body :data]))))

(defn- wert [typ k v attr]
  (get-in (os/one (os/where (menge typ) (p/= k v))) [:values attr]))

(defn- werte [rows attr] (set (map #(get-in % [:values attr]) rows)))

(defn- signatur []
  (testing "§10.4 eine Vorschau schreibt nichts — auch nicht in einen späteren Lauf hinein"
    (is (ok? (install-in-project {:schema ontologie :preview? true})))
    (is (empty? (:object-types (:body (api/get! :schema))))))

  (testing "Ω_auth installiert als ein Schritt: Typen, Interface, Wertetyp, Links, Actions"
    (let [r (install-in-project {:schema ontologie})]
      (is (ok? r) (fehler r)))
    (let [r (api/post! :staging)]
      (is (ok? r) (fehler r))))

  (testing "derive: ein Implementierer trägt die Eigenschaften seines Interfaces"
    (let [schema (:body (api/get! :schema))
          tw     (some #(when (= :triebwerk (:object-type-name %)) %)
                       (vals (:object-types schema)))]
      (is (= #{:seriennummer :betriebsstunden :kennzeichen :egt-marge}
             (set (map :attribute-name (:attribute-list tw)))))))

  (testing "A2 statisch vor dynamisch: ein Titel, der auf nichts zeigt, wird beim Install abgewiesen"
    (let [r (install-in-project {:schema
                                   {:objects {:hangar {:primary-key :nr :attributes [[:nr :string]]
                                                       :title :gibt-es-nicht
                                                       :plural "Hangars"}}}
                                 :preview? true})]
      (is (seq (get-in r [:body :problems :title-key-errors])))))

  (testing "ein API-Name ist ASCII; die Ablehnung nennt die Umschrift, der Anzeigename darf alles"
    (let [r (install-in-project {:schema {:objects {:pruefung {:attributes [[:prüfer :string]]
                                                               :plural "Pruefungen"}}}})]
      (is (= 400 (:status r)))
      (is (re-find #"Write :pruefer" (pr-str (:body r)))))
    (let [r (install-in-project
             {:schema {:objects {:pruefung {:name "Prüfung"
                                            :attributes [[:pruefer :string {:name "Prüfer"}]]
                                            :plural "Prüfungen"}}}
              :preview? true})]
      (is (ok? r) (fehler r))))

  (testing "H3 referentiell: eine Action auf einen unbekannten Typ installiert nicht"
    (let [r (install-in-project
             {:schema {:actions {:phantom
                                 {:params {:x {:name "X" :type :string}}
                                  :rules [{:op :create :object-type :gibt-es-nicht
                                           :values {:a [:get :x]}}]
                                  :edits [{:object-type :gibt-es-nicht}]}}}})
          c (when (ok? r) (api/post! :staging))]
      (is (not (and (ok? r) (ok? c))))
      (api/delete! :staging))))

(defn- identitaet-und-werte []
  (testing "eine Flotte entsteht über Actions"
    (doseq [[k m] [["D-AINA" "A320"] ["D-AINB" "A320"] ["D-ABYC" "B748"]]]
      (let [r (tun! :luftfahrzeug-registrieren {:kennzeichen k :muster m})]
        (is (ok? r) (fehler r)))))

  (testing "§2.3 ein Schlüssel benennt genau ein Objekt — ein zweites mit demselben Kennzeichen ist :key-taken"
    (let [r (tun! :luftfahrzeug-registrieren {:kennzeichen "D-AINA" :muster "A321"})]
      (is (= 409 (:status r)))
      (is (= "A320" (wert :luftfahrzeug :kennzeichen "D-AINA" :muster)) "nichts überschrieben")))

  (testing "§2.4 ein Objektparameter löst über den Schlüssel wie über die id auf"
    (let [id (:id (os/one (os/where (menge :luftfahrzeug) (p/= :kennzeichen "D-AINB"))))]
      (is (ok? (tun! :triebwerk-einbauen {:seriennummer "ESN-1" :betriebsstunden 1200.1M
                                          :luftfahrzeug "D-AINA"})))
      (is (ok? (tun! :triebwerk-einbauen {:seriennummer "ESN-2" :betriebsstunden 800.2M
                                          :luftfahrzeug "D-AINA"})))
      (is (ok? (tun! :triebwerk-einbauen {:seriennummer "ESN-3" :luftfahrzeug id})))
      (is (= "D-AINB" (wert :triebwerk :seriennummer "ESN-3" :kennzeichen))))
    (let [r (tun! :triebwerk-einbauen {:seriennummer "ESN-9" :luftfahrzeug "D-ZZZZ"})]
      (is (= 422 (:status r)) "ein Kennzeichen, das niemand trägt, ist :not-found")))

  (testing "§2.1 Num ist exakt dezimal: 1200.1 + 800.2 = 2000.3, nicht 2000.2999…"
    (is (== 2000.3M (bigdec (wert :luftfahrzeug :kennzeichen "D-AINA" :triebwerksstunden)))))

  (testing "§2.1 eine Abwesenheit ist ⊥, kein leerer Wert und keine Null"
    (is (nil? (wert :triebwerk :seriennummer "ESN-3" :betriebsstunden)))
    (is (== 0 (wert :luftfahrzeug :kennzeichen "D-ABYC" :triebwerksstunden))
        "eine Summe über nichts ist 0")
    (is (nil? (wert :luftfahrzeug :kennzeichen "D-ABYC" :mittlere-stunden))
        "ein Mittelwert über nichts ist ⊥")
    (is (== 0 (wert :luftfahrzeug :kennzeichen "D-AINB" :triebwerksstunden))
        "§5.3 ⊥-Werte überspringt die Summe: D-AINB hat nur ESN-3 ohne Stunden")))

(defn- invarianten []
  (testing "L3 der Wertetyp trägt seine Einschränkung: ein Kennzeichen außerhalb der Rolle wird abgewiesen"
    (let [r (tun! :luftfahrzeug-registrieren {:kennzeichen "N12345" :muster "C172"})]
      (is (= 422 (:status r)) (str (:status r) " " (fehler r)))
      (is (nil? (os/one (os/where (menge :luftfahrzeug) (p/= :kennzeichen "N12345")))))))

  (testing "L3 one-of: eine Mangelkategorie außerhalb A/B/C wird abgewiesen"
    (let [r (tun! :mangel-melden {:mangel-nr "M-X" :luftfahrzeug "D-AINA"
                                  :beschreibung "x" :kategorie "Z"})]
      (is (= 400 (:status r)) (fehler r))))

  (testing "L3 min: Betriebsstunden sind nicht negativ"
    (let [r (tun! :triebwerk-einbauen {:seriennummer "ESN-N" :betriebsstunden -1M
                                       :luftfahrzeug "D-AINA"})]
      (is (= 422 (:status r)) (fehler r))))

  (testing "§8.4 required: ein fehlender Pflichtparameter ist 422 und schreibt nichts"
    (let [r (tun! :mangel-melden {:mangel-nr "M-Y" :luftfahrzeug "D-AINA" :kategorie "A"})]
      (is (= 422 (:status r)) (fehler r))
      (is (nil? (os/one (os/where (menge :mangel) (p/= :mangel-nr "M-Y"))))))))

(defn- actions []
  (testing "§8.2 decode: ein Datum ist streng JJJJ-MM-TT"
    (let [r (tun! :mangel-melden {:mangel-nr "M-D" :luftfahrzeug "D-AINA" :beschreibung "x"
                                  :kategorie "C" :gemeldet-am "9.10.2026"})]
      (is (= 400 (:status r)) (fehler r))))

  (testing "§8.3 Quellen: :now füllt ein leeres Datum, :current-user bindet immer"
    (let [r (tun! :mangel-melden {:mangel-nr "M-1" :luftfahrzeug "D-AINA"
                                  :beschreibung "Hydraulikleck links" :kategorie "A"
                                  :gemeldet-von "jemand-anderes"})]
      (is (ok? r) (fehler r)))
    (is (some? (wert :mangel :mangel-nr "M-1" :gemeldet-am)))
    (is (not= "jemand-anderes" (wert :mangel :mangel-nr "M-1" :gemeldet-von))
        "der Aufrufer kann den Melder nicht fälschen"))

  (testing "abgeleitete Eigenschaften folgen dem Zustand: ein offener Mangel zählt"
    (is (= 1 (wert :luftfahrzeug :kennzeichen "D-AINA" :offene-maengel)))
    (is (= "A320" (wert :mangel :mangel-nr "M-1" :muster)) "connected-one über den FK-Link"))

  (testing "§10.4 dry-run plant und schreibt nichts"
    (let [r (api/mutate :mangel-melden {:mangel-nr "M-PLAN" :luftfahrzeug "D-AINB"
                                        :beschreibung "nur geplant" :kategorie "C"}
                        {:dry-run? true})]
      (is (ok? r) (fehler r))
      (is (nil? (os/one (os/where (menge :mangel) (p/= :mangel-nr "M-PLAN")))))))

  (testing "Lebenszyklus: AOG verlangt einen offenen Mangel (Kriterium über eine abgeleitete Eigenschaft)"
    (let [r (tun! :aog-erklaeren {:luftfahrzeug "D-AINB"})]
      (is (= 422 (:status r)))
      (is (re-find #"Ohne offenen Mangel" (str (get-in r [:body :error])))))
    (is (ok? (tun! :aog-erklaeren {:luftfahrzeug "D-AINA"})))
    (is (= "aog" (wert :luftfahrzeug :kennzeichen "D-AINA" :status))
        "der Übergang setzt den Zustand, obwohl die Regel :status nicht schreibt"))

  (testing "Lebenszyklus: aus :aog führt kein zweites AOG"
    (is (not (ok? (tun! :aog-erklaeren {:luftfahrzeug "D-AINA"})))))

  (testing "§8.4 Kriterien in Reihenfolge, das erste falsche nennt seine Nachricht"
    (is (ok? (tun! :mechaniker-anlegen {:lizenz "B1-747" :name "Knut" :muster "B748"})))
    (is (ok? (tun! :mechaniker-anlegen {:lizenz "B1-320" :name "Pia" :muster "A320"})))
    (let [r (tun! :mangel-beheben {:mangel "M-1" :mechaniker "B1-747"})]
      (is (= 422 (:status r)))
      (is (re-find #"nicht berechtigt" (str (get-in r [:body :error])))))
    (is (= "offen" (wert :mangel :mangel-nr "M-1" :zustand))
        "abgewiesen heißt: nichts geschrieben"))

  (testing "Freigabe ist gesperrt, solange ein Mangel offen ist"
    (let [r (tun! :freigeben {:luftfahrzeug "D-AINA"})]
      (is (= 422 (:status r)))
      (is (re-find #"Offene Mängel" (str (get-in r [:body :error]))))))

  (testing "die berechtigte Mechanikerin behebt; danach greift das erste Kriterium"
    (is (ok? (tun! :mangel-beheben {:mangel "M-1" :mechaniker "B1-320"})))
    (is (= "B1-320" (wert :mangel :mangel-nr "M-1" :behoben-von)))
    (is (= "Pia" (wert :mangel :mangel-nr "M-1" :pruefer)))
    (let [r (tun! :mangel-beheben {:mangel "M-1" :mechaniker "B1-320"})]
      (is (re-find #"nicht offen" (str (get-in r [:body :error]))))))

  (testing "Freigabe: Übergang zurück in Betrieb, mit Aktionsprotokoll"
    (is (ok? (tun! :freigeben {:luftfahrzeug "D-AINA"})))
    (is (= "in-betrieb" (wert :luftfahrzeug :kennzeichen "D-AINA" :status)))
    (is (= 1 (os/total (menge :freigeben-log)))))

  (testing "§8.7 ein :multiple-Parameter ist ein implizites for — leer ist kein Löschen-aller"
    (doseq [n ["M-2" "M-3" "M-4"]]
      (is (ok? (tun! :mangel-melden {:mangel-nr n :luftfahrzeug "D-ABYC"
                                     :beschreibung "Kabine" :kategorie "C"
                                     :mel-faehig? (= n "M-2")}))))
    (is (= #{"M-2"} (werte (os/rows (os/where (menge :mangel) (p/= :mel-faehig? true))) :mangel-nr))
        "ein ?-Attribut liest und filtert unter seinem API-Namen")
    (is (ok? (tun! :maengel-zurueckstellen {:maengel []})))
    (is (= 3 (wert :luftfahrzeug :kennzeichen "D-ABYC" :offene-maengel)))
    (is (ok? (tun! :maengel-zurueckstellen {:maengel ["M-2" "M-3"]})))
    (is (= 1 (wert :luftfahrzeug :kennzeichen "D-ABYC" :offene-maengel)))))

(defn- nummern [q] (werte (os/rows q) :mangel-nr))

(defn- objektmengen []
  (let [c-maengel (os/where (menge :mangel) (p/= :kategorie "C"))
        offene    (os/where (menge :mangel) (p/= :zustand "offen"))]
    (testing "§3.3 filter: die Mitglieder, die das Prädikat erfüllen"
      (is (= #{"M-2" "M-3" "M-4"} (nummern c-maengel))))

    (testing "§3.3 around: ein Sprung liefert den fernen Typ, und nur, was verbunden ist"
      (let [{:keys [object-type rows]}
            (os/run (os/around (os/where (menge :luftfahrzeug) (p/= :muster "A320")) :maengel))]
        (is (= :mangel object-type))
        (is (= #{"M-1"} (werte rows :mangel-nr))))
      (is (empty? (os/rows (os/around (os/where (menge :luftfahrzeug) (p/= :kennzeichen "D-AINB"))
                                      :maengel)))
          "ein Luftfahrzeug ohne Mängel springt in die leere Menge"))

    (testing "§3.3 around ist eine Mengenoperation: dreimal erreicht, einmal enthalten"
      (is (= ["D-ABYC" "D-AINA"]
             (sort (map #(get-in % [:values :kennzeichen])
                        (os/rows (os/around (menge :mangel) :luftfahrzeug)))))))

    (testing "§3.3 union, intersect, subtract"
      (is (= #{"M-2" "M-3" "M-4"} (nummern (os/union c-maengel offene))))
      (is (= #{"M-4"} (nummern (os/intersect c-maengel offene))))
      (is (= #{"M-2" "M-3"} (nummern (os/subtract c-maengel offene)))))

    (testing "§3.2 ein Interface ist die Vereinigung seiner Implementierer, konkret typisiert"
      (is (ok? (tun! :fahrwerk-einbauen {:seriennummer "LG-1" :landungen 4000
                                         :luftfahrzeug "D-ABYC"})))
      (let [rows (os/rows (menge :wartbares-teil))]
        (is (= #{"ESN-1" "ESN-2" "ESN-3" "LG-1"} (werte rows :seriennummer)))))

    (testing "§5.2 Ordnung: ⊥ steht aufsteigend zuletzt, absteigend zuerst"
      (let [geordnet #(mapv (fn [r] (get-in r [:values :mangel-nr]))
                            (os/rows (os/order (menge :mangel) :behoben-von %)))]
        (is (= "M-1" (first (geordnet :asc))))
        (is (= "M-1" (last (geordnet :desc))))))

    (testing "§5.2 Seiten über einer Ordnung zerlegen die Menge ohne Lücke und Doppel"
      (let [seiten (os/pages (os/order (menge :mangel) :kategorie) 3)]
        (is (= [3 1] (mapv count seiten)))
        (is (= #{"M-1" "M-2" "M-3" "M-4"} (set (mapcat #(werte % :mangel-nr) seiten)))))
      (is (= 3 (os/total (os/limit c-maengel 1))) "die Anzahl zählt vor dem Fenster"))

    (testing "§6 Aggregate: Gruppen und Metriken"
      (let [je-kategorie (fn [q]
                           (into {} (map (juxt #(get-in % [:group :kategorie])
                                               #(get-in % [:metrics :n])))
                                 (:aggregations (os/run q))))]
        (is (= {"A" 1 "C" 3}
               (je-kategorie (os/aggregate (menge :mangel) [{:fn :count :name :n}] [:kategorie])))))
      (let [stunden (into {} (map (juxt #(get-in % [:group :kennzeichen])
                                        #(get-in % [:metrics :h])))
                          (:aggregations
                           (os/run (os/aggregate (menge :triebwerk)
                                                 [{:fn :sum :attr :betriebsstunden :name :h}]
                                                 [:kennzeichen]))))]
        (is (== 2000.3M (bigdec (stunden "D-AINA"))) "exakt summiert")
        (is (== 0 (stunden "D-AINB")) "die Summe über keine Werte ist 0, wie beim Rollup")))

    (testing "ein Interface ohne Implementierer ist die leere Menge, keine Ablehnung"
      (let [r (tx/send! :post (paths/url :ontology-query) {:find :pruefbar})]
        (is (ok? r))
        (is (= [] (get-in r [:body :rows])))))

    (testing "explain beschreibt den Read, statt ihn auszuführen, und nennt den Typ, den er liefert"
      (let [ex (fn [spec]
                 (:body (tx/send! :post (str (paths/url :ontology-query) "?explain=true") spec)))
            sprung (ex {:find :luftfahrzeug :around [:maengel] :limit 5})]
        (is (= {:max-hops 3 :max-page 10000 :max-static-members 10000}
               (select-keys (:caps sprung) [:max-hops :max-page :max-static-members]))
            "die Grenzen eines Reads stehen im Plan …")
        (is (= 400 (:status (tx/send! :post (paths/url :ontology-query)
                                      {:find :luftfahrzeug
                                       :around [:maengel :luftfahrzeug :maengel :luftfahrzeug]})))
            "… und werden durchgesetzt: ein vierter Sprung ist eine Ablehnung")
        (is (= 400
               (:status (tx/send! :post (paths/url :ontology-query) {:find :mangel :limit 10001}))))
        (is (nil? (:rows sprung)))
        (is (= (:root (ex {:find :mangel})) (:root sprung))
            "ein Sprung liest den fernen Typ — daran prüft die Plattform das Leserecht")
        (is (= 1 (get-in sprung [:estimate :hops])))))

    (testing "die Instanzsuche findet über alle Typen, ohne Groß/Klein-Unterschied"
      (let [treffer (fn [q] (->> (:body (tx/send! :get (str (paths/url :instances-search) "?q=" q)))
                                 :objects vals (apply concat)
                                 (map #(or (get-in % [:values :mangel-nr])
                                           (get-in % [:values :kennzeichen])))
                                 set))]
        (is (= #{"M-1"} (treffer "hydraulik")))
        (is (= (treffer "d-ain") (treffer "D-AIN")))
        (is (contains? (treffer "D-AIN") "D-AINA"))))

    (testing "die Instanzsuche gibt je Typ höchstens 50 Zeilen und zählt den Rest — ein Typ mit vielen Treffern verdrängt keinen mit wenigen"
      (let [triebwerke (for [i (range 60)] {:seriennummer (str "ESN-S-" i) :luftfahrzeug "D-ABYC"})]
        (is (ok? (api/mutate-batch :triebwerk-einbauen triebwerke {:publish? true}))
            "nur eine ungebatchte Funktion ist auf 20 Aufrufe begrenzt"))
      (is (ok? (tun! :fahrwerk-einbauen {:seriennummer "ESN-S-FW" :luftfahrzeug "D-ABYC"})))
      (let [gruppen (:groups (:body (tx/send! :get
                                              (str (paths/url :instances-search) "?q=esn-s"))))]
        (is (= [[1 1] [60 50]]
               (mapv (fn [{:keys [count rows]}] [count (clojure.core/count rows)]) gruppen)))))))

(defn- gespeichert [name & [args]]
  (let [r (tx/send! :post (paths/url :ontology-query)
                    (cond-> {:object-set name :find :mangel} args (assoc :object-set-args args)))]
    (if (ok? r) (werte (:rows (:body r)) :mangel-nr) (:status r))))

(defn- gespeicherte-mengen []
  (testing "§7 eine gespeicherte Menge installiert wie jede Deklaration"
    (let [r (api/post! :install
                       {:schema {:object-sets
                                 {:offene-maengel
                                  {:root :mangel :where [:= :zustand "offen"]
                                   :description "Was noch zu tun ist."}
                                  :maengel-der-kategorie
                                  {:root :mangel :where [:= :kategorie [:param :kat]]
                                   :param-slots [{:name :kat :type :string}]}}}})]
      (is (ok? r) (fehler r)))
    (is (ok? (api/post! :staging))))

  (testing "§7.1 sie ist über ihren Namen adressierbar und bedeutet ihre Definition"
    (is (= #{"M-4"} (gespeichert :offene-maengel)))
    (is (= (nummern (os/where (menge :mangel) (p/= :zustand "offen")))
           (gespeichert :offene-maengel))))

  (testing "§7.1 und über ihre RID"
    (let [rid (some (fn [[r e]] (when (= :offene-maengel (keyword (name (:object-set-name e)))) r))
                    (:object-sets (:body (api/get! :schema))))]
      (is (some? rid))
      (is (= #{"M-4"} (gespeichert rid)))))

  (testing "§7.4 ein Template füllt seine Slots; eine Liste heißt: einer davon"
    (is (= #{"M-2" "M-3" "M-4"} (gespeichert :maengel-der-kategorie {:kat "C"})))
    (is (= #{"M-1" "M-2" "M-3" "M-4"} (gespeichert :maengel-der-kategorie {:kat ["A" "C"]})))
    (is (= #{} (gespeichert :maengel-der-kategorie {:kat []}))
        "die leere Liste ist die leere Menge"))

  (testing "ein fehlendes oder unbekanntes Argument ist eine Ablehnung, kein Leerwert"
    (is (#{400 422} (gespeichert :maengel-der-kategorie)))
    (is (#{400 422} (gespeichert :maengel-der-kategorie {:kat "C" :zustand "offen"}))))

  (testing "ein Schlüssel, den die Form nicht kennt, installiert nicht"
    (let [r (install-in-project {:schema
                                   {:object-sets {:kaputt {:root :mangel :via :luftfahrzeug}}}})
          c (when (ok? r) (api/post! :staging))]
      (is (not (and (ok? r) (ok? c))))
      (api/delete! :staging)))

  (testing "§6.1 eine gespeicherte Menge, die eine Näherung fordert, wird nicht still exakt beantwortet"
    (is (ok? (install-in-project {:schema {:object-sets {:ungefaehr {:root :mangel :aggregate :count
                                                                     :accuracy :approximate}}}})))
    (is (ok? (api/post! :staging)))
    (is (#{400 422} (gespeichert :ungefaehr))))

  (testing "eine unbekannte Menge ist eine Ablehnung"
    (is (#{400 404} (gespeichert :gibt-es-nicht)))))

(defn- mit-attribut [schema typ attr]
  (update-in schema [:objects typ :attributes] conj attr))

(defn- schema-evolution []
  (testing "§9.4 ein neues Attribut ist auf bestehenden Objekten ⊥"
    (let [neu (mit-attribut ontologie :luftfahrzeug [:standort :string])]
      (is (ok? (install-in-project {:schema neu})))
      (is (ok? (api/post! :staging))))
    (is (nil? (wert :luftfahrzeug :kennzeichen "D-AINA" :standort))))

  (testing "§9.1 retire: ein Typ, auf den eine Action zeigt, wird nicht gelöscht"
    (let [r (api/delete-object-type :mechaniker)]
      (is (= 409 (:status r)) (fehler r)))
    (is (= 2 (os/total (menge :mechaniker))) "und seine Objekte bleiben"))

  (testing "§9.1 ein unbenutzter Typ geht"
    (is (ok? (install-in-project {:schema
                                    {:objects {:hangar {:primary-key :nr :attributes [[:nr :string]]
                                                        :plural "Hangars"}}}})))
    (is (ok? (api/post! :staging)))
    (is (ok? (api/delete-object-type :hangar)))
    (is (not (some #(= :hangar (:object-type-name %))
                   (vals (:object-types (:body (api/get! :schema)))))))))

(defn- branches []
  (let [zweig (str "c-check-" (subs (str (java.util.UUID/randomUUID)) 0 8))]
    (testing "life: ein Zweig gabelt den Stand von main"
      (is (ok? (api/post! :branches {:parent "main" :name zweig}))))

    (tx/with-branch zweig
    (testing "was auf dem Zweig geschieht, sieht main nicht"
      (is (ok? (tun! :mangel-melden {:mangel-nr "M-Z1" :luftfahrzeug "D-AINB"
                                     :beschreibung "nur im Zweig" :kategorie "B"})))
      (is (some #(= "M-Z1" (get-in % [:values :mangel-nr]))
                (:rows (:body (api/list-objects :mangel))))
          "der Zweig liest seine eigene Schreibung")
      (is (nil? (os/one (os/where (menge :mangel) (p/= :mangel-nr "M-Z1"))))
          "main nicht")
      (is (ok? (install-in-project {:schema
                                      {:objects
                                         {:werkstattplatz
                                            {:primary-key :nr :attributes [[:nr :string]]
                                             :plural "Werkstattplätze"}}}})))
      (is (ok? (api/post! :staging)))
      (is (not (some #(= :werkstattplatz (:object-type-name %))
                     (vals (:object-types (:body (client/request (client) :get :schema)))))))))

    (testing "ein Proposal trägt den Zweig nach main"
      (let [r (api/post! :proposals {:source zweig :target "main" :title "Werkstattplatz"})]
        (is (ok? r) (fehler r))
        (is (seq (get-in r [:body :preview :diff])))
        (let [m (api/post! :proposal-merge {:id (get-in r [:body :id])} nil)]
          (is (ok? m) (fehler m)))))
    (is (some #(= :werkstattplatz (:object-type-name %))
              (vals (:object-types (:body (api/get! :schema))))))))

(defn- zugriff []
  (let [mech-id (str (java.util.UUID/randomUUID))]
    (testing "§2.6 Projekte grenzen ab: wer nur die Wartung sieht, sieht keine Exportkontrolle"
      (api/mutate :create-object-type {:name :user :project :flotte
                                       :attributes [[:user-id :string] [:rolle :string]]
                                       :primary-key :user-id :plural "Users"}
                  {:stage? true})
      (is (ok? (api/post! :staging)))
      (is (ok? (install-in-project {:schema {:objects
                                             {:user {:attributes [[:user-id :string]
                                                                  [:rolle :string]]
                                                     :primary-key :user-id
                                                     :plural "Users"}}}})))
      (is (ok? (api/mutate :create-project {:name :wartung
                                            :grants [{:principal mech-id :role :ontology-viewer}]}
                           {:stage? true})))
      (is (ok? (api/mutate :create-project {:name :exportkontrolle} {:stage? true})))
      (is (ok? (api/post! :staging)))
      (is (ok? (api/post! :install
                          {:schema {:objects {:arbeitskarte {:project :wartung
                                                             :primary-key :titel
                                                             :attributes [[:titel :string]]
                                                             :plural "Arbeitskarten"}
                                              :ausfuhrdaten {:project :exportkontrolle
                                                             :primary-key :titel
                                                             :attributes [[:titel :string]]
                                                             :plural "Ausfuhrdaten"}}}})))
      (is (ok? (api/post! :staging)))
      (is (ok? (api/post! :register {:username "pia" :password "pia" :user-id mech-id})))
      (let [tok (-> (api/post! :login {:username "pia" :password "pia"}) :body :token)]
        (is (some? tok))
        (tx/with-token tok
          (is (ok? (api/list-objects :arbeitskarte)))
          (is (= 403 (:status (api/list-objects :ausfuhrdaten)))))))))

(deftest wartungsbetrieb
  (signatur)
  (identitaet-und-werte)
  (invarianten)
  (actions)
  (objektmengen)
  (gespeicherte-mengen)
  (schema-evolution)
  (branches)
  (zugriff))
