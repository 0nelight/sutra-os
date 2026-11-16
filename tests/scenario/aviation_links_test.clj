(ns ^:api scenario.aviation-links-test
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
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(def ^:private welt
  (-> ontologie
      (assoc-in [:interfaces :wartbares-teil :link-constraints 0 :required] true)
      (update-in [:objects :mangel :attributes] conj [:ursache-nr :string])
      (assoc-in [:objects :arbeitskarte]
                {:match-keys [:karten-nr]
                 :primary-key :karten-nr
                 :attributes [[:karten-nr :string] [:kennzeichen-liste [:array :string]]]
                 :plural "Arbeitskarten"})
      (update :links conj
              [:mangel-ursache :mangel :mangel :many-to-one
               {:fk :ursache-nr :forward-api-name :ursache :reverse-api-name :folgemaengel}]
              [:karte-lfz :arbeitskarte :luftfahrzeug :many-to-one
               {:fk :kennzeichen-liste :forward-api-name :flugzeuge
                :reverse-api-name :arbeitskarten}]
              [:mangel-zeuge :mangel :mechaniker :many-to-many
               {:external-source {:datasource :instandhaltungssystem :table "MANGEL_ZEUGE"
                                  :from-key-column :mangel_nr :to-key-column :lizenz}}]
              [:pruefung :mangel :wartbares-teil :many-to-many
               {:forward-api-name :gepruefte-teile :reverse-api-name :pruefbefunde
                :external-source {:datasource :instandhaltungssystem :table "PRUEFBEFUND"
                                  :from-key-column :mangel_nr :to-key-column :seriennummer}}])

      (update :actions merge
              {:mangel-umhaengen
               {:params {:mangel {:name "Mangel" :type :mangel :required true}
                         :kennzeichen {:name "Kennzeichen" :type :string :required true}}
                :rules [{:op :update :object-type :mangel :target [:get :mangel]
                         :values {:kennzeichen [:get :kennzeichen]}}]
                :edits [{:object-type :mangel}]}
               :ursache-setzen
               {:params {:mangel {:name "Mangel" :type :mangel :required true}
                         :ursache {:name "Ursache" :type :string :required true}}
                :rules [{:op :update :object-type :mangel :target [:get :mangel]
                         :values {:ursache-nr [:get :ursache]}}]
                :edits [{:object-type :mangel}]}
               :karte-anlegen
               {:params {:karten-nr {:name "Karten nr" :type :string :required true}
                         :kennzeichen {:name "Kennzeichen" :type [:array :string] :required true}}
                :rules [{:op :create :object-type :arbeitskarte
                         :values {:karten-nr [:get :karten-nr]
                                  :kennzeichen-liste [:get :kennzeichen]}}]
                :edits [{:object-type :arbeitskarte}]}
               :teil-pruefen
               {:params {:mangel {:name "Mangel" :type :mangel :required true}
                         :teil {:name "Teil" :type :wartbares-teil :required true}}
                :rules [{:op :create-link :link-type :pruefung
                         :from-id [:get :mangel :id] :to-id [:get :teil :id]}]}})))

(defonce ^:private klient (atom nil))

(defn- als-admin [f]
  (binding [tx/*auth-token* (-> (api/post! :login {:username grants/admin-username
                                                   :password pr/default-admin-password})
                                :body :token)]
    (reset! klient (client/client {:base-url (tx/current-base)
                                :username grants/admin-username
                                :password pr/default-admin-password}))
    (f)))

(defn- tun! [action args] (api/mutate action args {:publish? true}))

(defn- mit-welt [f]
  (let [r (install-in-project {:schema welt})]
    (assert (ok? r) (pr-str (:body r))))
  (assert (ok? (api/post! :staging)))
  (doseq [[k m] [["D-AINA" "A320"] ["D-AINB" "A320"] ["D-ABYA" "B747"] ["D-AXXX" "D-AINA"]]]
    (assert (ok? (tun! :luftfahrzeug-registrieren {:kennzeichen k :muster m}))))
  (f))

(use-fixtures :once pod/with-pod-auth als-admin mit-welt)

(defn- menge [t] (os/of @klient t))

(defn- werte [q attr] (set (map #(get-in % [:values attr]) (os/rows q))))

(defn- von [t attr v] (os/where (menge t) (p/= attr v)))

(defn- install! [schema]
  (let [r (install-in-project {:schema schema})
        c (when (ok? r) (api/post! :staging))
        text (pr-str (:body r) (:body c))]
    (when-not (and (ok? r) (ok? c)) (api/delete! :staging))
    {:ok? (boolean (and (ok? r) (ok? c))) :text text}))

(deftest fk-links-sind-eine-funktion-des-attributs
  (is (ok? (tun! :mangel-melden
                 {:mangel-nr "L-1" :luftfahrzeug "D-AINA" :beschreibung "x" :kategorie "C"})))

  (testing "§2.3 die Kante folgt dem FK-Wert; nichts wird nachgezogen"
    (is (= #{"D-AINA"}
           (werte (os/around (von :mangel :mangel-nr "L-1") :luftfahrzeug) :kennzeichen)))
    (is (ok? (tun! :mangel-umhaengen {:mangel "L-1" :kennzeichen "D-AINB"})))
    (is (= #{"D-AINB"}
           (werte (os/around (von :mangel :mangel-nr "L-1") :luftfahrzeug) :kennzeichen)))
    (is (= #{"L-1"}
           (werte (os/around (von :luftfahrzeug :kennzeichen "D-AINB") :maengel) :mangel-nr)))
    (is (not (contains? (werte (os/around (von :luftfahrzeug :kennzeichen "D-AINA") :maengel)
                               :mangel-nr) "L-1"))))

  (testing "ein Wert, der der Schlüssel des einen und die Beschriftung eines anderen ist, gehört dem Schlüssel allein"
    (is (ok? (tun! :mangel-umhaengen {:mangel "L-1" :kennzeichen "D-AINA"})))
    (is (= #{"D-AINA"}
           (werte (os/around (von :mangel :mangel-nr "L-1") :luftfahrzeug) :kennzeichen))))

  (testing "H6 nur der Schlüssel der Gegenseite zählt — ein Wert, der eine andere Eigenschaft trifft, verlinkt nicht"
    (is (ok? (tun! :mangel-umhaengen {:mangel "L-1" :kennzeichen "A320"})))
    (is (empty? (os/rows (os/around (von :mangel :mangel-nr "L-1") :luftfahrzeug)))))

  (testing "ein FK-Wert ohne Gegenstück ist keine Kante und kein Fehler"
    (is (ok? (tun! :mangel-umhaengen {:mangel "L-1" :kennzeichen "D-ZZZZ"})))
    (is (empty? (os/rows (os/around (von :mangel :mangel-nr "L-1") :luftfahrzeug)))))

  (testing "… und wird eine, sobald die Schlüsselseite entsteht"
    (is (ok? (tun! :luftfahrzeug-registrieren {:kennzeichen "D-ZZZZ" :muster "A321"})))
    (is (= #{"D-ZZZZ"}
           (werte (os/around (von :mangel :mangel-nr "L-1") :luftfahrzeug) :kennzeichen))))

  (testing "§8 ein abgeleiteter Link wird nicht geschrieben — das ist eine statische Ablehnung"
    (let [{:keys [ok? text]} (install!
                              {:actions {:lfz-trennen
                                         {:params
                                            {:mangel {:name "Mangel" :type :mangel :required true}
                                             :luftfahrzeug
                                               {:name "Luftfahrzeug" :type :luftfahrzeug
                                                :required true}}
                                          :rules [{:op :delete-link
                                                   :link-type :mangel-luftfahrzeug
                                                   :from-id [:get :mangel :id]
                                                   :to-id [:get :luftfahrzeug :id]}]}}})]
      (is (not ok?))
      (is (re-find #"mangel-luftfahrzeug" text) text))))

(deftest ein-array-fk-verlinkt-jeden-schluessel
  (let [r (tun! :karte-anlegen {:karten-nr "AK-1" :kennzeichen ["D-AINA" "D-ABYA" "D-QQQQ"]})]
    (is (ok? r) (pr-str (:body r))))
  (is (= #{"D-AINA" "D-ABYA"}
         (werte (os/around (von :arbeitskarte :karten-nr "AK-1") :flugzeuge) :kennzeichen)))
  (is (= #{"AK-1"}
         (werte (os/around (von :luftfahrzeug :kennzeichen "D-ABYA") :arbeitskarten) :karten-nr)))
  (is (empty? (os/rows (os/around (von :luftfahrzeug :kennzeichen "D-AINB") :arbeitskarten)))))

(deftest selbst-links-tragen-ihre-richtung-im-seitennamen
  (doseq [[nr k] [["S-1" "D-AINA"] ["S-2" "D-AINA"] ["S-3" "D-AINA"]]]
    (is (ok? (tun! :mangel-melden
                   {:mangel-nr nr :luftfahrzeug k :beschreibung "x" :kategorie "C"}))))
  (is (ok? (tun! :ursache-setzen {:mangel "S-2" :ursache "S-1"})))
  (is (ok? (tun! :ursache-setzen {:mangel "S-3" :ursache "S-1"})))

  (testing "§5.1 vorwärts und rückwärts sind zwei Seiten desselben Links"
    (is (= #{"S-1"} (werte (os/around (von :mangel :mangel-nr "S-2") :ursache) :mangel-nr)))
    (is (= #{"S-2" "S-3"}
           (werte (os/around (von :mangel :mangel-nr "S-1") :folgemaengel) :mangel-nr)))
    (is (empty? (os/rows (os/around (von :mangel :mangel-nr "S-1") :ursache)))))

  (testing "H2 ohne Seitennamen hätte ein Selbst-Link zwei gleichnamige Seiten — die Installation nennt sie"
    (let [{:keys [ok?
                  text]}
            (install! {:objects
                         {:werkstattauftrag {:attributes [[:nr :string] [:vorgaenger :string]]
                                                 :match-keys [:nr]
                                             :plural "Werkstattaufträge"}}
                           :links [[:auftrag-folge :werkstattauftrag :werkstattauftrag :many-to-one
                                    {:fk :vorgaenger}]]})]
      (is (not ok?))
      (is (re-find #"auftrag-folge" text) text)))

  (testing "§5.1 sind beide Seiten benannt, ist der Linkname kein Griff mehr"
    (let [{:keys [ok? text]} (install! (assoc-in welt [:objects :mangel :attributes]
                                                 (conj (get-in welt [:objects :mangel :attributes])
                                                       [:folgen :integer
                                                        {:derived
                                                           {:expression
                                                              [:connected-count
                                                               :mangel-ursache]}}])))]
      (is (not ok?))
      (is (re-find #"mangel-ursache" text) text))))

(deftest ein-interface-als-linkende
  (is (ok? (tun! :mangel-melden
                 {:mangel-nr "I-1" :luftfahrzeug "D-AINA" :beschreibung "x" :kategorie "C"})))
  (is (ok? (tun! :triebwerk-einbauen
                 {:seriennummer "ESN-L1" :betriebsstunden 10M :luftfahrzeug "D-AINA"})))
  (is (ok? (tun! :fahrwerk-einbauen {:seriennummer "LG-L1" :landungen 3 :luftfahrzeug "D-AINA"})))
  (is (ok? (tun! :teil-pruefen {:mangel "I-1" :teil "ESN-L1"})))
  (is (ok? (tun! :teil-pruefen {:mangel "I-1" :teil "LG-L1"})))

  (testing "§1.3 ein Linkende kann ein Interface sein: der Sprung liefert die Implementierer, jeder mit seinem Typ"
    (let [rows (os/rows (os/around (von :mangel :mangel-nr "I-1") :gepruefte-teile))]
      (is (= #{"ESN-L1" "LG-L1"} (set (map #(get-in % [:values :seriennummer]) rows))))
      (is (= 2 (count (set (map :object-type-ref rows)))))))

  (testing "der Sprung beginnt auch an einer Interface-Menge"
    (is (= #{"I-1"}
           (werte (os/around (os/where (menge :wartbares-teil) (p/= :seriennummer "ESN-L1"))
                             :pruefbefunde)
                  :mangel-nr))))

  (testing "ein Link ohne Seitennamen heißt auf beiden Seiten wie er selbst"
    (is (= [] (vec (os/rows (os/around (von :mangel :mangel-nr "I-1") :mangel-zeuge)))))
    (is (= [] (vec (os/rows (os/around (menge :mechaniker) :mangel-zeuge))))))

  (testing "und rückwärts von jedem Implementierer aus"
    (is (= #{"I-1"}
           (werte (os/around (von :fahrwerk :seriennummer "LG-L1") :pruefbefunde) :mangel-nr)))))

(deftest statische-tore
  (testing "H5 ein Implementierer ohne Link, der die Pflicht-Beschränkung erfüllt, wird abgelehnt"
    (let [{:keys [ok?
                  text]}
            (install! {:objects
                         {:apu {:primary-key :seriennummer
                                :attributes [:seriennummer :betriebsstunden [:kennzeichen :string]]
                                    :match-keys [:seriennummer]
                                    :implements [:wartbares-teil]
                                :plural "APUs"}}})]
      (is (not ok?))
      (is (re-find #"(?i)eingebaut-in" text) text)))

  (testing "H5 … und eine Kardinalität, die nicht passt, erfüllt sie nicht"
    (let [{:keys [ok?
                  text]}
            (install! {:objects
                         {:apu {:primary-key :seriennummer
                                :attributes [:seriennummer :betriebsstunden [:kennzeichen :string]]
                                    :match-keys [:seriennummer]
                                    :implements [:wartbares-teil]
                                :plural "APUs"}}
                           :links [[:apu-luftfahrzeug :luftfahrzeug :apu :many-to-one
                                    {:fk :kennzeichen :forward-api-name :apu-teile
                                     :reverse-api-name :eingebaut-in
                                     :satisfies
                                       [{:constraint :eingebaut-in
                                         :interface :wartbares-teil}]}]]})]
      (is (not ok?) text)))

  (testing "… mit dem passenden Link geht es"
    (let [{:keys [ok?
                  text]}
            (install! {:objects
                         {:apu {:primary-key :seriennummer
                                :attributes [:seriennummer :betriebsstunden [:kennzeichen :string]]
                                    :match-keys [:seriennummer]
                                    :implements [:wartbares-teil]
                                :plural "APUs"}}
                           :links [[:apu-luftfahrzeug :apu :luftfahrzeug :many-to-one
                                    {:fk :kennzeichen :forward-api-name :eingebaut-in
                                     :reverse-api-name :apus
                                     :satisfies
                                       [{:constraint :eingebaut-in
                                         :interface :wartbares-teil}]}]]})]
      (is ok? text)))

  (testing "ein als FK deklariertes Attribut ohne Link ist ein Fehler, kein stiller Wert"
    (let [{:keys [ok?
                  text]}
            (install! {:objects
                         {:werkzeugkiste
                            {:primary-key :nr
                             :attributes [[:nr :string] [:kennzeichen :string {:fk :luftfahrzeug}]]
                                 :match-keys [:nr]
                             :plural "Werkzeugkisten"}}})]
      (is (not ok?))
      (is (re-find #"(?i)no link-type connects|kennzeichen" text) text)))

  (testing "H6 ein FK-Link hat genau einen Schlüssel: keiner auf :many-to-many, keiner auf einem abgeleiteten Attribut"
    (let [{:keys [ok?
                  text]} (install! {:links [[:mangel-sichter :mangel :mechaniker :many-to-many
                                             {:fk :pruefer}]]})]
      (is (not ok?))
      (is (re-find #"many-to-many" text) text))
    (let [{:keys [ok?
                  text]} (install! {:links [[:mangel-baumuster :mangel :luftfahrzeug :many-to-one
                                             {:fk :muster}]]})]
      (is (not ok?))
      (is (re-find #"mangel-baumuster" text) text)
      (is (re-find #"muster" text) text)
      (is (re-find #":role :supp" text) text)))

  (testing "eine Verbindungstabelle (:join) gibt es nur für :many-to-many, und ein Link hat ein Backing"
    (let [{:keys [ok? text]} (install! {:links [[:mangel-sichter :mangel :mechaniker :many-to-one
                                                 {:join {:on [:pruefer :name] :from-pk :mangel-nr
                                                         :to-pk :lizenz}}]]})]
      (is (not ok?))
      (is (re-find #":join" text) text))
    (let [{:keys [ok? text]} (install! {:links [[:mangel-sichter :mangel :mechaniker :many-to-one
                                                 {:fk :pruefer
                                                  :fk-resolve
                                                    {:key :mangel-nr :as :pruefer-lizenz
                                                     :query "SELECT 1"}}]]})]
      (is (not ok?))
      (is (re-find #"one backing" text) text)))

  (testing "ein Erzeuger, der Kanten liefert, backt nur :many-to-many; ein FK-Erzeuger vergleicht nur gegen den Schlüssel"
    (let [kanten {:language :pg-sql
                  :query "SELECT m.mangel_nr AS from_id, x.lizenz AS to_id FROM m JOIN x ON m.pruefer = x.name"
                  :inputs {:m {:kind :object-type :object-type :mangel}
                           :x {:kind :object-type :object-type :mechaniker}}
                  :output {:kind :link-type :link-type :mangel-sichter
                           :from-key-column :from_id :to-key-column :to_id}}
          {:keys [ok? text]} (install! {:links [[:mangel-sichter :mangel :mechaniker :many-to-one]]
                                        :transforms {:sichter-kanten kanten}})]
      (is (not ok?))
      (is (re-find #"edge-pair join table" text) text))
    (let [{:keys [ok? text]} (install! {:links [[:mangel-sichter :mangel :mechaniker :many-to-one]]
                                        :transforms
                                          {:sichter-fk
                                             {:input {:kind :object-type :object-type :mangel}
                                              :output {:kind :link-type :link-type :mangel-sichter
                                                       :fk-attribute :pruefer
                                                       :match-column :name}}}})]
      (is (not ok?))
      (is (re-find #"match-column" text) text))
    (let [{:keys [ok? text]} (install!
                              {:links [[:mangel-sichter :mangel :mechaniker :many-to-many]]
                               :transforms
                                 {:sichter-gleich {:strategy :resolve-by-shared-property
                                                   :language :pg-sql
                                                   :query "SELECT mangel_nr AS from_id,
                                                           pruefer AS name
                                                           FROM m"
                                                   :inputs
                                                     {:m {:kind :object-type :object-type :mangel}}
                                                   :output {:kind :link-type
                                                            :link-type :mangel-sichter
                                                            :from-key-column :from_id
                                                            :match-column :name
                                                            :shared-property :seriennummer}}}})]
      (is (not ok?))
      (is (re-find #"resolve-by-shared-property" text) text)))

  (testing "der Link am Attribut ist derselbe Link wie von Hand geschrieben"
    (is (:ok? (install!
               {:objects {:werkzeugkiste {
                                          :primary-key :nr
                                          :attributes [[:nr :string]
                                                       [:kennzeichen :string
                                                        {:link {:to :luftfahrzeug
                                                                :as :standort
                                                                :reverse :werkzeugkisten}}]]
                                          :match-keys [:nr]
                                          :plural "Werkzeugkisten"}}
                :actions {:kiste-anlegen {:params {:nr {:name "Nr" :type :string :required true}
                                                   :kennzeichen
                                                     {:name "Kennzeichen" :type :string
                                                      :required true}}
                                          :rules [{:op :create :object-type :werkzeugkiste
                                                   :values {:nr [:get :nr]
                                                            :kennzeichen [:get :kennzeichen]}}]
                                          :edits [{:object-type :werkzeugkiste}]}}})))
    (is (ok? (tun! :kiste-anlegen {:nr "K-1" :kennzeichen "D-ABYA"})))
    (is (= #{"D-ABYA"} (werte (os/around (von :werkzeugkiste :nr "K-1") :standort) :kennzeichen)))
    (is (= #{"K-1"}
           (werte (os/around (von :luftfahrzeug :kennzeichen "D-ABYA") :werkzeugkisten) :nr))))

  (testing "eine Beziehung steht an einer Stelle: Zucker und ausgeschriebener Link zugleich werden abgelehnt"
    (let [{:keys [ok? text]} (install!
                              {:objects {:werkzeugkiste {:primary-key :nr
                                                         :attributes [[:nr :string]
                                                                      [:kennzeichen :string
                                                                       {:link
                                                                        {:to :luftfahrzeug
                                                                         :as :standort
                                                                         :reverse
                                                                         :werkzeugkisten}}]]
                                                         :match-keys [:nr]
                                                         :plural "Werkzeugkisten"}}
                               :links [[:werkzeugkiste-standort :werkzeugkiste :luftfahrzeug
                                        :many-to-one {:fk :kennzeichen}]]})]
      (is (not ok?) text))))
