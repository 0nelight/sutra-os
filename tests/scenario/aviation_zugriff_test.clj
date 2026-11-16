(ns ^:api scenario.aviation-zugriff-test
  (:require [sutra.catalog.grants :as grants]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [support.in-project :refer [install-in-project]]
            [sutra.access.principals :as pr]
            [support.aviation :refer [kanten-aus ontologie quelle]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.types.paths :as paths]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(def ^:private welt
  (-> ontologie
      (update :objects merge {:user {:attributes [[:user-id :string] [:rolle :string]]
                                     :primary-key :user-id :plural "Benutzer"}
                              :marking {:attributes [[:name :string]] :match-keys [:name]
                                        :primary-key :name :plural "Marken"}})
      (update :links conj (kanten-aus [:cleared-for :user :marking :many-to-many]
                                       :verzeichnisdienst "SICHERHEITSFREIGABE"))
      (update :actions merge
              {:benutzer-anlegen
               {:params {:id {:name "Id" :type :string :required true}
                         :rolle {:name "Rolle" :type :string :required true}}
                :rules [{:op :create :object-type :user :id :id
                         :values {:user-id [:get :id] :rolle [:get :rolle]}}]
                :edits [{:object-type :user}]}
               :marking-anlegen
               {:params {:name {:name "Name" :type :string :required true}}
                :rules [{:op :create :object-type :marking :values {:name [:get :name]}}]
                :edits [{:object-type :marking}]}
               :freigeben-fuer
               {:params {:user {:name "User" :type :user :required true}
                         :marking {:name "Marking" :type :marking :required true}}
                :rules [{:op :create-link :link-type :cleared-for
                         :from-id [:get :user :id] :to-id [:get :marking :id]}]}
               :beschreiben
               {:params {:mangel {:name "Mangel" :type :mangel :required true}
                         :text {:name "Text" :type :string :required true}}
                :rules [{:op :update :object-type :mangel :target [:get :mangel]
                         :values {:beschreibung [:get :text]}}]
                :edits [{:object-type :mangel}]}})))

(def ^:private admin (atom nil))

(defn- anmelden [u p] (-> (api/post! :login {:username u :password p}) :body :token))

(defn- tun! [action args] (api/mutate action args {:publish? true}))

(defn- benutzer! [name rolle]
  (let [id (str (java.util.UUID/randomUUID))]
    (assert (ok? (tun! :benutzer-anlegen {:id id :rolle rolle})))
    (assert (ok? (api/post! :register {:username name :password name :user-id id})))
    id))

(defn- id-von [T attr v]
  (some #(when (= v (get-in % [:values attr])) (:id %)) (:rows (:body (api/list-objects T)))))

(defn- vs! [T attr v]
  (let [r (api/mutate :apply-marking {:id (id-von T attr v) :object-type (name T) :marking "vs"}
                      {:publish? true})]
    (assert (ok? r) (pr-str (:body r)))))

(def ^:private planer (atom nil))
(def ^:private techniker (atom nil))

(defn- mit-welt [f]
  (reset! admin (anmelden grants/admin-username pr/default-admin-password))
  (binding [tx/*auth-token* @admin]
    (let [r (install-in-project {:schema welt})] (assert (ok? r) (pr-str (:body r))))
    (assert (ok? (api/post! :staging)))
    (assert (ok? (tun! :marking-anlegen {:name "vs"})))
    (let [_p (benutzer! "planer" "ontology-viewer")
          t (benutzer! "techniker" "ontology-editor")]
      (assert (ok? (tun! :freigeben-fuer {:user t :marking "vs"}))))
    (doseq [k ["D-AINA" "D-AINB"]]
      (assert (ok? (tun! :luftfahrzeug-registrieren {:kennzeichen k :muster "A320"}))))
    (doseq [[nr k] [["V-1" "D-AINA"] ["V-2" "D-AINA"] ["V-3" "D-AINB"]]]
      (assert (ok? (tun! :mangel-melden
                         {:mangel-nr nr :luftfahrzeug k :beschreibung "x" :kategorie "C"}))))
    (vs! :mangel :mangel-nr "V-2")
    (vs! :luftfahrzeug :kennzeichen "D-AINB")
    (reset! planer (anmelden "planer" "planer"))
    (reset! techniker (anmelden "techniker" "techniker"))
    (f)))

(use-fixtures :once pod/with-mem-pod-auth mit-welt)

(defn- query [spec] (tx/send! :post (paths/url :ontology-query) spec))

(defn- nummern [spec] (set (map #(get-in % [:values :mangel-nr]) (:rows (:body (query spec))))))

(deftest die-sicht-ist-die-welt-ohne-das-verborgene
  (testing "wer freigegeben ist, sieht alles"
    (tx/with-token @techniker
      (is (= #{"V-1" "V-2" "V-3"} (nummern {:find :mangel})))))

  (tx/with-token @planer
    (testing "§2.6 ein verborgenes Objekt gibt es nicht: nicht in der Liste, nicht im Filter"
      (is (= #{"V-1" "V-3"} (nummern {:find :mangel})))
      (is (empty? (nummern {:find :mangel :where {:mangel-nr "V-2"}}))))

    (testing "nicht als Ziel eines Sprungs"
      (is (empty? (:rows (:body (query {:find :mangel :where {:mangel-nr "V-3"}
                                        :around [:luftfahrzeug]}))))))

    (testing "nicht in einer Zählung"
      (is (= 2
             (get-in (:body (query {:find :mangel :aggs [{:fn :count :name :n}]}))
                     [:aggregations 0 :metrics :n]))))

    (testing "nicht in einem Rollup: offene Mängel zählt, was p sehen darf"
      (let [lfz (some #(when (= "D-AINA" (get-in % [:values :kennzeichen])) %)
                      (:rows (:body (query {:find :luftfahrzeug}))))]
        (is (= 1 (get-in lfz [:values :offene-maengel])) (pr-str lfz))))

    (testing "nicht beim Zugriff über die ID"
      (let [id (binding [tx/*auth-token* @admin] (id-von :mangel :mangel-nr "V-2"))]
        (is (empty? (:rows (:body (query {:find :mangel :where {:id id}})))))))

    (testing "über eine Objektmenge gilt dieselbe Sicht"
      (let [rid (binding [tx/*auth-token* @admin]
                  (some (fn [[r o]] (when (= :mangel (:object-type-name o)) r))
                        (:object-types (:body (api/get! :schema)))))
            t   (api/post! :object-sets-temp {:ir {:op :scan :root rid}})]
        (is (ok? t) (pr-str (:body t)))
        (is (= #{"V-1" "V-3"} (nummern {:find :mangel :object-set (get-in t [:body :rid])})))))

    (testing "nicht in der Suche"
      (is (not-any? #(= "V-2" (get-in % [:values :mangel-nr]))
                    (:rows (:body (query {:find :mangel :keywords "x"}))))))

    (testing "eine Action, die es nennt, findet es nicht — über den Schlüssel wie über die ID"
      (let [r (tun! :beschreiben {:mangel "V-2" :text "gesehen"})]
        (is (= 422 (:status r)) (pr-str (:body r)))
        (is (not (re-find #"V-2\b.*beschreibung|nachsehen" (pr-str (:body r))))))
      (let [id (binding [tx/*auth-token* @admin] (id-von :mangel :mangel-nr "V-2"))]
        (is (= 422 (:status (tun! :beschreiben {:mangel id :text "gesehen"}))))))))

(deftest schreiben-prueft-allowed
  (testing "§2.6 eine Action braucht Lesen, nicht die Operation: wer sie ausführen darf, sagen
            ihre Submission Criteria (edits only via actions)"
    (tx/with-token @planer
      (let [r (api/mutate :mangel-melden
                          {:mangel-nr "V-9" :luftfahrzeug "D-AINA" :beschreibung "x"
                           :kategorie "C"}
                          {:dry-run? true})]
        (is (ok? r) (pr-str (:body r))))))
  (testing "mit Operation und ohne Freigabe: das Unmarkierte schreibt sie, das Markierte gibt es für sie nicht"
    (let [ohne (do (binding [tx/*auth-token* @admin] (benutzer! "aushilfe" "ontology-editor"))
                   (anmelden "aushilfe" "aushilfe"))]
      (tx/with-token ohne
        (is (ok? (tun! :beschreiben {:mangel "V-1" :text "ja"})))
        (is (= 422 (:status (tun! :beschreiben {:mangel "V-2" :text "nein"})))))
      (binding [tx/*auth-token* @admin]
        (is (= "x" (get-in (some #(when (= "V-2" (get-in % [:values :mangel-nr])) %)
                                 (:rows (:body (api/list-objects :mangel))))
                           [:values :beschreibung]))))))
  (testing "wer freigegeben ist und die Operation hat, schreibt auch das Markierte"
    (tx/with-token @techniker
      (is (ok? (tun! :beschreiben {:mangel "V-2" :text "nachgesehen"})))))
  (testing "Markings setzen dürfen nur Eigentümer"
    (tx/with-token @techniker
      (is (= 403
             (:status (api/mutate :apply-marking
                                  {:id (binding [tx/*auth-token* @admin]
                                         (id-von :mangel :mangel-nr "V-1"))
                                   :object-type "mangel" :marking "vs"} {:publish? true})))))))

(deftest listen-und-query-sehen-dasselbe
  (tx/with-token @planer
    (is (= (set (map :id (:rows (:body (api/list-objects :mangel)))))
           (set (map :id (:rows (:body (query {:find :mangel})))))))
    (is (= 2 (:total (:body (api/list-objects :mangel)))) "auch die Zählung der Liste")))

(deftest ohne-anmeldung-ist-niemand
  (tx/with-token nil
    (is (#{401 403} (:status (query {:find :mangel}))))
    (is (#{401
           403} (:status (tun! :mangel-melden
                               {:mangel-nr "A-1" :luftfahrzeug "D-AINA" :beschreibung "x"
                                :kategorie "C"}))))))

(deftest rollen
  (let [uid (binding [tx/*auth-token* @admin] (benutzer! "edda" "consumer"))
        sie (anmelden "edda" "edda")]
    (testing "eine Rolle wird gelesen, nach Name und nach ID"
      (tx/with-token @admin
        (is (= "consumer" (get-in (api/get! :access-role {:user "edda"}) [:body :role])))
        (is (= "edda" (get-in (api/get! :access-role {:user uid}) [:body :username])))))
    (testing "eine Konsumentin erreicht weder Automationen noch das Verzeichnis der Principals"
      (tx/with-token sie
        (is (= 403 (:status (api/get! :automations))))
        (is (= 403 (:status (api/get! :principals))))))
    (testing "nur der Admin setzt Rollen; admin ist keine Rolle, eine unbekannte wird abgewiesen"
      (tx/with-token sie
        (is (= 403 (:status (api/post! :access-role {:user "edda"} {:role "ontology-owner"})))))
      (tx/with-token @admin
        (is (= 400 (:status (api/post! :access-role {:user "edda"} {:role "admin"}))))
        (is (= 400 (:status (api/post! :access-role {:user "edda"} {:role "chef"}))))
        (is (= 404 (:status (api/post! :access-role {:user "niemand"} {:role "consumer"}))))))
    (testing "gesetzt: vorher, nachher, in der Übersicht und im Audit"
      (tx/with-token @admin
        (let [r (:body (api/post! :access-role {:user "edda"} {:role "ontology-finder"}))]
          (is (= ["consumer" "ontology-finder" true] [(:before r) (:role r) (:changed r)])))
        (is (= "ontology-finder" (:role (some #(when (= "edda" (:username %)) %)
                                               (:principals (:body (api/get! :access)))))))
        (let [b (:body (api/get! :login-audit))
              e (some #(when (= "access-role" (:provider %)) %) (or (:events b) (:rows b) b))]
          (is (= ["edda" grants/admin-username] [(:username e) (:subject e)])))))
    (testing "die Änderung gilt mit der nächsten Anfrage, ohne neue Anmeldung"
      (tx/with-token sie
        (is (ok? (api/get! :projects)))))
    (testing "der Admin sieht das Verzeichnis: ID und Name, kein Geheimnis"
      (tx/with-token @admin
        (let [ps (get-in (api/get! :principals) [:body :principals])]
          (is (some #(= "edda" (:username %)) ps))
          (is (every? :user-id ps))
          (is (not-any? #(contains? % :pw-hash) ps)))))))

(deftest projekte-grenzen-ab
  (let [mid (binding [tx/*auth-token* @admin] (benutzer! "werkstatt" "member"))]
    (binding [tx/*auth-token* @admin]
      (is (ok? (api/mutate :create-project
                           {:name :linie :grants [{:principal mid :role :ontology-viewer}]}
                           {:stage? true})))
      (is (ok? (api/mutate :create-project {:name :ausfuhr} {:stage? true})))
      (is (ok? (api/post! :staging)))
      (is (ok? (install-in-project {:schema
                                      {:objects
                                         {:schicht
                                            {:project :linie :primary-key :titel
                                             :attributes [[:titel :string]]
                                             :plural "Schichten"}
                                          :ausfuhrdaten
                                            {:project :ausfuhr :primary-key :titel
                                             :attributes [[:titel :string]]
                                             :plural "Ausfuhrdaten"}}
                                       :actions
                                         {:schicht-anlegen
                                            {:params
                                               {:titel {:name "Titel" :type :string :required true}}
                                             :rules [{:op :create :object-type :schicht
                                                      :values {:titel [:get :titel]}}]
                                             :edits [{:object-type :schicht}]}}}})))
      (is (ok? (api/post! :staging)))
      (is (ok? (tun! :schicht-anlegen {:titel "Früh"}))))
    (tx/with-token (anmelden "werkstatt" "werkstatt")
      (testing "§2.6 im gewährten Projekt: lesen, mit Zählung"
        (is (= 1 (:total (:body (api/list-objects :schicht))))))
      (testing "im fremden Projekt: 403, über die Liste wie über die Query"
        (is (= 403 (:status (api/list-objects :ausfuhrdaten))))
        (is (= 403 (:status (query {:find :ausfuhrdaten})))))
      (testing "Leserecht genügt für eine Action ohne Kriterien — das Recht liegt an der Action"
        (is (ok? (tun! :schicht-anlegen {:titel "Spät"}))))
      (testing "was einem nicht gewährten Projekt gehört, ist nicht sichtbar"
        (is (= 403 (:status (api/list-objects :luftfahrzeug))))))

    (testing "ein Griff über die Projektgrenze braucht keinen Import — Kopplung wird gemessen"
      (binding [tx/*auth-token* @admin]
        (let [link {:links [(kanten-aus [:ausfuhr-schicht :ausfuhrdaten :schicht :many-to-many
                                         {:project :ausfuhr}]
                                        :zollsystem "AUSFUHR_SCHICHT")]
                    :sources {:zollsystem (quelle :ausfuhr "zoll"
                                                  "Ausfuhrabwicklung: welche Schicht welche
                                                   Ausfuhr bearbeitet hat.")}}
              r    (install-in-project {:schema link})]
          (is (ok? r) (pr-str (:body r)))
          (is (ok? (api/post! :staging))))))

    (testing "eine Projektänderung ohne :grants lässt sie stehen; eine leere Liste entzieht den letzten"
      (binding [tx/*auth-token* @admin]
        (is (ok? (api/mutate :update-project {:name :linie :description "Linienwartung"}
                             {:stage? true})))
        (is (ok? (api/post! :staging))))
      (tx/with-token (anmelden "werkstatt" "werkstatt")
        (is (ok? (api/list-objects :schicht))))
      (binding [tx/*auth-token* @admin]
        (is (ok? (api/mutate :update-project {:name :linie :grants []} {:stage? true})))
        (is (ok? (api/post! :staging))))
      (tx/with-token (anmelden "werkstatt" "werkstatt")
        (is (= 403 (:status (api/list-objects :schicht))))))))

(defn- installieren [schema]
  (let [r (install-in-project {:schema schema})
        c (when (ok? r) (api/post! :staging))]
    (when-not (and (ok? r) (ok? c)) (api/delete! :staging))
    {:ok? (boolean (and (ok? r) (ok? c))) :text (pr-str (:body r) (:body c)) :body (:body r)}))

(deftest projekt-tore-und-abhaengigkeiten
  (binding [tx/*auth-token* @admin]
    (testing "ein Eigentümer ist ein deklariertes Projekt — auch der eines Links, in seinen Optionen"
      (let [{:keys [ok?
                    text]}
              (installieren {:objects
                               {:spind {:project :geistesblitz :primary-key :nr
                                        :attributes [[:nr :string]]
                                        :plural "Spinde"}}})]
        (is (not ok?))
        (is (re-find #"geistesblitz.*not a declared project" text) text))
      (let [{:keys [ok?
                    text]}
              (installieren {:links [[:spind-lfz :mechaniker :luftfahrzeug :many-to-many
                                      {:project :geistesblitz}]]})]
        (is (not ok?))
        (is (re-find #"geistesblitz" text) text)))
    (testing "… ein Import nennt ein deklariertes Projekt (ein Verweis bleibt ein Verweis)"
      (let [{:keys [ok? text]} (installieren {:projects {:hangar {:imports [:geist]}}})]
        (is (not ok?))
        (is (re-find #"geist.*not a declared project" text) text)))
    (testing "Importe dürfen einen Kreis bilden — die Ontologie hat keine Ladereihenfolge"
      (let [{:keys [ok?
                    text]}
              (installieren {:projects {:k1 {:imports [:k2]} :k2 {:imports [:k3]}
                                        :k3 {:imports [:k1]}}})]
        (is ok? text)))
    (testing "zwei Projekte, die einander referenzieren, installieren — ohne ein einziges :imports"
      (let [{:keys [ok? text]} (installieren
                                {:projects {:hin {} :her {}}
                                 :objects
                                   {:anfrage {:project :hin :primary-key :nr
                                              :attributes [[:nr :string]]
                                              :plural "Anfragen"}
                                    :antwort {:project :her :primary-key :nr
                                              :attributes [[:nr :string]]
                                              :plural "Antworten"}}
                                 :links [(kanten-aus [:anfrage-antwort :anfrage :antwort
                                                      :many-to-many {:project :hin}]
                                                     :anfragesystem "ANFRAGE_ANTWORT")
                                         (kanten-aus [:antwort-anfrage :antwort :anfrage
                                                      :many-to-many {:project :her}]
                                                     :antwortsystem "ANTWORT_ANFRAGE")]
                                 :sources {:anfragesystem (quelle :hin "anfragen" "Anfragen.")
                                           :antwortsystem (quelle :her "antworten" "Antworten.")}})]
        (is ok? text))
      (let [{:keys [edges undeclared]} (:body (api/get! :project-deps))]
        (is (some #(and (= "hin" (:from %)) (= "her" (:to %))) edges) "die Kopplung ist gemessen …")
        (is (some #(and (= "her" (:from %)) (= "hin" (:to %))) edges) "… in beide Richtungen")
        (is (some #(= "hin" (:project %)) undeclared)
            "… und als Abweichung von der (leeren) Absicht genannt, nicht verweigert")))
    (testing "ein Projekt hat keine Stufe — seine Texte haben sie (die Facetten)"
      (let [{:keys [ok? text]} (installieren {:projects {:rohdaten {:stage :datasource}}})]
        (is (not ok?) "ein :stage ist kein Feld eines Projekts mehr")
        (is (re-find #"stage" text) text)))
    (testing "ein Import, der ein anderes Projekt nennt, sperrt nichts"
      (let [{:keys [ok?
                    text]}
              (installieren {:projects {:einkauf {} :planung {:imports [:hangar-x]} :hangar-x {}}
                                 :objects
                                   {:bestellung
                                      {:project :einkauf :primary-key :nr
                                       :attributes [[:nr :string]]
                                       :plural "Bestellungen"}
                                    :arbeitspaket
                                      {:project :planung :primary-key :nr
                                       :attributes [[:nr :string]]
                                       :plural "Arbeitspakete"}}
                                 :links [(kanten-aus [:paket-bestellung :arbeitspaket
                                                      :bestellung :many-to-many
                                                      {:project :planung}]
                                                     :planungssystem "PAKET_BESTELLUNG")]
                                 :sources {:planungssystem
                                           (quelle :planung "planung"
                                                   "Arbeitsvorbereitung.")}})]
        (is ok? text)))
    (testing "project deps misst die Kanten: deklariert, und ein Import, der nichts trägt"
      (is (:ok? (installieren {:projects {:einkauf {} :planung {:imports [:einkauf]}
                                          :lager {:imports [:einkauf]}}
                               :objects
                                 {:bestellung
                                    {:project :einkauf :primary-key :nr :attributes [[:nr :string]]
                                     :plural "Bestellungen"}
                                  :arbeitspaket
                                    {:project :planung :primary-key :nr :attributes [[:nr :string]]
                                     :plural "Arbeitspakete"}}
                               :links [(kanten-aus [:paket-bestellung :arbeitspaket :bestellung
                                                    :many-to-many {:project :planung}]
                                                   :planungssystem "PAKET_BESTELLUNG")
                                       (kanten-aus [:bestellung-paket-lose :bestellung
                                                    :arbeitspaket :many-to-many]
                                                   :einkaufssystem "BESTELLUNG_PAKET")]
                               :sources {:planungssystem
                                         (quelle :planung "planung"
                                                 "Arbeitsvorbereitung: Bestellungen je Paket.")
                                         :einkaufssystem
                                         (quelle :einkauf "einkauf"
                                                 "Einkauf: Pakete je Bestellung.")}})))
      (let [{:keys [edges stale undeclared without-project]} (:body (api/get! :project-deps))]
        (is (not-any? #(re-find #"bestellung-paket-lose" (pr-str %)) without-project)
            "ein Link ohne :project steht im Projekt seines Quelltyps — er ist nicht ohne Ort …")
        (is (some #(and (= "einkauf" (:project %)) (re-find #"bestellung-paket-lose" (pr-str %)))
                  undeclared)
            "… seine Reichweite nach :planung ist gemessen — als Information, nicht als Verbot")
        (is (some #(and (= "planung" (:from %)) (= "einkauf" (:to %)) (:declared %)) edges)
            (pr-str edges))
        (is (some #(= {:project "lager" :import "einkauf"} %) stale) (pr-str stale))
        (is (not-any? #(= "planung" (:project %)) undeclared) (pr-str undeclared))))))

