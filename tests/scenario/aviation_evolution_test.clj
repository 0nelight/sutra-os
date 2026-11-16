(ns ^:api scenario.aviation-evolution-test
  (:require [sutra.catalog.grants :as grants]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [support.in-project :refer [install-in-project]]
            [sutra.access.principals :as pr]
            [support.aviation :refer [ontologie]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(def ^:private mangel-plus
  (update-in ontologie [:objects :mangel :attributes] into
             [[:bemerkung :string] [:reserve :decimal] [:entwurf :string]]))

(def ^:private welt
  (assoc-in mangel-plus [:actions :bemerken]
            {:params {:mangel {:name "Mangel" :type :mangel :required true}
                      :text {:name "Text" :type :string :required true}
                      :reserve {:name "Reserve" :type :decimal}}
             :rules [{:op :update :object-type :mangel :target [:get :mangel]
                      :values {:bemerkung [:get :text] :reserve [:get :reserve]}}]
             :edits [{:object-type :mangel}]}))

(defn- als-admin [f]
  (binding [tx/*auth-token* (-> (api/post! :login {:username grants/admin-username
                                                   :password pr/default-admin-password})
                                :body :token)]
    (f)))

(defn- tun! [action args] (api/mutate action args {:publish? true}))

(defn- mit-welt [f]
  (assert (ok? (install-in-project {:schema welt})))
  (assert (ok? (api/post! :staging)))
  (assert (ok? (tun! :luftfahrzeug-registrieren {:kennzeichen "D-AINA" :muster "A320"})))
  (assert (ok? (tun! :mangel-melden
                     {:mangel-nr "M-1" :luftfahrzeug "D-AINA" :beschreibung "x" :kategorie "C"})))
  (assert (ok? (tun! :bemerken {:mangel "M-1" :text "nachsehen" :reserve 1.5M})))
  (f))

(use-fixtures :once pod/with-mem-pod-auth als-admin mit-welt)

(defn- typen []
  (set (map :object-type-name (vals (:object-types (:body (api/get! :schema)))))))

(defn- links []
  (set (map :link-type-name (vals (:link-types (:body (api/get! :schema)))))))

(defn- install! [schema]
  (let [r (install-in-project {:schema schema})
        c (when (ok? r) (api/post! :staging))
        text (pr-str (:body r) (:body c))]
    (when-not (and (ok? r) (ok? c)) (api/delete! :staging))
    {:ok? (boolean (and (ok? r) (ok? c))) :text text}))

(defn- ohne-attr [schema & as]
  (update-in schema [:objects :mangel :attributes]
             (fn [xs] (vec (remove #(and (vector? %) ((set as) (first %))) xs)))))

(deftest loeschen
  (testing "§9.1 ein Link auf den Typ blockiert sein Löschen und wird genannt; der Link selbst geht frei"
    (is (:ok? (install! {:objects {:werkbank {:primary-key :nr
                                              :attributes [[:nr :string] [:kennzeichen :string]]
                                              :plural "Werkbänke"}}
                         :links [[:werkbank-lfz :werkbank :luftfahrzeug :many-to-one
                                  {:fk :kennzeichen}]]})))
    (let [r (api/delete-object-type :werkbank)]
      (is (= 409 (:status r)) (pr-str (:body r)))
      (is (re-find #"werkbank-lfz|link" (pr-str (:body r)))))
    (is (ok? (api/delete-conn-type :werkbank-lfz)))
    (testing "der erzeugte Link-Erzeuger und sein Dataset gehen mit dem Link"
      (let [s (:body (api/get! :schema))]
        (is (not-any? #(= :link-werkbank-lfz (keyword (:transform-name %))) (vals (:transforms s))))
        (is (not-any? #(= :link-werkbank-lfz (keyword (:dataset-name %))) (vals (:datasets s))))))
    (is (ok? (api/delete-object-type :werkbank)))
    (is (not (contains? (typen) :werkbank))))

  (testing "§9.1 mit Kaskade gehen die Links des Typs mit"
    (let [r (install! {:objects {:werkbank {:primary-key :nr
                                            :attributes [[:nr :string] [:kennzeichen :string]]
                                            :plural "Werkbänke"}}
                       :links [[:werkbank-lfz :werkbank :luftfahrzeug :many-to-one
                                {:fk :kennzeichen}]]})]
      (is (:ok? r) (subs (:text r) 0 (min 1500 (count (:text r))))))
    (is (ok? (api/delete-object-type :werkbank true)))
    (is (not (contains? (typen) :werkbank)))
    (is (not (contains? (links) :werkbank-lfz))))

  (testing "§9.4 ein gelöschter Typ hinterlässt nichts: derselbe Name beginnt leer"
    (is (:ok? (install! {:objects {:werkbank {:primary-key :nr :attributes [[:nr :string]]
                                              :plural "Werkbänke"}}
                         :actions
                           {:werkbank-anlegen
                              {:params {:nr {:name "Nr" :type :string :required true}}
                               :rules [{:op :create :object-type :werkbank
                                        :values {:nr [:get :nr]}}]
                               :edits [{:object-type :werkbank}]}}})))
    (is (ok? (tun! :werkbank-anlegen {:nr "W-1"})))
    (is (ok? (api/delete-action-type :werkbank-anlegen)))
    (is (ok? (api/delete-object-type :werkbank)))
    (is (:ok? (install! {:objects {:werkbank {:primary-key :nr :attributes [[:nr :string]]
                                              :plural "Werkbänke"}}})))
    (is (empty? (:rows (:body (api/list-objects :werkbank))))))

  (testing "§9.1 ein aktiver Typ ist geschützt — gegen Löschen wie gegen Umbenennen; erst abkündigen"
    (is (:ok? (install! {:objects {:werft {:primary-key :nr :attributes [[:nr :string]]
                                           :status :active
                                           :plural "Werften"}}})))
    (is (= 409 (:status (api/delete-object-type :werft))))
    (is (not (ok? (api/rename :object-type :werft :schiffswerft))))
    (is (:ok? (install! {:objects {:werft {:primary-key :nr :attributes [[:nr :string]]
                                           :status :deprecated
                                           :plural "Werften"}}})))
    (is (ok? (api/delete-object-type :werft)))
    (is (not (contains? (typen) :werft)))))

(deftest migration
  (testing "§9.4 ein Attribut ohne Edits zu entfernen ist eine gewöhnliche Änderung"
    (is (:ok? (install! (ohne-attr welt :entwurf)))))

  (testing "§9.2 ein Attribut mit Edits zu entfernen verlangt eine Migration und nennt sie"
    (is (ok? (api/delete-action-type :bemerken)))
    (let [{:keys [ok?
                  text]} (install! (-> (ohne-attr welt :entwurf :bemerkung)
                                       (update :actions dissoc :bemerken)))]
      (is (not ok?))
      (is (re-find #"bemerkung" text))
      (is (re-find #"drop-edits" text))))

  (testing "§9.4 P-7 ein Typwechsel eines bearbeiteten Attributs hat keinen Cast"
    (let [{:keys [ok? text]} (install! (-> (ohne-attr welt :entwurf :bemerkung)
                                           (update :actions dissoc :bemerken)
                                           (update-in [:objects :mangel :attributes] conj
                                                      [:bemerkung :integer])))]
      (is (not ok?))
      (is (re-find #"(?i)cast" text))))

  (testing "[:decimal p s] trägt eine Eigenschaft — eine Skala über der Präzision lehnt die Grammatik ab und erklärt das"
    (let [{:keys [ok?
                  text]} (install! (-> (ohne-attr welt :entwurf :reserve)
                                       (update :actions dissoc :bemerken)
                                           (update-in [:objects :mangel :attributes] conj
                                                      [:reserve [:decimal 4 6]])))]
      (is (not ok?))
      (is (re-find #"scale is at most the precision" text))))

  (testing "Löschen neben Hinzufügen wird als Frage gestellt: vielleicht eine Umbenennung"
    (let [{:keys [ok? text]} (install! (-> (ohne-attr welt :entwurf :bemerkung)
                                           (update :actions dissoc :bemerken)
                                           (update-in [:objects :mangel :attributes] conj
                                                      [:anmerkung :string])))]
      (is (not ok?))
      (is (re-find #"(?i)rename" text)))))

(deftest deklarationen
  (testing "eine Objekttyp-Deklaration ohne :attributes spricht für alle Attribute — das Migrationstor schützt die Edits"
    (let [{:keys [ok?
                  text]}
            (install! {:objects {:mangel {:description "Ein Befund an einem Luftfahrzeug."}}})]
      (is (not ok?))
      (is (re-find #"migration" text))))

  (testing "dieselbe Deklaration mit ihren Attributen ändert nur die Beschreibung"
    (is (:ok? (install! (assoc-in welt [:objects :mangel :description]
                                  "Ein Befund an einem Luftfahrzeug."))))
    (let [m (some #(when (= :mangel (:object-type-name %)) %)
                  (vals (:object-types (:body (api/get! :schema)))))]
      (is (= "Ein Befund an einem Luftfahrzeug." (:description m)))
      (is (= [:mangel-nr] (mapv keyword (:match-keys m))))))

  (testing "zwei Attribute, die auf dieselbe Spalte fallen, sind eines zu viel — ein ? unterscheidet nichts, ein Umlaut kommt gar nicht erst in einen API-Namen"
    (let [{:keys [ok?
                  text]}
            (install! {:objects {:pruefauftrag {:attributes [[:nr :string] [:wiederholung? :boolean]
                                                                 [:wiederholung :string]]
                                                :plural "Pruefaufträge"}}})]
      (is (not ok?))
      (is (re-find #"SAME physical column :wiederholung" text) text))
    (let [{:keys [ok?
                  text]}
            (install! {:objects {:pruefauftrag {:attributes [[:nr :string] [:fälligkeit :date]
                                                                 [:faelligkeit :string]]
                                                :plural "Pruefaufträge"}}})]
      (is (not ok?))
      (is (re-find #"spelled in ASCII" text) text))))

(defn- typ [n]
  (some #(when (= n (:object-type-name %)) %) (vals (:object-types (:body (api/get! :schema))))))

(defn- zeile [t nr]
  (some #(when (= nr (get-in % [:values :nr])) %) (:rows (:body (api/list-objects t)))))

(def ^:private werkzeug
  {:objects {:werkzeug {:primary-key :nr :attributes [[:nr :string] [:pruef_stand :string]]
                        :match-keys [:nr]
                        :plural "Werkzeuge"}}
   :actions {:werkzeug-anlegen {:params {:nr {:name "Nr" :type :string :required true}}
                                :rules [{:op :create :object-type :werkzeug
                                         :values {:nr [:get :nr]}}]
                                :edits [{:object-type :werkzeug}]}
             :werkzeug-pruefen {:params {:werkzeug {:name "Werkzeug" :type :werkzeug :required true}
                                         :stand {:name "Stand" :type :string :required true}}
                                :rules [{:op :update :object-type :werkzeug :target [:get :werkzeug]
                                         :values {:pruef_stand [:get :stand]}}]
                                :edits [{:object-type :werkzeug}]}}})

(deftest umbenennen
  (is (:ok? (install! werkzeug)))
  (is (ok? (tun! :werkzeug-anlegen {:nr "WZ-1"})))
  (is (ok? (tun! :werkzeug-pruefen {:werkzeug "WZ-1" :stand "geprüft"})))

  (testing "derselbe Spaltenname, neuer API-Name: die Edits bleiben und tragen den neuen Namen"
    (is (:ok? (install! (-> werkzeug
                            (assoc-in [:objects :werkzeug :attributes 1] [:pruef-stand :string])
                            (assoc-in [:actions :werkzeug-pruefen :rules 0 :values]
                                      {:pruef-stand [:get :stand]})))))
    (let [o (zeile :werkzeug "WZ-1")]
      (is (= "geprüft" (get-in o [:values :pruef-stand])) (pr-str o))
      (is (not (contains? (:values o) :pruef_stand))))
    (let [h (:body (api/edit-history :werkzeug))]
      (is (some #(= "geprüft" (get-in % [:edited-values :pruef-stand])) (:history h)) (pr-str h))
      (is (not (re-find #":pruef_stand" (pr-str h))))))

  (testing "ein umbenannter Typ behält RID, Objekte und Edits; Action-Parameter folgen ihm"
    (let [rid (:id (typ :werkzeug))]
      (is (ok? (api/rename :object-type :werkzeug :geraet)))
      (is (= rid (:id (typ :geraet))))
      (is (nil? (typ :werkzeug)))
      (is (= "geprüft" (get-in (zeile :geraet "WZ-1") [:values :pruef-stand])))
      (let [r (tun! :werkzeug-pruefen {:werkzeug "WZ-1" :stand "nachgeprüft"})]
        (is (ok? r) (pr-str (:body r))))
      (is (= "nachgeprüft" (get-in (zeile :geraet "WZ-1") [:values :pruef-stand])))
      (let [a (some #(when (= :werkzeug-pruefen (:action-name %)) %)
                    (vals (:actions (:body (api/get! :schema)))))]
        (is (= rid (get-in a [:params :werkzeug :type])) (pr-str a))))))

(deftest deklarationen-achsen
  (let [apu {:primary-key :seriennummer :plural "APUs"
             :attributes [:seriennummer [:nr :string]] :implements [:pruefbar]
             :match-keys [:seriennummer]}]
    (let [r (install! {:objects {:apu apu}})] (is (:ok? r) (:text r)))
    (is (seq (:implements (typ :apu))))
    (is (seq (:match-keys (typ :apu))))

    (testing "die Identitätsachse: eine ganze Deklaration ohne :match-keys nimmt sie zurück; :implements spricht nur, wenn genannt"
      (is (:ok? (install! {:objects {:apu (dissoc apu :match-keys :implements)}})))
      (is (empty? (:match-keys (typ :apu))) (pr-str (typ :apu)))
      (is (seq (:implements (typ :apu)))))

    (testing "eine leere Interface-Liste nimmt das letzte Interface weg"
      (is (:ok? (install! {:objects {:apu (assoc (dissoc apu :match-keys) :implements [])}})))
      (is (empty? (:implements (typ :apu))))
      (is (not (contains? (typ :apu) :implements))))))
