(ns support.aviation
  (:require [support.in-project :refer [in-project]]))

(def ^:private wartung
  {:value-types
   {:kennzeichen {:type :string
                  :constraints {:regex "D-[A-Z]{4}"}
                  :description "Luftfahrzeugkennzeichen der deutschen Rolle"}}

   :shared-properties
   {:seriennummer    {:name "Seriennummer" :type :string}
    :betriebsstunden {:name "Betriebsstunden" :type :decimal :constraints {:min 0}}}

   :interfaces
   {:pruefbar
    {:description "Was eine Prüfung braucht; noch implementiert es niemand."
     :property-list [:seriennummer]}
    :wartbares-teil
    {:description "Was eine Seriennummer hat, Stunden sammelt und in einem Luftfahrzeug sitzt."
     :property-list [:seriennummer :betriebsstunden]
     :link-constraints [{:link-name :eingebaut-in :to-object-type :luftfahrzeug
                         :cardinality :many-to-one}]}}

   :objects
   {:luftfahrzeug
    {:name "Luftfahrzeug" :plural "Luftfahrzeuge" :title :kennzeichen :match-keys [:kennzeichen]
     :primary-key :kennzeichen
     :lifecycle {:property :status
                 :states [:in-betrieb :aog]
                 :transitions [{:action :aog-erklaeren :from [:in-betrieb] :to :aog}
                               {:action :freigeben :from [:aog] :to :in-betrieb}]}
     :attributes [[:kennzeichen :string {:value-type :kennzeichen}]
                  [:muster :string]
                  [:status :string]
                  [:offene-maengel :integer
                   {:derived {:expression [:connected-count :maengel [:= :zustand "offen"]]}}]
                  [:triebwerksstunden :decimal
                   {:derived {:expression [:connected-sum :triebwerke :betriebsstunden]}}]
                  [:mittlere-stunden :decimal
                   {:derived {:expression [:connected-avg :triebwerke :betriebsstunden]}}]]}

    :triebwerk
    {:name "Triebwerk" :plural "Triebwerke" :title :seriennummer :match-keys [:seriennummer]
     :primary-key :seriennummer
     :implements [:wartbares-teil]
     :attributes [:seriennummer :betriebsstunden [:kennzeichen :string]
                  [:egt-marge :decimal
                   {:description "Abgastemperatur-Reserve in K; negativ heißt: über dem Limit."}]]}

    :fahrwerk
    {:name "Fahrwerk" :plural "Fahrwerke" :title :seriennummer :match-keys [:seriennummer]
     :primary-key :seriennummer
     :implements [:wartbares-teil]
     :attributes [:seriennummer :betriebsstunden [:kennzeichen :string] [:landungen :integer]]}

    :mangel
    {:name "Mangel" :plural "Mängel" :title :mangel-nr :match-keys [:mangel-nr]
     :primary-key :mangel-nr
     :attributes [[:mangel-nr :string]
                  [:kennzeichen :string]
                  [:beschreibung :string]
                  [:kategorie :string {:constraints {:one-of ["A" "B" "C"]}}]
                  [:zustand :string]
                  [:gemeldet-am :date]
                  [:gemeldet-von :string]
                  [:behoben-von :string]
                  [:mel-faehig? :boolean
                   {:name "MEL-fähig?"
                    :description "Darf nach der Mindestausrüstungsliste zurückgestellt werden."}]
                  [:pruefer :string {:name "Prüfer"}]
                  [:muster :string
                   {:derived {:expression [:connected-one :luftfahrzeug :muster]}}]]}

    :mechaniker
    {:name "Mechaniker" :plural "Mechaniker" :title :name :match-keys [:lizenz]
     :primary-key :lizenz
     :attributes [[:lizenz :string] [:name :string] [:muster :string]]}}

   :links
   [[:mangel-luftfahrzeug :mangel :luftfahrzeug :many-to-one
     {:fk :kennzeichen :forward-api-name :luftfahrzeug :reverse-api-name :maengel}]
    [:triebwerk-luftfahrzeug :triebwerk :luftfahrzeug :many-to-one
     {:fk :kennzeichen :forward-api-name :eingebaut-in :reverse-api-name :triebwerke
      :satisfies [{:constraint :eingebaut-in :interface :wartbares-teil}]}]
    [:fahrwerk-luftfahrzeug :fahrwerk :luftfahrzeug :many-to-one
     {:fk :kennzeichen :forward-api-name :eingebaut-in :reverse-api-name :fahrwerke
      :satisfies [{:constraint :eingebaut-in :interface :wartbares-teil}]}]
    [:berechtigung :mechaniker :luftfahrzeug :many-to-many
     {:forward-api-name :berechtigt-fuer :reverse-api-name :berechtigte}]]

   :datasets
   {:freigabeberechtigungen
    {:kind :standalone
     :description "Berechtigungsregister des Qualitätsmanagements (Part-145), vom QM
                   hochgeladen: je Zeile darf ein Freigabeberechtigter ein Luftfahrzeug
                   freigeben."
     :columns [{:attribute-name :lizenz :attribute-type :string}
               {:attribute-name :kennzeichen :attribute-type :string}]}}

   :transforms
   {:berechtigung-kanten
    {:language :clojure
     :code "(fn [{:keys [register]}] register)"
     :inputs {:register {:kind :dataset :dataset :freigabeberechtigungen}}
     :output {:kind :link-type :link-type :berechtigung
              :from-key-column :lizenz :to-key-column :kennzeichen}}}

   :sources
   {:instandhaltungssystem
    {:type :postgres
     :description "M&E-System des Betriebs: Mangelmeldungen samt Zeugen, Prüfbefunde je
                   Bauteil und die Werkzeugausgabe je Luftfahrzeug."
     :jdbc-url "jdbc:postgresql://mro:5432/mro" :egress ["mro:5432"]}
    :verzeichnisdienst
    {:type :postgres
     :description "Verzeichnisdienst des Betriebs: Benutzer, Gruppen, ihre Mitgliedschaften
                   und die Sicherheitsfreigaben je Benutzer."
     :jdbc-url "jdbc:postgresql://verzeichnis:5432/iam" :egress ["verzeichnis:5432"]}}

   :actions
   {:luftfahrzeug-registrieren
    {:params {:kennzeichen {:name "Kennzeichen" :type :string :required true}
              :muster      {:name "Muster" :type :string :required true}}
     :rules  [{:op :create :object-type :luftfahrzeug
               :values {:kennzeichen [:get :kennzeichen] :muster [:get :muster]
                        :status "in-betrieb"}}]
     :edits  [{:object-type :luftfahrzeug}]}

    :triebwerk-einbauen
    {:params {:seriennummer    {:name "Seriennummer" :type :string :required true}
              :betriebsstunden {:name "Betriebsstunden" :type :decimal}
              :egt-marge       {:name "EGT-Marge" :type :decimal}
              :luftfahrzeug    {:name "Luftfahrzeug" :type :luftfahrzeug :required true}}
     :rules  [{:op :create :object-type :triebwerk
               :values {:seriennummer [:get :seriennummer]
                        :betriebsstunden [:get :betriebsstunden]
                        :egt-marge [:get :egt-marge]
                        :kennzeichen [:get :luftfahrzeug :kennzeichen]}}]
     :edits  [{:object-type :triebwerk}]}

    :fahrwerk-einbauen
    {:params {:seriennummer {:name "Seriennummer" :type :string :required true}
              :landungen    {:name "Landungen" :type :integer}
              :luftfahrzeug {:name "Luftfahrzeug" :type :luftfahrzeug :required true}}
     :rules  [{:op :create :object-type :fahrwerk
               :values {:seriennummer [:get :seriennummer] :landungen [:get :landungen]
                        :kennzeichen [:get :luftfahrzeug :kennzeichen]}}]
     :edits  [{:object-type :fahrwerk}]}

    :mechaniker-anlegen
    {:params {:lizenz {:name "Lizenz" :type :string :required true}
              :name   {:name "Name" :type :string :required true}
              :muster {:name "Muster" :type :string :required true}}
     :rules  [{:op :create :object-type :mechaniker
               :values {:lizenz [:get :lizenz] :name [:get :name] :muster [:get :muster]}}]
     :edits  [{:object-type :mechaniker}]}

    :berechtigen
    {:params {:mechaniker   {:name "Mechaniker" :type :mechaniker :required true}
              :luftfahrzeug {:name "Luftfahrzeug" :type :luftfahrzeug :required true}}
     :rules  [{:op :create-link :link-type :berechtigung
               :from-id [:get :mechaniker :id] :to-id [:get :luftfahrzeug :id]}]}

    :berechtigung-entziehen
    {:params {:mechaniker   {:name "Mechaniker" :type :mechaniker :required true}
              :luftfahrzeug {:name "Luftfahrzeug" :type :luftfahrzeug :required true}}
     :rules  [{:op :delete-link :link-type :berechtigung
               :from-id [:get :mechaniker :id] :to-id [:get :luftfahrzeug :id]}]}

    :mangel-melden
    {:params {:mangel-nr    {:name "Mangelnummer" :type :string :required true}
              :luftfahrzeug {:name "Luftfahrzeug" :type :luftfahrzeug :required true}
              :beschreibung {:name "Beschreibung" :type :string :required true}
              :kategorie    {:name "Kategorie" :type :string :required true
                             :constraints {:one-of ["A" "B" "C"]}}
              :gemeldet-am  {:name "Gemeldet am" :type :date :prefill :now}
              :gemeldet-von {:type :string :exposure :hidden :prefill :current-user}
              :mel-faehig?  {:name "MEL-fähig?" :type :boolean}}
     :rules  [{:op :create :object-type :mangel
               :values {:mangel-nr [:get :mangel-nr]
                        :kennzeichen [:get :luftfahrzeug :kennzeichen]
                        :beschreibung [:get :beschreibung]
                        :kategorie [:get :kategorie]
                        :zustand "offen"
                        :gemeldet-am [:get :gemeldet-am]
                        :gemeldet-von [:get :gemeldet-von]
                        :mel-faehig? [:get :mel-faehig?]}}]
     :edits  [{:object-type :mangel}]}

    :aog-erklaeren
    {:on     :luftfahrzeug
     :params {:luftfahrzeug {:name "Luftfahrzeug" :type :luftfahrzeug :required true}}
     :submission-criteria
     [{:message "Ohne offenen Mangel ist ein Luftfahrzeug nicht AOG."
       :test {:n [:get :luftfahrzeug :offene-maengel]}
       :pred [:> :n 0]}]
     :rules  [{:op :update :object-type :luftfahrzeug :target [:get :luftfahrzeug]
               :values {}}]
     :edits  [{:object-type :luftfahrzeug}]}

    :mangel-beheben
    {:params {:mangel     {:name "Mangel" :type :mangel :required true}
              :mechaniker {:name "Mechaniker" :type :mechaniker :required true}}
     :submission-criteria
     [{:message "Der Mangel ist nicht offen."
       :test {:z [:get :mangel :zustand]}
       :pred [:= :z "offen"]}
      {:message "Der Mechaniker ist für dieses Muster nicht berechtigt."
       :test {:m [:get :mechaniker :muster] :a [:get :mangel :muster]}
       :pred [:= :m :a]}]
     :rules  [{:op :update :object-type :mangel :target [:get :mangel]
               :values {:zustand "behoben" :behoben-von [:get :mechaniker :lizenz]
                        :pruefer [:get :mechaniker :name]}}]
     :edits  [{:object-type :mangel}]}

    :maengel-zurueckstellen
    {:params {:maengel {:name "Mängel" :type :mangel :multiple true :required true}}
     :rules  [{:op :update :object-type :mangel :target [:get :maengel]
               :values {:zustand "zurueckgestellt"}}]
     :edits  [{:object-type :mangel}]}

    :freigeben
    {:on         :luftfahrzeug
     :action-log true
     :params     {:luftfahrzeug {:name "Luftfahrzeug" :type :luftfahrzeug :required true}}
     :submission-criteria
     [{:message "Offene Mängel verhindern die Freigabe."
       :test {:n [:get :luftfahrzeug :offene-maengel]}
       :pred [:= :n 0]}]
     :rules      [{:op :update :object-type :luftfahrzeug :target [:get :luftfahrzeug]
                   :values {}}]
     :edits      [{:object-type :luftfahrzeug}]}}})

(def ontologie
  (in-project :flotte wartung))

(defn kanten-aus
  [link quelle tabelle]
  (update link 4 (fnil assoc {})
          :external-source {:datasource quelle :table tabelle
                            :from-key-column :von_schluessel :to-key-column :nach_schluessel}))

(defn quelle
  [projekt host beschreibung]
  {:project projekt :type :postgres :description beschreibung
   :jdbc-url (str "jdbc:postgresql://" host ":5432/" host) :egress [(str host ":5432")]})
