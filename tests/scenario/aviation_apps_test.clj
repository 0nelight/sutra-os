(ns ^:api scenario.aviation-apps-test
  (:require [sutra.catalog.grants :as grants]
            [clojure.data.json :as json]
            [support.in-project :refer [install-in-project]]
            [clojure.set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.access.principals :as pr]
            [support.aviation :refer [ontologie]]
            [support.pod :as pod]
            [support.wire :refer [ok? publish!]]
            [sutra.types.paths :as paths]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(def ^:private welt
  (-> ontologie
      (assoc-in [:value-types :eur]
                {:type :decimal
                 :format {:kind :currency :currency "EUR" :decimals 2 :grouping true}})
      (update-in [:objects :mangel :attributes] conj [:kosten :decimal {:value-type :eur}]
                 [:foto :media-reference])
      (assoc-in [:objects :user] {:attributes [[:user-id :string] [:rolle :string]]
                                  :primary-key :user-id :plural "Users"})
      (assoc-in [:objects :flug]
                {:primary-key :flug-nr
                 :attributes [[:flug-nr :string] [:kennzeichen :string] [:datum :date]
                              [:block-min :integer]]
                 :match-keys [:flug-nr]
                 :plural "Flüge"})
      (update :links conj [:flug-luftfahrzeug :flug :luftfahrzeug :many-to-one
                           {:fk :kennzeichen :forward-api-name :geflogen-von
                            :reverse-api-name :fluege}])
      (update :actions merge
              {:benutzer-anlegen {:params {:id {:name "Id" :type :string :required true}
                                           :rolle {:name "Rolle" :type :string :required true}}
                                  :rules [{:op :create :object-type :user :id :id
                                           :values {:user-id [:get :id]
                                                    :rolle [:get :rolle]}}]
                                  :edits [{:object-type :user}]}
               :flug-erfassen {:params {:flug-nr {:name "Flug nr" :type :string :required true}
                                        :kennzeichen
                                        {:name "Kennzeichen" :type :string :required true}
                                        :datum {:name "Datum" :type :date :required true}
                                        :block-min
                                        {:name "Block min" :type :integer :required true}}
                               :rules [{:op :create :object-type :flug
                                        :values {:flug-nr [:get :flug-nr]
                                                 :kennzeichen [:get :kennzeichen]
                                                 :datum [:get :datum]
                                                 :block-min [:get :block-min]}}]
                               :edits [{:object-type :flug}]}
               :foto-anhaengen {:params {:mangel {:name "Mangel" :type :mangel :required true}
                                         :foto {:name "Foto" :type :string :required true}}
                                :rules [{:op :update :object-type :mangel :target [:get :mangel]
                                         :values {:foto [:get :foto]}}]
                                :edits [{:object-type :mangel}]}
               :kosten-schaetzen {:params {:mangel {:name "Mangel" :type :mangel :required true}
                                           :kosten {:name "Kosten" :type :number}}
                                  :rules [{:op :update :object-type :mangel :target [:get :mangel]
                                           :values {:kosten [:get :kosten]}}]
                                  :edits [{:object-type :mangel}]}})))

(def ^:private admin (atom nil))
(def ^:private sie (atom nil))
(def ^:private sie-id (atom nil))
(def ^:private apps (atom {}))

(defn- anmelden [u p] (-> (api/post! :login {:username u :password p}) :body :token))

(defn- tun! [action args] (api/mutate action args {:publish? true}))

(defn- stage! [verb args] (assert (ok? (api/mutate verb args {:stage? true})) (pr-str verb args)))

(defn- mit-welt [f]
  (reset! admin (anmelden grants/admin-username pr/default-admin-password))
  (binding [tx/*auth-token* @admin]
    (let [r (install-in-project {:schema welt})] (assert (ok? r) (pr-str (:body r))))
    (assert (ok? (api/post! :staging)))
    (reset! sie-id (str (java.util.UUID/randomUUID)))
    (assert (ok? (tun! :benutzer-anlegen {:id @sie-id :rolle "consumer"})))
    (assert (ok? (api/post! :register {:username "linie" :password "linie" :user-id @sie-id})))
    (stage! :create-project
            {:name :linienwartung :grants [{:principal @sie-id :role :ontology-viewer}]})
    (stage! :update-project
            {:name :flotte :grants [{:principal @sie-id :role :ontology-viewer}]})
    (stage! :create-project {:name :werftplanung})
    (assert (ok? (api/post! :staging)))
    (stage! :create-collection
            {:name :betrieb :project :linienwartung :description "Betrieb" :order 1})
    (assert (ok? (api/post! :staging)))
    (stage! :create-application
            {:name :maengelportal :promoted true :collection :betrieb
             :project :linienwartung :object-type :mangel :icon "🛠"
             :description (apply str (repeat 40 "für die Linie "))
             :layout {:object-types [:mangel :mechaniker]
                      :aggregate-types [:flug]
                      :action {:action :mangel-beheben :parameters [:mangel :mechaniker]}}})
    (stage! :create-application
            {:name :kostenblatt :promoted true :collection :betrieb
             :project :linienwartung :object-type :mangel
             :layout {:object-types [:mangel] :action {:action :kosten-schaetzen}}})
    (stage! :create-application {:name :werftplan :promoted true
                                 :project :werftplanung :object-type :mangel
                                 :layout {:object-types [:mangel]
                                          :action {:action :mangel-beheben :parameters [:mangel]}}})
    (stage! :create-application {:name :teileliste :project :linienwartung
                                 :object-type :wartbares-teil
                                 :layout {:object-types [:wartbares-teil]}})
    (stage! :create-application
            {:name :luftfahrzeug-view-full :project :linienwartung
             :object-type :luftfahrzeug :form-factor :full
             :layout {:kind :rows
                      :vars {:alle-mechaniker {:source :mechaniker}
                             :ein-mangel {:from :mangel}
                             :neu {:default nil}}
                      :items [{:widget :action-form :action :berechtigen}
                              {:widget :combo-input :sel {:var :neu} :items {:val :alle-mechaniker}
                               :item-value :id :item-label :name}
                              {:widget :object-view :object {:val :ein-mangel} :hide-header? true}
                              {:widget :action-form :action :mechaniker-anlegen
                               :output {:var :neu}}]}})
    (stage! :create-application
            {:name :flottenmodul :project :linienwartung
             :object-type :luftfahrzeug
             :layout {:kind :rows
                      :object-types [:luftfahrzeug :mangel :triebwerk]
                      :items [{:widget :object-view :object {:from :luftfahrzeug}}]}})
    (stage! :create-application {:name :leise :project :linienwartung})
    (stage! :create-application {:name :entwurf :project :linienwartung})
    (assert (ok? (api/post! :staging)))
    (doseq [a [:maengelportal :kostenblatt :werftplan :teileliste :luftfahrzeug-view-full
               :flottenmodul :leise]]
      (assert (ok? (publish! a)) (str a)))
    (reset! apps
            (into {}
                  (for [[rid a] (:applications (:body (api/get! :schema)))]
                    [(keyword (:application-name a)) rid])))
    (assert (ok? (tun! :luftfahrzeug-registrieren {:kennzeichen "D-AINA" :muster "A320"})))
    (assert (ok? (tun! :mechaniker-anlegen {:lizenz "L-1" :name "Jannik" :muster "A320"})))
    (assert (ok? (tun! :mechaniker-anlegen {:lizenz "L-2" :name "Maren" :muster "B747"})))
    (assert (ok? (tun! :mechaniker-anlegen
                       {:lizenz "DE.66.B1.0815" :name "Oskar Hafenmeier" :muster "A320"})))
    (assert (ok? (tun! :triebwerk-einbauen
                       {:seriennummer "ESN-1" :betriebsstunden 10M :luftfahrzeug "D-AINA"})))
    (assert (ok? (tun! :fahrwerk-einbauen
                       {:seriennummer "LG-1" :landungen 4 :luftfahrzeug "D-AINA"})))
    (assert (ok? (tun! :mangel-melden
                       {:mangel-nr "P-1" :luftfahrzeug "D-AINA" :beschreibung "Leck"
                        :kategorie "C"})))
    (doseq [[nr tag min] [["F-1" "2026-08-03" 240] ["F-2" "2026-08-05" 180] ["F-3" "2026-08-11" 300]
                          ["F-4" "2026-01-01" 60]]]
      (assert (ok? (tun! :flug-erfassen
                         {:flug-nr nr :kennzeichen "D-AINA" :datum tag :block-min min}))))
    (reset! sie (anmelden "linie" "linie"))
    (f)))

(use-fixtures :once pod/with-mem-pod-auth mit-welt)

(defn- app [n] (tx/send! :get (paths/url :my-application {:rid (@apps n)})))

(defn- absenden! [n params]
  (tx/post-json! (paths/url :my-application-submit {:rid (@apps n)})
                 (json/write-str {:parameters params})))

(defn- aggregat [n spec]
  (tx/send! :post (paths/url :my-application-aggregate {:rid (@apps n)}) spec))

(defn- je-typ
  [d tabelle t]
  (let [rid (some (fn [[r n]] (when (= (name n) (name t)) r)) (:object-names d))]
    (get-in d [tabelle rid])))

(defn- mangel-wert
  [nr attr]
  (tx/with-token @admin
    (some #(when (= nr (get-in % [:values :mangel-nr])) (get-in % [:values attr]))
          (:rows (:body (api/list-objects :mangel))))))

(defn- objekt [daten typ attr v] (some #(when (= v (get-in % [:values attr])) %) (get daten typ)))

(deftest die-konsumentin-sieht-ihr-portal
  (tx/with-token @sie
    (testing "sie ist Konsumentin; Bauwerkzeuge sind ihr verschlossen, das Portal nicht"
      (is (= "consumer" (:role (:body (api/get! :me)))))
      (is (= 403 (:status (api/get! :snapshot))))
      (is (= 403
             (:status (api/mutate :create-object-type
                                  {:name :leck :project :linienwartung :attributes [[:x :string]]
                                   :plural "Lecks"}
                                  {:stage? true}))))
      (is (= 200 (:status (tx/send! :get (paths/url :my-applications))))))
    (testing "die Rolle meldet sich für jeden gleich, nicht nur für Konsumenten"
      (is (= "admin" (:role (:body (tx/with-token @admin (api/get! :me)))))))

    (testing "der Starter zeigt genau die veröffentlichten, beworbenen Apps ihres Projekts"
      (let [meine (:applications (:body (tx/send! :get (paths/url :my-applications))))]
        (is (= #{:maengelportal :kostenblatt} (set (map (comp keyword :name) meine)))
            (pr-str (map :name meine)))
        (let [d (:description (some #(when (= "maengelportal" (name (:name %))) %) meine))]
          (is (<= (count d) 250))
          (is (str/ends-with? d "…")))))

    (testing "eine App eines fremden Projekts gibt es für sie nicht — auch nicht über die RID"
      (is (= 404 (:status (app :werftplan)))))
    (testing "der Admin sieht alle veröffentlichten Apps, aber keine Entwürfe"
      (let [alle (set (map (comp keyword :name)
                           (:applications
                            (:body (tx/with-token @admin
                                     (tx/send! :get (paths/url :my-applications)))))))]
        (is (contains? alle :werftplan))
        (is (not (contains? alle :entwurf)))))

    (testing "die App liefert ihre Typen mit den Anzeigeformaten der Ontologie"
      (let [b (:body (app :kostenblatt))
            anzeige (je-typ (:application b) :property-display :mangel)]
        (is (= {:kind :currency :currency "EUR" :decimals 2 :grouping true}
               (get-in anzeige [:kosten :format]))
            "das Format des Wertetyps erreicht eine Eigenschaft, die selbst keins erklärt")
        (is (= "Prüfer" (get-in anzeige [:pruefer :name])))
        (is (= "EUR"
               (get-in b [:application :action-params :kosten-schaetzen :kosten :format :currency]))
            "ein Zahl-Parameter zeigt sich wie die Eigenschaft, in die seine Regel schreibt")
        (is (objekt (get-in b [:body :data] (:data b)) :mangel :mangel-nr "P-1"))))

    (testing "ein Typ nur zum Auswerten wird gefragt, nicht ausgeliefert"
      (let [b (:body (app :maengelportal))]
        (is (seq (get-in b [:data :mangel])))
        (is (nil? (get-in b [:data :flug])))
        (is (= #{:mangel :mechaniker}
               (set (keys (get-in b [:application :action-params :mangel-beheben])))))
        (is (every? :required (vals (get-in b [:application :action-params :mangel-beheben]))))))))

(deftest die-regeln-der-action-gelten-fuer-jeden
  (let [daten (tx/with-token @sie (get-in (app :maengelportal) [:body :data]))
        p1    (objekt daten :mangel :mangel-nr "P-1")
        maren  (objekt daten :mechaniker :name "Maren")
        jannik (objekt daten :mechaniker :name "Jannik")]
    (testing "ein Kriterium lehnt ab, mit der Meldung, wie sie verfasst ist, und nichts wird geschrieben"
      (tx/with-token @sie
        (let [r (absenden! :maengelportal {:mangel (:id p1) :mechaniker (:id maren)})]
          (is (= 422 (:status r)))
          (is (= "Der Mechaniker ist für dieses Muster nicht berechtigt."
                 (get-in r [:body :error])))))
      (is (= "offen" (mangel-wert "P-1" :zustand))))
    (testing "dieselbe Regel gilt für den Admin über die API — sie steht in der Action, nicht in der App"
      (tx/with-token @admin
        (is (= 422 (:status (tun! :mangel-beheben {:mangel (:id p1) :mechaniker (:id maren)}))))))
    (testing "ein gültiges Absenden wirkt und kommt mit den neuen Daten zurück; die Historie nennt sie"
      (tx/with-token @sie
        (let [r (absenden! :maengelportal {:mangel (:id p1) :mechaniker (:id jannik)})]
          (is (= 200 (:status r)) (pr-str (:body r)))
          (is (some? (objekt (get-in r [:body :data]) :mangel :mangel-nr "P-1")))
          (is (= "behoben" (mangel-wert "P-1" :zustand)))))
      (tx/with-token @admin
        (let [{:keys [provenance]} (:body (api/edit-history :mangel))]
          (is (= @sie-id (some #(when (= (:id p1) (:pk %)) (:edited_by %)) provenance))))))
    (testing "Zahlen kommen an der App-Tür als JSON und werden nach dem Parametertyp gelesen"
      (tx/with-token @sie
        (let [r (absenden! :kostenblatt {:mangel (:id p1) :kosten 1234.5})]
          (is (= 200 (:status r)) (pr-str (:body r))))
        (is (== 1234.5 (mangel-wert "P-1" :kosten)))))
    (testing "an eine fremde App absenden: 404"
      (tx/with-token @sie
        (is (= 404 (:status (absenden! :werftplan {:mangel (:id p1)}))))))))

(deftest die-app-wertet-in-ihrem-rahmen-aus
  (tx/with-token @sie
    (testing "Blockstunden je ISO-Woche"
      (let [r  (aggregat :maengelportal
                         {:find :flug :aggs [{:fn :sum :attr :block-min :name :minuten}]
                          :group-by [[:bucket :datum :week]]})
            wk (into {}
                     (map (juxt #(get-in % [:group :datum-week]) #(get-in % [:metrics :minuten])))
                     (get-in r [:body :aggregations]))]
        (is (ok? r) (pr-str (:body r)))
        (is (= {:group [:datum-week] :metrics [:minuten]} (get-in r [:body :keys])))
        (is (== 420 (get wk "2026-W32")))
        (is (== 300 (get wk "2026-W33")))
        (is (== 60 (get wk "2026-W01")) "der 1.1.2026 ist ein Donnerstag: Woche 1, nicht 53")
        (is (= (sort-by #(get-in % [:group :datum-week]) (get-in r [:body :aggregations]))
               (get-in r [:body :aggregations]))
            "die Buckets kommen nach ihrem Schlüssel geordnet")))
    (testing ":limit über eine Metrik ist Top-N über die Buckets"
      (let [r (aggregat :maengelportal
                        {:find :flug :aggs [{:fn :sum :attr :block-min :name :minuten}]
                         :group-by [[:bucket :datum :week]] :order-by :minuten :sort-dir :desc
                         :limit 1})]
        (is (= [420]
               (map #(long (get-in % [:metrics :minuten])) (get-in r [:body :aggregations]))))))
    (testing "der Rahmen ist das Layout der App: ein anderer Typ, ein unbekannter, ein Sprung hinaus — nichts davon"
      (is (= 404 (:status (aggregat :maengelportal {:find :luftfahrzeug :aggs [{:fn :count}]}))))
      (is (= 404 (:status (aggregat :maengelportal {:find :gibtsnicht :aggs [{:fn :count}]}))))
      (is (= 404
             (:status (aggregat :maengelportal
                                {:find :flug :where {:geflogen-von {:muster {:= "A320"}}}
                                 :aggs [{:fn :count}]})))
          "ein Filter über einen Link liest die ferne Seite")
      (let [r (aggregat :maengelportal {:find :flug :aggs [{:fn :count}] :around [:geflogen-von]})]
        (is (= 400 (:status r)))
        (is (re-find #"around" (str (get-in r [:body :error])))))
      (is (= 400
             (:status (aggregat :maengelportal
                                {:find :flug :object-set "alles" :aggs [{:fn :count}]}))))
      (is (= 400
             (:status (aggregat :maengelportal {:find :flug :aggs [{:fn :count}] :explain? true})))
          "kein SQL für Konsumenten"))
    (testing "ohne :aggs ist es keine Auswertung; eine falsche wird mit der Meldung der Grammatik abgelehnt"
      (is (= 400 (:status (aggregat :maengelportal {:find :flug :limit 5}))))
      (let [r (aggregat :maengelportal
                        {:find :flug :aggs [{:fn :sum :attr :block-min}]
                         :group-by [[:bucket :datum :fortnight]]})]
        (is (= 400 (:status r)))
        (is (re-find #"bucket unit" (str (get-in r [:body :error])))))
      (is (= 400 (:status (aggregat :maengelportal {:find :flug :aggs [{:fn :sum}]})))))
    (testing "eine fremde App wertet für sie nicht aus"
      (is (= 404 (:status (aggregat :werftplan {:find :mangel :aggs [{:fn :count}]})))))))

(deftest eine-app-ueber-einem-interface
  (let [r    (tx/with-token @admin (app :teileliste))
        objs (get-in r [:body :objects])]
    (testing "eine Lesung, beide Implementierer, jede Zeile mit ihrem konkreten Typ"
      (is (ok? r) (pr-str (:body r)))
      (is (= #{"ESN-1" "LG-1"} (set (map #(get-in % [:values :seriennummer]) objs))))
      (is (= 2 (count (set (map :object-type-ref objs)))))
      (is (= 2 (count (get-in r [:body :data :wartbares-teil])))))
    (testing "der Anzeigename der gemeinsamen Eigenschaft reist mit"
      (is (= "Seriennummer"
             (get-in (je-typ (get-in r [:body :application]) :property-display :wartbares-teil)
                     [:seriennummer :name])))))
  (testing "die Auswertung zählt über die Interface-Wurzel, und der Rahmen bleibt eng"
    (tx/with-token @admin
      (let [r (aggregat :teileliste {:find :wartbares-teil :aggs [{:fn :count :name :n}]})]
        (is (= 2 (get-in (first (get-in r [:body :aggregations])) [:metrics :n]))
            (pr-str (:body r))))
      (is (= 404 (:status (aggregat :teileliste {:find :mangel :aggs [{:fn :count}]})))))))

(deftest ein-objektparameter-wird-auf-dem-server-gesucht
  (let [lfz   (tx/with-token @admin
                (:id (some #(when (= "D-AINA" (get-in % [:values :kennzeichen])) %)
                           (:rows (:body (api/list-objects :luftfahrzeug))))))
        frage (fn [suche]
                (tx/with-token @admin
                  (tx/post-json! (paths/url :my-application-action-params
                                            {:rid (@apps :luftfahrzeug-view-full)})
                                 (json/write-str
                                  (cond-> {:action "berechtigen"
                                           :parameters {:luftfahrzeug lfz :mechaniker nil}}
                                    suche (assoc :search {:mechaniker suche}))))))
        namen (fn [r]
                (set (map #(get-in % [:values :name])
                          (get-in r [:body :options :mechaniker :rows]))))]
    (testing "ohne Begriff wird der ganze Typ angeboten"
      (is (= #{"Jannik" "Maren" "Oskar Hafenmeier"} (namen (frage nil)))))
    (testing "ein Begriff grenzt auf dem Server ein, über den Titel, ohne Groß/Klein"
      (is (= #{"Oskar Hafenmeier"} (namen (frage "hafen")))))
    (testing "… und über die anderen Texteigenschaften"
      (is (= #{"Oskar Hafenmeier"} (namen (frage "B1.0815")))))))

(deftest die-standardansicht-zeigt-das-objekt-und-seine-seiten
  (let [lfz  (tx/with-token @admin
               (:id (some #(when (= "D-AINA" (get-in % [:values :kennzeichen])) %)
                          (:rows (:body (api/list-objects :luftfahrzeug))))))
        um   (fn [body]
               (tx/send! :post (paths/url :my-application-around {:rid (@apps :flottenmodul)})
                         body))]
    (tx/with-token @sie
      (testing "die App beschreibt je Link-Seite einen Eintrag — nur zu Typen, die sie bindet"
        (let [info (je-typ (get-in (app :flottenmodul) [:body :application]) :standard-views
                           :luftfahrzeug)]
          (is (= #{:maengel :triebwerke :berechtigte}
                 (set (map (comp keyword name :api) (:links info))))
              "Mechaniker erreicht das Modul über die eingebettete Luftfahrzeug-Ansicht; Fahrwerke nicht")))
      (testing "die Standardlesung: das Objekt ganz, seine Nachbarn nach Seite"
        (let [b (:body (um {:type "luftfahrzeug" :id lfz :standard true}))]
          (is (= "A320" (get-in b [:object :values :muster])) (pr-str b))
          (is (= #{"P-1"}
                 (set (map #(get-in % [:values :mangel-nr]) (get-in b [:links :maengel])))))
          (is (= #{"ESN-1"}
                 (set (map #(get-in % [:values :seriennummer]) (get-in b [:links :triebwerke])))))
          (is (not (contains? (:links b) :fahrwerke))
              "ein Typ, den die App nicht bindet, wird nicht erreicht")))
      (testing ":standard öffnet keinen Typ außerhalb der App"
        (is (= 404 (:status (um {:type "fahrwerk" :id lfz :standard true}))))))
    (testing "die eigenständige Objektseite mit eigener Ansicht liest, was diese nennt"
      (tx/with-token @admin
        (let [b (:body (tx/send! :post (paths/url :object-view-standard)
                                 {:type "luftfahrzeug" :id lfz}))]
          (is (= "A320" (get-in b [:object :values :muster])))
          (is (not (contains? (:links b) :fahrwerke))
              "eine Seite, die die Ansicht nicht zeigt, wird nicht gelesen")
          (is (contains? (set (map (comp keyword name :api) (get-in b [:standard-view :links])))
                         :berechtigte)
              "beschrieben bleibt die Standardansicht trotzdem"))))
    (testing "… und mit :standard alle Seiten, für einen Entwurf, den der Server nicht kennt"
      (tx/with-token @admin
        (let [b (:body (tx/send! :post (paths/url :object-view-standard)
                                 {:type "luftfahrzeug" :id lfz :standard true}))]
          (is (= #{"LG-1"}
                 (set (map #(get-in % [:values :seriennummer])
                           (get-in b [:links :fahrwerke]))))))))))

(defn- apps-liste [] (:applications (:body (tx/send! :get (paths/url :applications)))))

(deftest eine-app-lebt-wie-jede-deklaration
  (tx/with-token @admin
    (testing "anlegen, in der Liste nach Namen, gebunden per RID"
      (stage! :create-application
              {:name :schichtbrett :project :linienwartung :description "Schicht" :icon "📋"
               :object-type :mechaniker})
      (is (ok? (api/post! :staging)))
      (is (= ["Schicht" nil] ((juxt :description :published) (get (apps-liste) :schichtbrett)))
          "gespeichert ist sie ein Entwurf")
      (is (ok? (publish! :schichtbrett)))
      (is (= "1" (:published (get (apps-liste) :schichtbrett))) "veröffentlicht: Version 1")
      (let [s (:body (api/get! :schema))
            a (some (fn [[_ a]] (when (= :schichtbrett (keyword (:application-name a))) a))
                    (:applications s))
            t (some (fn [[r o]] (when (= :mechaniker (:object-type-name o)) r)) (:object-types s))]
        (is (= t (:object-type a)))))
    (testing "ändern setzt die ganze Deklaration, löschen nimmt sie weg"
      (stage! :update-application
              {:name :schichtbrett :project :linienwartung :description "Spätschicht"
               :icon "📋"})
      (is (ok? (api/post! :staging)))
      (is (= ["Spätschicht" "1"]
             ((juxt :description :published) (get (apps-liste) :schichtbrett)))
          "ein Speichern ohne :auto-publish lässt die veröffentlichte Version stehen")
      (stage! :delete-application {:name :schichtbrett})
      (is (ok? (api/post! :staging)))
      (is (not (contains? (apps-liste) :schichtbrett))))))

(deftest eine-app-liefert-nur-die-dateien-ihrer-objekte
  (let [eigen  (tx/with-token @admin
                 (:hash (:body (api/put-blob (.getBytes "%PDF-1.4 Befundfoto" "UTF-8")))))
        fremd  (tx/with-token @admin
                 (:hash (:body (api/put-blob (.getBytes "%PDF-1.4 Personalakte" "UTF-8")))))
        p1     (tx/with-token @admin (:id (some #(when (= "P-1" (get-in % [:values :mangel-nr])) %)
                                                (:rows (:body (api/list-objects :mangel))))))
        holen  (fn [h]
                 (tx/get-bytes! (paths/url :my-application-blob
                                           {:rid (@apps :maengelportal) :hash h})))]
    (is (some? eigen))
    (tx/with-token @admin (is (ok? (tun! :foto-anhaengen {:mangel p1 :foto eigen}))))
    (tx/with-token @sie
      (testing "eine Datei, auf die ein Objekt der App zeigt, kommt durch, mit erkanntem Typ"
        (let [r (holen eigen)]
          (is (= 200 (:status r)))
          (is (= "application/pdf" (:content-type r)))))
      (testing "eine Datei, auf die nichts in der App zeigt, gibt es nicht — auch wenn sie existiert"
        (is (= 404 (:status (holen fremd)))))
      (testing "ein kaputter Hash ist eine saubere 400"
        (is (= 400 (:status (holen "kein-hash"))))))))

(deftest einen-entwurf-sieht-nur-wer-ihn-bearbeiten-darf
  (let [entwurf {:name :schichtentwurf :project :linienwartung
                 :object-types [:mechaniker]
                 :layout {:kind :rows
                          :items [{:widget :object-table :rows {:from :mechaniker}
                                   :cols [{:prop :name}]}]}}
        vorschau! (fn [datei]
                    (tx/send! :post (paths/url :application-preview {:name "schichtentwurf"})
                              {:file datei}))
        ed-id  (str (java.util.UUID/randomUUID))]
    (tx/with-token @admin
      (is (ok? (tun! :benutzer-anlegen {:id ed-id :rolle "ontology-editor"})))
      (is (ok? (api/post! :register {:username "planerin" :password "planerin" :user-id ed-id})))
      (stage! :update-project
              {:name :linienwartung :grants [{:principal @sie-id :role :ontology-viewer}
                                             {:principal ed-id :role :ontology-editor}]})
      (is (ok? (api/post! :staging))))
    (let [planerin (anmelden "planerin" "planerin")]
      (testing "wer nur liest, bekommt keine Vorschau"
        (tx/with-token @sie (is (= 403 (:status (vorschau! entwurf))))))
      (let [id (tx/with-token planerin (get-in (vorschau! entwurf) [:body :id]))]
        (is (string? id))
        (testing "die Bearbeiterin liest den Entwurf mit lebenden Daten"
          (tx/with-token planerin
            (let [r (tx/send! :get (paths/url :my-application {:rid id}))]
              (is (every? (set (map #(get-in % [:values :name])
                                    (get-in r [:body :data :mechaniker])))
                          ["Jannik" "Maren" "Oskar Hafenmeier"]))
              (is (= id (get-in r [:body :application :id]))))))
        (testing "die Vorschau-ID gehört ihr allein"
          (tx/with-token @sie
            (is (#{403 404} (:status (tx/send! :get (paths/url :my-application {:rid id})))))))
        (testing "eine Datei, die die Tür ablehnt, wird auch nicht vorgeschaut"
          (tx/with-token planerin
            (is (= 400
                   (:status (vorschau! (assoc-in entwurf [:layout :items 0 :cols 0 :gibt-es-nicht]
                                                 1)))))))))))

(deftest zum-schluss-ein-arbeitsbereich-sucht-in-seinem-rahmen
  (tx/with-token @admin
    (let [linie {:project :linienwartung :apps [:maengelportal]
                 :home [{:column :a :items [{:kind :object-type :object-type :mangel}
                                            {:kind :object-type :object-type :mechaniker}]}]}]
      (testing "gibt es Arbeitsbereiche, steht jede beworbene App auf einem — sonst gäbe es zwei Wege hinein"
        (let [r (api/post! :workspace-file {:name "linie"} {:file linie})]
          (is (not (ok? r)))
          (is (re-find #"kostenblatt" (pr-str (:body r))))
          (api/delete! :staging)))
      (stage! :update-application
              {:name :werftplan :promoted false :project :werftplanung
               :object-type :mangel
               :layout {:object-types [:mangel]
                        :action {:action :mangel-beheben :parameters [:mangel]}}})
      (is (ok? (api/post! :staging)))
      (is (ok? (api/post! :workspace-file {:name "linie"}
                          {:file (assoc linie :apps [:maengelportal :kostenblatt])})))
      (api/post! :staging))
    (doseq [i (range 24)]
      (is (ok? (tun! :mechaniker-anlegen
                     {:lizenz (format "LX-%02d" i) :name (format "Leck-Team %02d" i)
                      :muster "A320"}))))
    (is (ok? (tun! :mangel-melden
                   {:mangel-nr "LECK-1" :luftfahrzeug "D-AINA" :beschreibung "Leck am Ventil"
                    :kategorie "C"}))))
  (let [ws  (some (fn [[r w]] (when (= "linie" (name (:workspace-name w))) r))
                  (:workspaces (:body (tx/with-token @admin (api/get! :schema)))))
        such (fn [q] (tx/with-token @sie (api/my-workspace-search ws q)))]
    (is (some? ws))
    (testing "die Suche findet, was die Module erreichen — gruppiert nach Typ, die mit wenigen Treffern zuerst"
      (let [gs (get-in (such {:q "leck" :limit 10}) [:body :groups])]
        (is (= [["mangel" 2 2] ["mechaniker" 24 10]]
               (mapv (juxt :type :count (comp count :rows)) gs)) (pr-str gs))))
    (testing "ein Typ-Modul blättert durch alle seine Objekte, ohne Wiederholung"
      (let [seite (fn [o]
                    (get-in (such {:type "mechaniker" :q "leck-team" :limit 10 :offset o})
                            [:body :groups 0]))]
        (is (= 24 (:count (seite 0))))
        (is (= [10 10 4] (map #(count (:rows (seite %))) [0 10 20])))
        (is (empty? (clojure.set/intersection (set (map :id (:rows (seite 0))))
                                              (set (map :id (:rows (seite 10)))))))))
    (testing "ein Typ außerhalb des Rahmens ist nicht da"
      (is (= 404 (:status (such {:type "luftfahrzeug"})))))
    (testing "die Standardansicht öffnet im Arbeitsbereich; die Beschreibung trägt sie ohne Actions"
      (let [m (tx/with-token @admin
                (:id (some #(when (= "LECK-1" (get-in % [:values :mangel-nr])) %)
                           (:rows (:body (api/list-objects :mangel))))))]
        (tx/with-token @sie
          (is (= m
                 (get-in (api/post! :my-workspace-around {:rid ws}
                                    {:type "mangel" :id m :standard true}) [:body :id])))
          (is (= []
                 (:actions (je-typ (get-in (api/get! :my-workspace {:rid ws}) [:body :workspace])
                                   :standard-views :mangel)))))))
    (testing "die Suche der Bauwerkzeuge bleibt ihr verschlossen"
      (tx/with-token @sie
        (is (= 403 (:status (api/get! :instances-search {:q "leck" :type nil}))))))
    (testing "ein Typ weiß, wer ihn nutzt; Lesezugriffe werden nicht gezählt, und das wird gesagt"
      (tx/with-token @admin
        (let [u (:body (api/get! :object-type-usage {:type "mangel"}))]
          (is (contains? (set (map :name (:applications u))) "maengelportal"))
          (is (= ["linie"] (mapv :name (:workspaces u))))
          (is (false? (:reads-counted u))))
        (let [u (:body (api/get! :object-type-usage {:type "fahrwerk"}))]
          (is (empty? (:applications u)))
          (is (empty? (:workspaces u))))
        (is (= 404 (:status (api/get! :object-type-usage {:type "nichts"}))))))))

(deftest eine-ansicht-liest-was-sie-nennt
  (tx/with-token @admin
    (stage! :create-application
            {:name :mangel-view-full :project :linienwartung :object-type :mangel
             :form-factor :full
             :layout {:kind :rows :items [{:widget :attributes}
                                          {:widget :action-button :action :kosten-schaetzen
                                           :label "schätzen"
                                           :params {:mangel {:item :id}}}]}})
    (stage! :create-application
            {:name :flugzeugmodul :project :linienwartung
             :object-type :luftfahrzeug
             :layout {:kind :rows :object-types [:luftfahrzeug]
                      :items [{:widget :object-view :object {:from :luftfahrzeug}}]}})
    (is (ok? (api/post! :staging)))
    (is (ok? (publish! :mangel-view-full)))
    (is (ok? (publish! :flugzeugmodul))))
  (reset! apps
          (into {} (for [[rid a] (:applications (:body (tx/with-token @admin (api/get! :schema))))]
                     [(keyword (:application-name a)) rid])))
  (let [lfz   (tx/with-token @admin
                (:id (some #(when (= "D-AINA" (get-in % [:values :kennzeichen])) %)
                           (:rows (:body (api/list-objects :luftfahrzeug))))))
        um    (fn [body]
                (tx/with-token @sie
                  (tx/send! :post (paths/url :my-application-around {:rid (@apps :flugzeugmodul)})
                            body)))]
    (testing "{:source} — die Ansicht fragt den ganzen Typ, die Antwort reist unter ihrer Variable"
      (let [b (:body (um {:type "luftfahrzeug" :id lfz}))]
        (is (= #{"Jannik" "Maren" "Oskar Hafenmeier"}
               (set (map #(get-in % [:values :name]) (get-in b [:answers :alle-mechaniker]))))
            (pr-str (keys b)))
        (is (empty? (get-in b [:objects :mechaniker])))))
    (testing "Nachbarn: über die App wie über die eigenständige Seite, abgeleitete Werte nur, wo genannt"
      (let [nachbar   (fn [rows] (some #(when (= "P-1" (get-in % [:values :mangel-nr])) %) rows))
            app-zeile (nachbar (get-in (:body (um {:type "luftfahrzeug" :id lfz}))
                                       [:objects :mangel]))
            lfz-rid   (some (fn [[r o]] (when (= :luftfahrzeug (:object-type-name o)) r))
                            (:object-types (:body (tx/with-token @admin (api/get! :schema)))))
            mangel-rid (some (fn [[r o]] (when (= :mangel (:object-type-name o)) r))
                             (:object-types (:body (tx/with-token @admin (api/get! :schema)))))
            seite     (tx/with-token @admin
                        (:body (api/post! :instances-around
                                          {:ids [lfz] :types ["luftfahrzeug"]})))
            oms-zeile (nachbar (or (get-in seite [:objects (keyword mangel-rid)])
                                   (get-in seite [:objects mangel-rid])))]
        (is (some? lfz-rid))
        (is (some? app-zeile))
        (is (not (contains? (:values app-zeile) :muster))
            "ein abgeleiteter Wert, den die Ansicht nicht nennt, wird nicht berechnet — und ist nicht da")
        (is (= "A320" (get-in oms-zeile [:values :muster]))
            "die eigenständige Seite rechnet ihn für den Nachbarn")
        (testing "ein Objektverweis traegt seinen Typ: ohne :types eine codierte Weigerung"
          (let [r (tx/with-token @admin (api/post! :instances-around {:ids [lfz] :types []}))]
            (is (= 400 (:status r)) (pr-str (:body r)))
            (is (re-find #"around-needs-types|AROUND" (pr-str (:body r))))))
        (testing "die Kanten allein kosten keinen Nachbarn"
          (let [nur (tx/with-token @admin
                      (:body (api/post! :instances-around {:ids [lfz] :types ["luftfahrzeug"]
                                                         :links-only? true})))]
            (is (empty? (:objects nur)))
            (is (= (set (map :id (:links seite))) (set (map :id (:links nur)))))))
        (testing "unter den genannten Objekten liegt nur die Kante, deren beide Enden genannt sind"
          (let [nachbar-id (str (:id oms-zeile))
                unter      (fn [ids]
                             (:links (tx/with-token @admin
                                       (:body (api/post! :instances-around
                                                         {:ids ids
                                                          :types ["luftfahrzeug" mangel-rid]
                                                          :links-only? true :among? true})))))]
            (is (empty? (unter [lfz])))
            (is (seq (unter [lfz nachbar-id])))))))
    (testing "die eigenständige Seite trägt die eingebettete Ansicht und den Schlüssel eines Typs mit"
      (let [seite (tx/with-token @admin
                    (:body (api/post! :object-view-standard {:type "luftfahrzeug" :id lfz})))]
        (is (some? (:full (je-typ seite :object-views :mangel))))
        (is (seq (get-in seite [:object-page :view]))
            "die eigene Ansicht steht in derselben Antwort wie ihre Daten")
        (is (= (str (@apps :luftfahrzeug-view-full)) (str (get-in seite [:object-page :app :id]))))
        (is (not (contains? (get-in seite [:object-page :app]) :layout)))
        (is (= [:lizenz] (mapv keyword (je-typ seite :object-keys :mechaniker))))
        (is (= 3 (count (filter #(#{"Jannik" "Maren" "Oskar Hafenmeier"} (get-in % [:values :name]))
                                (get-in seite [:answers :alle-mechaniker])))))))
    (testing "eine eingebettete Ansicht ist über den Wirt erreichbar, ihre Action freigegeben"
      (let [a (tx/with-token @sie (get-in (app :flugzeugmodul) [:body :application]))]
        (is (some? (:full (je-typ a :object-views :mangel))))
        (is (contains? (set (keys (:action-params a))) :kosten-schaetzen))))
    (testing "eine Action über die Ansicht gibt die Objekte zurück, die sie angelegt hat"
      (let [r (tx/with-token @sie
                (tx/post-json! (paths/url :my-application-submit
                                          {:rid (@apps :luftfahrzeug-view-full)})
                               (json/write-str {:action "mechaniker-anlegen"
                                                :parameters
                                                {:lizenz "L-9" :name "Neu" :muster "A320"}})))]
        (is (ok? r) (pr-str (:body r)))
        (is (= ["Neu"] (map #(get-in % [:values :name]) (get-in r [:body :created]))))))))

(deftest eine-app-schreibt-nicht-auf-einen-geschlossenen-zweig
  (testing "ein Absenden über die App geht durch denselben Schreibschutz wie jede Action:
            ein geschlossener Zweig nimmt nichts mehr an"
    (let [z (str "zu-" (subs (str (random-uuid)) 0 8))]
      (is (ok? (api/post! :branches {:parent "main" :name z})))
      (is (ok? (api/post! :branch-status {:name z} {:closed true})))
      (let [r (tx/with-branch z
                (tx/post-json! (paths/url :my-application-submit
                                          {:rid (@apps :luftfahrzeug-view-full)})
                               (json/write-str {:action "mechaniker-anlegen"
                                                :parameters
                                                {:lizenz "L-Z" :name "Zu" :muster "A320"}})))]
        (is (= 409 (:status r)) (pr-str (:body r)))))))

(deftest ein-batch-schreibt-nicht-auf-einen-geschlossenen-zweig
  (testing "ein Batch über REST geht durch denselben Schreibschutz wie ein einzelner Dispatch"
    (let [z (str "zu-" (subs (str (random-uuid)) 0 8))]
      (is (ok? (api/post! :branches {:parent "main" :name z})))
      (is (ok? (api/post! :branch-status {:name z} {:closed true})))
      (let [r (tx/with-branch z
                (api/mutate-batch :mechaniker-anlegen
                                  [{:lizenz "L-B" :name "Batch" :muster "A320"}]
                                  {:publish? true}))]
        (is (= 409 (:status r)) (pr-str (:body r)))))))

(deftest die-quittung-eines-dispatch-verraet-die-welt-nicht
  (testing "ein Dispatch quittiert, was er tat — nicht die Welt, unter der er lief (sie nennt
            jedes Projekt und seine Grants), nicht seine Bindungen und seinen tx-vector"
    (let [z (str "q-" (subs (str (random-uuid)) 0 8))]
      (is (ok? (api/post! :branches {:parent "main" :name z})))
      (tx/with-branch z
        (doseq [r [(api/mutate :mechaniker-anlegen {:lizenz "L-Q1" :name "Q1" :muster "A320"}
                               {:publish? true})
                   (api/mutate-batch :mechaniker-anlegen
                                     [{:lizenz "L-Q2" :name "Q2" :muster "A320"}]
                                     {:publish? true})]]
          (is (ok? r) (pr-str (:body r)))
          (is (some? (get-in r [:body :result :action-id])))
          (is (empty? (select-keys (get-in r [:body :result]) [:world :bindings :ops :tx-vector]))
              (pr-str (keys (get-in r [:body :result])))))))))

(deftest eine-wiederholung-mit-demselben-schluessel-quittiert-gleich
  (testing "ein Dispatch mit Idempotency-Key schreibt einmal; die Wiederholung bekommt dieselbe
            Quittung, mit Ergebnis, nur als replayed markiert"
    (let [z (str "i-" (subs (str (random-uuid)) 0 8))
          k (str "k-" (random-uuid))]
      (is (ok? (api/post! :branches {:parent "main" :name z})))
      (tx/with-branch z
        (let [send! #(tx/with-headers {"Idempotency-Key" k}
                       (api/mutate :mechaniker-anlegen {:lizenz "L-I" :name "Ida" :muster "A320"}
                                   {:publish? true}))
              a     (send!)
              b     (send!)]
          (is (ok? a) (pr-str (:body a)))
          (is (some? (get-in a [:body :result :action-id])))
          (is (true? (get-in b [:body :replayed])))
          (is (= (:body a) (dissoc (:body b) :replayed)))
          (is (= 1 (count (filter #(= "Ida" (get-in % [:values :name]))
                                  (:rows (:body (api/list-objects :mechaniker))))))))))))

(deftest eine-auf-dem-zweig-geaenderte-app-zeigt-sich-dort
  (tx/with-token @admin
    (let [z       (str "app-" (subs (str (random-uuid)) 0 8))
          brett   (fn [text] {:name :zweigbrett :project :linienwartung :description text :icon "📋"
                              :object-type :mechaniker})
          lesen   #(get-in (tx/send! :get (paths/url :my-application {:rid "zweigbrett"}))
                           [:body :application :description])
          liste   #(set (map (comp keyword :name)
                             (:applications (:body (api/get! :my-applications)))))
          publish #(api/post! :resource-versions-publish
                              {:section "applications" :name "zweigbrett"} {})]
      (stage! :create-application (brett "alt"))
      (is (ok? (api/post! :staging)))
      (is (ok? (publish)))
      (is (ok? (api/post! :staging)))
      (stage! :update-application (brett "entwurf auf main"))
      (is (ok? (api/post! :staging)))
      (is (ok? (api/post! :branches {:parent "main" :name z})))
      (testing "unveraendert: der Zweig zeigt die veroeffentlichte Fassung, nicht den Entwurf"
        (is (= "alt" (tx/with-branch z (lesen)))))
      (tx/with-branch z
        (stage! :update-application (brett "neu auf dem zweig"))
        (is (ok? (api/post! :staging)))
        (stage! :create-application
                {:name :nur-auf-dem-zweig :project :linienwartung :promoted true
                 :object-types [:mechaniker]
                 :layout {:kind :rows
                          :items [{:widget :object-table :rows {:from :mechaniker}
                                   :cols [{:prop :name}]}]}})
        (is (ok? (api/post! :staging)))
        (is (ok? (publish! :nur-auf-dem-zweig)) "eine neue App öffnet erst veröffentlicht"))
      (testing "auf dem Zweig gespeichert: ohne Veroeffentlichen sichtbar"
        (is (= "neu auf dem zweig" (tx/with-branch z (lesen)))))
      (testing "eine neue App steht nur in der Liste ihres Zweigs"
        (is (contains? (tx/with-branch z (liste)) :nur-auf-dem-zweig))
        (is (not (contains? (liste) :nur-auf-dem-zweig))))
      (testing "main bleibt, wie es war"
        (is (= "alt" (lesen)))))))

