(ns ^:api scenario.aviation-rollen-test
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.access.principals :as pr]
            [sutra.catalog.grants :as grants]
            [sutra.serve.server :as server]
            [sutra.types.paths :as paths]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]
            [support.aviation :refer [kanten-aus quelle]]
            [support.in-project :refer [install-in-project]]
            [support.pod :as pod]
            [support.wire :refer [ok? uid]]))

(def ^:private gruppen
  (into {} (map (juxt identity (fn [_] (uid))))
        [:alle :fuehrung :personal :sicherheit :fliegerarzt :werkstatt-ed :werkstatt-chef]))

(defn- nur [& gs] [:intersects [:user :groups] (into #{} (map gruppen) gs)])

(defn- wer-darf [funktion]
  {:test {:f [:get :wer :funktion]} :pred [:= :f funktion]
   :message (str "Das darf nur: " funktion)})

(def ^:private wer {:type :user :prefill :current-user :exposure :hidden})

(defn- str-param [n] {:name n :type :string :required true})

(def ^:private system-typen
  {:objects {:user {:attributes [[:user-id :string] [:rolle :string]
                                 [:funktion :string] [:station :string]]
                    :primary-key :user-id :plural "Benutzer"}
             :group {:attributes [[:group-id :string] [:name :string]]
                     :primary-key :group-id :plural "Gruppen"}}
   :links [(kanten-aus [:member-of :user :group :many-to-many]
                       :verzeichnisdienst "GRUPPENMITGLIED")]
   :sources {:verzeichnisdienst (quelle :flotte "verzeichnis"
                                        "Verzeichnisdienst: Benutzer und ihre Gruppen.")}
   :actions
   {:benutzer-anlegen
    {:params {:id (str-param "Id") :rolle (str-param "Rolle")
              :funktion (str-param "Funktion") :station (str-param "Station")}
     :rules [{:op :create :object-type :user :id :id
              :values {:user-id [:get :id] :rolle [:get :rolle]
                       :funktion [:get :funktion] :station [:get :station]}}]
     :edits [{:object-type :user}]}
    :gruppe-anlegen
    {:params {:id (str-param "Id") :name (str-param "Name")}
     :rules [{:op :create :object-type :group :id :id
              :values {:group-id [:get :id] :name [:get :name]}}]
     :edits [{:object-type :group}]}
    :aufnehmen
    {:params {:user {:name "User" :type :user :required true}
              :group {:name "Group" :type :group :required true}}
     :rules [{:op :create-link :link-type :member-of :from-id [:get :user :id]
              :to-id [:get :group :id]}]}}})

(defn- erfassen
  [projekt typ felder]
  {:project projekt
   :params (assoc (into {} (map (juxt identity (comp str-param name))) felder) :wer wer)
   :submission-criteria [(wer-darf "stammdaten")]
   :rules [{:op :create :object-type typ
            :values (into {} (map (juxt identity #(vector :get %))) felder)}]
   :edits [{:object-type typ}]})

(def ^:private airline
  {:objects
   {:flug
    {:project :betrieb :primary-key :flug-nr :plural "Flüge"
     :attributes [[:flug-nr :string] [:station :string] [:status :string] [:erloes :string]]
     :security {:policy true
                :property-policies {:ergebnis {:properties [:erloes] :policy (nur :fuehrung)}}}}
    :mangel
    {:project :betrieb :primary-key :mangel-nr :plural "Mängel"
     :attributes [[:mangel-nr :string] [:text :string] [:status :string]]}
    :passagier
    {:project :betrieb :primary-key :pnr :plural "Passagiere"
     :attributes [[:pnr :string] [:station :string] [:name :string]]
     :security {:policy [:or (nur :fuehrung) [:= [:user :station] [:property :station]]]}}
    :besatzung
    {:project :personal :primary-key :personal-nr :plural "Besatzungen"
     :attributes [[:personal-nr :string] [:name :string] [:gehalt :string] [:befund :string]]
     :security {:policy true
                :property-policies
                {:lohn {:properties [:gehalt] :policy (nur :personal :fuehrung)}
                 :medizin {:properties [:befund] :policy (nur :fliegerarzt)}}}}
    :altakte
    {:project :personal :primary-key :nr :plural "Altakten" :attributes [[:nr :string]]}
    :umzugsakte
    {:project :personal :primary-key :nr :plural "Umzugsakten" :attributes [[:nr :string]]}
    :werkzeug
    {:project :werkstatt :primary-key :nr :plural "Werkzeuge" :attributes [[:nr :string]]}
    :sicherheitsbericht
    {:project :sicherheit :primary-key :bericht-nr :plural "Sicherheitsberichte"
     :attributes [[:bericht-nr :string] [:hergang :string] [:melder :string]]
     :security {:policy true
                :property-policies {:melderschutz {:properties [:melder]
                                                   :policy (nur :sicherheit)}}}}}
   :actions
   {:flug-erfassen (erfassen :betrieb :flug [:flug-nr :station :status :erloes])
    :passagier-erfassen (erfassen :betrieb :passagier [:pnr :station :name])
    :besatzung-erfassen (erfassen :personal :besatzung [:personal-nr :name :gehalt :befund])
    :bericht-erfassen (erfassen :sicherheit :sicherheitsbericht [:bericht-nr :hergang :melder])
    :mangel-melden
    {:project :betrieb
     :params {:mangel-nr (str-param "Nr") :text (str-param "Text")}
     :rules [{:op :create :object-type :mangel
              :values {:mangel-nr [:get :mangel-nr] :text [:get :text] :status "offen"}}]
     :edits [{:object-type :mangel}]}
    :freigabe-zeichnen
    {:project :betrieb
     :params {:mangel {:name "Mangel" :type :mangel :required true} :wer wer}
     :submission-criteria [(wer-darf "certifying-staff")]
     :rules [{:op :update :object-type :mangel :target [:get :mangel]
              :values {:status "freigegeben"}}]
     :edits [{:object-type :mangel}]}
    :flug-verzoegern
    {:project :betrieb
     :params {:flug {:name "Flug" :type :flug :required true} :wer wer}
     :submission-criteria [(wer-darf "dispatcher")]
     :rules [{:op :update :object-type :flug :target [:get :flug]
              :values {:status "verspaetet"}}]
     :edits [{:object-type :flug}]}}
   :applications
   {:betriebsportal
    {:project :betrieb
     :layout {:kind :tabs :object-types [:flug :passagier :besatzung]
              :tabs [{:id :fluege :title "Flüge"
                      :body {:kind :rows
                             :items [{:widget :table :source {:source :flug}}
                                     {:widget :table :source {:source :passagier}}]}}
                     {:id :crew :title "KANARIE-REITER" :visibility {:link :besatzung}
                      :body {:kind :rows
                             :items [{:widget :table :source {:source :besatzung}}]}}]}}}})

(def ^:private belegschaft
  {"konzernchef"  ["member"   "accountable-manager" "FRA" [:alle :fuehrung]]
   "safety"       ["member"   "safety-manager"      "FRA" [:alle :sicherheit]]
   "personal"     ["member"   "personalreferent"    "FRA" [:alle :personal]]
   "fliegerarzt"  ["member"   "aeromedical"         "FRA" [:alle :fliegerarzt]]
   "dispatcher"   ["member"   "dispatcher"          "FRA" [:alle]]
   "b1-technik"   ["member"   "certifying-staff"    "FRA" [:alle]]
   "mechaniker"   ["member"   "mechaniker"          "FRA" [:alle]]
   "vorfeld-fra"  ["consumer" "vorfeld"             "FRA" [:alle]]
   "vorfeld-muc"  ["consumer" "vorfeld"             "MUC" [:alle]]})

(def ^:private werkstatt
  {"werkstatt-ed"   ["member" "werkstatt" "FRA" [:alle :werkstatt-ed]]
   "werkstatt-chef" ["member" "werkstatt" "FRA" [:alle :werkstatt-chef]]})

(def ^:private kanarien
  {"KANARIE-ERLOES" #{"konzernchef"}
   "KANARIE-PNR-MUC" #{"konzernchef" "vorfeld-muc"}
   "KANARIE-GEHALT" #{"konzernchef" "personal"}
   "KANARIE-BEFUND" #{"fliegerarzt"}
   "KANARIE-NAME" #{"konzernchef" "personal" "fliegerarzt"}
   "KANARIE-AKTE" #{"konzernchef" "personal" "fliegerarzt"}
   "KANARIE-REITER" #{"konzernchef" "personal" "fliegerarzt"}
   "KANARIE-HERGANG" #{"konzernchef" "safety"}
   "KANARIE-MELDER" #{"safety"}})

(def ^:private admin (atom nil))
(def ^:private tokens (atom {}))
(def ^:private ids (atom {}))

(defn- anmelden [u p] (-> (api/post! :login {:username u :password p}) :body :token))

(defn- tun!
  ([action args] (tun! action args {:publish? true}))
  ([action args opts] (api/mutate action args opts)))

(defn- muss! [r] (assert (ok? r) (pr-str (:status r) (:body r))) r)

(defn- id-von [typ attr v]
  (some #(when (= v (get-in % [:values attr])) (:id %)) (:rows (:body (api/list-objects typ)))))

(defn- anlegen-welt! []
  (muss! (install-in-project {:schema system-typen}))
  (muss! (api/post! :staging))
  (doseq [[k id] gruppen] (muss! (tun! :gruppe-anlegen {:id id :name (name k)})))
  (doseq [[projekt & gs] [[:betrieb :alle] [:personal :personal :fuehrung :fliegerarzt]
                          [:sicherheit :sicherheit :fuehrung]]]
    (muss! (api/mutate :create-project
                       {:name projekt
                        :grants (mapv #(hash-map :principal (gruppen %) :role :ontology-viewer) gs)}
                       {:stage? true})))
  (muss! (api/mutate :create-project
                     {:name :werkstatt
                      :grants [{:principal (gruppen :werkstatt-ed) :role :ontology-editor}
                               {:principal (gruppen :werkstatt-chef) :role :ontology-owner}]}
                     {:stage? true}))
  (muss! (api/mutate :create-dataset
                     {:name :werkzeugliste :project :werkstatt
                      :columns [{:attribute-name :nr :attribute-type :string}]}
                     {:stage? true}))
  (muss! (api/mutate :create-dataset
                     {:name :personalakte :project :personal
                      :columns [{:attribute-name :akte :attribute-type :string}]}
                     {:stage? true}))
  (muss! (api/post! :staging))
  (muss! (api/post! :dataset-write {:name "personalakte"}
                    {:rows [{:values {:akte "KANARIE-AKTE"}}] :mode :snapshot
                     :message "Lieferung"}))
  (muss! (api/post! :install {:schema airline}))
  (muss! (api/post! :staging)))

(defn- anlegen-belegschaft! []
  (let [stamm (uid)]
    (muss! (tun! :benutzer-anlegen {:id stamm :rolle "ontology-editor" :funktion "stammdaten"
                                    :station "FRA"}))
    (muss! (api/post! :register {:username "stamm" :password "stamm" :user-id stamm})))
  (doseq [[u [rolle funktion station gs]] (merge belegschaft werkstatt)
          :let [id (uid)]]
    (muss! (tun! :benutzer-anlegen {:id id :rolle rolle :funktion funktion :station station}))
    (muss! (api/post! :register {:username u :password u :user-id id}))
    (swap! ids assoc u id)
    (doseq [g gs :when (gruppen g)]
      (muss! (tun! :aufnehmen {:user id :group (gruppen g)})))))

(defn- anlegen-daten! []
  (tx/with-token (anmelden "stamm" "stamm")
    (muss! (tun! :flug-erfassen {:flug-nr "LX-1" :station "FRA" :status "planmaessig"
                                 :erloes "KANARIE-ERLOES"}))
    (muss! (tun! :passagier-erfassen {:pnr "P-FRA" :station "FRA" :name "Fra Gast"}))
    (muss! (tun! :passagier-erfassen {:pnr "P-MUC" :station "MUC" :name "KANARIE-PNR-MUC"}))
    (muss! (tun! :besatzung-erfassen {:personal-nr "C-1" :name "KANARIE-NAME"
                                      :gehalt "KANARIE-GEHALT" :befund "KANARIE-BEFUND"}))
    (muss! (tun! :bericht-erfassen {:bericht-nr "ASR-1" :hergang "KANARIE-HERGANG"
                                    :melder "KANARIE-MELDER"})))
  (muss! (tun! :mangel-melden {:mangel-nr "M-1" :text "Reifen abgefahren"})))

(defn- mit-welt [f]
  (reset! admin (anmelden grants/admin-username pr/default-admin-password))
  (binding [tx/*auth-token* @admin]
    (anlegen-welt!)
    (anlegen-belegschaft!)
    (anlegen-daten!))
  (reset! tokens (into {} (map (fn [u] [u (anmelden u u)]))
                       (keys (merge belegschaft werkstatt))))
  (f))

(use-fixtures :once pod/with-mem-pod-auth mit-welt)

(defmacro ^:private als [u & body] `(tx/with-token (@tokens ~u) ~@body))

(defn- werte [typ] (mapv :values (:rows (:body (api/list-objects typ)))))

(defn- portal [] (tx/send! :get (paths/url :my-application {:rid "betriebsportal"})))

(defn- im-portal
  [typ]
  (mapv :values (get-in (portal) [:body :data typ])))

(defn- lesen [u typ]
  (als u (if (= "consumer" (first (belegschaft u))) (im-portal typ) (werte typ))))

(deftest vom-chef-bis-zum-vorfeld-lesen-alle-denselben-flug
  (doseq [u (keys belegschaft)]
    (testing u
      (is (= ["LX-1"] (mapv :flug-nr (lesen u :flug)))))))

(deftest die-zelle-folgt-dem-radius-nicht-dem-rang
  (testing "der Erlös eines Flugs gehört der Führung"
    (is (= "KANARIE-ERLOES" (:erloes (first (als "konzernchef" (werte :flug))))))
    (doseq [u ["safety" "b1-technik" "vorfeld-fra"]]
      (is (nil? (:erloes (first (lesen u :flug)))) u)))
  (testing "ein Befund gehört dem Fliegerarzt, auch vor dem Konzernchef"
    (is (nil? (:befund (first (als "konzernchef" (werte :besatzung))))))
    (is (= "KANARIE-GEHALT" (:gehalt (first (als "konzernchef" (werte :besatzung))))))
    (is (= "KANARIE-BEFUND" (:befund (first (als "fliegerarzt" (werte :besatzung))))))
    (is (nil? (:gehalt (first (als "fliegerarzt" (werte :besatzung)))))))
  (testing "VO 376/2014: den Melder kennt nur der Safety Manager, nicht der Accountable Manager"
    (let [chef (first (als "konzernchef" (werte :sicherheitsbericht)))]
      (is (= "KANARIE-HERGANG" (:hergang chef)))
      (is (nil? (:melder chef))))
    (is (= "KANARIE-MELDER" (:melder (first (als "safety" (werte :sicherheitsbericht))))))))

(deftest die-zeile-folgt-der-station
  (is (= #{"P-FRA" "P-MUC"} (set (map :pnr (lesen "konzernchef" :passagier)))))
  (is (= #{"P-FRA"} (set (map :pnr (lesen "vorfeld-fra" :passagier)))))
  (is (= #{"P-MUC"} (set (map :pnr (lesen "vorfeld-muc" :passagier)))))
  (testing "eine PNR der anderen Station gibt es auch über die ID nicht"
    (let [muc (binding [tx/*auth-token* @admin] (id-von :passagier :pnr "P-MUC"))
          url (paths/url :my-application-around {:rid "betriebsportal"})
          r   (als "vorfeld-fra" (tx/send! :post url {:type "passagier" :id muc}))]
      (is (not (str/includes? (pr-str (:body r)) "KANARIE-PNR-MUC"))))))

(deftest der-typ-bleibt-im-projekt
  (doseq [[u typ] [["vorfeld-fra" :besatzung] ["mechaniker" :besatzung]
                   ["vorfeld-fra" :sicherheitsbericht] ["personal" :sicherheitsbericht]]]
    (testing (str u " " typ)
      (is (= 403 (:status (als u (api/list-objects typ))))))))

(defn- freigabe! [u args]
  (als u (tun! :freigabe-zeichnen (merge {:mangel "M-1"} args))))

(deftest schreiben-folgt-der-funktion-nicht-dem-rang
  (testing "melden darf jeder, der den Betrieb liest"
    (is (ok? (als "mechaniker" (tun! :mangel-melden {:mangel-nr "M-2" :text "Leck"})))))
  (testing "Part-145: die Freigabe zeichnet Certifying Staff, nicht der Konzernchef"
    (doseq [u ["konzernchef" "mechaniker" "dispatcher"]]
      (is (= 422 (:status (freigabe! u {}))) u))
    (is (ok? (freigabe! "b1-technik" {}))))
  (testing "die Identität im verborgenen Parameter kommt vom Server, nicht vom Client"
    (let [fremd (@ids "b1-technik")]
      (is (= 422 (:status (freigabe! "mechaniker" {:wer fremd})))
          "wer sich als Prüfer ausgibt, zeichnet nicht")
      (is (= 422 (:status (freigabe! "mechaniker" {:wer "b1-technik"}))))))
  (testing "den Flug verzögert der Dispatcher"
    (is (= 422 (:status (als "b1-technik" (tun! :flug-verzoegern {:flug "LX-1"})))))
    (is (ok? (als "dispatcher" (tun! :flug-verzoegern {:flug "LX-1"})))))
  (testing "wer den Typ nicht liest, schreibt ihn nicht, auch nicht als Stammdaten"
    (is (not (ok? (als "mechaniker" (tun! :besatzung-erfassen
                                          {:personal-nr "X" :name "x" :gehalt "1"
                                           :befund "x"})))))))

(deftest ein-reiter-folgt-dem-typ-und-die-daten-ebenso
  (testing "wer die Besatzung nicht liest: ohne Reiter, und die Seite lädt"
    (let [r (als "mechaniker" (portal))]
      (is (ok? r) (pr-str (:status r) (:body r)))
      (is (not (str/includes? (pr-str (:body r)) "KANARIE-REITER")))
      (is (empty? (get-in r [:body :data :besatzung])))))
  (testing "wer sie liest: mit Reiter"
    (let [r (als "personal" (portal))]
      (is (ok? r))
      (is (str/includes? (pr-str (:body r)) "KANARIE-REITER"))))
  (testing "der Reiter ist Anzeige, der Riegel sitzt am Typ"
    (is (= 403 (:status (als "mechaniker" (api/list-objects :besatzung)))))))

(def ^:private fuellung
  (fn [objekt-id]
    {:rid "betriebsportal" :name "besatzung" :type "besatzung" :object-type "besatzung"
     :section "object-types" :id objekt-id :uuid objekt-id :branch "main" :version "1"
     :project "personal" :username "personal" :user "personal" :dataset "besatzung"
     :transform "besatzung"}))

(defn- adresse [id objekt-id]
  (let [url  (paths/url id)
        ps   (paths/params-of url)
        full (assoc (fuellung objekt-id) :q "KANARIE" :search "KANARIE" :query "KANARIE")
        frag (keep (fn [k] (when-let [v (get full k)] (str (name k) "=" v)))
                   (keys (paths/query id)))]
    (cond-> (paths/path-for url (merge (zipmap ps (repeat "x")) (select-keys full ps)))
      (seq frag) (str "?" (str/join "&" frag)))))

(defn- fragen [methode url]
  (let [f (future (try (tx/send! methode url)
                       (catch Throwable t {:status ::kaputt :body (ex-message t)})))
        r (deref f 3000 {:status ::haengt})]
    (when (= ::haengt (:status r)) (future-cancel f))
    r))

(defn- get-endpunkte []
  (for [[id binds] (server/handler-bindings) :when (:get binds)] id))

(defn- durchlauf [u urls]
  (let [token (@tokens u)]
    (doall (pmap (fn [[id url]] [id (tx/with-token token (fragen :get url))]) urls))))

(def ^:private offen-lesbar #{[:schema "KANARIE-REITER"] [:applications "KANARIE-REITER"]})

(deftest kein-endpunkt-verraet-eine-kanarie
  (let [objekt (binding [tx/*auth-token* @admin] (id-von :besatzung :personal-nr "C-1"))
        urls   (mapv (juxt identity #(adresse % objekt)) (get-endpunkte))
        funde  (atom [])
        offen  (atom #{})]
    (is (> (count urls) 100) "der Durchlauf muss wirklich über die Endpunkte gehen")
    (doseq [u (keys belegschaft)
            [id r] (durchlauf u urls)
            :let [text (when (= 200 (:status r)) (pr-str (:body r)))]]
      (when text (swap! offen conj id))
      (doseq [[kanarie darf] kanarien
              :when (and text (str/includes? text kanarie) (not (darf u))
                         (not (offen-lesbar [id kanarie])))]
        (swap! funde conj [u id kanarie])))
    (is (> (count @offen) 20) "und mindestens ein Teil muss antworten")
    (is (empty? @funde) (str "Datenabfluss: " (pr-str (vec (sort @funde)))))))

(deftest die-konsumentin-erreicht-nur-ihr-portal
  (let [frei (set/union (set (paths/ids-with-auth :public)) (set (paths/ids-with-auth :consumer))
                        (set (paths/ids-with-auth :build)) (set paths/consumer-surface))
        ids  (for [[id binds] (server/handler-bindings) :when (not (frei id))
                   m [:get :post :put :delete] :when (get binds m)]
               [id m])
        raus (for [[id m] ids
                   :let [r (als "vorfeld-fra" (fragen m (adresse id "x")))]
                   :when (not (or (= 403 (:status r))
                                  (and (= 400 (:status r)) (:request (paths/endpoints id)))))]
               [id m (:status r)])]
    (is (> (count ids) 100))
    (is (empty? raus) (pr-str (vec raus)))
    (testing "mit gültigem Körper: auch die allgemeine Action-Tür bleibt zu"
      (is (= 403 (:status (als "vorfeld-fra" (tun! :mangel-melden {:mangel-nr "M-9"
                                                                    :text "x"}))))))))

(deftest ein-admin-endpunkt-weist-jeden-anderen-ab
  (let [ids (for [id (keys paths/admin-only)
                  m [:get :post :put :delete] :when (get-in (server/handler-bindings) [id m])]
              [id m])
        raus (for [u ["konzernchef" "b1-technik"]
                   [id m] ids
                   :let [r (als u (fragen m (adresse id "x")))]
                   :when (not= 403 (:status r))]
               [u id m (:status r)])]
    (is (seq ids))
    (is (empty? raus) (pr-str (vec raus)))))

(deftest ohne-anmeldung-antworten-nur-die-oeffentlichen
  (let [ids (for [[id binds] (server/handler-bindings)
                  :when (and (:get binds) (not ((set (paths/ids-with-auth :public)) id))
                             (not= :build (:auth (paths/endpoints id)))
                             (not= :repo-token (:auth (paths/endpoints id))))]
              id)
        raus (for [id ids
                   :let [r (tx/with-token nil (fragen :get (adresse id "x")))]
                   :when (not= 401 (:status r))]
               [id (:status r)])]
    (is (seq ids))
    (is (empty? raus) (pr-str (vec raus)))))

(defn- rid-von [abschnitt feld n]
  (binding [tx/*auth-token* @admin]
    (some (fn [[r x]] (when (= n (name (get x feld))) r)) (abschnitt (:body (api/get! :schema))))))

(deftest ein-fremdes-projekt-ist-auch-neben-den-objekten-zu
  (testing "Daten nur mit Recht am Typ"
    (let [r (als "mechaniker" (api/get! :object-type-edit-history {:type "besatzung"}))]
      (is (not (str/includes? (pr-str (:body r)) "KANARIE-NAME"))
          "der Verlauf eines Typs im fremden Projekt")))
  (testing "Datasets: Zeilen nur mit Recht am Projekt"
    (let [rid (rid-von :datasets :dataset-name "personalakte")]
      (is rid)
      (is (not (str/includes?
                (pr-str (:body (als "mechaniker" (api/get! :dataset-preview {:id rid}))))
                "KANARIE-AKTE")))
      (is (not (str/includes?
                (pr-str (:body (als "mechaniker"
                                    (api/get! :transform-rows {:name "personalakte"}))))
                "KANARIE-AKTE")))))
  (testing "Datasets: Schreiben erst recht nicht"
    (is (= 403 (:status (als "mechaniker"
                             (api/post! :dataset-write {:name "personalakte"}
                                        {:rows [{:values {:akte "x"}}] :mode :snapshot
                                         :message "fremd"})))))))

(defn- beispiel-ziel
  [{[sektionen parameter] :on}]
  (let [s (if (keyword? sektionen) #{sektionen} (set (when (sequential? sektionen) sektionen)))]
    (cond-> {}
      parameter (assoc parameter (cond (s :object-types) "besatzung"
                                       (s :datasets)     "personalakte"
                                       :else             "x")))))

(defn- pflicht-adresse [id rq]
  (let [ziel (beispiel-ziel rq)
        url  (paths/url id)
        ps   (paths/params-of url)
        frag (remove (set ps) (keys ziel))]
    (cond-> (paths/path-for url (merge (zipmap ps (repeat "x")) (select-keys ziel ps)))
      (seq frag) (str "?" (str/join "&" (map #(str (name %) "=" (get ziel %)) frag))))))

(deftest jede-erklaerte-pflicht-greift-am-server
  (let [zeilen (for [[id binds] (server/handler-bindings)
                     m (keys binds)
                     :let [rq (paths/requirement id m)]
                     :when (:op rq)]
                 [id m rq])
        token  (@tokens "mechaniker")
        raus   (pmap (fn [[id m rq]]
                       (let [r (tx/with-token token (fragen m (pflicht-adresse id rq)))]
                         (when-not (or (= 403 (:status r))
                                       (and (= 400 (:status r)) (:request (paths/endpoints id))))
                           [id m (:status r)])))
                     zeilen)]
    (is (> (count zeilen) 100) "der Durchlauf muss die Tabelle abdecken")
    (is (empty? (remove nil? raus)) (pr-str (vec (remove nil? raus))))))

(deftest die-ontologie-liest-noch-jeder
  (is (contains? (set (map (comp name :object-type-name)
                           (vals (:object-types (:body (als "mechaniker" (api/get! :schema)))))))
                 "besatzung"))
  (let [r (als "mechaniker" (tx/get-bytes! (str (paths/url :kompass-export) "?project=personal")))]
    (is (str/includes? (String. ^bytes (:bytes r)) "befund"))))

(deftest wer-nichts-darf-aendert-nichts
  (testing "ein Neustart ist kein Recht jedes Angemeldeten"
    (is (= 403 (:status (als "mechaniker" (api/post! :reboot))))))
  (testing "Builds startet, wer am Ergebnis schreiben darf"
    (is (= 403 (:status (als "mechaniker" (api/post! :import-run nil))))))
  (testing "ein Typ im fremden Projekt bleibt, auch nach Löschen und Commit"
    (let [d (als "mechaniker" (api/delete! :object-type-by {:name "altakte"}))
          c (als "mechaniker" (api/post! :staging))]
      (is (= 403 (:status d)) (pr-str (:body d)))
      (is (some? (rid-von :object-types :object-type-name "altakte")) (pr-str (:body c))))))

(defn- typ-projekt [n]
  (binding [tx/*auth-token* @admin]
    (let [schema (:body (api/get! :schema))]
      (some-> (get-in schema [:object-types (rid-von :object-types :object-type-name n) :project])
              (#(get-in schema [:projects % :project-name]))
              name))))

(defn- neuer-typ [n projekt]
  {:name n :project projekt :primary-key :nr :plural (name n) :attributes [[:nr :string]]})

(deftest im-eigenen-projekt-entscheidet-die-rolle
  (testing "der Editor schreibt die Zeilen seines Projekts, der Fremde nicht"
    (let [schreiben #(api/post! :dataset-write {:name "werkzeugliste"}
                                {:rows [{:values {:nr %}}] :mode :snapshot :message "Inventur"})]
      (is (ok? (als "werkstatt-ed" (schreiben "W-1"))))
      (is (= 403 (:status (als "mechaniker" (schreiben "W-2")))))))
  (testing "der Editor legt im eigenen Projekt an, der Fremde nicht"
    (let [r (als "werkstatt-ed" (api/post! :object-type (neuer-typ :kaliber :werkstatt)))]
      (is (ok? r) (pr-str (:body r))))
    (is (= "werkstatt" (typ-projekt "kaliber")))
    (is (= 403 (:status (als "mechaniker"
                             (api/post! :object-type (neuer-typ :schmuggel :werkstatt))))))
    (is (nil? (rid-von :object-types :object-type-name "schmuggel"))))
  (testing "der Sammelweg über :install bleibt dem Fremden ebenso zu"
    (is (= 403 (:status (als "mechaniker"
                             (api/post! :install {:schema {:objects
                                                           {:schmuggel2
                                                            (dissoc (neuer-typ :x :personal)
                                                                    :name)}}}))))))
  (testing "löschen darf der Owner, nicht der Editor (Rollentabelle in catalog.grants)"
    (is (= 403 (:status (als "werkstatt-ed" (api/delete! :object-type-by {:name "werkzeug"})))))
    (let [r (als "werkstatt-chef" (api/delete! :object-type-by {:name "werkzeug"}))]
      (is (ok? r) (pr-str (:body r))))
    (is (nil? (rid-von :object-types :object-type-name "werkzeug"))))
  (testing "aus einem fremden Projekt holt niemand einen Typ zu sich"
    (is (= 403 (:status (als "werkstatt-ed" (api/put! :object-type-by {:name "umzugsakte"}
                                                     {:project :werkstatt})))))
    (is (= "personal" (typ-projekt "umzugsakte")))))

(defn- enthaelt? [r s] (str/includes? (pr-str (:body r)) s))

(defn- locator [rid] (last (str/split rid #"\.")))

(deftest angriff-zeilen-ueber-den-locator
  (let [rid (rid-von :datasets :dataset-name "personalakte")
        loc (locator rid)]
    (testing "der Fremde adressiert das Dataset über den Locator aus dem Schema"
      (let [r (als "mechaniker" (api/get! :transform-rows {:name loc}))]
        (println "[angriff] locator mechaniker" (:status r))
        (is (not (enthaelt? r "KANARIE-AKTE")))))
    (testing "der Editor legt im eigenen Projekt einen Namensvetter des Locators an"
      (let [anlegen (als "werkstatt-ed"
                         (api/post! :datasets
                                    {:name loc :project :werkstatt
                                     :columns [{:attribute-name :akte
                                                :attribute-type :string}]}))
            r (als "werkstatt-ed" (api/get! :transform-rows {:name loc}))]
        (println "[angriff] namensvetter anlegen" (:status anlegen)
                 "lesen" (:status r) (subs (pr-str (:body r)) 0
                                           (min 160 (count (pr-str (:body r))))))
        (is (not (enthaelt? r "KANARIE-AKTE")))))))

(defn- datei [typ nm & [projekt]]
  (cond-> {:object-type typ :kind :exploration :name nm :query {:find typ :where {}}
           :state {:viz :table}}
    projekt (assoc :project projekt)))

(defn- gelistet? [u id]
  (some #(= id (:id %)) (get-in (als u (api/get! :explorations)) [:body :explorations])))

(deftest private-exploration-kennt-nur-ihr-besitzer
  (let [v  (als "mechaniker" (api/post! :explorations (datei :flug "Meine")))
        id (str (get-in v [:body :id]))]
    (is (ok? v) (pr-str (:body v)))
    (is (true? (get-in v [:body :private])))
    (testing "der Besitzer ändert sie, ohne Recht am Typ"
      (is (ok? (als "mechaniker" (api/put! :exploration {:id id} {:name "Meine 2"})))))
    (testing "niemand sonst erfährt von ihr, auch der Admin nicht"
      (is (not (gelistet? "dispatcher" id)))
      (is (= 404 (:status (als "dispatcher" (api/get! :exploration {:id id})))))
      (is (= 404 (:status (als "dispatcher" (api/put! :exploration {:id id} {:name "x"})))))
      (is (= 404 (:status (als "dispatcher" (api/delete! :exploration {:id id})))))
      (is (= 404 (:status (binding [tx/*auth-token* @admin]
                            (api/get! :exploration {:id id}))))))
    (is (ok? (als "mechaniker" (api/delete! :exploration {:id id}))))))

(deftest oeffentliche-exploration-folgt-dem-ablageort
  (let [v  (als "werkstatt-ed" (api/post! :explorations (datei :flug "Wartung" "werkstatt")))
        id (str (get-in v [:body :id]))]
    (is (ok? v) (pr-str (:body v)))
    (is (false? (get-in v [:body :private])))
    (testing "ohne Editor-Rolle am Ort speichert niemand dorthin"
      (is (= 403 (:status (als "mechaniker"
                               (api/post! :explorations (datei :flug "x" "werkstatt")))))))
    (testing "wer den Ort nicht liest, sieht die Datei nicht, auch wenn er den Typ liest"
      (is (not (gelistet? "mechaniker" id)))
      (is (= 403 (:status (als "mechaniker" (api/get! :exploration {:id id})))))
      (is (= 403 (:status (als "mechaniker" (api/put! :exploration {:id id} {:name "x"})))))
      (is (= 403 (:status (als "mechaniker" (api/delete! :exploration {:id id}))))))
    (testing "die Rolle am Ort entscheidet, nicht wer sie angelegt hat"
      (is (ok? (als "werkstatt-chef" (api/put! :exploration {:id id} {:name "Neu"})))))
    (testing "privat machen nimmt sie aus dem Ablageort"
      (let [r (als "werkstatt-chef" (api/put! :exploration {:id id} {:private true}))]
        (is (ok? r) (pr-str (:body r)))
        (is (true? (get-in r [:body :private])))
        (is (= 404 (:status (als "werkstatt-ed" (api/get! :exploration {:id id})))))))
    (is (ok? (als "werkstatt-chef" (api/delete! :exploration {:id id}))))))

(deftest private-exploration-wandert-nur-mit-rolle-am-ziel
  (let [v  (als "mechaniker" (api/post! :explorations (datei :flug "Meine")))
        id (str (get-in v [:body :id]))
        w  (als "werkstatt-ed" (api/post! :explorations (datei :flug "Seine")))
        wd (str (get-in w [:body :id]))]
    (is (= 403 (:status (als "mechaniker"
                             (api/post! :kompass-place {:ref (str "exploration/" id)
                                                        :project "werkstatt"})))))
    (let [r (als "werkstatt-ed" (api/post! :kompass-place {:ref (str "exploration/" wd)
                                                           :project "werkstatt"}))]
      (is (ok? r) (pr-str (:body r))))
    (is (false? (get-in (als "werkstatt-chef" (api/get! :exploration {:id wd}))
                        [:body :private])))
    (testing "in einen Ordner kommt sie nicht: ihr Name stünde dort für jeden Leser"
      (is (= 400 (:status (als "werkstatt-ed"
                               (api/post! :kompass-place {:ref (str "exploration/" wd)
                                                          :parent-rid "folder"}))))))))

(deftest geteilte-exploration-oeffnet-keine-daten
  (let [v  (binding [tx/*auth-token* @admin]
             (api/post! :explorations (datei :besatzung "Lohnliste" "betrieb")))
        id (str (get-in v [:body :id]))]
    (is (ok? v) (pr-str (:body v)))
    (testing "die Datei sieht, wer den Ort liest"
      (is (ok? (als "mechaniker" (api/get! :exploration {:id id})))))
    (testing "ihre Objekte nicht"
      (let [r (als "mechaniker" (api/post! :ontology-query {:object-set id}))]
        (println "[angriff] geteilte exploration" (:status r))
        (is (not (ok? r)))
        (is (not (enthaelt? r "KANARIE-NAME")))))
    (testing "und ändern darf er sie als Leser nicht"
      (is (= 403 (:status (als "mechaniker" (api/put! :exploration {:id id} {:name "x"}))))))))

(deftest angriff-dashboard-mit-leserecht
  (is (= 403 (:status (als "mechaniker" (api/put! :explorer-dashboard {:object-type "flug"}
                                                  {:layout {:gekapert true}}))))))

(deftest angriff-schnappschuss-zeigt-fremde-actions
  (let [r (als "mechaniker" (api/get! :snapshot))]
    (println "[angriff] snapshot" (:status r)
             (vec (distinct (map :action-name (get-in r [:body :action-log-tail])))))
    (is (nil? (get-in r [:body :action-log-tail])) "das Protokoll nennt fremde Actions")
    (is (some? (get-in r [:body :max-action-seq])))))

(deftest angriff-zaehler-fremder-typen
  (let [rid (rid-von :object-types :object-type-name "sicherheitsbericht")
        r   (als "mechaniker" (api/get! :object-counts))]
    (println "[angriff] object-counts" (:status r) "enthaelt fremden Typ" (enthaelt? r rid))
    (is (not (enthaelt? r rid)))))

(deftest angriff-eingaben-fremder-laeufe
  (let [start (binding [tx/*auth-token* @admin]
                (api/post! :function-run {:language :clojure :source "(defn probe [geheim] [])"
                                      :inputs {:geheim "KANARIE-EINGABE"}}))
        laeufe (als "mechaniker" (api/get! :runs))
        ids    (keep #(or (:id %) (:run-id %)) (let [b (:body laeufe)] (if (map? b) (:runs b) b)))
        treffer (for [i (take 50 ids)
                      :let [r (als "mechaniker" (api/get! :run {:id i}))]
                      :when (enthaelt? r "KANARIE-EINGABE")]
                  i)]
    (println "[angriff] run start" (:status start) "liste" (:status laeufe)
             "ids" (count ids) "treffer" (count treffer)
             "liste enthaelt" (enthaelt? laeufe "KANARIE-EINGABE"))
    (is (empty? treffer))
    (is (not (enthaelt? laeufe "KANARIE-EINGABE")))))

(deftest angriff-verweigerung-nennt-das-fremde-projekt
  (let [r (als "mechaniker" (api/get! :transform-rows {:name "personalakte"}))]
    (println "[angriff] verweigerung" (:status r) (:body r))
    (is (= 403 (:status r)))
    (is (not (enthaelt? r "ri.sutra.main.project")))))

(deftest angriff-projektwechsel-auf-eigenem-zweig
  (let [z (str "angriff-" (uid))]
    (is (ok? (als "werkstatt-ed" (api/post! :branches {:parent "main" :name z}))))
    (let [r (als "werkstatt-ed"
                 (tx/with-branch z
                   (api/put! :object-type-by {:name "umzugsakte"} {:project :werkstatt})))]
      (println "[angriff] zweig umzug" (:status r))
      (is (= 403 (:status r))))))

(deftest wer-darf-liest-weiter-seine-zeilen
  (let [rid (rid-von :datasets :dataset-name "personalakte")]
    (testing "über den Namen und über die RID"
      (is (enthaelt? (als "personal" (api/get! :transform-rows {:name "personalakte"}))
                     "KANARIE-AKTE"))
      (is (enthaelt? (als "personal" (api/get! :transform-rows {:name rid})) "KANARIE-AKTE")))
    (testing "was das Schema nicht kennt, ist keine Tabelle, auch nicht für den Admin"
      (is (= 404 (:status (binding [tx/*auth-token* @admin]
                            (api/get! :transform-rows
                                      {:name (locator (rid-von :datasets :dataset-name
                                                               "werkzeugliste"))}))))))))

(deftest ein-lauf-zeigt-seine-eingaben-dem-der-ihn-startete
  (binding [tx/*auth-token* @admin]
    (let [id (:run-id (:body (api/post! :function-run
                                        {:language :clojure :source "(defn probe [geheim] [])"
                                         :inputs {:geheim "KANARIE-EIGEN"}})))]
      (is (enthaelt? (api/get! :run {:id id}) "KANARIE-EIGEN"))
      (is (not (enthaelt? (als "mechaniker" (api/get! :run {:id id})) "KANARIE-EIGEN")))
      (is (ok? (als "mechaniker" (api/get! :run {:id id}))) "der Fortschritt bleibt offen"))))

(deftest ein-zip-entpackt-nur-bis-zur-grenze-eines-release
  (let [zip (fn [n]
              (let [bos (java.io.ByteArrayOutputStream.)]
                (with-open [z (java.util.zip.ZipOutputStream. bos)]
                  (.putNextEntry z (java.util.zip.ZipEntry. "a.txt"))
                  (.write z (byte-array n))
                  (.closeEntry z))
                (.encodeToString (java.util.Base64/getEncoder) (.toByteArray bos))))
        unzipped @(requiring-resolve 'sutra.widget-sets.http/unzipped)]
    (is (= #{"a.txt"} (set (keys (unzipped (zip 10))))))
    (is (thrown? clojure.lang.ExceptionInfo (unzipped (zip (* 6 1024 1024)))))))

(defn- dokument-in [projekt]
  (get-in (api/post! :documents {:name "Vertraulich" :format :markdown :body "KANARIE-DOK"
                                 :project projekt})
          [:body :document :id]))

(deftest eine-rolle-auf-einer-datei-oeffnet-nur-diese-datei
  (binding [tx/*auth-token* @admin]
    (let [id     (dokument-in "personal")
          ander  (dokument-in "personal")
          ref    (str "document/" id)
          lesen  (fn [u i] (als u (:status (api/get! :document-of {:id i}))))
          geben! (fn [] (api/post! :kompass-file-grants
                                   {:ref ref :principal (@ids "mechaniker") :role :viewer}))]
      (testing "ohne Rolle kein Zugriff und ohne Projektschalter keine Rolle"
        (is (= 403 (lesen "mechaniker" id)))
        (is (= 409 (:status (geben!)))))
      (is (ok? (api/post! :kompass-project-sharing {:name "personal"} {:on true})))
      (is (ok? (geben!)))
      (testing "die Rolle oeffnet genau diese Datei, zum Lesen"
        (is (= 200 (lesen "mechaniker" id)))
        (is (= 403 (lesen "mechaniker" ander)))
        (is (= 403 (als "mechaniker" (:status (api/put! :document-of {:id id} {:body "weg"}))))))
      (testing "sie darf nicht selbst weitergeben und sieht die Rollen nicht"
        (is (= 404 (als "mechaniker" (:status (api/get! :kompass-file-grants {:ref ref})))))
        (is (= 404 (als "dispatcher" (:status (api/post! :kompass-share-link
                                                           {:ref ref :on true :role :owner}))))))
      (testing "ein Link gibt dem, der ihn oeffnet, seine eigene Rolle; abgeschaltet nichts mehr"
        (let [token (get-in (api/post! :kompass-share-link {:ref ref :on true :role :viewer})
                            [:body :link :token])]
          (is (= 403 (lesen "dispatcher" id)))
          (is (ok? (als "dispatcher" (api/post! :kompass-share-redeem {:token token} {}))))
          (is (= 200 (lesen "dispatcher" id)))
          (is (ok? (api/post! :kompass-share-link {:ref ref :on false})))
          (is (= 404 (als "b1-technik" (:status (api/post! :kompass-share-redeem
                                                             {:token token} {})))))))
      (testing "Abschalten am Projekt nimmt alle Rollen"
        (is (ok? (api/post! :kompass-project-sharing {:name "personal"} {:on false})))
        (is (= 403 (lesen "mechaniker" id)))
        (is (= 403 (lesen "dispatcher" id)))))))

(deftest ein-fremdes-projekt-schaltet-niemand-um
  (is (= 403 (als "mechaniker" (:status (api/post! :kompass-project-sharing {:name "personal"}
                                                     {:on true}))))))
