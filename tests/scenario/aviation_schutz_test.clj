(ns ^:api scenario.aviation-schutz-test
  (:require [sutra.catalog.grants :as grants]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [support.in-project :refer [install-in-project]]
            [sutra.access.principals :as pr]
            [support.aviation :refer [ontologie]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(def ^:private welt
  (-> ontologie
      (assoc-in [:objects :user] {:attributes [[:user-id :string] [:rolle :string]]
                                  :primary-key :user-id :plural "Users"})
      (assoc-in [:actions :benutzer-anlegen]
                {:params {:id {:name "Id" :type :string :required true}
                          :rolle {:name "Rolle" :type :string :required true}}
                 :rules [{:op :create :object-type :user :id :id
                 :values {:user-id [:get :id] :rolle [:get :rolle]}}]
                 :edits [{:object-type :user}]})
      (assoc-in [:projects :werkstatt] {})
      (assoc-in [:objects :werkzeug]
                {:project :werkstatt :primary-key :nr :attributes [[:nr :string] [:name :string]]
                 :plural "Werkzeuge"})
      (assoc-in [:objects :halle] {:project :werkstatt :primary-key :nr :attributes [[:nr :string]]
                                   :plural "Hallen"})
      (assoc-in [:objects :spind] {:project :werkstatt :primary-key :nr :attributes [[:nr :string]]
                                   :plural "Spinde"})
      (assoc-in [:objects :regal-a]
                {:project :werkstatt :primary-key :nr :attributes [[:nr :string]]
                 :plural "Regale A"})
      (assoc-in [:objects :regal-b]
                {:project :werkstatt :primary-key :nr :attributes [[:nr :string]]
                 :plural "Regale B"})))

(defn- anmelden [u p] (-> (api/post! :login {:username u :password p}) :body :token))

(def ^:private leute (atom {}))
(def ^:private admin (atom nil))

(defn- benutzer! [name]
  (let [id (str (java.util.UUID/randomUUID))]
    (assert (ok? (api/mutate :benutzer-anlegen {:id id :rolle "ontology-editor"} {:publish? true})))
    (assert (ok? (api/post! :register {:username name :password name :user-id id})))
    (swap! leute assoc name {:id id :token (anmelden name name)})))

(defn- eigentuemer! [projekt & namen]
  (assert (ok? (api/mutate :update-project
                           {:name projekt
                            :grants (vec (for [n namen]
                                           {:principal (get-in @leute [n :id])
                                            :role :ontology-owner}))}
                           {:stage? true})))
  (assert (ok? (api/post! :staging))))

(defn- mit-welt [f]
  (reset! admin (anmelden grants/admin-username pr/default-admin-password))
  (binding [tx/*auth-token* @admin]
    (let [r (install-in-project {:schema welt})] (assert (ok? r) (pr-str (:body r))))
    (assert (ok? (api/post! :staging)))
    (doseq [n ["werki" "gast"]] (benutzer! n))
    (eigentuemer! :werkstatt "werki")
    (f)))

(use-fixtures :once pod/with-mem-pod-auth mit-welt)

(defmacro ^:private als [wer & body] `(tx/with-token (get-in @leute [~wer :token]) ~@body))

(defn- direkt!
  [objects]
  (let [r (install-in-project :werkstatt {:schema {:objects objects}})]
    (if (ok? r)
      (let [c (api/post! :staging)]
        (when-not (ok? c) (api/delete! :staging))
        c)
      (do (api/delete! :staging) r))))

(defn- attribute [typ]
  (->> (vals (:object-types (:body (api/get! :schema))))
       (some #(when (= typ (:object-type-name %)) %))
       :attribute-list (map :attribute-name) set))

(defn- vorschlag! [wer objects]
  (let [z (str "schutz-" (subs (str (java.util.UUID/randomUUID)) 0 8))]
    (als wer
         (assert (ok? (api/post! :branches {:parent "main" :name z})))
         (tx/with-branch z
           (assert (ok? (install-in-project :werkstatt {:schema {:objects objects}})))
           (assert (ok? (api/post! :staging))))
         (let [r (api/post! :proposals {:source z :target "main" :title "schutz"})]
           (assert (ok? r) (pr-str (:body r)))
           (get-in r [:body :id])))))

(deftest schutz-je-ressource
  (testing "nur ein Eigentümer des Projekts schützt"
    (is (= 403
           (:status (als "gast"
                         (api/post! :protections
                                    {:resources ["object-types/werkzeug"] :on true})))))
    (let [r (als "werki" (api/post! :protections {:resources ["object-types/werkzeug"] :on true}))]
      (is (ok? r) (pr-str (:body r)))
      (is (= ["object-types/werkzeug"] (get-in r [:body :resources])))))

  (testing "die geschützte Ressource ändert sich direkt nicht — die ungeschützte schon"
    (tx/with-token @admin
      (let [c (direkt! {:werkzeug {:project :werkstatt
                                   :primary-key :nr
                                   :attributes [[:nr :string] [:name :string] [:ort :string]]}})]
        (is (= 403 (:status c)) (pr-str (:body c)))
        (is (re-find #"werkzeug" (str (get-in c [:body :error])))))
      (is (not (contains? (attribute :werkzeug) :ort)))
      (is (ok? (direkt! {:halle {:project :werkstatt
                                 :primary-key :nr
                                 :attributes [[:nr :string] [:flaeche :decimal]]}})))
      (is (contains? (attribute :halle) :flaeche))))

  (testing "über ein Proposal landet sie"
    (let [id (vorschlag! "gast"
                         {:werkzeug {:project :werkstatt
                                     :primary-key :nr
                                     :attributes [[:nr :string] [:name :string] [:ort :string]]}})]
      (testing "wer nicht bearbeiten darf, wartet auf einen Bearbeiter"
        (is (not= "merged"
                  (get-in (als "gast" (api/post! :proposal-merge {:id id} nil)) [:body :status])))
        (is (ok? (als "werki" (api/post! :proposal-approve {:id id} {})))))
      (is (= "merged"
             (get-in (als "gast" (api/post! :proposal-merge {:id id} nil)) [:body :status])))
      (is (contains? (attribute :werkzeug) :ort))))

  (testing "aufgehoben ist sie wieder direkt änderbar"
    (is (ok? (als "werki"
                  (api/post! :protections {:resources ["object-types/werkzeug"] :on false}))))
    (tx/with-token @admin
      (is (ok? (direkt! {:werkzeug {:project :werkstatt
                                    :primary-key :nr :attributes
                                      [[:nr :string] [:name :string] [:ort :string]
                                       [:preis :decimal]]}}))))))

(deftest ein-projekt-schuetzt-was-neu-entsteht
  (is (= 403
         (:status (als "gast"
                       (api/post! :project-policy {:name "werkstatt"} {:protect-all true})))))
  (is (ok? (als "werki" (api/post! :project-policy {:name "werkstatt"} {:protect-all true}))))
  (try
    (tx/with-token @admin
      (testing "Vorhandenes bleibt offen, das Anlegen selbst geht direkt"
        (is (ok? (direkt! {:regal-a {:project :werkstatt :primary-key :nr :plural "Regale A"
                                     :attributes [[:nr :string] [:fach :string]]}})))
        (is (ok? (direkt! {:kran {:project :werkstatt :primary-key :nr :plural "Kräne"
                                  :attributes [[:nr :string]]}}))))
      (testing "danach ist die neue Ressource geschützt"
        (is (= 403
               (:status (direkt! {:kran {:project :werkstatt :primary-key :nr :plural "Kräne"
                                         :attributes [[:nr :string] [:last :long]]}})))))
      (testing "eine Ressource eines anderen Projekts nicht"
        (is (ok? (install-in-project {:schema
                                        {:objects
                                           {:schlepper {:primary-key :nr :attributes [[:nr :string]]
                                                        :plural "Schlepper"}}}})))
        (is (ok? (api/post! :staging)))))
    (finally
      (als "werki" (api/post! :project-policy {:name "werkstatt"} {:protect-all false})))))

(deftest ein-checkpoint-verlangt-eine-begruendung
  (is (ok? (als "werki" (api/post! :project-policy {:name "werkstatt"}
                                   {:review [{:when [:always] :needs [:justification]}]}))))
  (is (ok? (als "werki" (api/post! :protections {:resources ["object-types/spind"] :on true}))))
  (try
    (let [id (vorschlag! "werki"
                         {:spind {:project :werkstatt :primary-key :nr
                                  :attributes [[:nr :string] [:tore :long]]}})]
      (testing "ohne Begründung bleibt die Aufgabe offen"
        (let [m (als "werki" (api/post! :proposal-merge {:id id} nil))]
          (is (not= "merged" (get-in m [:body :status])))
          (is (re-find #"justification" (str (get-in m [:body :error]))) (pr-str (:body m)))))
      (testing "mit Begründung landet es — kein Prüfer verlangt"
        (is (ok? (als "werki"
                      (api/post! :proposal-ask-review {:id id}
                                 {:justification "Neue Tore nach Umbau"}))))
        (is (= "merged"
               (get-in (als "werki" (api/post! :proposal-merge {:id id} nil)) [:body :status])))))
    (testing "eine ungeschützte Ressource antwortet der Projektregel nicht"
      (let [id (vorschlag! "werki"
                           {:halle {:project :werkstatt :primary-key :nr
                                    :attributes [[:nr :string] [:flaeche :decimal]
                                                 [:tore :long]]}})]
        (is (= "merged"
               (get-in (als "werki" (api/post! :proposal-merge {:id id} nil)) [:body :status])))))
    (finally
      (als "werki" (api/post! :protections {:resources ["object-types/spind"] :on false}))
      (als "werki" (api/post! :project-policy {:name "werkstatt"} {:review nil})))))

(deftest landungen-auf-ein-ziel-laufen-durch-eine-warteschlange
  (testing "zwei Proposals verschiedener Ressourcen, gleichzeitig gelandet: beide landen"
    (let [a (vorschlag! "werki"
                        {:regal-a {:project :werkstatt
                                   :primary-key :nr
                                   :attributes [[:nr :string] [:gewicht :decimal]]}})
          b (vorschlag! "werki"
                        {:regal-b {:project :werkstatt
                                   :primary-key :nr :attributes [[:nr :string] [:hoehe :decimal]]}})
          fs (mapv (fn [id] (future (als "werki" (api/post! :proposal-merge {:id id} nil)))) [a b])
          rs (mapv deref fs)]
      (is (every? #(= "merged" (get-in % [:body :status])) rs) (pr-str (mapv :body rs)))
      (is (contains? (attribute :regal-a) :gewicht))
      (is (contains? (attribute :regal-b) :hoehe)))))

(defn- zeilen [opts] (:branches (:body (api/get! :branches opts))))

(deftest eine-ressource-kehrt-zum-stand-des-ziels-zurueck
  (let [z (str "weg-" (subs (str (java.util.UUID/randomUUID)) 0 8))]
    (is (ok? (als "werki" (api/post! :branches {:parent "main" :name z}))))
    (als "werki"
         (tx/with-branch z
           (is (ok? (install-in-project
                     :werkstatt
                     {:schema {:objects {:lager {:project :werkstatt :primary-key :nr
                                                 :plural "Lager" :attributes [[:nr :string]]}
                                         :halle {:project :werkstatt :primary-key :nr
                                                 :attributes [[:nr :string]
                                                              [:breite :decimal]]}}}})))
           (is (ok? (api/post! :staging)))))
    (let [aufgaben #(:tasks (:body (als "werki" (api/get! :branch-changes {:name z}))))
          von      (fn [n] (:task (some #(when (= n (:name %)) %) (aufgaben))))]
      (is (= #{"lager" "halle"} (set (map :name (aufgaben)))))
      (testing "nur was der Zweig geändert hat"
        (is (= 400 (:status (als "werki" (api/post! :branch-resources {:name z}
                                                    {:remove ["object-types/gibt-es-nicht"]}))))))
      (testing "eine geänderte Ressource hat danach wieder den Stand von main"
        (is (ok? (als "werki" (api/post! :branch-resources {:name z} {:remove [(von "halle")]}))))
        (is (= #{"lager"} (set (map :name (aufgaben)))))
        (is (not (contains? (tx/with-branch z (attribute :halle)) :breite))))
      (testing "eine neue Ressource ist danach vom Zweig verschwunden"
        (is (ok? (als "werki" (api/post! :branch-resources {:name z} {:remove [(von "lager")]}))))
        (is (empty? (aufgaben)))))))

(deftest maschinenzweige-stehen-nicht-in-der-liste
  (tx/with-token @admin
    (install-in-project :werkstatt {:schema {:objects {:spind {:project :werkstatt :primary-key :nr
                                                               :attributes [[:nr :string]
                                                                            [:farbe :string]]}}}})
    (try
      (let [namen #(set (map :name (zeilen %)))
            alle  (namen {:machine true})]
        (is (some #(re-find #"^staging/" %) alle) "der Entwurf liegt auf einem Maschinenzweig")
        (is (not-any? #(re-find #"^staging/" %) (namen {}))))
      (finally (api/delete! :staging)))))

(deftest ein-merge-nennt-was-zu-bauen-waere-und-steht-im-verlauf
  (let [id (vorschlag! "werki" {:halle {:project :werkstatt :primary-key :nr
                                        :attributes [[:nr :string] [:hoehe :decimal]]}})
        m  (als "werki" (api/post! :proposal-merge {:id id} nil))]
    (is (= "merged" (get-in m [:body :status])) (pr-str (:body m)))
    (is (some #{"halle"} (get-in m [:body :build-scope :modified])))
    (is (= ["merged"] (mapv :outcome (get-in (api/get! :proposal {:id id})
                                             [:body :merge-history]))))))

(deftest instanzweites-bleibt-auf-offenem-main-beim-admin
  (is (false? (:protected (:body (tx/with-token @admin (api/get! :branch-policy {:name "main"}))))))
  (let [r (als "werki" (let [i (install-in-project {:schema {:projects {:fremd {}}}})]
                         (if (ok? i) (api/post! :staging) i)))]
    (is (= 403 (:status r)) (pr-str (:body r)))
    (als "werki" (api/delete! :staging))))
