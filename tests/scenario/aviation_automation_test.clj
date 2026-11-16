(ns ^:api scenario.aviation-automation-test
  (:require [sutra.catalog.grants :as grants]
            [sutra.kit.anomaly :as anomaly]
            [clojure.data.json :as json]
            [support.in-project :refer [install-in-project]]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.access.principals :as pr]
            [support.aviation :refer [ontologie]]
            [support.pod :as pod]
            [support.system :as tsys]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(def ^:private einordnen-quelle
  "(defn mangel-einordnen-ki [mangel]
     (let [l (OntologyClient/use-llm
               {:model \"grok-test\" :source :xai
                :system \"Du ordnest Befunde eines Wartungsbetriebs einem ATA-Kapitel zu.\"
                :prompt \"Befund: {{mangel.beschreibung}}. Welches ATA-Kapitel, warum?\"
                :output {:struct {:ata :string :begruendung :string}}
                :max-iterations 2})]
       (OntologyClient/apply-action
        :mangel-einordnen
        {:mangel mangel :ata (:ata l) :begruendung (:begruendung l)})))")

(def ^:private welt
  (-> ontologie
      (update-in [:objects :mangel :attributes] into
                 [[:ata-kapitel :string] [:begruendung :string]])
      (assoc-in [:objects :user] {:attributes [[:user-id :string] [:rolle :string]]
                                  :primary-key :user-id :plural "Users"})
      (assoc :functions
             {:mangel-einordnen-ki {:name :mangel-einordnen-ki :language :clojure
                                    :target-type :mangel
                                    :param-types {:mangel :mangel}
                                    :source einordnen-quelle}})
      (update :actions merge
              {:mangel-einordnen
               {:params {:mangel {:name "Mangel" :type :mangel :required true}
                         :ata {:name "Ata" :type :string :required true
                               :constraints {:regex "[0-9]{2}"}}
                         :begruendung {:name "Begruendung" :type :string}}
                :rules [{:op :update :object-type :mangel :target [:get :mangel]
                         :values {:ata-kapitel [:get :ata] :begruendung [:get :begruendung]}}]
                :edits [{:object-type :mangel}]}
               :ki-einordnen
               {:params {:mangel {:name "Mangel" :type :mangel :required true}
                         :wer    {:type :string :prefill :current-user :exposure :hidden}}
                :submission-criteria [{:test {:w [:get :wer]} :pred [:not= :w "gast"]
                                       :message "Einordnen darf nur, wer Mängel bearbeitet."}]
                :rules [{:op :apply-edit-function :function :mangel-einordnen-ki}]
                :edits [{:object-type :mangel}]}
               :benutzer-anlegen
               {:params {:id {:name "Id" :type :string :required true}
                         :rolle {:name "Rolle" :type :string :required true}}
                :rules [{:op :create :object-type :user :id :id
                :values {:user-id [:get :id] :rolle [:get :rolle]}}]
                :edits [{:object-type :user}]}})))

(def ^:private admin (atom nil))
(def ^:private technikerin (atom nil))
(def ^:private gast (atom nil))

(defn- anmelden [u p] (-> (api/post! :login {:username u :password p}) :body :token))

(defn- tun! [action args] (api/mutate action args {:publish? true}))

(defn- benutzer! [name rolle]
  (let [id (str (java.util.UUID/randomUUID))]
    (assert (ok? (tun! :benutzer-anlegen {:id id :rolle rolle})))
    (assert (ok? (api/post! :register {:username name :password name :user-id id})))
    (anmelden name name)))

(defn- mit-welt [f]
  (reset! admin (anmelden grants/admin-username pr/default-admin-password))
  (binding [tx/*auth-token* @admin]
    (let [r (install-in-project {:schema welt})] (assert (ok? r) (pr-str (:body r))))
    (assert (ok? (api/post! :staging)))
    (assert (ok? (api/mutate :create-llm-model {:name "grok-test" :in 0.1 :out 0.1}
                             {:stage? true})))
    (assert (ok? (api/post! :staging)))
    (reset! technikerin (benutzer! "technikerin" "ontology-editor"))
    (reset! gast (benutzer! "gast" "ontology-viewer"))
    (doseq [k ["D-AINA" "D-AINB"]]
      (assert (ok? (tun! :luftfahrzeug-registrieren {:kennzeichen k :muster "A320"}))))
    (doseq [[nr k b] [["A-1" "D-AINA" "Hydraulikleck am linken Hauptfahrwerk"]
                      ["A-2" "D-AINA" "Leselampe Reihe 12 defekt"]
                      ["A-3" "D-AINB" "Kratzer an der Frontscheibe"]]]
      (assert (ok? (tun! :mangel-melden
                         {:mangel-nr nr :luftfahrzeug k :beschreibung b :kategorie "C"}))))
    (f)))

(use-fixtures :once pod/with-mem-pod-auth mit-welt)

(defn- antwort [v]
  {:tool_calls [{:id "c1" :type "function"
                 :function {:name "final_answer" :arguments (json/write-str {"value" v})}}]})

(defn- mit-modell
  [antwort-von f]
  (let [gesehen (atom [])]
    (tsys/with-provided {:llm/complete (fn [req] (swap! gesehen conj req)
                                         {:request-id (str "req-" (count @gesehen))
                                          :message
                                            (if (fn? antwort-von) (antwort-von req)
                                              (antwort antwort-von))})}
      [(f) @gesehen])))

(defn- mangel [nr]
  (binding [tx/*auth-token* @admin]
    (some #(when (= nr (get-in % [:values :mangel-nr])) %)
          (:rows (:body (api/list-objects :mangel))))))

(deftest ein-mensch-loest-die-einordnung-aus
  (let [[r gesehen] (mit-modell {"ata" "32" "begruendung" "Fahrwerk"}
                                #(tx/with-token @technikerin (tun! :ki-einordnen {:mangel "A-1"})))]
    (testing "das Modell liest den Befund; seine Antwort wird über die Action geschrieben"
      (is (ok? r) (pr-str (:body r)))
      (is (= 1 (count gesehen)))
      (is (str/includes? (pr-str (:request (first gesehen))) "Hydraulikleck")
          "der Prompt trägt den Befund")
      (is (= ["32" "Fahrwerk"] ((juxt :ata-kapitel :begruendung) (:values (mangel "A-1"))))))

    (testing "§2.5 die Antwort ist ein Orakel und steht im Laufergebnis: Modell, Prompt, Anfrage, Ausgabe"
      (let [[d] (get-in r [:body :result :decisions])]
        (is (= "grok-test" (:model d)) (pr-str d))
        (is (str/includes? (:prompt d) "Hydraulikleck"))
        (is (re-matches #"[0-9a-f]{64}" (str (:prompt-sha d))))
        (is (some #(= "req-1" (:request-id %)) (:log d)))
        (is (some #(= {:ata "32" :begruendung "Fahrwerk"} (:output %)) (:log d)))))

    (testing "die Edit-Historie nennt, wer die Action ausgelöst hat"
      (let [h (binding [tx/*auth-token* @admin] (:history (:body (api/edit-history :mangel))))]
        (is (some #(= "32" (get-in % [:edited-values :ata-kapitel])) h)))))

  (testing "was die Action nicht zulässt, schreibt das Modell nicht"
    (let [[r] (mit-modell {"ata" "Fahrwerk" "begruendung" "?"}
                          #(tx/with-token @technikerin (tun! :ki-einordnen {:mangel "A-2"})))]
      (is (not (ok? r)))
      (is (re-find #"ata" (pr-str (:body r))))
      (is (nil? (get-in (mangel "A-2") [:values :ata-kapitel])))))

  (testing "eine Antwort, die nicht zur erklärten Form passt, schreibt nichts"
    (let [[r] (mit-modell {"kapitel" 32}
                          #(tx/with-token @technikerin (tun! :ki-einordnen {:mangel "A-2"})))]
      (is (not (ok? r)))
      (is (nil? (get-in (mangel "A-2") [:values :ata-kapitel])))))

  (testing "ein Modell, das nicht antwortet oder dessen Budget erschöpft ist, schreibt nichts und sagt es"
    (let [[r] (mit-modell (fn [_]
                            (anomaly/raise!
                             :upstream "language-model call refused (429): daily LLM budget reached"
                             {:status 429}))
                          #(tx/with-token @technikerin (tun! :ki-einordnen {:mangel "A-2"})))]
      (is (not (ok? r)))
      (is (re-find #"429" (pr-str (:body r))))
      (is (nil? (get-in (mangel "A-2") [:values :ata-kapitel])))))

  (testing "wen die Action nicht zulässt, der lässt auch das Modell nicht ändern — das Recht
            liegt an der Action und ihren Kriterien, nicht an der Rolle"
    (let [[r _gesehen] (mit-modell {"ata" "33" "begruendung" "Licht"}
                                  #(tx/with-token @gast (tun! :ki-einordnen {:mangel "A-2"})))]
      (is (not (ok? r)) (pr-str (:body r)))
      (is (nil? (get-in (mangel "A-2") [:values :ata-kapitel]))))))

(defn- automation-id [nm]
  (binding [tx/*auth-token* @admin]
    (some #(when (= nm (:name %)) (:id %)) (:automations (:body (api/get! :automations))))))

(defn- laufen! [id] (binding [tx/*auth-token* @admin] (api/post! :automation-run {:id id} nil)))

(defn- vorschlaege [id & [status]]
  (binding [tx/*auth-token* @admin] (:body (api/automation-proposals id status))))

(defn- ereignisse [id] (binding [tx/*auth-token* @admin] (:body (api/automation-events id))))

(deftest eine-automation-schlaegt-vor-ein-mensch-entscheidet
  (let [nm "einordnung-vorschlagen"]
    (tx/with-token @technikerin
      (is (ok? (api/post! :automations
                          {:name nm :on :mangel
                           :when [:and [:= [:get :zustand] "offen"] [:= [:get :ata-kapitel] nil]]
                           :effect {:function :mangel-einordnen-ki :inputs {:mangel {:self :id}}
                                    :mode :propose}
                           :every-ms 3600000}))))
    (let [id (automation-id nm)]
      (testing "wer sie anlegt, besitzt sie"
        (is (= "technikerin"
               (:owner (some #(when (= nm (:name %)) %)
                             (:automations
                              (:body (binding [tx/*auth-token* @admin]
                                       (api/get! :automations)))))))))

      (testing "§10.4 propose: je passendem Mangel ein Vorschlag, wie oft sie auch läuft — das Modell wird nicht erneut gefragt"
        (let [[_
               gesehen] (mit-modell (fn [req] (antwort (if (str/includes? (pr-str req) "Leselampe")
                                                           {"ata" "33" "begruendung" "Licht"}
                                                           {"ata" "56" "begruendung" "Fenster"})))
                                      #(do (laufen! id) (laufen! id)))]
          (is (= 2 (count gesehen)) "A-2 und A-3; A-1 ist schon eingeordnet"))
        (let [{:keys [proposals]} (vorschlaege id)]
          (is (= 2 (count proposals)))
          (is (every? #(= "mangel-einordnen" (name (:action %))) proposals))
          (is (every? #(= "grok-test" (-> % :decision-log first :model)) proposals)))
        (is (nil? (get-in (mangel "A-2") [:values :ata-kapitel])) "vorgeschlagen, nicht angewandt"))

      (testing "angenommen wird angewandt, auf Wunsch mit dem Wert der Prüferin; abgelehnt bleibt es, wie es war"
        (let [ps (into {} (map (juxt #(str/includes? (pr-str %) "Leselampe") identity))
                       (:proposals (vorschlaege id)))]
          (is (= "applied"
                 (:status (:body (binding [tx/*auth-token* @admin]
                                   (api/accept-proposal
                                    id (:id (ps true))
                                    {:ata "33" :begruendung "Kabinenbeleuchtung"}))))))
          (is (ok? (binding [tx/*auth-token* @admin]
                     (api/post! :automation-proposal-reject {:id id :proposal (:id (ps false))}
                                {}))))
          (is (= ["33" "Kabinenbeleuchtung"]
                 ((juxt :ata-kapitel :begruendung) (:values (mangel "A-2")))))
          (is (nil? (get-in (mangel "A-3") [:values :ata-kapitel])))))

      (testing "ein entschiedener Vorschlag kommt beim nächsten Lauf nicht wieder"
        (mit-modell {"ata" "56" "begruendung" "Fenster"} #(laufen! id))
        (is (zero? (count (filter #(= "open" (name (:status % ""))) (:proposals (vorschlaege id)))))
            (pr-str (map :status (:proposals (vorschlaege id)))))))))

(deftest eine-automation-wendet-an-und-holt-nach
  (let [nm "einordnen"]
    (tx/with-token @technikerin
      (is (ok? (api/post! :automations {:name nm :on :mangel
                                        :when [:= [:get :mangel-nr] "A-3"]
                                        :effect {:function :mangel-einordnen-ki :mode :apply}
                                        :every-ms 3600000}))))
    (let [id (automation-id nm)]
      (testing "ein vorübergehender Fehler schreibt nichts und wird festgehalten"
        (mit-modell (fn [_]
                      (throw (ex-info "language-model call refused (429): daily LLM budget reached"
                                      {:status 429})))
                    #(laufen! id))
        (let [{:keys [events counts]} (ereignisse id)]
          (is (= 1 (get counts "failed")) (pr-str counts))
          (is (true? (:transient (first events))))
          (is (re-find #"429" (str (:error (first events))))))
        (is (nil? (get-in (mangel "A-3") [:values :ata-kapitel]))))
      (testing "die Vorschau zeigt, was sie täte, und schreibt nichts"
        (let [[r] (mit-modell {"ata" "56" "begruendung" "Fenster"}
                              #(binding [tx/*auth-token* @admin]
                                 (api/preview-automation
                                  id {:object-id (:id (mangel "A-3")) :with-model? true})))]
          (is (ok? r) (pr-str (:body r)))
          (is (nil? (get-in (mangel "A-3") [:values :ata-kapitel])))))
      (testing "ein Lauf von Hand holt das fehlgeschlagene Ereignis sofort nach; die Änderung läuft als ihr Besitzer"
        (let [[_ gesehen] (mit-modell {"ata" "56" "begruendung" "Fenster"} #(laufen! id))]
          (is (= 1 (count gesehen))))
        (is (= "56" (get-in (mangel "A-3") [:values :ata-kapitel])))
        (let [h (binding [tx/*auth-token* @admin] (:history (:body (api/edit-history :mangel))))
              e (some #(when (= "56" (get-in % [:edited-values :ata-kapitel])) %) h)]
          (is (some? e))
          (is (not= "admin" (:edited-by e)) "nicht als der, der den Lauf angestoßen hat"))))))

(deftest eine-regel-erklaert-aog
  (tx/with-token @technikerin
    (is (ok? (api/post! :automations
                        {:name "kategorie-a-aog" :on :mangel
                         :when [:= [:get :kategorie] "A"]
                         :effect {:action :aog-erklaeren
                                  :args
                                    {:luftfahrzeug {:ref :luftfahrzeug :pk {:self :kennzeichen}}}}
                         :every-ms 3600000}))))
  (binding [tx/*auth-token* @admin]
    (is (ok? (tun! :mangel-melden
                   {:mangel-nr "A-9" :luftfahrzeug "D-AINB" :beschreibung "Triebwerksbrand-Warnung"
                    :kategorie "A"}))))
  (laufen! (automation-id "kategorie-a-aog"))
  (let [lfz (binding [tx/*auth-token* @admin]
              (some #(when (= "D-AINB" (get-in % [:values :kennzeichen])) %)
                    (:rows (:body (api/list-objects :luftfahrzeug)))))]
    (is (= "aog" (get-in lfz [:values :status])) (pr-str lfz))))
