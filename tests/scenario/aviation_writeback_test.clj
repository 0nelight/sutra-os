(ns ^:api scenario.aviation-writeback-test
  (:require [clojure.data.json :as json]
            [sutra.catalog.grants :as grants]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.sources.egress :as egress]
            [next.jdbc :as jdbc]
            [sutra.access.principals :as pr]
            [support.aviation :refer [ontologie]]
            [support.in-project :refer [in-project install-in-project]]
            [support.pod :as pod]
            [support.wire :refer [ok? publish!]]
            [sutra.types.paths :as paths]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]
            [next.jdbc.result-set]))

(defonce ^:private erp (atom nil))

(defn- erp-url [] (str (:base @erp) "/mro?user=root"))

(defn- erp-zeilen []
  (with-open [c (jdbc/get-connection (erp-url))]
    (jdbc/execute! c
                   ["SELECT MANGEL_NR, BESCHREIBUNG, KENNZEICHEN FROM MANGEL_ERP ORDER BY MANGEL_NR"]
                   {:builder-fn next.jdbc.result-set/as-unqualified-lower-maps})))

(defn- quelle []
  {:type :mysql :jdbc-url (erp-url) :egress [(:host @erp)] :access :read-write :project :werft
   :writeback {"MANGEL_ERP" {:object-type :mangel :pk "MANGEL_NR" :identity "MANGEL_NR"
                             :writable-columns ["BESCHREIBUNG" "KENNZEICHEN"]
                             :insertable-columns ["BESCHREIBUNG" "KENNZEICHEN"]
                             :column-map {"mangel-nr" "MANGEL_NR" "beschreibung" "BESCHREIBUNG"
                                          "kennzeichen" "KENNZEICHEN"}
                             :log :none}}
   :webhooks {:melden {:kind :table :table "MANGEL_ERP" :op :insert}}})

(def ^:private aktionen
  {:extern-melden
   {:params {:luftfahrzeug {:name "Luftfahrzeug" :type :luftfahrzeug :required true}
             :beschreibung {:name "Beschreibung" :type :string :required true}}
    :rules [{:op :create :object-type :mangel
             :values {:mangel-nr [:get :wb :identity] :kennzeichen [:get :luftfahrzeug :kennzeichen]
                      :beschreibung [:get :beschreibung] :kategorie "B" :zustand "offen"}}
            {:op :invoke-webhook :phase :writeback :source :erp :webhook :melden :as :wb
             :params {:beschreibung [:get :beschreibung]
                      :kennzeichen [:get :luftfahrzeug :kennzeichen]}}]
    :edits [{:object-type :mangel}]}
   :zwei-extern-melden
   {:rules [{:op :invoke-webhook :phase :writeback :source :erp :webhook :melden :as :wb
             :params [{:beschreibung "Riss Klappe" :kennzeichen "D-AINA"}
                      {:beschreibung "Reifen abgefahren" :kennzeichen "D-AINA"}]}]}
   :zwei-mit-fehler
   {:rules [{:op :invoke-webhook :phase :writeback :source :erp :webhook :melden :as :wb
             :params [{:beschreibung "Delle Nase" :kennzeichen "D-AINA"}
                      {:beschreibung "x" :kennzeichen "D-AINA"}]}]}
   :nachmelden
   {:params {:luftfahrzeug {:name "Luftfahrzeug" :type :luftfahrzeug :required true}}
    :rules [{:op :update :object-type :luftfahrzeug :target [:get :luftfahrzeug]
             :values {:muster "geprueft"}}
            {:op :invoke-webhook :phase :side-effect :source :erp :webhook :melden
             :params [{:beschreibung "Ölstand" :kennzeichen "D-AINA"}
                      {:beschreibung "Bremsbelag" :kennzeichen "D-AINA"}]}]
    :edits [{:object-type :luftfahrzeug}]}})

(defn- mit-erp [f]
  (reset! erp (pod/start-source-mysql!))
  (try
    (with-open [c (jdbc/get-connection (str (:base @erp) "/?user=root"))]
      (jdbc/execute! c ["CREATE DATABASE mro"]))
    (with-open [c (jdbc/get-connection (erp-url))]
      (jdbc/execute! c
                     [(str "CREATE TABLE MANGEL_ERP (MANGEL_NR INT AUTO_INCREMENT PRIMARY KEY,"
                           " BESCHREIBUNG VARCHAR(200) NOT NULL CHECK (CHAR_LENGTH(BESCHREIBUNG) > "
                           "2),"
                           " KENNZEICHEN VARCHAR(10) NOT NULL)")]))
    (f)
    (finally (pod/stop-source-mysql! @erp))))

(defn- mit-agent [f]
  (let [kf (doto (java.io.File/createTempFile "werft-egress" ".pkcs8") (.delete))]
    (try
      (egress/stop!)
      (egress/start! {:keyfile (str kf) :port (pod/egress-port)})
      (f)
      (finally (try (egress/stop!) (catch Exception _ nil))))))

(defn- als-admin [f]
  (binding [tx/*auth-token* (-> (api/post! :login {:username grants/admin-username
                                                   :password pr/default-admin-password})
                                :body :token)]
    (f)))

(defn- mit-welt [f]
  (let [r (install-in-project {:schema (in-project :werft (-> ontologie
                                                         (assoc :sources {:erp (quelle)})
                                                         (update :actions merge aktionen)))})]
    (assert (ok? r) (pr-str (:body r))))
  (let [c (api/post! :staging)] (assert (ok? c) (pr-str (:body c))))
  (assert (ok? (api/mutate :luftfahrzeug-registrieren {:kennzeichen "D-AINA" :muster "A320"}
                           {:publish? true})))
  (f))

(use-fixtures :once mit-erp pod/with-mem-pod-auth mit-agent als-admin mit-welt)

(defn- maengel []
  (into {} (map (juxt #(get-in % [:values :mangel-nr]) identity))
        (:rows (:body (api/list-objects :mangel)))))

(deftest ein-writeback-geht-voran
  (let [n0 (count (erp-zeilen))]
    (testing "der Webhook läuft vor der Regel, die vor ihm steht, und ihre Werte lesen seine Antwort"
      (let [r (api/mutate :extern-melden {:luftfahrzeug "D-AINA" :beschreibung "Hydraulikleck"}
                          {:publish? true})]
        (is (ok? r) (pr-str (:body r))))
      (let [zeile (last (erp-zeilen))]
        (is (= (inc n0) (count (erp-zeilen))))
        (is (= "Hydraulikleck" (:beschreibung zeile)))
        (is (= "Hydraulikleck" (get-in (maengel) [(str (:mangel_nr zeile)) :values :beschreibung]))
            "die Nummer, die das ERP vergab, ist der Schlüssel des Mangels")))

    (testing "lehnt das ERP ab, ändert die Action nichts"
      (let [vorher (count (maengel))
            r      (api/mutate :extern-melden {:luftfahrzeug "D-AINA" :beschreibung "x"}
                               {:publish? true})]
        (is (= 502 (:status r)) (pr-str (:body r)))
        (is (re-find #"writeback-failed" (pr-str (:body r))) (pr-str (:body r)))
        (is (= vorher (count (maengel))))
        (is (= (inc n0) (count (erp-zeilen))))))

    (testing "ein Probelauf plant den Webhook und ruft das ERP nicht"
      (let [vorher (count (maengel))
            r      (api/mutate :extern-melden {:luftfahrzeug "D-AINA" :beschreibung "Probe"}
                               {:dry-run? true})]
        (is (ok? r) (pr-str (:body r)))
        (is (= (inc n0) (count (erp-zeilen))))
        (is (= vorher (count (maengel))))))

    (testing "eine Liste geht in einem Aufruf hinaus: ganz oder gar nicht"
      (is (ok? (api/mutate :zwei-extern-melden {} {:publish? true})))
      (is (= (+ n0 3) (count (erp-zeilen))))
      (let [r (api/mutate :zwei-mit-fehler {} {:publish? true})]
        (is (not (ok? r)))
        (is (= (+ n0 3) (count (erp-zeilen)))
            "auch die gültige erste Zeile ist nicht geschrieben")))))

(deftest ein-seiteneffekt-geht-nach-dem-commit-einmal-je-nutzlast
  (let [vorher (count (erp-zeilen))
        r      (api/mutate :nachmelden {:luftfahrzeug "D-AINA"} {:publish? true})]
    (is (ok? r) (pr-str (:body r)))
    (let [deadline (+ (System/currentTimeMillis) 30000)]
      (while (and (< (count (erp-zeilen)) (+ vorher 2)) (< (System/currentTimeMillis) deadline))
        (Thread/sleep 250)))
    (Thread/sleep 6000)
    (is (= #{"Ölstand" "Bremsbelag"} (set (map :beschreibung (drop vorher (erp-zeilen))))))
    (is (= (+ vorher 2) (count (erp-zeilen))) "jede Nutzlast genau einmal")))

(defn- ueber-die-app!
  [action parameters]
  (tx/post-json! (paths/url :my-application-submit {:rid "melde-app"})
                 (json/write-str {:action action :parameters parameters})))

(deftest eine-app-auf-dem-zweig-erreicht-das-erp-nicht
  (assert (ok? (api/mutate :create-application
                           {:name :melde-app :project :werft
                            :object-type :luftfahrzeug
                            :layout {:kind :rows :items []
                                     :actions [{:action :nachmelden} {:action :extern-melden}]}}
                           {:stage? true})))
  (assert (ok? (api/post! :staging)))
  (assert (ok? (publish! :melde-app)))
  (let [z      (str "app-erp-" (subs (str (random-uuid)) 0 8))
        vorher (count (erp-zeilen))]
    (is (ok? (api/post! :branches {:parent "main" :name z})))
    (tx/with-branch z
      (testing "ein Seiteneffekt bleibt zurueck, die App erfaehrt es"
        (let [r (ueber-die-app! "nachmelden" {:luftfahrzeug "D-AINA"})]
          (is (= 200 (:status r)) (pr-str (:body r)))
          (is (= ["webhooks"] (map name (:held-back (:body r)))) (pr-str (:body r)))))
      (testing "ein Writeback vor den Regeln schreibt ebenfalls nichts ins ERP"
        (ueber-die-app! "extern-melden" {:luftfahrzeug "D-AINA" :beschreibung "Probelauf"})))
    (testing "Gegenprobe: dieselbe Tuer auf main schreibt; waere auf dem Zweig etwas durchgegangen,
              stuenden hier mehr als zwei Zeilen"
      (let [r (ueber-die-app! "nachmelden" {:luftfahrzeug "D-AINA"})]
        (is (= 200 (:status r)) (pr-str (:body r)))
        (is (empty? (:held-back (:body r)))))
      (let [deadline (+ (System/currentTimeMillis) 30000)]
        (while (and (< (count (erp-zeilen)) (+ vorher 2)) (< (System/currentTimeMillis) deadline))
          (Thread/sleep 250)))
      (Thread/sleep 6000)
      (is (= (+ vorher 2) (count (erp-zeilen))))
      (is (not-any? #(= "Probelauf" (:beschreibung %)) (erp-zeilen))))))

