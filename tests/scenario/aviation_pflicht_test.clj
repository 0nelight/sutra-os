(ns ^:api scenario.aviation-pflicht-test
  (:require [sutra.catalog.grants :as grants]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [support.in-project :refer [in-project]]
            [sutra.access.principals :as pr]
            [support.aviation :refer [kanten-aus ontologie]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(def ^:private welt
  (-> ontologie
      (update :objects merge {:user {:attributes [[:user-id :string] [:rolle :string]]
                                     :primary-key :user-id :plural "Benutzer"}
                              :marking {:attributes [[:name :string]] :match-keys [:name]
                                        :primary-key :name :plural "Marken"}})
      (assoc-in [:objects :mangel :markings] [:vs])
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
                         :from-id [:get :user :id] :to-id [:get :marking :id]}]}})))

(def ^:private admin (atom nil))
(def ^:private planer (atom nil))
(def ^:private techniker (atom nil))

(defn- anmelden [u p] (-> (api/post! :login {:username u :password p}) :body :token))

(defn- tun! [action args] (api/mutate action args {:publish? true}))

(defn- benutzer! [name rolle]
  (let [id (str (java.util.UUID/randomUUID))]
    (assert (ok? (tun! :benutzer-anlegen {:id id :rolle rolle})))
    (assert (ok? (api/post! :register {:username name :password name :user-id id})))
    id))

(defn- install!
  [schema]
  (let [r (api/post! :install {:schema (in-project :flotte schema)})
        c (if (ok? r) (api/post! :staging) r)]
    (when-not (ok? c) (api/delete! :staging))
    c))

(defn- mit-welt [f]
  (reset! admin (anmelden grants/admin-username pr/default-admin-password))
  (binding [tx/*auth-token* @admin]
    (let [r (install! welt)] (assert (ok? r) (pr-str (:body r))))
    (assert (ok? (tun! :marking-anlegen {:name "vs"})))
    (benutzer! "planer" "ontology-viewer")
    (let [t (benutzer! "techniker" "ontology-editor")]
      (assert (ok? (tun! :freigeben-fuer {:user t :marking "vs"}))))
    (assert (ok? (tun! :luftfahrzeug-registrieren {:kennzeichen "D-AINA" :muster "A320"})))
    (assert (ok? (tun! :mangel-melden
                       {:mangel-nr "P-1" :luftfahrzeug "D-AINA" :beschreibung "x" :kategorie "C"})))
    (reset! planer (anmelden "planer" "planer"))
    (reset! techniker (anmelden "techniker" "techniker"))
    (f)))

(use-fixtures :once pod/with-mem-pod-auth mit-welt)

(defn- maengel [token]
  (tx/with-token token
    (let [r (api/list-objects :mangel)]
      (when (ok? r) (set (map #(get-in % [:values :mangel-nr]) (:rows (:body r))))))))

(deftest ein-markierter-typ-ist-ohne-freigabe-nicht-da
  (testing "wer freigegeben ist, sieht ihn"
    (is (= #{"P-1"} (maengel @techniker))))
  (testing "wer es nicht ist, sieht keinen Mangel — welche Rolle er auch hat"
    (is (empty? (maengel @planer))))
  (testing "was nicht markiert ist, bleibt für ihn offen"
    (tx/with-token @planer
      (is (seq (:rows (:body (api/list-objects :luftfahrzeug))))))))

(deftest ein-verbotenes-marking-nimmt-kein-projekt-an
  (binding [tx/*auth-token* @admin]
    (let [r (install! (-> welt
                          (assoc-in [:projects :offen] {:forbidden-markings [:vs]})
                          (assoc-in [:objects :mangel :project] :offen)))]
      (is (not (ok? r)))
      (is (str/includes? (pr-str (:body r)) "marking-constraint") (pr-str (:body r))))))

(deftest ein-marking-entfernen-erweitert-den-zugang
  (binding [tx/*auth-token* @admin]
    (let [offen (update-in welt [:objects :mangel] dissoc :markings)]
      (testing "direkt auf main wird es abgewiesen"
        (let [r (install! offen)]
          (is (not (ok? r)))
          (is (str/includes? (pr-str (:body r)) "protected") (pr-str (:body r))))
        (is (empty? (maengel @planer)) "nichts ist gelandet"))
      (testing "über einen Zweig und ein Proposal wartet es auf einen Eigentümer"
        (is (ok? (api/post! :branches {:parent "main" :name "freigabe"})))
        (tx/with-branch "freigabe"
          (is (ok? (install! offen))))
        (let [id (get-in (api/post! :proposals {:source "freigabe" :target "main" :title "vs weg"})
                         [:body :id])
              t  (first (filter :expands-access
                                (get-in (api/get! :proposal-tasks {:id id}) [:body :tasks])))]
          (is (some? t) "die Aufgabe weiß, dass sie den Zugang erweitert")
          (is (re-find #"expands-access" (pr-str (:requires t))))
          (is (re-find #"still open"
                       (str (get-in (api/post! :proposal-merge {:id id} nil) [:body :error]))))
          (is (ok? (api/post! :proposal-approve {:id id} {})))
          (let [m (api/post! :proposal-merge {:id id} nil)]
            (is (ok? m) (pr-str (:body m))))))
      (testing "danach sieht ihn auch, wer nicht freigegeben ist"
        (is (= #{"P-1"} (maengel @planer)))))))

