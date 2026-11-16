(ns ^:api scenario.aviation-anfragen-test
  (:require [sutra.catalog.grants :as grants]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [support.in-project :refer [install-in-project]]
            [sutra.access.principals :as pr]
            [support.aviation :refer [kanten-aus ontologie]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(def ^:private welt
  (-> ontologie
      (update :objects merge {:user  {:attributes [[:user-id :string] [:rolle :string]]
                                      :primary-key :user-id :plural "Benutzer"}
                              :group {:attributes [[:name :string]] :primary-key :name
                                      :plural "Gruppen"}})
      (update :links conj (kanten-aus [:member-of :user :group :many-to-many]
                                       :verzeichnisdienst "GRUPPENMITGLIED"))
      (assoc-in [:projects :werkstatt] {})
      (assoc-in [:objects :werkzeug] {:project :werkstatt :attributes [[:nr :string]]
                                      :primary-key :nr :plural "Werkzeuge"})
      (update :actions merge
              {:benutzer-anlegen
               {:params {:id {:name "Id" :type :string :required true}
                         :rolle {:name "Rolle" :type :string :required true}}
                :rules [{:op :create :object-type :user :id :id
                         :values {:user-id [:get :id] :rolle [:get :rolle]}}]
                :edits [{:object-type :user}]}
               :gruppe-anlegen
               {:params {:name {:name "Name" :type :string :required true}}
                :rules [{:op :create :object-type :group :values {:name [:get :name]}}]
                :edits [{:object-type :group}]}})))

(defn- anmelden [u p] (-> (api/post! :login {:username u :password p}) :body :token))

(def ^:private leute (atom {}))

(defn- benutzer! [name rolle]
  (let [id (str (java.util.UUID/randomUUID))]
    (assert (ok? (api/mutate :benutzer-anlegen {:id id :rolle rolle} {:publish? true})))
    (assert (ok? (api/post! :register {:username name :password name :user-id id})))
    (swap! leute assoc name {:id id :token (anmelden name name)})))

(defn- eigentuemer! [projekt name]
  (assert (ok? (api/mutate :update-project
                           {:name projekt
                            :grants [{:principal (get-in @leute [name :id]) :role :ontology-owner}]}
                           {:stage? true})))
  (assert (ok? (api/post! :staging))))

(defn- mit-welt [f]
  (binding [tx/*auth-token* (anmelden grants/admin-username pr/default-admin-password)]
    (let [r (install-in-project {:schema welt})] (assert (ok? r) (pr-str (:body r))))
    (assert (ok? (api/post! :staging)))
    (benutzer! "werki" "member")
    (benutzer! "flotti" "member")
    (benutzer! "gast" "member")
    (eigentuemer! :werkstatt "werki")
    (eigentuemer! :flotte "flotti")
    (assert (ok? (api/mutate :gruppe-anlegen {:name "nachtschicht"} {:publish? true})))
    (f)))

(use-fixtures :once pod/with-mem-pod-auth mit-welt)

(defmacro ^:private als [wer & body] `(tx/with-token (get-in @leute [~wer :token]) ~@body))

(deftest eine-projektrolle-erfragen
  (testing "ohne Rolle: kein Zugang"
    (is (= 403 (:status (als "gast" (api/list-objects :werkzeug))))))
  (let [r  (als "gast"
                (api/post! :access-requests
                           {:kind "project-role" :project "werkstatt" :role "ontology-viewer"
                            :justification "Inventur"}))
        id (get-in r [:body :id])]
    (is (ok? r) (pr-str (:body r)))
    (testing "wer nicht Eigentümer des Projekts ist, entscheidet nicht — auch nicht der Antragsteller"
      (is (= 400 (:status (als "gast" (api/post! :access-request-approve {:id id} {})))))
      (is (= 400 (:status (als "flotti" (api/post! :access-request-approve {:id id} {})))))
      (is (not-any? #(= id (:id %))
                    (get-in (als "flotti" (api/get! :access-requests {:inbox true}))
                            [:body :requests]))))
    (testing "der Eigentümer sieht sie im Eingang und gibt frei; sie wendet sich selbst an"
      (is (some #(= id (:id %))
                (get-in (als "werki" (api/get! :access-requests {:inbox true})) [:body :requests])))
      (let [d (als "werki" (api/post! :access-request-approve {:id id} {:note "ok"}))]
        (is (= "completed" (get-in d [:body :status])) (pr-str (:body d)))
        (is (= "ontology-viewer" (name (get-in d [:body :applied :grant :role]))))))
    (testing "der Grant gilt"
      (is (ok? (als "gast" (api/list-objects :werkzeug)))))))

(deftest eine-gruppe-erfragen
  (let [r  (als "gast"
                (api/post! :access-requests {:kind "group-membership" :group "nachtschicht"}))
        id (get-in r [:body :id])]
    (is (ok? r) (pr-str (:body r)))
    (testing "die Gruppe gehört dem Projekt ihres Typs — dessen Eigentümer entscheidet"
      (is (= 400 (:status (als "werki" (api/post! :access-request-approve {:id id} {})))))
      (is (= "completed"
             (get-in (als "flotti" (api/post! :access-request-approve {:id id} {}))
                     [:body :status]))))
    (testing "die Mitgliedschaft ist verknüpft: access explain sieht sie, sobald die Gruppe einen Grant hat"
      (binding [tx/*auth-token* (anmelden grants/admin-username pr/default-admin-password)]
        (let [gid (get-in (api/get! :access-request {:id id}) [:body :group-id])]
          (is (ok? (api/mutate :update-project
                               {:name :flotte
                                :grants [{:principal (get-in @leute ["flotti" :id])
                                          :role :ontology-owner}
                                         {:principal gid :role :ontology-viewer}]}
                               {:stage? true})))
          (is (ok? (api/post! :staging)))
          (let [e (:body (api/get! :access-explain {:user "gast" :op "read" :project "flotte"}))]
            (is (true? (:allow? e)) (pr-str e))
            (is (some #(= "member-of" (name (first (:fact %))))
                      (tree-seq :from :from (:because e))))))))))

(deftest ablehnen-und-schliessen
  (let [a (get-in (als "gast"
                       (api/post! :access-requests
                                  {:kind "project-role" :project "werkstatt"
                                   :role "ontology-owner"})) [:body :id])
        b (get-in (als "gast"
                       (api/post! :access-requests
                                  {:kind "project-role" :project "werkstatt"
                                   :role "ontology-editor"})) [:body :id])]
    (is (= "rejected"
           (get-in (als "werki" (api/post! :access-request-reject {:id a} {:note "zu viel"}))
                   [:body :status])))
    (is (= 403 (:status (als "flotti" (api/delete! :access-request {:id b}))))
        "nur der Antragsteller schließt")
    (is (= "closed" (get-in (als "gast" (api/delete! :access-request {:id b})) [:body :status])))
    (is (= 400 (:status (als "werki" (api/post! :access-request-approve {:id b} {}))))
        "geschlossen ist geschlossen")))
