(ns ^:api scenario.aviation-gruppen-test
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
                              :group {:attributes [[:group-id :string] [:name :string]]
                                      :primary-key :group-id :plural "Gruppen"}})
      (update :links conj
              (kanten-aus [:member-of :user :group :many-to-many]
                          :verzeichnisdienst "GRUPPENMITGLIED")
              (kanten-aus [:subgroup-of :group :group :many-to-many
                           {:forward-api-name :obergruppen :reverse-api-name :untergruppen}]
                          :verzeichnisdienst "UNTERGRUPPE"))
      (update :actions merge
              {:benutzer-anlegen
               {:params {:id {:name "Id" :type :string :required true}
                         :rolle {:name "Rolle" :type :string :required true}}
                :rules [{:op :create :object-type :user :id :id
                         :values {:user-id [:get :id] :rolle [:get :rolle]}}]
                :edits [{:object-type :user}]}
               :gruppe-anlegen
               {:params {:id {:name "Id" :type :string :required true}
                         :name {:name "Name" :type :string :required true}}
                :rules [{:op :create :object-type :group :id :id
                         :values {:group-id [:get :id] :name [:get :name]}}]
                :edits [{:object-type :group}]}
               :aufnehmen
               {:params {:user {:name "User" :type :user :required true}
                         :group {:name "Group" :type :group :required true}}
                :rules [{:op :create-link :link-type :member-of :from-id [:get :user :id]
                         :to-id [:get :group :id]}]}
               :einordnen
               {:params {:teil {:name "Teil" :type :group :required true}
                         :ganzes {:name "Ganzes" :type :group :required true}}
                :rules [{:op :create-link :link-type :subgroup-of :from-id [:get :teil :id]
                         :to-id [:get :ganzes :id]}]}})))

(def ^:private admin (atom nil))

(defn- anmelden [u p] (-> (api/post! :login {:username u :password p}) :body :token))

(defn- tun! [action args]
  (let [r (api/mutate action args {:publish? true})] (assert (ok? r) (pr-str action (:body r))) r))

(defn- mit-welt [f]
  (reset! admin (anmelden grants/admin-username pr/default-admin-password))
  (binding [tx/*auth-token* @admin]
    (let [r (install-in-project {:schema welt})] (assert (ok? r) (pr-str (:body r))))
    (assert (ok? (api/post! :staging))))
  (f))

(use-fixtures :once pod/with-mem-pod-auth mit-welt)

(defn- uuid [] (str (java.util.UUID/randomUUID)))

(deftest ein-gruppen-grant-gilt-fuer-ihre-mitglieder
  (let [eva     (uuid)
        team    (uuid)
        schicht (uuid)]
    (binding [tx/*auth-token* @admin]
      (tun! :benutzer-anlegen {:id eva :rolle "member"})
      (is (ok? (api/post! :register {:username "eva" :password "eva" :user-id eva})))
      (tun! :gruppe-anlegen {:id team :name "lager-team"})
      (tun! :gruppe-anlegen {:id schicht :name "nachtschicht"})
      (tun! :einordnen {:teil schicht :ganzes team})
      (is (ok? (api/mutate :create-project
                           {:name :lager :grants [{:principal team :role :ontology-viewer}]}
                           {:stage? true})))
      (is (ok? (api/mutate :create-project {:name :ausfuhr} {:stage? true})))
      (is (ok? (api/post! :staging)))
      (is (ok? (install-in-project {:schema
                                      {:objects
                                         {:regal {:project :lager :primary-key :nr
                                                  :attributes [[:nr :string]]
                                                  :plural "Regale"}
                                          :zollakte
                                            {:project :ausfuhr :primary-key :nr
                                             :attributes [[:nr :string]]
                                             :plural "Zollakten"}}}})))
      (is (ok? (api/post! :staging))))

    (testing "ohne Mitgliedschaft: kein Zugang"
      (tx/with-token (anmelden "eva" "eva")
        (is (= 403 (:status (api/list-objects :regal))))))

    (testing "Mitglied der Nachtschicht, die zum Lager-Team gehört: der Grant des Teams gilt"
      (binding [tx/*auth-token* @admin]
        (tun! :aufnehmen {:user eva :group schicht}))
      (tx/with-token (anmelden "eva" "eva")
        (is (ok? (api/list-objects :regal)))
        (is (= 403 (:status (api/list-objects :zollakte))) "nur im Projekt des Grants")))

    (testing "access explain nennt die Kette: Mitgliedschaft, Gruppe in Gruppe, Grant"
      (tx/with-token @admin
        (let [r     (:body (api/get! :access-explain {:user "eva" :op "read" :project "lager"}))
              facts (set (map :fact (tree-seq :from :from (:because r))))]
          (is (true? (:allow? r)) (pr-str r))
          (is (some #(and (= "member-of" (name (first %))) (= [eva schicht] (second %))) facts)
              (pr-str facts))
          (is (some #(and (= "member-of" (name (first %))) (= [schicht team] (second %))) facts))
          (is (some #(and (= "grant" (name (first %))) (= team (first (second %)))) facts)))
        (let [r (:body (api/get! :access-explain {:user "eva" :op "update" :project "lager"}))]
          (is (false? (:allow? r)) "Lesen ist kein Schreiben")
          (is (nil? (:because r))))))))
