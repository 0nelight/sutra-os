(ns ^:api platform.identity-sync-test
  (:require [sutra.catalog.grants :as grants]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.access.http.identity :as idp]
            [sutra.access.principals :as pr]
            [support.pod :as pod]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(use-fixtures :each pod/with-mem-pod-auth)

(def ^:private fake
  {:label     "Fake"
   :source    "fake"
   :settings  {:enabled {:key "fake_enabled" :type :boolean}}
   :ready?    (comp boolean :enabled)
   :directory (fn [_]
                {:users {"anna@x" {:attributes {:sam "anna"} :groups #{"einkauf"}}}
                 :groups {"einkauf" {}}})
   :preview   (fn [_ snap have]
                {:users (vec (for [[s {{:keys [sam]} :attributes}] (:users snap)]
                               {:subject s :sam sam :display sam
                                :action (if (have s) :exists :create) :user-id (have s)
                                :grants [{:project :portal :role :ontology-viewer}]}))
                 :skipped 0})})

(defn- project [schema nm]
  (some (fn [[_ s]] (when (= (name nm) (some-> (:project-name s) name)) s)) (:projects schema)))

(deftest the-host-plans-and-applies-a-directory-sync
  (with-redefs [idp/providers (constantly {:fake fake})]
    (tx/with-token (-> (api/post! :login
                                  {:username grants/admin-username
                                   :password pr/default-admin-password})
                       :body :token)
      (api/mutate :create-project {:name :personal} {:stage? true})
      (api/post! :staging)
      (api/mutate :create-object-type
                  {:name :user :primary-key :user-id
                   :attributes [[:user-id :string] [:name :string] [:rolle :string]
                                [:disabled :boolean]]
                   :plural "Users"}
                  {:stage? true})
      (is (= 200 (:status (api/post! :staging))) "the :user type")
      (api/mutate :create-object-type {:name :group :primary-key :name :attributes [[:name :string]]
                                       :plural "Groups"} {:stage? true})
      (api/mutate :create-dataset
                  {:name :member_of_rows
                   :columns [{:attribute-name :from_key :attribute-type :string}
                             {:attribute-name :to_key :attribute-type :string}]}
                  {:stage? true})
      (api/mutate :create-link-type
                  {:name :member-of :from :user :to :group :cardinality :many-to-many
                   :external-source {:strategy :join-table :dataset :member_of_rows
                                     :from-key-column :from_key :to-key-column :to_key}}
                  {:stage? true})
      (is (= 200 (:status (api/post! :staging))) "groups and membership")
      (is (= 200 (:status (api/post! :identity-config {:provider "fake"} {:enabled true}))))
      (testing "a dry run plans and writes nothing"
        (let [{:keys [status body]} (api/post! :identity-sync {:provider "fake"} {:dry-run? true})]
          (is (= 200 status) (pr-str body))
          (is (= [:create-project :create-user :grant] (mapv :step (:steps body))))
          (is (= [:create-group :join] (mapv :step (:memberships body))))
          (is (nil? (:results body)))
          (is (nil? (project (:body (api/get! :schema)) :portal)))))
      (testing "a sync provisions the user, its principal, the project and the grant"
        (let [{:keys [status body]} (api/post! :identity-sync {:provider "fake"} {})
              uid (some :user-id (:results body))]
          (is (= 200 status) (pr-str body))
          (is (string? uid))
          (is (= [{:principal uid :role :ontology-viewer}]
                 (mapv #(select-keys % [:principal :role])
                       (:grants (project (:body (api/get! :schema)) :portal)))))
          (is (some #(= "anna" (:username %)) (:principals (:body (api/get! :principals)))))))
      (testing "a second sync creates nothing and grants nothing twice"
        (let [{:keys [body]} (api/post! :identity-sync {:provider "fake"} {})]
          (is (= [:grant] (mapv :step (:steps body))))
          (is (= [] (:memberships body)) "the group and the membership exist")
          (is (= 1 (count (:grants (project (:body (api/get! :schema)) :portal)))))))
      (testing "a membership the directory drops is left"
        (with-redefs [idp/providers
                        (constantly
                         {:fake
                            (assoc fake
                                   :directory
                                     (fn [_]
                                       {:users {"anna@x" {:attributes {:sam "anna"} :groups #{}}}
                                      :groups {"einkauf" {}}}))})]
          (let [{:keys [body]} (api/post! :identity-sync {:provider "fake"} {:dry-run? true})]
            (is (= [:leave] (mapv :step (:memberships body)))))))
      (testing "a user gone from the directory is reported, and disabled only when asked"
        (with-redefs [idp/providers
                        (constantly {:fake
                                       (assoc fake
                                              :directory
                                                (fn [_]
                                                  {:users {"zoe@x" {:attributes {:sam "zoe"}}}}))})]
          (let [{:keys [body]} (api/post! :identity-sync {:provider "fake"} {:dry-run? true})]
            (is (= ["anna@x"] (:gone body)))
            (is (not-any? #(= :disable (:step %)) (:steps body))))
          (let [{:keys [body]} (api/post! :identity-sync {:provider "fake"} {:disable-gone? true})]
            (is (some #(and (= :disable (:step %)) (:done %)) (:results body)) (pr-str body))))))))

(deftest the-instance-describes-its-providers
  (with-redefs [idp/providers (constantly {:fake fake})]
    (tx/with-token (-> (api/post! :login
                                  {:username grants/admin-username
                                   :password pr/default-admin-password})
                       :body :token)
      (let [{:keys [status body]} (api/get! :system)]
        (is (= 200 status))
        (is (some #{:system} (:routes body)))
        (is (contains? (set (keys (:connector-profiles body))) :postgres))
        (is (seq (:policy-fields body)))))))
