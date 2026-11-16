(ns ^:api platform.product-marketplace-api-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [sutra.access.principals :as pr]
            [sutra.authoring.http.products :as products]
            [sutra.store.access.scope :as ctx]
            [support.system :as system]
            [support.pod :as pod]
            [support.wire :refer [commit! ok?]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(use-fixtures :each pod/with-pod-auth)

(def ^:private v1
  {:projects {:vertrag {:description "Verträge"}}
   :objects  {:vertrag {:description "v1" :primary-key :nr :attributes [[:nr :string]]
                        :match-keys [:nr] :project :vertrag :plural "Verträge"}}})

(defn- desc [schema t]
  (get-in schema [:object-types (lookup/rid-of schema :object-types t) :description]))

(def ^:private by-hand
  {:objects {:k-vertrag (assoc (get-in v1 [:objects :vertrag])
                               :description "von Hand" :project :vertrag-kopie)}})

(defn- why [r] (pr-str (select-keys (:body r) [:error :data])))

(deftest a-product-is-published-carried-installed-locked-and-upgraded
  (tx/with-token (-> (api/post! :login {:username grants/admin-username
                                        :password pr/default-admin-password})
                     :body :token)
    (api/delete! :staging)
    (is (ok? (api/post! :install {:schema v1})))
    (is (ok? (commit!)))

    (testing "a project is published as a release; a version never changes"
      (let [r (api/post! :products {:product "vertraege" :version "1.0.0"
                                    :projects ["vertrag"] :channel :stable :release? true})]
        (is (ok? r) (why r))
        (is (= :released (get-in r [:body :published :state]))))
      (is (= 409 (:status (api/post! :products {:product "vertraege" :version "1.0.0"
                                                :projects ["vertrag"]})))))

    (testing "a bundle is signed with the instance's key and taken in only unchanged"
      (let [bundle (:bundle (:body (api/get! :product-bundle {:product "vertraege"
                                                               :version "1.0.0"})))
            issuer (:body (api/get! :product-trust))]
        (is (= 64 (count (:digest bundle))))
        (is (= (:fingerprint issuer) (get-in bundle [:signature :iss])))
        (is (= 400 (:status (api/post! :product-imports
                                       {:bundle (assoc bundle :description "verändert")})))
            "changed content is refused")
        (is (= 400 (:status (api/post! :product-imports
                                       {:bundle (assoc-in bundle [:signature :public-key]
                                                          "MCowBQYDK2VwAyEA")})))
            "a key this instance does not trust is refused")
        (is (ok? (api/post! :product-imports {:bundle bundle})))))

    (let [r  (api/post! :product-install {:product "vertraege"}
                        {:suffix "kopie" :prefix "k" :lock? true :land? true})
          id (get-in r [:body :install-id])]
      (testing "an installation lands under its suffix and prefix"
        (is (ok? r) (why r))
        (is (some? id))
        (is (= "v1" (desc (system/seated-schema) :k-vertrag)))
        (is (some? (lookup/rid-of (system/seated-schema) :projects :vertrag-kopie))))

      (testing "installing it again under the same names is refused"
        (is (= 400 (:status (api/post! :product-install {:product "vertraege"}
                                       {:suffix "kopie" :prefix "k" :land? true})))))

      (testing "a locked installation refuses an edit of what it installed"
        (is (ok? (api/post! :install {:schema by-hand})))
        (is (not (ok? (commit!))))
        (api/delete! :staging)
        (is (= "v1" (desc (system/seated-schema) :k-vertrag))))

      (testing "the next release reaches it through an upgrade, lock or not"
        (is (ok? (api/post! :install {:schema (assoc-in v1 [:objects :vertrag :description]
                                                        "v2")})))
        (is (ok? (commit!)))
        (is (ok? (api/post! :products {:product "vertraege" :version "1.1.0"
                                       :projects ["vertrag"] :channel :stable
                                       :release? true})))
        (is (= "1.1.0" (some #(when (= id (:install-id %)) (:upgrade %))
                             (:installations (:body (api/get! :product-installations))))))
        (let [u (api/post! :product-installation-upgrade {:id id} {:land? true})]
          (is (ok? u) (why u))
          (is (= "v2" (desc (system/seated-schema) :k-vertrag)))))

      (testing "settings are checked; unlocking opens the projects again"
        (is (= 400 (:status (api/post! :product-installation {:id id}
                                       {:window {:from "25:00" :to "04:00"}}))))
        (is (ok? (api/post! :product-installation {:id id}
                            {:locked false :auto-upgrade true :channel :test
                             :window {:days ["sat"] :from "22:00" :to "04:00"}})))
        (is (ok? (api/post! :install {:schema by-hand})))
        (is (ok? (commit!))))

      (testing "an installation that upgrades itself takes the next compatible release"
        (is (ok? (api/post! :product-installation {:id id} {:window nil})))
        (is (ok? (api/post! :install
                            {:schema (-> v1
                                         (assoc-in [:objects :vertrag :description] "v2")
                                         (assoc-in [:objects :vertrag :attributes]
                                                   [[:nr :string] [:laufzeit :string]]))})))
        (is (ok? (commit!)))
        (is (ok? (api/post! :products {:product "vertraege" :version "1.2.0"
                                       :projects ["vertrag"] :channel :test
                                       :release? true})))
        (is (ok? (api/post! :products {:product "vertraege" :version "2.0.0"
                                       :projects ["vertrag"] :channel :stable
                                       :release? true})))
        (products/auto-upgrade! (ctx/system-store "main"))
        (let [i (some #(when (= id (:install-id %)) %)
                      (:installations (:body (api/get! :product-installations))))]
          (is (= "1.2.0" (:version i)) (pr-str (:last-error i)))
          (is (= "von Hand" (desc (system/seated-schema) :k-vertrag))
              "what only the installation changed stays")
          (is (= "2.0.0" (:upgrade i)) "a breaking release waits for a person"))))

    (testing "a withdrawn release neither installs nor exports"
      (is (ok? (api/post! :product-release {:product "vertraege" :version "1.0.0"}
                          {:event :withdraw})))
      (is (= 400 (:status (api/post! :product-install {:product "vertraege"}
                                     {:version "1.0.0" :suffix "drei"}))))
      (is (= 400 (:status (api/get! :product-bundle {:product "vertraege"
                                                      :version "1.0.0"})))))))

(def ^:private akte
  {:projects {:akte {:description "Akten"}}
   :objects  {:akte {:description "eine Akte" :primary-key :nr :attributes [[:nr :string]]
                     :match-keys [:nr] :project :akte :plural "Akten"}}})

(defn- installation [id]
  (some #(when (= id (:install-id %)) %)
        (:installations (:body (api/get! :product-installations)))))

(deftest a-staged-install-settles-carries-its-documents-and-uninstalls
  (tx/with-token (-> (api/post! :login {:username grants/admin-username
                                        :password pr/default-admin-password})
                     :body :token)
    (api/delete! :staging)
    (is (ok? (api/post! :install {:schema akte})))
    (is (ok? (commit!)))
    (is (ok? (api/post! :documents {} {:name "Use Case Akten" :format "markdown"
                                       :body "# Zusammenfassung" :project "akte"})))
    (let [r (api/post! :products {:product "akten" :version "1.0.0" :projects ["akte"]
                                  :channel :stable :release? true})]
      (is (ok? r) (why r)))

    (let [r  (api/post! :product-install {:product "akten"}
                        {:suffix "neu" :prefix "n" :lock? true :land? false})
          id (get-in r [:body :install-id])]
      (testing "a staged install is recorded and becomes an installation once committed"
        (is (ok? r) (why r))
        (is (= "staged" (some-> (get-in r [:body :state]) name)))
        (is (nil? (lookup/rid-of (system/seated-schema) :object-types :n-akte)))
        (is (ok? (commit!)) "its own commit is not refused by its lock")
        (is (= "installed" (some-> (:state (installation id)) name)))
        (products/settle-all! (ctx/system-store "main"))
        (is (= 1 (count (:documents (installation id)))) "the project's document came along"))

      (testing "the lock holds on the branch the installation landed on"
        (is (ok? (api/post! :install
                            {:schema {:objects {:n-akte (assoc (get-in akte [:objects :akte])
                                                               :description "anders"
                                                               :project :akte-neu)}}})))
        (is (not (ok? (commit!))) "on its own branch the lock holds")
        (api/delete! :staging))

      (testing "uninstalling previews, then removes what it installed"
        (let [p (api/post! :product-installation-uninstall {:id id} {:preview? true})]
          (is (ok? p) (why p))
          (is (some? (lookup/rid-of (system/seated-schema) :object-types :n-akte))
              "a preview removes nothing"))
        (let [u (api/post! :product-installation-uninstall {:id id} {:land? true})]
          (is (ok? u) (why u))
          (is (= "uninstalled" (some-> (get-in u [:body :state]) name)))
          (is (nil? (lookup/rid-of (system/seated-schema) :object-types :n-akte)))
          (is (nil? (installation id)))
          (is (some? (lookup/rid-of (system/seated-schema) :object-types :akte))
              "the source stays")))

      (testing "the emptied project an uninstall left is taken back by the next install"
        (let [again (api/post! :product-install {:product "akten"}
                               {:suffix "neu" :prefix "n" :land? true})]
          (is (ok? again) (why again))
          (is (some? (lookup/rid-of (system/seated-schema) :object-types :n-akte)))
          (api/post! :product-installation-uninstall {:id (get-in again [:body :install-id])}
                     {:land? true}))))))
