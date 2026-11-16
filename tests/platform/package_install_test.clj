(ns ^:api platform.package-install-test
  (:require [sutra.catalog.grants :as grants]
            [support.system :as system]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.catalog.lookup :as lookup]
            [sutra.store.seat]
            [sutra.packages.standard :as packages]
            [sutra.parser.adopt :as adopt]
            [sutra.authoring.http.products :as products]
            [sutra.serve.server :as server]
            [sutra.access.principals :as pr]
            [support.pod :as pod]
            [support.wire :refer [commit! ok?]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(use-fixtures :each pod/with-pod-auth)

(defn- staged-names
  []
  (into #{} (keep #(some-> % :args :name keyword)) (:staged (:body (api/get! :staging)))))

(defn- package [listing id]
  (some #(when (= id (:id %)) %) (:packages listing)))

(deftest the-door-is-no-weaker-than-install
  (testing "same declared auth + branch scope as POST /api/install"
    (is (= (select-keys ((server/endpoint-facts) :install) [:auth :branch? :service])
           (select-keys ((server/endpoint-facts) :package-install) [:auth :branch? :service])
           (select-keys ((server/endpoint-facts) :packages) [:auth :branch? :service]))))
  (testing "and an anonymous caller is refused before anything is read"
    (is (= 401 (:status (api/get! :packages))))
    (is (= 401 (:status (api/install-package "basis" {:preview? true}))))))

(deftest list-preview-then-install-basis
  (tx/with-token (-> (api/post! :login
                                {:username grants/admin-username
                                 :password pr/default-admin-password})
                     :body :token)
    (api/delete! :staging)
    (is (empty? (staged-names)))
    (let [{:keys [status body]} (api/get! :packages)
          basis (package body "basis")]
      (is (= 200 status) (pr-str body))
      (is (= ["basis" "privat" "betrieb" "einzelunternehmen" "mail"] (mapv :id (:packages body))))
      (is (= :not-installed (:status basis)))
      (is (some #{:person} (:object-types basis)))
      (is (= ["basis"] (:requires (package body "privat")))))

    (testing "a preview names what it would create and keeps nothing"
      (let [{:keys [status body]} (api/install-package "basis" {:preview? true})
            would (into #{} (keep #(some-> % :args :name keyword)) (:staged body))]
        (is (= 200 status) (pr-str (select-keys body [:error :data])))
        (is (true? (:preview? body)))
        (is (contains? would :person) (pr-str would))
        (is (empty? (staged-names)) "nothing staged")
        (is (nil? (lookup/rid-of (system/seated-schema) :object-types :person))
            "nothing committed")))

    (testing "an unknown package is refused by name"
      (let [{:keys [status body]} (api/install-package "gibts-nicht")]
        (is (= 404 status))
        (is (= ["basis" "privat" "betrieb" "einzelunternehmen" "mail"]
               (get-in body [:data :known])))))

    (testing "install stages; the normal staging commit lands it"
      (let [{:keys [status body]} (api/install-package "basis")]
        (is (= 200 status) (pr-str (select-keys body [:error :data])))
        (is (contains? (staged-names) :person)))
      (is (ok? (commit!)))
      (is (some? (lookup/rid-of (system/seated-schema) :object-types :person)))
      (is (= :installed (:status (package (:body (api/get! :packages)) "basis"))))
      (is (= :partial (:status (package (:body (api/get! :packages)) "privat")))
          "privat carries basis's types plus its own"))))

(def ^:private v1 {:projects {:demo {:description "das Demo-Paket"}}
                   :objects {:kartei {:description "v1" :primary-key :nr :attributes [[:nr :string]]
                                      :match-keys [:nr] :project :demo
                                      :plural "Karteien"}
                             :notiz  {:description "v1" :primary-key :nt :attributes [[:nt :string]]
                                      :match-keys [:nt] :project :demo
                                      :plural "Notizen"}}})

(def ^:private v2 (-> v1
                      (assoc-in [:objects :kartei :description] "v2")
                      (assoc-in [:objects :etikett]
                                {:description "v2" :primary-key :et :attributes [[:et :string]]
                                 :match-keys [:et]
                                 :project :demo
                                 :plural "Etiketten"})))

(defn- offering [release version]
  {:demo {:name "Demo" :description "zum Testen" :version version :order 0 :requires []
          :schema (constantly release)}})

(deftest an-installed-package-upgrades-onto-what-was-changed-since
  (tx/with-token (-> (api/post! :login
                                {:username grants/admin-username
                                 :password pr/default-admin-password})
                     :body :token)
    (api/delete! :staging)
    (with-redefs [products/offers (constantly (offering v1 "1.0.0"))]
      (testing "an install asked to land is committed and its release recorded"
        (let [{:keys [status body]} (api/post! :package-install {:package "demo" :land? true})]
          (is (= 200 status) (pr-str (select-keys body [:error :data])))
          (is (:landed body))
          (is (empty? (staged-names)))
          (is (= "1.0.0" (:installed-version (package (:body (api/get! :packages)) "demo")))))))
    (testing "the instance changes one field of the package"
      (is (ok? (api/post! :install {:schema {:objects {:notiz (assoc (get-in v1 [:objects :notiz])
                                                                    :description "meine")}}})))
      (is (ok? (commit!))))
    (with-redefs [products/offers (constantly (offering v2 "2.0.0"))]
      (is (:upgrade? (package (:body (api/get! :packages)) "demo")))
      (testing "the upgrade takes what only the release changed and keeps what only the instance changed"
        (let [{:keys [status body]} (api/post! :package-upgrade {:package "demo" :land? true})
              schema (system/seated-schema)
              desc   #(get-in schema
                              [:object-types (lookup/rid-of schema :object-types %) :description])]
          (is (= 200 status) (pr-str (select-keys body [:error :data])))
          (is (= "1.0.0" (:from body)))
          (is (= "v2" (desc :kartei)))
          (is (= "meine" (desc :notiz)))
          (is (some? (lookup/rid-of schema :object-types :etikett))
              "a type new in the release comes in")
          (is (= "2.0.0" (:installed-version (package (:body (api/get! :packages)) "demo"))))))
      (testing "the same release again changes nothing"
        (is (:unchanged (:body (api/post! :package-upgrade {:package "demo"}))))))
    (with-redefs [products/offers
                    (constantly (offering (assoc-in v2 [:objects :notiz :description] "v3")
                                          "3.0.0"))]
      (testing "a field both changed is a conflict until decided"
        (let [{:keys [status body]} (api/post! :package-upgrade {:package "demo" :land? true})]
          (is (= 409 status) (pr-str body))
          (is (some #(= [:objects "notiz"] (:node %)) (get-in body [:data :conflicts]))))
        (is (= 200
               (:status (api/post! :package-upgrade
                                   {:package "demo" :land? true :resolve :theirs}))))))))

(deftest adopting-into-a-package-dataset-binds-it-to-a-source
  (tx/with-token (-> (api/post! :login
                                {:username grants/admin-username
                                 :password pr/default-admin-password})
                     :body :token)
    (api/delete! :staging)
    (is (= 200 (:status (api/post! :package-install {:package "mail" :land? true}))))
    (is (ok? (api/post! :install
                        {:schema {:sources {:postfach {:project :mail :type :postgres
                                                       :jdbc-url "jdbc:postgresql://db:5432/mail"
                                                       :egress ["db:5432"]}}}})))
    (is (ok? (commit!)))
    (let [cols (mapv (fn [{:keys [attribute-name attribute-type]}]
                       {:name (name attribute-name) :ontology-type attribute-type})
                     (get-in (packages/read-package "5_mail.ent")
                             [:datasets :postfach_mails :columns]))
          {:keys [schema
                  problem]} (adopt/plan {:source :postfach :table "messages" :columns cols
                                         :into :postfach_mails
                                                :existing {:datasets #{:postfach_mails}}})
          _      (is (nil? problem))
          r      (api/post! :install {:schema schema})]
      (is (ok? r) (pr-str (select-keys (:body r) [:error :data])))
      (is (ok? (commit!)))
      (let [s  (system/seated-schema)
            ds (get-in s [:datasets (lookup/rid-of s :datasets :postfach_mails)])]
        (is (= :virtual (keyword (name (:kind ds)))))
        (is (= "messages" (get-in ds [:virtual :table])) "the declaration survives the install")))))

(deftest the-mail-package-installs-on-its-own
  (tx/with-token (-> (api/post! :login
                                {:username grants/admin-username
                                 :password pr/default-admin-password})
                     :body :token)
    (api/delete! :staging)
    (let [{:keys [status body]} (api/post! :package-install {:package "mail" :land? true})
          schema (system/seated-schema)]
      (is (= 200 status) (pr-str (select-keys body [:error :data])))
      (is (:landed body))
      (doseq [t [:mail :anhang]]
        (is (some? (lookup/rid-of schema :object-types t)) (str t " is installed")))
      (is (= :installed (:status (package (:body (api/get! :packages)) "mail")))))))
