(ns ^:api ontology.data-freshness-route-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [sutra.catalog.grants :as grants]
            [sutra.store.access.scope :as ctx]
            [sutra.store.control.import-keys :as import-keys]
            [sutra.access.principals :as pr]
            [support.pod :as pod]
            [support.seed :as seed]
            [support.wire :refer [commit! ok? publish! stage! uid]]
            [sutra.types.paths :as paths]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(use-fixtures :once pod/with-pod-auth)

(defn- app-rid [schema nm]
  (some (fn [[rid s]] (when (= nm (:application-name s)) rid)) (:applications schema)))

(defn- answers
  [resp]
  (into {} (map (juxt (juxt :kind :name) :at)) (get-in resp [:body :updates])))

(deftest the-app-door-answers-what-its-widget-declares
  (tx/with-token (-> (api/post! :login {:username grants/admin-username
                                        :password pr/default-admin-password})
                     :body :token)
    (stage! :create-project {:name :frisch-proj})
    (is (ok? (commit!)))
    (stage! :create-dataset {:name :frisch_roh :kind :standalone
                             :columns [{:attribute-name :nr :attribute-type :string}]
                             :project :frisch-proj})
    (is (ok? (commit!)))
    (stage! :create-object-type {:name :frisch :project :frisch-proj :primary-key :nr
                                 :attributes [[:nr :string]] :plural "Frische"})
    (stage! :create-object-type {:name :anders :project :frisch-proj :primary-key :nr
                                 :attributes [[:nr :string]] :plural "Andere"})
    (is (ok? (commit!)))
    (stage! :create-application
            {:name :frischetafel :project :frisch-proj
             :object-type :frisch
             :layout {:kind :rows :object-types [:frisch]
                      :items [{:widget :data-freshness
                               :items  [{:object-type :frisch
                                         :sources [{:resource :frisch_roh
                                                    :override-name "Roh"}]}]}]}})
    (is (ok? (commit!)))
    (is (ok? (publish! :frischetafel)) "saved, the app opens to no one until it is published")
    (seed/object! :frisch (uid) {:nr "1"})
    (let [rid  (app-rid (:body (api/get! :schema)) :frischetafel)
          ask  #(tx/send! :post (paths/url :my-application-data-freshness {:rid rid}) %)
          _    (is (some? rid) "the app was committed")
          resp (ask {:items [{:object-type :anders}]})
          got  (answers resp)]
      (is (ok? resp) (pr-str (:body resp)))
      (is (contains? got ["object-type" "frisch"]))
      (is (contains? got ["source" "frisch_roh"]))
      (is (not (contains? got ["object-type" "anders"]))
          "a request cannot ask about what no widget of the app declares"))
    (let [resp (tx/send! :post (paths/url :data-freshness)
                         {:items [{:object-type :anders} {:object-type :gibts-nicht}]})
          got  (answers resp)]
      (is (ok? resp) (pr-str (:body resp)))
      (is (contains? got ["object-type" "anders"]) "the object page reads under its own rights")
      (is (not (contains? got ["object-type" "gibts-nicht"]))))
    (let [rid (some (fn [[r t]] (when (= :frisch (keyword (:object-type-name t))) r))
                    (:object-types (:body (api/get! :schema))))]
      (is (some? rid))
      (import-keys/write-index-times! (ctx/store "main") "main" [rid] "lauf"))
    (let [status #(get-in (api/get! :import-status) [:body :per-type %])
          asked  #(get (answers (tx/send! :post (paths/url :data-freshness)
                                          {:items [{:object-type %}]}))
                       ["object-type" (name %)])
          at     (asked :frisch)]
      (is (some? at) "an index stamps the type")
      (is (some? (:last-at (status :frisch))))
      (is (ok? (api/rename :object-type :frisch :beleg-frisch)) "rename")
      (is (= at (asked :beleg-frisch)) "the type keeps its index time through a rename")
      (is (some? (:last-at (status :beleg-frisch))) "the status names it as it is called now")
      (is (nil? (status :frisch))))))
