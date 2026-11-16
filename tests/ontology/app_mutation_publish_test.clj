(ns ^:api ontology.app-mutation-publish-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [sutra.catalog.grants :as grants]
            [sutra.access.principals :as pr]
            [support.pod :as pod]
            [support.wire :refer [commit! ok? publish! stage!]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(use-fixtures :once pod/with-pod-auth)

(defn- app
  [nm]
  (let [l (:body (api/get! :resource-versions {:section "applications" :name (name nm)}))]
    {:published (:published l) :releases (map :version (:versions l))}))

(defn- layout [title] {:kind :rows :items [{:widget :text :text title}]})

(deftest an-app-a-schema-action-saves-is-a-draft-keeping-its-history
  (tx/with-token (-> (api/post! :login {:username grants/admin-username
                                        :password pr/default-admin-password})
                     :body :token)
    (stage! :create-project {:name :mut-pub})
    (is (ok? (commit!)))
    (stage! :create-application {:name :mut-tafel :project :mut-pub :layout (layout "a")})
    (is (ok? (commit!)))
    (is (nil? (:published (app :mut-tafel))) "saved, it opens to no one")
    (is (ok? (publish! :mut-tafel)))
    (stage! :update-application {:name :mut-tafel :project :mut-pub :layout (layout "b")})
    (is (ok? (commit!)))
    (is (= {:published "1" :releases ["1"]} (app :mut-tafel))
        "a change keeps its history and what is published")
    (stage! :update-application {:name :mut-tafel :project :mut-pub :layout (layout "c")
                                 :auto-publish true})
    (is (ok? (commit!)))
    (is (= "2" (:published (app :mut-tafel))) ":auto-publish cuts the next version")
    (stage! :create-application {:name :mut-sofort :project :mut-pub :layout (layout "a")
                                 :auto-publish true})
    (is (ok? (commit!)))
    (is (= "1" (:published (app :mut-sofort))) "saved and published, a new app opens at 1")))
