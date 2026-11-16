(ns ^:api scenario.aviation-kontext-test
  (:require [sutra.catalog.grants :as grants]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [support.in-project :refer [install-in-project]]
            [sutra.access.principals :as pr]
            [support.aviation :refer [ontologie]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(defn- mit-welt [f]
  (binding [tx/*auth-token* (-> (api/post! :login
                                           {:username grants/admin-username
                                            :password pr/default-admin-password})
                                :body :token)]
    (let [r (install-in-project {:schema (assoc-in ontologie [:projects :lager] {})})]
      (assert (ok? r) (pr-str (:body r))))
    (assert (ok? (api/post! :staging)))
    (f)))

(use-fixtures :once pod/with-mem-pod-auth mit-welt)

(defn- projekt-von [section name-key nm]
  (let [s (:body (api/get! :schema))
        r (some #(when (= nm (get % name-key)) %) (vals (get s section)))]
    (some-> (get-in s [:projects (:project r) :project-name]) name)))

(defn- anlegen! [nm]
  (let [r (api/mutate :create-dataset
                      {:name nm :columns [{:attribute-name :id :attribute-type :string}]}
                      {:stage? true})
        c (when (ok? r) (api/post! :staging))]
    (when-not (and (ok? r) (ok? c)) (api/delete! :staging))
    (or (when-not (ok? r) r) c)))

(deftest ohne-kontext-fragt-das-tor
  (let [r (anlegen! :ohne_ort)]
    (is (not (ok? r)))
    (is (re-find #"no project" (pr-str (:body r))) (pr-str (:body r)))))

(deftest das-projekt-der-anfrage
  (tx/with-project "lager"
    (is (ok? (anlegen! :im_lager))))
  (is (= "lager" (projekt-von :datasets :dataset-name :im_lager))))

(deftest das-vorgabe-projekt-der-instanz
  (is (ok? (api/post! :default-project {:project "lager"})))
  (try
    (is (= "lager" (get-in (api/get! :default-project) [:body :project])))
    (is (ok? (anlegen! :vorgabe)))
    (is (= "lager" (projekt-von :datasets :dataset-name :vorgabe)))
    (testing "die Anfrage schlägt die Vorgabe"
      (tx/with-project "flotte" (is (ok? (anlegen! :angefragt))))
      (is (= "flotte" (projekt-von :datasets :dataset-name :angefragt))))
    (finally (api/post! :default-project {:project nil}))))

(deftest die-ableitung-geht-vor
  (tx/with-project "lager"
    (is (ok? (install-in-project :flotte
                                 {:schema
                                    {:objects
                                       {:kiste {:project :flotte :primary-key :nr
                                                :attributes [[:nr :string] [:kennzeichen :string]]
                                                :plural "Kisten"}}
                                     :links [[:kiste-lfz :kiste :luftfahrzeug :many-to-one
                                              {:fk :kennzeichen}]]}})))
    (is (ok? (api/post! :staging))))
  (is (= "flotte" (projekt-von :link-types :link-type-name :kiste-lfz))
      "der Link steht beim Quelltyp, nicht im Kontext"))
