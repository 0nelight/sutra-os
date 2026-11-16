(ns ^:api scenario.aviation-kanten-test
  (:require [sutra.catalog.grants :as grants]
            [clojure.test :refer [deftest is use-fixtures]]
            [support.in-project :refer [install-in-project]]
            [sutra.access.principals :as pr]
            [support.aviation :refer [ontologie]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(defn- als-admin [f]
  (tx/with-token (-> (api/post! :login
                                {:username grants/admin-username
                                 :password pr/default-admin-password})
                     :body :token)
    (f)))

(defn- mit-welt [f]
  (assert (ok? (install-in-project {:schema ontologie})))
  (assert (ok? (api/post! :staging)))
  (doseq [k ["D-AINA" "D-AINB"]]
    (assert (ok? (api/mutate :luftfahrzeug-registrieren {:kennzeichen k :muster "A320"}
                             {:publish? true}))))
  (f))

(use-fixtures :once pod/with-pod-auth als-admin mit-welt)

(def ^:private link
  [[:einsatz :mechaniker :luftfahrzeug :many-to-many
    {:forward-api-name :einsatz-an :reverse-api-name :einsatz-mechaniker}]])

(def ^:private sql-kanten
  {:language :pg-sql
   :query
     "SELECT m.lizenz AS from_id, l.kennzeichen AS to_id FROM m JOIN l ON m.muster = l.muster"
   :inputs   {:m {:kind :object-type :object-type :mechaniker}
              :l {:kind :object-type :object-type :luftfahrzeug}}
   :output   {:kind :link-type :link-type :einsatz :from-key-column :from_id
              :to-key-column :to_id}})

(defn- einsatz [lizenz]
  (let [r (api/post! :ontology-query
                     {:find :mechaniker :where {:lizenz {:= lizenz}} :around [:einsatz-an]})]
    (is (ok? r) (pr-str (:body r)))
    (set (map #(get-in % [:values :kennzeichen]) (:rows (:body r))))))

(deftest ein-neuer-kanten-erzeuger-ueber-ein-proposal-raeumt-die-alten-kanten
  (is (ok? (install-in-project {:schema {:links link :transforms {:einsatz-kanten sql-kanten}}})))
  (is (ok? (api/post! :staging)))
  (is (ok? (api/mutate :mechaniker-anlegen {:lizenz "E-1" :name "Edda" :muster "A320"}
                       {:publish? true})))
  (is (ok? (api/post! :import-run {:force true})))
  (is (= #{"D-AINA" "D-AINB"} (einsatz "E-1")) "der SQL-Erzeuger schreibt die Kanten")
  (is (ok? (api/post! :branches {:parent "main" :name "einsatz-neu"})))
  (tx/with-branch "einsatz-neu"
    (is (ok? (install-in-project
              {:schema {:links link
                        :transforms
                          {:einsatz-kanten (-> sql-kanten (dissoc :query)
                                               (assoc :language :clojure :code "(fn [_] [])"))}}})))
    (is (ok? (api/post! :staging))))
  (let [p (api/post! :proposals {:source "einsatz-neu" :target "main" :title "Kanten"})
        m (api/post! :proposal-merge {:id (get-in p [:body :id])} nil)]
    (is (= "merged" (get-in m [:body :status])) (pr-str (:body m))))
  (is (= #{} (einsatz "E-1"))
      "gleich nach dem Merge, vor jedem Build: die Kanten des alten Erzeugers sind weg"))
