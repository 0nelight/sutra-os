(ns ^:integration platform.pg-store-conformance-test
  (:require [support.system :as tsys]
            [sutra.store.storage.pg-store :as pg-store]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [next.jdbc :as jdbc]
            [sutra.store.api :as api]
            [sutra.store.conformance :as store-conf]
            [sutra.store.core :as core]
            [sutra.store.core.engine]
            [sutra.store.pg :as store-pg]
            [support.pod :as pod]))

(use-fixtures :once pod/with-stores-only)

(defonce ^:private pg-handles (atom []))

(defn- fresh-engine []
  (let [{:keys [url user password] :as h} (pod/start-empty-pg!)]
    (swap! pg-handles conj h)
    (store-pg/engine
     {:datasource (jdbc/get-datasource {:jdbcUrl url :user user :password password})})))

(def ^:private driver
  {:engine fresh-engine
   :db     (fn [engine collections]
             (doto (core/db engine {:collections collections}) core/adopt!))})

(deftest pg-conforms-to-the-db-contract
  (binding [sutra.store.core.engine/*one-handle-per-lane* false]
    (try
      (store-conf/check-store driver)
      (store-conf/check-drops-with #(pg-store/store (tsys/ds) {:collections %}))
      (finally
        (run! pod/stop-pg! @pg-handles)
        (reset! pg-handles [])))))

(defn- with-root-main
  [f]
  (let [handles (atom [])]
    (try
      (f (fn [collections]
           (let [{:keys [url user password] :as h} (pod/start-empty-pg!)]
             (swap! handles conj h)
             (doto (store-pg/store {:datasource
                                      (jdbc/get-datasource
                                       {:jdbcUrl url :user user :password password})}
                                   {:collections collections})
               core/adopt!))))
      (finally (run! pod/stop-pg! @handles)))))

(deftest pg-inherits-on-a-root-main
  (with-root-main store-conf/check-inheritance))

(deftest pg-forks-main
  (testing "a fork of main is a namespace of its own"
    (let [db  (pg-store/store (tsys/ds) {:collections store-conf/collections})
          b   (str "pf" (subs (str (random-uuid)) 0 6))
          at  (with-open [s (api/snapshot db {:branch "main"})] (api/basis s))
          row (api/branch! db {:op :fork :name b :from at})]
      (is (= {:branch "main" :rev (:rev at)} (:base row)))
      (api/transact! db {:basis (with-open [s (api/snapshot db {:branch b})] (api/basis s))
                         :actor {:principal "t"} :schema {:authored store-conf/world :migration []}
                         :ops [(store-conf/create store-conf/widget 1 {:name "only-on-fork"})]})
      (with-open [s (api/snapshot db {:branch b})]
        (is (= ["only-on-fork"]
               (map (comp :name :values)
                    (api/read s {:system? true} [:objects store-conf/widget])))))
      (with-open [s (api/snapshot db {:branch "main"})]
        (is (not= store-conf/world (api/read s nil [:schema/world]))
            "main does not see the fork's world")))))
