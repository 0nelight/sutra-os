(ns ^:integration platform.pg-shadow-test
  (:require [clojure.test :refer [deftest use-fixtures]]
            [next.jdbc :as jdbc]
            [sutra.store.conformance :as store-conf]
            [sutra.store.core :as core]
            [sutra.store.core.engine]
            [sutra.store.pg :as store-pg]
            [sutra.store.shadow :as shadow]
            [support.pod :as pod]))

(use-fixtures :once pod/with-stores-only)

(defonce ^:private handles (atom []))

(defn- fresh-engine []
  (let [{:keys [url user password] :as h} (pod/start-empty-pg!)]
    (swap! handles conj h)
    (shadow/wrap
     (store-pg/engine
      {:datasource (jdbc/get-datasource {:jdbcUrl url :user user :password password})}))))

(deftest pg-answers-the-reference-semantics-under-the-store-contract
  (binding [sutra.store.core.engine/*one-handle-per-lane* false]
    (try
      (store-conf/check-store
       {:engine fresh-engine
        :db     (fn [engine collections]
                  (doto (core/db engine {:collections collections}) core/adopt!))})
      (finally (run! pod/stop-pg! @handles) (reset! handles [])))))
