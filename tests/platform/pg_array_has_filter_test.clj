(ns ^:integration platform.pg-array-has-filter-test
  (:require [clojure.test :refer [deftest is]]
            [next.jdbc :as jdbc]
            [support.array-has :as h]
            [support.pod :as pod]
            [sutra.store.core :as core]
            [sutra.store.core.engine]
            [sutra.store.pg :as store-pg]))

(deftest pg-answers-every-has-case-as-the-reference
  (let [{:keys [url user password] :as pg} (pod/start-empty-pg!)
        ds (jdbc/get-datasource {:jdbcUrl url :user user :password password})]
    (try
      (let [db (doto (store-pg/store {:datasource ds} {}) core/adopt!)]
        (h/seed! db)
        (doseq [[where codes] h/cases]
          (binding [sutra.store.core.engine/*verify* true]
            (is (= codes (h/codes db where)) (pr-str where)))))
      (finally (pod/stop-pg! pg)))))
