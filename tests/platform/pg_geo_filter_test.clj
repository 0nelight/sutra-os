(ns ^:integration platform.pg-geo-filter-test
  (:require [clojure.test :refer [deftest is]]
            [next.jdbc :as jdbc]
            [support.geo :as g]
            [support.pod :as pod]
            [sutra.store.core :as core]
            [sutra.store.core.engine]
            [sutra.store.pg :as store-pg]
            [sutra.store.pg.select :as select]))

(deftest pg-answers-every-geo-case-as-the-reference
  (let [{:keys [url user password] :as h} (pod/start-empty-pg!)
        ds (jdbc/get-datasource {:jdbcUrl url :user user :password password})]
    (try
      (let [db (doto (store-pg/store {:datasource ds} {}) core/adopt!)]
        (g/seed! db)
        (doseq [[where codes] g/cases]
          (let [compiled (atom 0)
                geo      @#'select/compile-geo]
            (with-redefs [select/compile-geo (fn [& args] (swap! compiled inc) (apply geo args))]
              (binding [sutra.store.core.engine/*verify* true]
                (is (= codes (g/codes db where)) (pr-str where))))
            (is (pos? @compiled) (str "pg itself answers " (pr-str where))))))
      (finally (pod/stop-pg! h)))))
