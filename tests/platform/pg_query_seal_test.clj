(ns ^:integration platform.pg-query-seal-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [support.system :as tsys]
            [support.pod :as pod]
            [sutra.datasets.api :as datasets]
            [sutra.store.pg.datasets :as pg-datasets]))

(use-fixtures :once pod/with-stores-only)

(def ^:private src {:branch "main" :dataset "d_seal_src"})

(defn- db []
  (let [d (pg-datasets/->PgDatasets (tsys/ds) (tsys/ds))]
    (datasets/drop-dataset! d src)
    (datasets/write! d src {:kind :snapshot :pk "id" :rows [{:id "1" :n 1} {:id "2" :n 2}]})
    d))

(defn- one [d sql] (first (vals (first (datasets/query d {:inputs {"src" src} :sql sql})))))

(defn- failure [f]
  (try (f) nil (catch Exception e (or (:refusal/code (ex-data e)) (.getMessage e)))))

(deftest the-query-reads-its-inputs-read-only-as-the-query-role
  (let [d (db)]
    (is (= 2 (long (one d "SELECT count(*) FROM src"))))
    (is (= "sutra_query" (one d "SELECT current_user")))
    (is (= "on" (one d "SELECT current_setting('transaction_read_only')")))
    (is (= "2min" (one d "SELECT current_setting('statement_timeout')")))
    (is (re-find #"permission denied"
                 (str (failure #(one d "SELECT count(*) FROM public.dataset_transactions"))))
        "a platform table is out of the role's reach")
    (is (= :refusal.foreign/sql-multiple
           (failure #(one d "SELECT 1; DROP TABLE public.dataset_transactions"))))
    (is (= :refusal.foreign/sql-forbidden-verb
           (failure #(one d "SELECT set_config('session_authorization', 'postgres', true)"))))
    (is (= 2 (long (one d "SELECT count(*) FROM src"))) "the pooled connection is itself again")))
