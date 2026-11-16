(ns ^:integration platform.pg-retention-sweep-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [next.jdbc :as jdbc]
            [support.system :as tsys]
            [support.pod :as pod]
            [sutra.datasets.api :as datasets]
            [sutra.store.pg.dataset :as pgds]
            [sutra.store.pg.datasets :as pg-datasets]))

(use-fixtures :once pod/with-stores-only)

(def ^:private a {:branch "main" :dataset "d_ret_sweep"})

(defn- fresh []
  (let [db (pg-datasets/->PgDatasets (tsys/ds) (tsys/ds))]
    (datasets/drop-dataset! db a)
    db))

(defn- history-rows []
  (:n (jdbc/execute-one! (tsys/ds) ["SELECT count(*) AS n FROM main.d_ret_sweep__history"])))

(defn- write! [db n]
  (:version (datasets/write! db a {:kind :update :pk "id"
                                   :rows (mapv (fn [i] {:id (str i) :n n}) (range 50))})))

(defn- partitions []
  (pgds/with-conn [c (tsys/ds)]
    (#'pgds/history-partitions c "main" "d_ret_sweep__history")))

(deftest a-large-partition-is-compacted-by-copy
  (let [db (fresh)
        _  (write! db 1)
        v2 (write! db 2)
        v3 (write! db 3)]
    (is (= 100 (history-rows)))
    (with-redefs-fn {#'pgds/compact-bytes -1}
      #(pgds/with-conn [c (tsys/ds)]
         (is (= 50 (pgds/sweep-history! c "main" "d_ret_sweep" v2)))))
    (is (= 50 (history-rows)) "the rows ended above the target stay")
    (is (= (set (range 50))
           (set (map (comp parse-long :id) (datasets/read db (assoc a :at v2))))))
    (is (every? #(= 3 (long (:n %))) (datasets/read db a)))
    (is (= 50 (count (:changed (datasets/delta db a v2 v3)))))))

(deftest a-partition-below-the-target-is-dropped-and-made-again
  (let [db (fresh)
        _  (write! db 1)
        _  (write! db 2)
        [p] (partitions)]
    (is (= 50 (history-rows)))
    (pgds/with-conn [c (tsys/ds)]
      (#'pgds/sweep-partition! c "main" p (dec (long (:hi p)))))
    (is (empty? (partitions)))
    (is (zero? (history-rows)))
    (write! db 3)
    (is (= 50 (history-rows)) "the next write makes its partition again")
    (is (every? #(= 3 (long (:n %))) (datasets/read db a)))))

(deftest a-partition-of-the-earlier-wider-span-keeps-its-versions
  (let [ds (tsys/ds)]
    (jdbc/execute! ds ["DROP TABLE IF EXISTS public.ret_span_h"])
    (jdbc/execute! ds [(str "CREATE TABLE public.ret_span_h (k TEXT, sys_to BIGINT)"
                            " PARTITION BY RANGE (sys_to)")])
    (jdbc/execute! ds [(str "CREATE TABLE public.ret_span_h_p0 PARTITION OF public.ret_span_h"
                            " FOR VALUES FROM (0) TO (1000000)")])
    (jdbc/execute! ds ["SELECT public.sutra_history_partition('public', 'ret_span_h',
                         'ret_span_h_p900000', 900000, 1000000)"])
    (jdbc/execute! ds ["INSERT INTO public.ret_span_h VALUES ('a', 950000)"])
    (is (= 1 (:n (jdbc/execute-one! ds ["SELECT count(*) AS n FROM public.ret_span_h_p0"]))))
    (jdbc/execute! ds ["SELECT public.sutra_history_partition('public', 'ret_span_h',
                         'ret_span_h_p1000000', 1000000, 1100000)"])
    (is (some? (:r (jdbc/execute-one!
                    ds ["SELECT to_regclass('public.ret_span_h_p1000000') AS r"])))
        "past the old span a partition of the new one is made")
    (jdbc/execute! ds ["DROP TABLE public.ret_span_h"])))
