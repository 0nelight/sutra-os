(ns ^:integration platform.pg-ports-certify-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [sutra.coord.api :as coord]
            [sutra.datasets.api :as datasets]
            [sutra.port.certify :as certify]
            [sutra.store.pg.blobs :as pg-blobs]
            [sutra.store.pg.coord :as pg-coord]
            [sutra.store.pg.datasets :as pg-datasets]
            [sutra.store.pg.streams :as pg-streams]
            [sutra.streams.api :as s]
            [support.pod :as pod]
            [support.system :as tsys]))

(use-fixtures :once pod/with-stores-only)

(defn- passes [port driver]
  (let [r (certify/certify port driver)]
    (is (:passed? r)
        (pr-str (into {} (remove #(= :proven (:status (val %)))) (:results r))))))

(defn- blobs []
  (pg-blobs/blobs tsys/ds (str (System/getProperty "java.io.tmpdir") "/no-legacy-" (random-uuid))))

(deftest pg-blobs-are-certified
  (passes :blobs {:make blobs :steps 120 :seed 5}))

(deftest pg-coord-is-certified
  (passes :coord {:make #(pg-coord/coordinator tsys/ds)
                  :suite-make #(let [co (pg-coord/coordinator tsys/ds)]
                                 (doseq [n ["x" "y" "z" "r"]] (coord/revoke! co n))
                                 co)
                  :steps 120 :seed 5}))

(deftest pg-streams-are-certified
  (passes :streams {:make #(pg-streams/->PgStreams tsys/ds)
                    :suite-make #(let [store (pg-streams/->PgStreams tsys/ds)]
                                   (s/drop! store {:branch "main" :stream "h_conf_a"})
                                   store)
                    :steps 120 :seed 5}))

(deftest pg-datasets-are-certified
  (passes :datasets
          {:make #(let [db (pg-datasets/->PgDatasets (tsys/ds) (tsys/ds))]
                    (datasets/drop! db "feature")
                    (doseq [ds ["d_conf_a" "d_conf_b" "d_scratch_1" "d_conf_q" "d_conf_r" "d_conf_v"
                                "d_conf_w" "d_conf_ret"]]
                      (datasets/drop-dataset! db {:branch "main" :dataset ds}))
                    db)}))
