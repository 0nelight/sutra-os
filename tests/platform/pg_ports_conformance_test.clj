(ns ^:integration platform.pg-ports-conformance-test
  (:require [support.system :as tsys]
            [sutra.store.pg.blobs :as pg-blobs]
            [sutra.store.pg.coord :as pg-coord]
            [sutra.store.pg.datasets :as pg-datasets]
            [sutra.store.pg.streams :as pg-streams]
            [sutra.blobs.conformance :as blobs-conf]
            [clojure.test :refer [deftest use-fixtures]]
            [sutra.coord.api :as coord]
            [sutra.coord.conformance :as coord-conf]
            [sutra.datasets.api :as datasets]
            [sutra.datasets.conformance :as datasets-conf]
            [sutra.streams.api :as s]
            [sutra.streams.conformance :as streams-conf]
            [support.pod :as pod]))

(use-fixtures :once pod/with-stores-only)

(deftest pg-blobs-conform
  (blobs-conf/check-all #(pg-blobs/blobs tsys/ds
                                         (str (System/getProperty "java.io.tmpdir") "/no-legacy-"
                                              (random-uuid)))))

(deftest pg-coord-conforms
  (coord-conf/check-all
   #(let [co (pg-coord/coordinator tsys/ds)]
      (doseq [n ["x" "y" "z" "r"]] (coord/revoke! co n))
      co)))

(deftest pg-streams-conform
  (streams-conf/check-all
   #(let [store (pg-streams/->PgStreams tsys/ds)]
      (s/drop! store {:branch "main" :stream "h_conf_a"})
      store)))

(defn- pg-datasets []
  (let [db (pg-datasets/->PgDatasets (tsys/ds) (tsys/ds))]
    (datasets/drop! db "feature")
    (doseq [ds ["d_conf_a" "d_conf_b" "d_scratch_1" "d_conf_q" "d_conf_r" "d_conf_v" "d_conf_w"
                "d_conf_ret" "d_conf_sparse" "d_conf_same"]]
      (datasets/drop-dataset! db {:branch "main" :dataset ds}))
    db))

(deftest pg-datasets-conform
  (datasets-conf/check-all pg-datasets))

(deftest pg-datasets-answer-the-transform-language
  (datasets-conf/check-query pg-datasets))
