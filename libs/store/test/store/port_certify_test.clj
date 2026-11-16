(ns ^:unit store.port-certify-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.blobs.fs :as blobs-fs]
            [sutra.blobs.mem :as blobs-mem]
            [sutra.coord.mem :as coord-mem]
            [sutra.datasets.mem :as datasets-mem]
            [sutra.port.certify :as certify]
            [sutra.store.shapes :as shapes]
            [sutra.streams.mem :as streams-mem]))

(defn- passes [port driver]
  (let [r (certify/certify port driver)]
    (is (shapes/valid? :driver/certification r) (pr-str (shapes/explain :driver/certification r)))
    (is (:passed? r)
        (pr-str (into {} (remove #(= :proven (:status (val %)))) (:results r))))))

(deftest the-in-memory-ports-are-certified
  (passes :blobs {:make blobs-mem/blobs :steps 200 :seed 5})
  (passes :coord {:make coord-mem/coordinator :steps 200 :seed 5})
  (passes :streams {:make streams-mem/streams :steps 200 :seed 5})
  (passes :datasets {:make datasets-mem/datasets}))

(deftest the-file-blobs-are-certified
  (passes :blobs {:make #(blobs-fs/blobs (str (System/getProperty "java.io.tmpdir")
                                              "/certify-blobs-" (random-uuid)))
                  :steps 200 :seed 9}))
