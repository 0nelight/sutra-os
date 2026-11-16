(ns ^:unit store.duties-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [sutra.blobs.api :as blobs]
            [sutra.coord.api :as coord]
            [sutra.datasets.api :as datasets]
            [sutra.port.certify :as port-certify]
            [sutra.store.certify :as certify]
            [sutra.store.guard :as guard]
            [sutra.store.shapes :as shapes]
            [sutra.store.spi :as spi]
            [sutra.streams.api :as streams]))

(def ^:private catalog (edn/read-string (slurp (io/resource "sutra/store/duties.edn"))))

(def ^:private duties (:duties catalog))

(deftest the-catalog-and-the-certificates-are-in-shape
  (is (shapes/valid? :driver/duties catalog) (pr-str (shapes/explain :driver/duties catalog)))
  (let [certs (edn/read-string (slurp (io/resource "sutra/store/certificates.edn")))]
    (is (shapes/valid? :driver/certificates certs)
        (pr-str (shapes/explain :driver/certificates certs)))))

(defn- methods-of [protocol] (set (map (comp symbol name) (keys (:sigs protocol)))))

(def ^:private protocols
  {nil       [spi/Engine spi/Pushdown spi/Snapshots spi/Records]
   :blobs    [blobs/Blobs]
   :coord    [coord/Coordinator]
   :streams  [streams/Streams]
   :datasets [datasets/Datasets]})

(deftest every-driver-method-is-under-a-duty
  (doseq [[port ps] protocols
          :let [covered (set (mapcat :methods (filter #(= port (:port %)) duties)))]
          p ps
          m (methods-of p)]
    (is (contains? covered m) (str (or port :store) " " m " is under no duty in duties.edn"))))

(deftest every-duty-names-methods-of-its-protocol
  (doseq [{:keys [id port methods]} duties
          :let [known (set (mapcat methods-of (protocols port)))]
          m methods]
    (is (contains? known m) (str id " names " m ", not a method of " (or port :store)))))

(deftest every-duty-is-checked-by-something-real
  (doseq [{:keys [id checks]} duties]
    (testing id
      (is (seq checks) "a duty without a check is only a claim")
      (doseq [[kind x y :as c] checks]
        (is (case kind
              :guard   (and (= x id) (contains? guard/duties x))
              :certify (contains? (get certify/proves x) id)
              :port    (contains? (get-in port-certify/proves [x y]) id)
              :shape   (some? (shapes/schema x))
              :core    (some? (requiring-resolve x))
              false)
            (str (pr-str c) " does not check " id))))))

(deftest every-checked-duty-is-in-the-catalog
  (let [ids (set (map :id duties))]
    (doseq [d (concat guard/duties (mapcat val certify/proves)
                      (mapcat val (mapcat val port-certify/proves)))]
      (is (contains? ids d) (str d " is checked but not in duties.edn")))))
