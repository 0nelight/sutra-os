(ns ^:unit platform.peering-build-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.builds.import.changelog :as changelog]
            [sutra.peering.jobs :as jobs]
            [sutra.peering.rows :as rows]
            [sutra.store.control.collections :as collections]
            [sutra.ref.clock :as clock]
            [sutra.ref.install :as ri]
            [sutra.store.mem :as store-mem]
            [sutra.store.world :as world]))

(def ^:private T "ri.sutra.main.object-type.8f4c36f9-3ea7-4e54-b12d-17ea707983bf")

(def ^:private schema
  {:object-types {T {:object-type-name :vertrag :primary-key :nr
                     :attribute-list [{:attribute-name :nr :attribute-type :string}]}}})

(defn- instance []
  (world/store (store-mem/store {:world (ri/derived {:sigma clock/system} schema)}
                                {:collections collections/all}) "main"
               {:clock #(java.util.Date.)}))

(defn- create! [s & ids]
  (world/index! (world/at s "main")
                (vec (for [id ids]
                       {:op :create-object :type T :payload {:id id :values {:nr id}}}))))

(defn- with-peering [s]
  (assoc s :sutra.kit.system/system {:provides {:objects/foreign-owned jobs/peered-ids}}))

(defn- without-peering [s]
  (assoc s :sutra.kit.system/system {:provides {}}))

(deftest a-build-sweep-leaves-peered-objects
  (let [s (instance)]
    (create! s "own-1" "own-2" "peered-1")
    (rows/walked! s [{:connection-id "c1" :local-id "peered-1" :remote-id "r-1"
                      :local-type "vertrag"}])
    (testing "the crosswalk names what a connection brought in, by type name or rid"
      (is (= #{"peered-1"} (jobs/peered-ids s T)))
      (is (= #{"peered-1"} (jobs/peered-ids s :vertrag))))
    (testing "without the peering module the sweep removes every object the build lacks"
      (is (= #{"own-2" "peered-1"}
             (set (:removed (changelog/object-removals (without-peering s) T "ds"
                                                       {:propagate-empty? true
                                                        :produced #{"own-1"}}))))))
    (testing "with it, the peered object stays and the build's own stale one goes"
      (is (= ["own-2"]
             (:removed (changelog/object-removals (with-peering s) T "ds"
                                                  {:propagate-empty? true
                                                   :produced #{"own-1"}})))))))
