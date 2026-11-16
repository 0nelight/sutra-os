(ns ^:unit platform.peering-links-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.peering.jobs :as jobs]
            [sutra.peering.objects :as objects]
            [sutra.peering.rows :as rows]
            [sutra.store.api :as api]
            [sutra.store.control.collections :as collections]
            [sutra.store.mem :as store-mem]
            [sutra.store.world :as world]
            [sutra.types.peering :as peering]))

(def ^:private T "ri.sutra.main.object-type.8f4c36f9-3ea7-4e54-b12d-17ea707983bf")
(def ^:private U "ri.sutra.main.object-type.11111111-3ea7-4e54-b12d-17ea707983bf")
(def ^:private L "ri.sutra.main.link-type.22222222-3ea7-4e54-b12d-17ea707983bf")

(defn- attrs [& as] (mapv (fn [a] {:attribute-name a :attribute-type :string}) as))

(def ^:private schema
  {:object-types {T {:object-type-name :vertrag :primary-key :nr :attribute-list (attrs :nr)}
                  U {:object-type-name :partner :primary-key :nr :attribute-list (attrs :nr)}}
   :link-types   {L {:link-type-name :vertrag-partner :from-object-type T :to-object-type U
                     :cardinality :many-to-one}}})

(defn- instance []
  (world/store (store-mem/store {:world schema} {:collections collections/all}) "main"
               {:clock #(java.util.Date.)}))

(defn- create! [s t id]
  (world/index! (world/at s "main")
                [{:op :create-object :type t :payload {:id id :values {:nr id}}}]))

(defn- links-of [s from]
  (world/with-snapshot (world/at s "main")
    (fn [snap] (mapv (comp str :to-object-id)
                     (api/read snap api/unrestricted [:links L {:from [from]}])))))

(deftest a-link-waits-for-its-target
  (let [s (instance) c {:connection-id "c1"}]
    (create! s T "v1")
    (testing "the target is not here: nothing is written, the edge is held"
      (let [{:keys [written held]} (objects/write-links! s schema "c1" {"v1" {L ["p1"]}})]
        (is (= 0 written))
        (is (= [{:link-type L :from-id "v1" :to-id "p1"}] held))
        (rows/hold! s "c1" #{["v1" L]} held))
      (is (= [] (links-of s "v1")))
      (is (= 1 (count (rows/pending s "c1")))))
    (testing "settling before the target arrives changes nothing"
      (is (= 0 (jobs/settle-links! s schema c)))
      (is (= 1 (count (rows/pending s "c1")))))
    (testing "once the target is here, the held edge is written and released"
      (create! s U "p1")
      (is (= 1 (jobs/settle-links! s schema c)))
      (is (= ["p1"] (links-of s "v1")))
      (is (empty? (rows/pending s "c1"))))
    (testing "a link the other side no longer names is no longer held"
      (rows/hold! s "c1" #{["v1" L]} [{:link-type L :from-id "v1" :to-id "p9"}])
      (rows/hold! s "c1" #{["v1" L]} [])
      (is (empty? (rows/pending s "c1"))))))

(deftest a-link-held-too-long-degrades-the-connection
  (let [now 10000000
        old {:since (java.util.Date. (- now peering/stale-link-ms 1))}
        new {:since (java.util.Date. (- now 1000))}]
    (is (= {:links 2 :oldest-ms (inc peering/stale-link-ms) :stale 1}
           (peering/pending-links [old new] now)))
    (is (= :degraded (:state (peering/health [] (peering/pending-links [old] now)))))
    (is (= :idle (:state (peering/health [] (peering/pending-links [new] now)))))
    (is (= :failing (:state (peering/health [{:data-type :source-data :way :in
                                               :state :failed}]
                                             (peering/pending-links [old] now)))))))
