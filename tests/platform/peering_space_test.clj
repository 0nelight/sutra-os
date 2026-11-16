(ns ^:unit platform.peering-space-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.peering.http :as http]
            [sutra.peering.rows :as rows]
            [sutra.store.control.collections :as collections]
            [sutra.ref.clock :as clock]
            [sutra.ref.install :as ri]
            [sutra.store.mem :as store-mem]
            [sutra.store.world :as world]))

(def ^:private S "ri.sutra.main.space.8f4c36f9-3ea7-4e54-b12d-17ea707983bf")

(defn- instance []
  (assoc (world/store (store-mem/store {:world (ri/derived {:sigma clock/system}
                                                {:spaces {S {:space-name :einkauf}}})}
                                       {:collections collections/all})
                      "main" {:clock #(java.util.Date.)})
         :authority {:operations #{}}))

(defn- req [s username ops]
  {:sutra/store s :principal {:username username}
   :sutra/auth {:project-operations ops}})

(deftest a-space-manager-manages-its-connections
  (let [s       (instance)
        owner   (req s "eva" {S #{:manage-permissions}})
        editor  (req s "max" {S #{:read :write}})
        admin   (req s "admin" {})]
    (testing "who can manage the space"
      (is (http/manages? owner "einkauf"))
      (is (not (http/manages? editor "einkauf")))
      (is (http/manages? admin "einkauf")))
    (testing "a connection without a space, or of a space that does not exist: admin only"
      (is (not (http/manages? owner nil)))
      (is (not (http/manages? owner "verkauf")))
      (is (http/manages? admin nil)))
    (testing "the listing shows only the connections of the spaces the caller manages"
      (rows/put-connection! s {:connection-id "c1" :name "a" :space "einkauf" :status :active})
      (rows/put-connection! s {:connection-id "c2" :name "b" :space nil :status :active})
      (let [names #(set (map :name (get-in (http/connections-handler
                                            (assoc % :request-method :get))
                                           [:body :connections])))]
        (is (= #{"a"} (names owner)))
        (is (= #{} (names editor)))
        (is (= #{"a" "b"} (names admin)))))
    (testing "a connection's own calls refuse who does not manage its space"
      (is (= 403 (:status (http/jobs-handler (assoc editor :path-params {:connection "c1"})))))
      (is (= 200 (:status (http/jobs-handler (assoc owner :path-params {:connection "c1"}))))))))
