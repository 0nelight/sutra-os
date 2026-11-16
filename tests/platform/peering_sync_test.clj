(ns ^:unit platform.peering-sync-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [sutra.access.federation :as fed]
            [sutra.datasets.mem :as datasets-mem]
            [sutra.peering.connections :as conns]
            [sutra.peering.http :as http]
            [sutra.peering.rows :as rows]
            [sutra.peering.sync :as sync]
            [sutra.peering.wire :as wire]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.access.scope :as ctx]
            [sutra.store.control.collections :as collections]
            [sutra.ref.clock :as clock]
            [sutra.ref.install :as ri]
            [sutra.store.mem :as store-mem]
            [sutra.store.world :as world]))

(def ^:private D "ri.sutra.main.dataset.6a1d4573-c08a-45c9-800f-da8508608b74")
(def ^:private K "ri.sutra.main.dataset.7b1d4573-c08a-45c9-800f-da8508608b74")

(def ^:private cols [{:attribute-name :nr :attribute-type :string}
                     {:attribute-name :betrag :attribute-type :long}])

(def ^:private schema
  {:datasets {D {:dataset-name :vertraege :columns cols}
              K {:dataset-name :kunden :columns cols :markings ["kunde"]}}})

(defn- instance []
  (world/store (store-mem/store {:world (ri/derived {:sigma clock/system} schema)}
                                {:collections collections/all}) "main"
               {:clock #(java.util.Date.) :datasets (datasets-mem/datasets)}))

(defn- trust! [s nm other]
  (fed/declare-trust! s "admin" {:name nm :url (str "https://" nm ".example")
                                 :public-key (:public-key (fed/identity-of other))
                                 :inbound? true :outbound? true}))

(defn- in-process [instances]
  (fn [store trust _path payload]
    (let [resp (http/peer-handler {:body-params (wire/envelope store payload)
                                   :sutra/store (get instances trust)
                                   :remote-addr "10.0.0.7"})]
      (if (= 200 (:status resp))
        (edn/read-string (get-in resp [:body :payload]))
        (throw (ex-info (str "peer refused: " (:status resp) " " (pr-str (:body resp)))
                        {:status (:status resp)}))))))

(defn- write! [s ds rs]
  (let [s (ctx/as-system s)]
    (dsets/ensure-datasets! s {ds cols})
    (dsets/write-rows! s ds :append (vec (for [[id v] rs] {:id id :values v})))))

(deftest a-shared-dataset-is-synced-by-the-other-side
  (let [home (instance) away (instance)]
    (trust! home "away" away)
    (trust! away "home" home)
    (write! home D {"1" {:nr "1" :betrag 100} "2" {:nr "2" :betrag 250}})
    (write! home K {"k" {:nr "k" :betrag 1}})
    (binding [wire/*transport* (in-process {"away" away "home" home})]
      (let [{:keys [invite connection-id]}
            (conns/connect! home "admin" {:name "fern" :trust "away" :datasets "export"})
            _ (conns/accept! away "admin" {:invite invite})
            _ (conns/activate! home connection-id)
            h {:store away :c (rows/connection away connection-id)}
            tables  (:tables sync/connector)
            columns (:columns sync/connector)
            scan    @#'sync/scan]
        (is (= :import (get-in (rows/connection away connection-id) [:data-types :datasets])))
        (testing "nothing is readable before it is shared"
          (is (= [] (tables h)))
          (is (thrown? clojure.lang.ExceptionInfo (scan h "vertraege" nil))))
        (sync/share! home "admin" connection-id "vertraege")
        (sync/share! home "admin" connection-id "kunden")
        (testing "a shared dataset is listed with its columns and read whole, as-is"
          (is (= ["vertraege"] (tables h)) "kunden carries a marking the ceiling lacks")
          (is (= ["nr" "betrag"] (mapv :name (columns h "vertraege"))))
          (is (= #{{"nr" "1" "betrag" 100} {"nr" "2" "betrag" 250}}
                 (set (scan h "vertraege" nil)))))
        (testing "a dataset whose markings exceed the ceiling is refused"
          (is (thrown? clojure.lang.ExceptionInfo (scan h "kunden" nil))))
        (testing "a change on the sharing side is read by the next sync"
          (write! home D {"3" {:nr "3" :betrag 7}})
          (is (= 3 (count (scan h "vertraege" nil)))))
        (testing "unshared, it is gone"
          (sync/unshare! home connection-id "vertraege")
          (is (= [] (tables h))))))))
