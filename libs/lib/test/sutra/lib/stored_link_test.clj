(ns sutra.lib.stored-link-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.lib :as lib]
            [sutra.lib.dsl :refer [defobject deflink defaction ns-ontology]]
            [sutra.lib.store :as store]
            [sutra.store.api :as api]
            [sutra.lib.test-store :as ts]))

(defobject mechanic
  {:match-keys [:licence]}
  [:licence :string {:required true}]
  [:sign-offs :integer {:derived {:rollup {:over :signed :agg :count}}}])

(defobject work-order
  {:match-keys [:order-no]}
  [:order-no :string {:required true}]
  [:hours :integer])

(deflink signed-by work-order :many-to-many mechanic
  {:forward-api-name :signers :reverse-api-name :signed})

(defaction sign-off
  "A mechanic signs a work order off."
  {:on work-order}
  [order    {:type work-order :required true}
   mechanic {:type mechanic :required true}]
  :rules [{:op :create-link :link-type :signed-by
           :from-id [:get :order :id] :to-id [:get :mechanic :id]}])

(def ent (ns-ontology 'sutra.lib.stored-link-test))

(deftest a-stored-link-is-written-and-read
  (let [clock (ts/fixed-clock)
        db    (ts/fresh clock)
        W     (store/install! db (lib/lower ent) {:clock clock})
        mech  (lib/type-rid W :mechanic)
        wo    (lib/type-rid W :work-order)
        L     (ffirst (:link-types W))
        sub!  #(store/submit! db W %1 %2 {:clock clock})
        links #(with-open [s (api/snapshot db {:branch "main"})]
                 (api/read s store/system [:links L {}]))]
    (store/put! db mech [{:id "m1" :values {:licence "B1-0815"}}])
    (store/put! db wo [{:id "w1" :values {:order-no "WO-1" :hours 3}}
                       {:id "w2" :values {:order-no "WO-2" :hours 5}}])
    (testing "parameters resolve by key, the link lands in the store"
      (is (nil? (:error (sub! :sign-off {:order "WO-1" :mechanic "B1-0815"}))))
      (is (nil? (:error (sub! :sign-off {:order "WO-2" :mechanic "B1-0815"}))))
      (is (= 2 (count (links)))))
    (testing "both directions read the stored link"
      (with-open [s (api/snapshot db {:branch "main"})]
        (let [ask #(api/read s store/system %)]
          (is (= #{"w1" "w2"}
                 (set (map :id (lib/traverse W ask (first (ask [:objects mech ["m1"]])) :signed)))))
          (is (= ["m1"]
                 (map :id (lib/traverse W ask (first (ask [:objects wo ["w1"]])) :signers)))))))
    (testing "a rollup counts across it"
      (is (= 2 (get-in (first (store/read db W mech ["m1"])) [:values :sign-offs]))))
    (testing "an unknown key is refused, and nothing is stored"
      (is (= :unresolved-param
             (-> (sub! :sign-off {:order "WO-9" :mechanic "B1-0815"}) :error :kind)))
      (is (= 2 (count (links)))))))
