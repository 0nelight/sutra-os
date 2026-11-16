(ns ^:unit platform.peering-markings-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.authoring.api :as authoring]
            [sutra.catalog.grants :as grants]
            [sutra.peering.markings :as markings]
            [sutra.store.control.collections :as collections]
            [sutra.store.mem :as store-mem]
            [sutra.store.world :as world]))

(defn- instance [markings]
  (world/store (store-mem/store {:world {:markings markings}} {:collections collections/all})
               "main" {:clock #(java.util.Date.)}))

(deftest an-unknown-marking-is-declared-with-nobody-cleared
  (testing "the fragment clears nobody"
    (is (= {:markings {:peer-c1-geheim
                       {:description (str "a marking of the instance at the other end of"
                                          " peer connection c1; nobody is cleared until"
                                          " someone is")}}}
           (markings/fragment "c1" ["peer-c1-geheim"]))))
  (testing "only what main does not declare yet is published, on main, as the system, guarded"
    (let [calls (atom [])]
      (with-redefs [authoring/publish-schema!
                    (fn [store schema _live opts callbacks]
                      (swap! calls conj {:branch (:branch store) :schema schema
                                         :system? (= grants/system-authority (:authority store))
                                         :opts (dissoc opts :planned)
                                         :planned? (fn? (:planned opts))
                                         :guard? (fn? (:guard callbacks))})
                      {:tally {}})]
        (is (= ["peer-c1-neu"]
               (markings/declare! (instance {:peer-c1-alt {}}) "c1"
                                  ["peer-c1-alt" "peer-c1-neu" "peer-c1-neu"])))
        (is (= [{:branch "main" :schema (markings/fragment "c1" ["peer-c1-neu"]) :system? true
                 :opts {:provenance "peer/c1"} :planned? true :guard? true}]
               @calls))
        (testing "nothing new: nothing is published"
          (reset! calls [])
          (is (empty? (markings/declare! (instance {:peer-c1-alt {}}) "c1" ["peer-c1-alt"])))
          (is (empty? @calls)))))))
