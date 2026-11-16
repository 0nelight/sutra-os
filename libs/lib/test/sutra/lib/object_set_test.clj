(ns sutra.lib.object-set-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.lib :as lib]
            [sutra.lib.dsl :refer [defobjectset defaction ns-decls ontology]]
            [sutra.lib.mro-test :refer [aircraft defect]]
            [sutra.lib.store :as store]
            [sutra.lib.test-store :as ts]))

(defobjectset open-defects-of
  "The open defects of one aircraft, by summary."
  {:root        defect
   :param-slots [{:name :reg :type :string}]
   :where       [:and [:= :status "open"] [:= :registration [:param :reg]]]
   :order-by    :summary})

(defobjectset minor-open-of
  {:root        defect
   :param-slots [{:name :reg :type :string}]
   :where       [:and [:= :status "open"] [:= :severity "minor"] [:= :registration [:param :reg]]]})

(defaction defer-all-minor
  "Defers every open minor defect of an aircraft."
  {:on aircraft}
  [aircraft {:type aircraft :required true}]
  :rules [{:op :query :object-set :minor-open-of
           :arguments {:reg [:get :aircraft :registration]} :as :found}
          {:op :for-each :collection :found :as :d
           :steps [{:op :update :target [:get :d :id] :object-type :defect
                    :values {:status "deferred"}}]}])

(def ent (ontology (ns-decls 'sutra.lib.mro-test) (ns-decls 'sutra.lib.object-set-test)))

(deftest object-sets-are-asked-of-the-store
  (let [clock (ts/fixed-clock)
        db    (ts/fresh clock)
        W     (store/install! db (lib/lower ent) {:clock clock})
        ac    (lib/type-rid W :aircraft)
        sub!  #(store/submit! db W %1 %2 {:clock clock})
        open  #(store/query db W :open-defects-of {:reg %})]
    (store/put! db ac [{:id "D-AINA" :values {:registration "D-AINA"}}
                       {:id "D-AIXB" :values {:registration "D-AIXB"}}])
    (doseq [[reg summary severity] [["D-AINA" "Seat 3C loose" "minor"]
                                    ["D-AINA" "Bird strike" "critical"]
                                    ["D-AINA" "Cabin light" "minor"]
                                    ["D-AIXB" "Tyre worn" "minor"]]]
      (sub! :report-defect {:aircraft reg :summary summary :severity severity}))
    (testing "a saved set with its slot filled: one aircraft's open defects, ordered"
      (is (= ["Bird strike" "Cabin light" "Seat 3C loose"]
             (map #(get-in % [:values :summary]) (open "D-AINA"))))
      (is (= ["Tyre worn"] (map #(get-in % [:values :summary]) (open "D-AIXB")))))
    (testing "an action whose rules query a set: the kernel asks the store through Σ :query"
      (let [r (sub! :defer-all-minor {:aircraft "D-AINA"})]
        (is (nil? (:error r)))
        (is (= 2 (count (:ops r)))))
      (is (= ["Bird strike"] (map #(get-in % [:values :summary]) (open "D-AINA"))))
      (is (= ["Tyre worn"] (map #(get-in % [:values :summary]) (open "D-AIXB")))
          "the other aircraft is untouched"))
    (testing "the aircraft read through a plan carries its derived count"
      (is (= [1 1] (map #(get-in % [:values :open-defects])
                        (store/select db W
                                      {:op :order :order-by :registration
                                       :in {:op :scan :root ac}})))))))
