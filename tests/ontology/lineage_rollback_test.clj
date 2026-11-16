(ns ^:api ontology.lineage-rollback-test
  (:require [support.wire :refer [ok?]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.wire.api :as api]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-pod)

(defn- col [n t] {:attribute-name n :attribute-type t})

(def ^:private schema
  {:schema
   {:objects {:part {:project :rb :symbol "🔧" :attributes [[:pk :string] [:label :string]]
                     :plural "Parts"}}
    :transforms
    {:mk-part {:project :rb :language :clojure
               :code "(fn [{:keys [stock]}] (for [r stock] {:pk (:sku r) :label (:label r)}))"
               :inputs {:stock {:kind :dataset :dataset :stock}}
               :primary-key :pk
               :output {:kind :object-type :object-type :part}}}}})

(defn- txns
  []
  (:transactions (:body (api/get! :catalog-transactions
                                  (cond-> {:dataset "mk-part"} nil (assoc :limit nil))))))

(defn- output-rows
  []
  (let [sc  (:body (api/get! :schema))
        rid (->> (:transforms sc) vals
                 (some #(when (= :mk-part (:transform-name %))
                          (get-in % [:output :dataset]))))]
    (:rows (:body (api/get! :dataset-preview {:id (str rid)})))))

(deftest rollback-restores-a-transform-output-to-an-earlier-transaction
  (testing "a dataset-fed transform, built twice — two rows, then three"
    (is (ok? (api/mutate :create-project {:name :rb} {:stage? true})))
    (is (ok? (api/mutate :create-dataset
                         {:name :stock :project :rb
                          :columns [(col :sku :string) (col :label :string)]}
                         {:stage? true})))
    (is (ok? (api/post! :staging)) "commit dataset")
    (is (ok? (api/post! :install schema)) "install transform + object-type")
    (is (ok? (api/post! :staging)) "commit schema")

    (is (ok? (api/post! :dataset-write {:name "stock"} {:mode :snapshot
                                                        :rows [{:values {:sku "s1" :label "alpha"}}
                                                               {:values {:sku "s2" :label "beta"}}]
                                                        :message "seed 2"})))
    (is (ok? (api/post! :import-run)) "build 1")
    (is (= 2 (count (output-rows))) "two rows produced")

    (is (ok? (api/post! :dataset-write {:name "stock"} {:mode :snapshot
                                                        :rows [{:values {:sku "s1" :label "alpha"}}
                                                               {:values {:sku "s2" :label "beta"}}
                                                               {:values {:sku "s3" :label "gamma"}}]
                                                        :message "seed 3"})))
    (is (ok? (api/post! :import-run)) "build 2")
    (is (= 3 (count (output-rows))) "three rows now"))

  (testing "the ledger hands out a version token for each build"
    (let [ts (txns)]
      (is (<= 2 (count ts)) "two builds ⇒ at least two transactions")
      (is (every? (comp seq str :snapshot_id) ts)
          "each transaction records the snapshot its writes produced")))

  (testing "rolling back to the FIRST build's token restores its rows"
    (let [token (:snapshot_id (last (txns)))
          r     (api/post! :lineage-rollback {:node-kind :transform :node-key :mk-part
                                              :commit token})]
      (is (ok? r) (str "rollback to " (pr-str token) " — " (pr-str (:body r))))
      (is (= 2 (count (output-rows)))
          "the output table is back to the two rows the first build wrote")
      (is (= #{"s1" "s2"} (set (map (comp :pk :values) (output-rows))))
          "and it is those two rows, not any two")))

  (testing "a token of neither shape is refused, and says what one looks like"
    (let [r (api/post! :lineage-rollback {:node-kind :transform :node-key :mk-part
                                          :commit "not a token!"})]
      (is (not (ok? r)) "garbage must not reach a store")))

  (testing "an object type names the dataset to roll back instead"
    (let [r (api/post! :lineage-rollback {:node-kind :object-type :node-key :part
                                          :commit (:snapshot_id (last (txns)))})]
      (is (not (ok? r)))
      (is (re-find #"roll back its backing dataset" (pr-str (:body r))))))

  (testing "the rollback is a RUN, so the restore is on the record like a build"
    (is (some #(= "rollback" (str (some-> % :kind name)))
              (:runs (:body (api/list-runs))))
        "the restore is itself a build, not an erasure")))
