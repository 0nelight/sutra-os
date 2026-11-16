(ns ^:api ontology.flaw-stale-fresh-test
  (:require [support.wire :refer [ok?]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.wire.api :as api]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-pod)

(defn- col [n t] {:attribute-name n :attribute-type t})

(def ^:private schema
  {:schema
   {:objects {:sfpart {:project :stalefresh :symbol "🔧"
                       :attributes [[:pk :string] [:label :string]]
                       :plural "Sfparts"}}
    :transforms
    {:mk-sfpart {:project :stalefresh :language :clojure
                 :code "(fn [{:keys [sfstock]}] (for [r sfstock] {:pk (:sku r) :label (:label r)}))"
                 :inputs {:sfstock {:kind :dataset :dataset :sfstock}}
                 :primary-key :pk
                 :output {:kind :object-type :object-type :sfpart}}}}})

(defn- txns []
  (:transactions (:body (api/get! :catalog-transactions {:dataset "mk-sfpart"}))))

(defn- output-rows []
  (let [sc  (:body (api/get! :schema))
        rid (->> (:transforms sc) vals
                 (some #(when (= :mk-sfpart (:transform-name %))
                          (get-in % [:output :dataset]))))]
    (:rows (:body (api/get! :dataset-preview {:id (str rid)})))))

(defn- seed! [skus]
  (api/post! :dataset-write {:name "sfstock"}
             {:mode :snapshot
              :rows (mapv (fn [s] {:values {:sku s :label (str "l-" s)}}) skus)
              :message (str "seed " (count skus))}))

(deftest a-build-after-rollback-recomputes-the-rolled-back-output
  "freshness is the provenance of the output's current version — after a rollback
   of the output, the next build (inputs unchanged) recomputes it."
  (testing "two builds: two rows, then three"
    (is (ok? (api/mutate :create-project {:name :stalefresh} {:stage? true})))
    (is (ok? (api/mutate :create-dataset
                         {:name :sfstock :project :stalefresh
                          :columns [(col :sku :string) (col :label :string)]}
                         {:stage? true})))
    (is (ok? (api/post! :staging)))
    (is (ok? (api/post! :install schema)))
    (is (ok? (api/post! :staging)))
    (is (ok? (seed! ["s1" "s2"])))
    (is (ok? (api/post! :import-run)))
    (is (ok? (seed! ["s1" "s2" "s3"])))
    (is (ok? (api/post! :import-run)))
    (is (= 3 (count (output-rows)))))
  (testing "rollback to the first build's version"
    (let [r (api/post! :lineage-rollback {:node-kind :transform :node-key :mk-sfpart
                                          :commit (:snapshot_id (last (txns)))})]
      (is (ok? r) (pr-str (:body r))))
    (is (= 2 (count (output-rows)))))
  (testing "a build without input change recomputes the producer"
    (is (ok? (api/post! :import-run)))
    (is (= 3 (count (output-rows)))
        "the output again reflects the three input rows — the producer was not fresh")))
