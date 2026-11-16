(ns ^:api ontology.rollback-parity-test
  (:require [support.wire :refer [ok?]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.wire.api :as api]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-pod)

(defn- col [n t] {:attribute-name n :attribute-type t})

(defn- dataset-rid [nm]
  (->> (:datasets (:schema (:body (api/get! :snapshot))))
       (some (fn [[rid d]] (when (= nm (:dataset-name d)) rid)))))

(defn- ks [nm]
  (sort (map (comp :k :values)
             (:rows (:body (api/get! :dataset-preview {:id (dataset-rid nm)}))))))

(defn- txns [nm]
  (:transactions (:body (api/get! :catalog-transactions {:dataset (name nm)}))))

(defn- token [nm] (:snapshot_id (first (txns nm))))

(defn- seed! [k]
  (is (ok? (api/post! :dataset-write {:name "rp_seed"}
                      {:mode :append :rows [{:values {:k k}}] :message k}))))

(defn- build! [] (is (ok? (api/post! :import-run))))

(def ^:private schema
  {:schema {:transforms {:rp-out {:project :rp :language :clojure
                                  :code "(fn [{:keys [rp_seed]}] (map #(select-keys % [:k]) rp_seed))"
                                  :inputs {:rp_seed {:kind :dataset :dataset :rp_seed}}
                                  :incremental true
                                  :output {:kind :dataset :name :rp_out :dataset :rp_out}}}}})

(deftest rollback-and-force-snapshot
  (is (ok? (api/mutate :create-project {:name :rp} {:stage? true})))
  (doseq [d [:rp_seed :rp_out]]
    (is (ok? (api/mutate :create-dataset {:name d :project :rp :columns [(col :k :string)]}
                         {:stage? true}))))
  (is (ok? (api/post! :staging)))
  (is (ok? (api/post! :install schema)))
  (is (ok? (api/post! :staging)))
  (seed! "a")
  (build!)
  (let [first-build (token :rp_out)]
    (seed! "b")
    (build!)
    (is (= ["a" "b"] (ks :rp_out)))

    (testing "a rollback rewinds the producer, so what it dropped is read again"
      (let [r (api/post! :lineage-rollback {:node-kind :dataset :node-key :rp_out
                                            :commit first-build})]
        (is (ok? r) (pr-str (:body r))))
      (is (= ["a"] (ks :rp_out)))
      (seed! "c")
      (build!)
      (is (= ["a" "b" "c"] (ks :rp_out)) "b came back with c, nothing was lost"))

    (testing "an uploaded dataset rolls back: it asks for transactional, not for a JobSpec"
      (let [r (api/post! :lineage-rollback {:node-kind :dataset :node-key :rp_seed
                                            :commit first-build})]
        (is (ok? r) (pr-str (:body r))))
      (is (= ["a"] (ks :rp_seed)))))

  (testing "a force snapshot changes nothing until the next build"
    (let [before (count (txns :rp_out))
          rows   (ks :rp_out)
          r      (api/post! :dataset-force-snapshot {:name "rp_out"} {:message "neu"})]
      (is (ok? r) (pr-str (:body r)))
      (is (= rows (ks :rp_out)) "the rows stand")
      (is (= before (count (txns :rp_out))) "and so does the history")
      (build!)
      (is (= ["a"] (ks :rp_out)) "the build read its input whole and replaced the rest")
      (is (= "SNAPSHOT" (:type (first (txns :rp_out)))))
      (seed! "d")
      (build!)
      (is (= ["a" "d"] (ks :rp_out)) "and the request is served: incremental again")
      (is (not= "SNAPSHOT" (:type (first (txns :rp_out)))))))

  (testing "a dataset no transform writes refuses a force snapshot and keeps its rows"
    (is (not (ok? (api/post! :dataset-force-snapshot {:name "rp_seed"} {}))))
    (is (= ["a" "d"] (ks :rp_seed)))))
