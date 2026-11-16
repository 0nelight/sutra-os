(ns ^:api ontology.semantic-version-backfill-test
  (:require [support.wire :refer [ok?]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.wire.api :as api]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-pod)

(defn- col [n t] {:attribute-name n :attribute-type t})

(defn- dataset-rid [nm]
  (->> (:datasets (:schema (:body (api/get! :snapshot))))
       (some (fn [[rid d]] (when (= nm (:dataset-name d)) rid)))))

(defn- out-by-k []
  (into {} (map (fn [r] [(:k (:values r)) (:values r)]))
        (:rows (:body (api/get! :dataset-preview {:id (dataset-rid :out)})))))

(defn- schema [cols sv]
  {:schema
   {:transforms
    {:mk-out (cond-> {:project :sv :language :clojure
                      :code (str "(fn [{:keys [seed]}] (map #(select-keys % " cols ") seed))")
                      :inputs {:seed {:kind :dataset :dataset :seed}}
                      :incremental true
                      :output {:kind :dataset :name :out :dataset :out :primary-key :k}}
               sv (assoc :semantic-version sv))}}})

(deftest semantic-version-bump-backfills-old-rows
  (testing "install seed[k,val] + out[k,val] + incremental producer (sv 1)"
    (is (ok? (api/mutate :create-project {:name :sv} {:stage? true})))
    (is (ok? (api/mutate :create-dataset
                         {:name :seed :project :sv :columns [(col :k :string) (col :val :string)]}
                         {:stage? true})))
    (is (ok? (api/mutate :create-dataset
                         {:name :out :project :sv :columns [(col :k :string) (col :val :string)]}
                         {:stage? true})))
    (is (ok? (api/post! :staging)))
    (is (ok? (api/post! :install (schema "[:k :val]" nil))))
    (is (ok? (api/post! :staging))))

  (testing "build 1 — out gets a, b (no extra column)"
    (is (ok? (api/post! :dataset-write {:name "seed"} {:mode :snapshot
                                 :rows [{:values {:k "a" :val "A"}}
                                        {:values {:k "b" :val "B"}}]
                                 :message "seed"})))
    (is (ok? (api/post! :import-run)))
    (let [out (out-by-k)]
      (is (= #{"a" "b"} (set (keys out))))
      (is (not (contains? (get out "a") :extra)) "no extra column yet")))

  (testing "widen source+output+query AND bump :semantic-version to backfill"
    (is (ok? (api/mutate :update-dataset
                         {:name :seed :project :sv
                          :columns [(col :k :string) (col :val :string) (col :extra :string)]}
                         {:stage? true})))
    (is (ok? (api/mutate :update-dataset
                         {:name :out :project :sv
                          :columns [(col :k :string) (col :val :string) (col :extra :string)]}
                         {:stage? true})))
    (is (ok? (api/post! :staging)))
    (is (ok? (api/post! :install (schema "[:k :val :extra]" 2))) "bump semantic-version 1→2")
    (is (ok? (api/post! :staging)) "the bump must not crash"))

  (testing "re-seed the source WITH extra, then rebuild — bump re-runs non-incrementally"
    (is (ok? (api/post! :dataset-write {:name "seed"} {:mode :snapshot
                                 :rows [{:values {:k "a" :val "A" :extra "X"}}
                                        {:values {:k "b" :val "B" :extra "Y"}}]
                                 :message "re-seed with extra"})))
    (is (ok? (api/post! :import-run))))

  (testing "the new column is BACKFILLED on the pre-existing rows"
    (let [out (out-by-k)]
      (is (= #{"a" "b"} (set (keys out))) "a, b still present (not lost by the bump's truncate)")
      (is (= "X" (:extra (get out "a"))) "extra backfilled on a (was written before the column)")
      (is (= "Y" (:extra (get out "b")))))))
