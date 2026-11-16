(ns ^:api ontology.staged-commit-atomic-test
  (:require [support.wire :refer [ok?]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.builds.run.rows :as run-rows]
            [sutra.store.access.scope :as ctx]
            [sutra.actions.action :as action]
            [sutra.wire.api :as api]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-pod)

(defn- staged [] (get-in (api/get! :staging) [:body :staged]))

(defn- publish-runs [] (run-rows/recent-runs (ctx/store) "main" 50 :publish))

(deftest a-refused-merge-deletes-no-staged-row
  (is (ok? (api/mutate :create-project {:name :atomic} {:stage? true})))
  (is (ok? (api/mutate :create-dataset {:name :atomic_seed :project :atomic
                                        :columns [{:attribute-name :k :attribute-type :string}]}
                       {:stage? true})))
  (is (= 2 (count (staged))))
  (let [runs-before (count (publish-runs))]
    (with-redefs [action/try-save! (fn [& _] (throw (ex-info "store refused the commit" {})))]
      (is (not (ok? (api/post! :staging))) "the commit is refused"))
    (testing "the staged rows are still staged"
      (is (= 2 (count (staged)))))
    (testing "the refused publish is a failed run, none left running"
      (let [new-runs (take (- (count (publish-runs)) runs-before) (publish-runs))]
        (is (= ["failed"] (mapv :status new-runs)) (pr-str new-runs))))
    (testing "the next commit applies the row and removes it in the same commit"
      (is (ok? (api/post! :staging)))
      (is (empty? (staged)))
      (is (= "succeeded" (:status (first (publish-runs))))))))
