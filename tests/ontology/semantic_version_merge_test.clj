(ns ^:api ontology.semantic-version-merge-test
  (:require [sutra.catalog.grants :as grants]
            [support.wire :refer [ok?]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]
            [sutra.access.principals :as pr]
            [support.pod :as pod]))

(defn- as-admin [f]
  (tx/with-token (-> (api/post! :login
                                {:username grants/admin-username
                                 :password pr/default-admin-password})
                     :body :token)
    (f)))

(use-fixtures :once pod/with-mem-pod-auth as-admin)

(defn- col [n t] {:attribute-name n :attribute-type t})

(defn- dataset-rid [nm]
  (->> (:datasets (:schema (:body (api/get! :snapshot))))
       (some (fn [[rid d]] (when (= nm (:dataset-name d)) rid)))))

(defn- out-keys []
  (set (map (comp :k :values)
            (:rows (:body (api/get! :dataset-preview {:id (dataset-rid :svm_out)}))))))

(defn- schema [sv]
  {:schema
   {:transforms
    {:svm-out (cond-> {:project :svm :language :clojure
                       :code "(fn [{:keys [svm_seed]}] (map #(select-keys % [:k]) svm_seed))"
                       :inputs {:svm_seed {:kind :dataset :dataset :svm_seed}}
                       :incremental true
                       :output {:kind :dataset :name :svm_out :dataset :svm_out :primary-key :k}}
                sv (assoc :semantic-version sv))}}})

(deftest a-bump-merged-by-proposal-starts-over-on-main
  (testing "on main: an incremental transform at version 1, built"
    (is (ok? (api/mutate :create-project {:name :svm} {:stage? true})))
    (is (ok? (api/mutate :create-dataset {:name :svm_seed :project :svm :columns [(col :k :string)]}
                         {:stage? true})))
    (is (ok? (api/mutate :create-dataset {:name :svm_out :project :svm :columns [(col :k :string)]}
                         {:stage? true})))
    (is (ok? (api/post! :staging)))
    (is (ok? (api/post! :install (schema 1))))
    (is (ok? (api/post! :staging)))
    (is (ok? (api/post! :dataset-write {:name "svm_seed"}
                        {:mode :snapshot :rows [{:values {:k "a"}} {:values {:k "b"}}]
                         :message "seed"})))
    (is (ok? (api/post! :import-run)))
    (is (= #{"a" "b"} (out-keys))))

  (testing "the bump to 2 is made on a branch and lands through a proposal"
    (is (ok? (api/post! :branches {:parent "main" :name "svm-bump"})))
    (tx/with-branch "svm-bump"
      (is (ok? (api/post! :install (schema 2))))
      (is (ok? (api/post! :staging))))
    (let [p (api/post! :proposals {:source "svm-bump" :target "main" :title "bump"})
          m (api/post! :proposal-merge {:id (get-in p [:body :id])} nil)]
      (is (ok? p) (pr-str (:body p)))
      (is (= "merged" (get-in m [:body :status])) (pr-str (:body m)))
      (is (nil? (get-in m [:body :reconcile-error])))))

  (testing "main's output was emptied and its consumer reset"
    (is (= #{} (out-keys)) "the landing started the transform over")
    (is (ok? (api/post! :import-run)))
    (is (= #{"a" "b"} (out-keys)) "the next build reads its input whole again"))

  (testing "a second reconcile of the same head resets nothing"
    (is (ok? (api/mutate :create-dataset
                         {:name :svm_other :project :svm :columns [(col :k :string)]}
                         {:stage? true})))
    (is (ok? (api/post! :staging)))
    (is (= #{"a" "b"} (out-keys)))))
