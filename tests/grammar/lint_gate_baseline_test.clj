(ns ^:api grammar.lint-gate-baseline-test
  (:require [sutra.catalog.grants :as grants]
            [sutra.parser.spec :as spec]
            [support.system :as system]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.catalog.lookup :as lookup]
            [sutra.store.seat]
            [sutra.actions.staging :as stg]
            [sutra.store.access.scope :as ctx]
            [sutra.parser.lint :as lint]
            [sutra.access.principals :as pr]
            [support.pod :as pod]
            [support.wire :refer [commit! ok? stage!]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(use-fixtures :each pod/with-mem-pod-auth)

(defn- mismatched-action [nm freigabeschein]
  {:name nm
   :params {:freigabeschein {:name "Freigabeschein" :type freigabeschein :required true
                             :prefill :current-object}
            :ausgestellt-am {:name "Ausgestellt am" :type :date
                             :default {:from :freigabeschein :property :ausgestellt-text}}}
   :rules [{:op :update :target [:get :freigabeschein] :values {:notiz "x"}}]})

(defn- mismatch-hits [schema]
  (filterv #(= :prefill-type-mismatch (:rule %)) (lint/run-lint schema (spec/vocabulary))))

(deftest a-finding-that-was-already-there-blocks-nothing
  (tx/with-token (-> (api/post! :login
                                {:username grants/admin-username
                                 :password pr/default-admin-password})
                     :body :token)
    (let [s (ctx/store)] (stg/clear-staged! s (stg/staging-view s)))
    (is (ok? (stage! :create-project {:name :luft})) "the project it sits in")
    (is (ok? (stage! :create-object-type
                     {:name :freigabeschein :project :luft :title :schein-nr
             :plural "Freigabescheine"
             :primary-key :schein-nr
                      :attributes [[:schein-nr :string] [:ausgestellt-text :string]
                                   [:notiz :string]]})))
    (is (ok? (commit!)) "object type")
    (testing "setup: the mismatch lands while the rule is not yet gated — as on prod"
      (with-redefs [lint/gated-rules #{}]
        (is (ok? (stage! :create-action
                         (mismatched-action :schein-alt-erfassen
                                            (lookup/rid-of
                                             (system/seated-schema) :object-types
                                             :freigabeschein)))))
        (is (ok? (commit!))))
      (is (= 1 (count (mismatch-hits (system/seated-schema)))) "the finding is live"))

    (testing "the live finding names its action"
      (is (re-find #"Action \"schein-alt-erfassen\""
                   (:message (first (mismatch-hits (system/seated-schema)))))))

    (testing "a preview of an unrelated candidate reports nothing it did not add"
      (let [r (api/post! :install
                         {:schema {:objects {:harmlos {:project :luft :plural "Harmlose"
                                                       :attributes [[:id :string]]}}}
                   :preview? true})]
        (is (ok? r) (pr-str (:body r)))
        (is (nil? (:lint-gate-violations (:problems (:body r))))
            (pr-str (:problems (:body r))))))

    (testing "an unrelated change stages and commits"
      (let [r (stage! :create-object-type
                      {:name :anderes :plural "Andere" :project :luft :title :id
                       :primary-key :id :attributes [[:id :string]]})]
        (is (ok? r) (pr-str (:body r))))
      (let [r (commit!)]
        (is (ok? r) (pr-str (:body r)))))

    (testing "a NEW mismatch is still refused"
      (let [r (stage! :create-action
                      (mismatched-action :schein-neu-erfassen
                                         (lookup/rid-of
                                          (system/seated-schema) :object-types :freigabeschein)))]
        (is (not (ok? r)) "refused at staging")
        (is (re-find #"schein-neu-erfassen" (pr-str (:body r))))
        (is (not (re-find #"schein-alt-erfassen" (pr-str (:body r))))
            "and the refusal names only what this change adds")))
    (let [s (ctx/store)] (stg/clear-staged! s (stg/staging-view s)))))
