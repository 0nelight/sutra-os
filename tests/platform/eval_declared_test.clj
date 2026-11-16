(ns ^:api platform.eval-declared-test
  (:require [sutra.catalog.grants :as grants]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.access.principals :as pr]
            [support.in-project :refer [in-project]]
            [support.pod :as pod]
            [support.seed :as seed]
            [support.wire :refer [ok? eval-run!]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(use-fixtures :once pod/with-mem-pod-auth)

(def ^:private schema
  {:objects
   {:notiz {:name "Notiz" :plural "Notizen"
            :primary-key :notiz-id :attributes [[:notiz-id :string] [:text :string]]}}
   :actions
   {:notiz-anlegen
    {:name   :notiz-anlegen
     :params {:id   {:type :string :prefill :uuid :exposure :hidden}
              :text {:name "Text" :type :string :required true}}
     :rules  [{:op :create :object-type :notiz :id [:get :id]
               :values {:notiz-id [:get :id] :text [:get :text]}}]
     :edits  [{:object-type :notiz}]}}
   :eval-suites
   {:notiz-plan
    {:action     :notiz-anlegen
     :cases      [{:id "hallo" :input {:text "hallo"}
                   :expected {:plan [{:op :create :target {:type :notiz}
                                      :after {:text "hallo"}}]}}
                  {:id "falsch" :input {:text "x"}
                   :expected {:plan [{:op :create :after {:text "nicht x"}}]}}]
     :evaluators [{:name :plant :kind :edits :field :plan}]}}})

(defn- admin-token []
  (-> (api/post! :login {:username grants/admin-username :password pr/default-admin-password}) :body
      :token))

(deftest a-declared-action-suite-runs-dry
  (tx/with-token (admin-token)
    (is (ok? (seed/identity-model!)))
    (is (ok? (api/post! :install {:schema (in-project :notizen schema)})))
    (is (ok? (api/post! :staging)))
    (testing "the declared suite is listed as declared"
      (let [s (some #(when (= "notiz-plan" (:name %)) %)
                    (:suites (:body (api/get! :eval-suites))))]
        (is (:declared s))
        (is (= "action:notiz-anlegen" (:transform s)))))
    (testing "a saved suite may not take a declared one's name"
      (is (= 409 (:status (api/post! :eval-suites
                                     {:name "notiz-plan"
                                      :spec {:name "notiz-plan" :action :notiz-anlegen
                                             :evaluators [{:name :p :kind :edits
                                                           :field :plan}]}})))))
    (let [{:keys [status body]} (eval-run! {:suite "notiz-plan"})
          by (into {} (map (juxt :id identity)) (:case-results body))]
      (is (= 200 status) (pr-str body))
      (is (true? (get-in by ["hallo" :passed])))
      (is (false? (get-in by ["falsch" :passed])))
      (is (= 1 (:iterations body)) "an action is no LLM: one iteration by default")
      (testing "nothing was written: the plan is a dry run"
        (is (empty? (:rows (:body (api/list-objects :notiz)))))))))
