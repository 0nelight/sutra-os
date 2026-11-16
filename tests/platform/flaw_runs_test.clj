(ns ^:api platform.flaw-runs-test
  (:require [support.wire :refer [ok? eval-run!]]
            [clojure.test :refer [deftest is use-fixtures]]
            [sutra.store.control :as ctl]
            [sutra.builds.run.rows :as run-rows]
            [sutra.store.access.scope :as ctx]
            [sutra.store.seat :as seat]
            [sutra.wire.api :as api]
            [support.in-project :refer [in-project]]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-pod)

(deftest resurrected-run
  (let [store (ctx/store "main")
        id (str (random-uuid))]
    (run-rows/start-run! store "main"
                         {:id id :kind "test" :actor "test" :owner "node-x" :lease-ms 60000})
    (is (pos? (run-rows/reap-run! store "main" id "swept")))
    (is (thrown? Exception (run-rows/finish-run! store "main" id {:objects-count 1}))
        "the swept run's late finish is refused")
    (is (= "failed" (:status (run-rows/fetch-run store "main" id))) "it stays failed")))

(def ^:private schema
  {:objects
   {:hl-notiz {:name "Notiz" :plural "Notizen"
               :primary-key :notiz-id :attributes [[:notiz-id :string] [:text :string]]}}
   :actions
   {:hl-anlegen
    {:name   :hl-anlegen
     :params {:id   {:type :string :prefill :uuid :exposure :hidden}
              :text {:name "Text" :type :string :required true}}
     :rules  [{:op :create :object-type :hl-notiz :id [:get :id]
               :values {:notiz-id [:get :id] :text [:get :text]}}]
     :edits  [{:object-type :hl-notiz}]}}})

(def ^:private suite
  {:name :hl-classify :action :hl-anlegen :iterations 1
   :cases [{:id "c1" :input {:text "a"} :expected {:plan [{:op :create :after {:text "a"}}]}}]
   :evaluators [{:name :plant :kind :edits :field :plan}]})

(deftest headless-eval
  (let [store (ctx/store (seat/seated-branch))]
    (is (ok? (api/post! :install {:schema (in-project :hl schema)})))
    (is (ok? (api/post! :staging)))
    (is (= 200 (:status (api/post! :eval-suites {:name (:name suite) :spec suite}))))
    (let [runs #(count (ctl/list store :eval-runs {:branch "main"}))
          n0   (runs)
          orig ctl/entry]
      (with-redefs [ctl/entry (fn [store t row]
                                 (if (= :eval-results t)
                                   (throw
                                    (ex-info "eingeschleust: Absturz zwischen Lauf und Ergebnissen"
                                             {}))
                                   (orig store t row)))]
        (is (not= 200 (:status (eval-run! {:suite "hl-classify"})))))
      (is (= n0 (runs)) "no eval run without its results")
      (is (= 200 (:status (eval-run! {:suite "hl-classify"}))))
      (is (= (inc n0) (runs))))))
