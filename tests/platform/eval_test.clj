(ns ^:api platform.eval-test
  (:require [sutra.catalog.grants :as grants]
            [support.wire :refer [ok? eval-run!]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.access.scope :as ctx]
            [sutra.access.principals :as pr]
            [sutra.store.seat :as seat]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]
            [support.in-project :refer [in-project]]
            [support.pod :as pod]
            [support.seed :as seed]))

(use-fixtures :once pod/with-mem-pod-auth)

(def ^:private results-columns
  (vec (for [c [:run_id :suite :ref :case_id :passed :metrics :input :expected :actual :meta
                :run_at]]
         {:attribute-name c :attribute-type (if (= :passed c) :boolean :string)})))

(def ^:private schema
  {:objects
   {:notiz {:name "Notiz" :plural "Notizen"
            :primary-key :notiz-id :attributes [[:notiz-id :string] [:text :string]]}}
   :datasets
   {:eval_results {:kind :standalone :columns results-columns}}
   :actions
   {:notiz-anlegen
    {:name   :notiz-anlegen
     :params {:id   {:type :string :prefill :uuid :exposure :hidden}
              :text {:name "Text" :type :string :required true}}
     :rules  [{:op :create :object-type :notiz :id [:get :id]
               :values {:notiz-id [:get :id] :text [:get :text]}}]
     :edits  [{:object-type :notiz}]}}})

(def ^:private suite
  {:name       :notiz
   :action     :notiz-anlegen
   :iterations 1
   :cases [{:id "c1" :input {:text "a"} :expected {:plan [{:op :create :after {:text "a"}}]}}
           {:id "c2" :input {:text "b"} :expected {:plan [{:op :create :after {:text "b"}}]}}
           {:id "c3" :input {:text "c"} :expected {:plan [{:op :create :after {:text "x"}}]}}]
   :evaluators [{:name :plant :kind :edits :field :plan}]})

(defn- admin-token []
  (-> (api/post! :login {:username grants/admin-username :password pr/default-admin-password}) :body
      :token))

(defn- installed! []
  (is (ok? (seed/identity-model!)))
  (is (ok? (api/post! :install {:schema (in-project :notizen schema)})))
  (is (ok? (api/post! :staging))))

(defn- dataset-rid [nm]
  (some (fn [[rid d]] (when (= nm (name (:dataset-name d))) rid))
        (:datasets (:body (api/get! :datasets)))))

(defn- suite-id []
  (->> (:body (api/get! :eval-suites)) :suites
       (some #(when (= "notiz" (:name %)) (:id %)))))

(deftest a-saved-suite-is-checked-run-and-kept
  (tx/with-token (admin-token)
    (installed!)
    (testing "a suite whose names do not resolve is refused when saved, with a did-you-mean"
      (let [{:keys [status body]}
            (api/post! :eval-suites
                       {:name "typo"
                        :spec (assoc suite :name "typo"
                                     :evaluators [{:name :plant :kind :edits :field :pla}])})]
        (is (= 400 status))
        (is (re-find #"did you mean \"plan\"" (str (:error body))))))
    (testing "a transform target and a mean threshold are refused, not run"
      (let [{:keys [status body]}
            (api/post! :eval-suites
                       {:name "alt"
                        :spec {:name "alt" :transform :classify :match :id
                               :evaluators [{:name :k :kind :exact-match :field :kind
                                             :target {:min 0.9}}]}})]
        (is (= 400 status))
        (is (re-find #"transform as eval target is removed" (str (:error body))))
        (is (re-find #":target is removed" (str (:error body))))))
    (testing "save + list the suite over HTTP"
      (let [saved (api/post! :eval-suites {:name (:name suite) :spec suite})]
        (is (= 200 (:status saved)))
        (is (= "notiz" (get-in saved [:body :name]))))
      (is (= "action:notiz-anlegen"
             (some #(when (= "notiz" (:name %)) (:transform %))
                   (:suites (:body (api/get! :eval-suites)))))))
    (testing "a run judges every case and keeps it"
      (let [{:keys [status body]} (eval-run! {:suite "notiz"})]
        (is (= 200 status) (pr-str body))
        (is (= 3 (:cases body)))
        (is (= {:passed 2 :total 3} (select-keys (:summary body) [:passed :total]))
            "the summary counts CASES, as the pass percentage does")
        (is (= {"c1" true "c2" true "c3" false}
               (into {} (map (juxt :id :passed)) (:case-results body))))
        (let [{:keys [status body]} (api/get! :eval-run-cases {:id (:run-id body)})
              c3 (some #(when (= "c3" (:case-id %)) %) (:cases body))]
          (is (= 200 status))
          (is (= 3 (count (:cases body))))
          (is (false? (:passed c3))))))
    (testing "the run is persisted to eval history"
      (let [{:keys [status body]} (api/get! :eval-results {:id (suite-id)})]
        (is (= 200 status))
        (is (= 1 (count (:results body))))
        (is (= 1 (count (:metrics (first (:results body))))))))
    (testing "an inline-map run attaches to the same suite's history"
      (is (= 200 (:status (eval-run! {:suite suite}))))
      (is (= 2 (count (:results (:body (api/get! :eval-results {:id (suite-id)})))))
          "both the bare-name and inline-map runs land under one suite"))
    (testing "scoring an unknown suite is a clean 400, not a 500"
      (let [{:keys [status body]} (eval-run! {:suite "does-not-exist"})]
        (is (= 400 status))
        (is (re-find #"no such eval suite" (str (:error body))))))
    (testing "an experiment writes every run into the results dataset"
      (let [{:keys [status body]}
            (eval-run! {:suite      (assoc suite :name "notiz-grid"
                                           :results-dataset :eval_results)
                        :experiment {:name "t-grid" :params {:text ["a" "b"]}}
                        :meta       {:ticket "T-1"}})]
        (is (= 200 status) (pr-str body))
        (is (= 2 (count (:runs body))) "one run per combination (grid search)")
        (is (= #{"a" "b"}
               (set (map #(get-in % [:summary :experiment :params :text]) (:runs body)))))
        (is (= 1 (count (set (map #(get-in % [:summary :experiment :id]) (:runs body))))))
        (is (= {:ticket "T-1"} (get-in (first (:runs body)) [:summary :meta])))
        (is (= 3 (get-in (first (:runs body)) [:results-dataset :written])))
        (let [rows (dsets/rows (ctx/store (seat/seated-branch)) (dataset-rid "eval_results")
                               nil)]
          (is (= 6 (count rows)))
          (is (= #{true false} (set (map #(get-in % [:values :passed]) rows))))
          (is (every? #(re-find #"t-grid" (str (get-in % [:values :meta]))) rows)
              "the results dataset says which experiment a row came from"))))
    (testing "the preview lists the combinations and starts nothing"
      (let [runs-before (count (:results (:body (api/get! :eval-results {:id (suite-id)}))))
            {:keys [status body]}
            (api/post! :eval-run
                       {:suite suite :preview true
                        :experiment {:name "vorschau" :params {:text ["a" "b" "c"]}}})]
        (is (= 200 status) (pr-str body))
        (is (= 3 (:runs body)))
        (is (= runs-before (count (:results (:body (api/get! :eval-results {:id (suite-id)}))))))))
    (testing "an experiment that cannot run is refused by the request"
      (let [{:keys [status body]}
            (eval-run! {:suite suite :experiment {:name "x" :params {:gibt-es-nicht ["a"]}}})]
        (is (= 400 status))
        (is (re-find #"no input of the target" (str (:error body))))))))
