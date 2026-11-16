(ns ^:api ontology.parallelized-schedule-test
  (:require [support.wire :refer [ok?]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.wire.api :as api]
            [sutra.compute.sandbox :as sandbox]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-pod)

(def ^:private code
  "from sutra_transforms import transform, Output, StringParam, IntegerParam

@transform(inputs={'suche_id': StringParam(None), 'n': IntegerParam(2)},
           output=Output(kind='dataset', dataset='fares', primary_key='angebot'))
def fares(suche_id, n):
    return [{'angebot': 'A%d' % i} for i in range(n.value)]
")

(def ^:private schema
  {:schema
   {:projects   {:reisen {:description "Flugsuche"}}
    :datasets   {:fares {:kind :transform-output :project :reisen}}
    :transforms {:fares {:language :python :code code :project :reisen
                         :output {:kind :dataset :dataset :fares} :primary-key :angebot}}
    :objects    {:suche {:project :reisen
                         :primary-key :suche-id :attributes [[:suche-id :string] [:lauf :string]]
                         :plural "Suchen"}}
    :actions    {:suchen {:project :reisen
                          :params {:suche-id {:name "Suche id" :type :string :required true}}
                          :rules  [{:op :run-schedule :schedule "fares-je-suche"
                                    :parameters {:suche_id [:get :suche-id] :n 1} :as :lauf}
                                   {:op :create :object-type :suche
                                    :values {:suche-id [:get :suche-id] :lauf [:get :lauf]}}]}}}})

(defn- dataset-rid [nm]
  (->> (:datasets (:schema (:body (api/get! :snapshot))))
       (some (fn [[rid d]] (when (= nm (:dataset-name d)) rid)))))

(defn- fares []
  (into #{} (map :values) (:rows (:body (api/get! :dataset-preview {:id (dataset-rid :fares)})))))

(defn- share [rows s] (into #{} (comp (filter #(= s (:suche_id %))) (map :angebot)) rows))

(defn- until [pred]
  (let [deadline (+ (System/currentTimeMillis) 15000)]
    (loop []
      (let [v (pred)]
        (cond v v
              (> (System/currentTimeMillis) deadline) nil
              :else (do (Thread/sleep 250) (recur)))))))

(defn- start! [id ps]
  (:body (api/post! :schedule-run {:id id} {:parameters ps})))

(deftest run-setups-write-their-own-share-of-a-union
  (if-not (sandbox/available?)
    (println "  (parallelized-schedule SKIPPED — python sandbox unavailable)")
    (do
      (testing "install a parameterized python transform"
        (let [r (api/post! :install schema)] (is (ok? r) (pr-str (:body r))))
        (let [r (api/post! :staging)] (is (ok? r) (pr-str (:body r)))))

      (testing "a parallelized schedule refuses an automated trigger"
        (is (not (ok? (api/post! :schedules {:name "fares-je-suche" :target "fares" :scope :self
                                             :parameters {:suche_id nil :n 2}
                                             :run-setup-key :suche_id
                                             :cron "0 2 * * *"})))))

      (let [id (:id (:body (api/post! :schedules
                                      {:name "fares-je-suche" :target "fares" :scope :self
                                       :parameters {:suche_id nil :n 2}
                                       :run-setup-key :suche_id})))]
        (is (some? id))

        (testing "a run setup without its key value is refused"
          (is (not (ok? (api/post! :schedule-run {:id id} {:parameters {:n 1}})))))

        (testing "two run setups: the dataset is the union of their shares"
          (let [r (start! id {:suche_id "S-1"})] (is (string? (:run-id r)) (pr-str r)))
          (is (until #(= #{"A0" "A1"} (share (fares) "S-1"))))
          (is (string? (:run-id (start! id {:suche_id "S-2" :n 3}))))
          (is (until #(= #{"A0" "A1" "A2"} (share (fares) "S-2"))))
          (is (= #{"A0" "A1"} (share (fares) "S-1")) "S-2's run left S-1's share as it was"))

        (testing "running a run setup again replaces its share, and only its share"
          (start! id {:suche_id "S-1" :n 1})
          (is (until #(= #{"A0"} (share (fares) "S-1"))) "A1 no longer in S-1's output, so gone")
          (is (= #{"A0" "A1" "A2"} (share (fares) "S-2"))))

        (testing "an action's schedule rule starts the build and records its run on the object it creates"
          (let [r (api/mutate :suchen {:suche-id "S-3"} {:publish? true})]
            (is (ok? r) (pr-str (:body r))))
          (let [suche (first (:rows (:body (api/list-objects :suche))))]
            (is (= "S-3" (get-in suche [:values :suche-id])))
            (is (string? (get-in suche [:values :lauf])) "the run id the rule bound"))
          (is (until #(= #{"A0"} (share (fares) "S-3")))))))))
