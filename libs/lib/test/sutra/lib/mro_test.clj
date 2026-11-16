(ns sutra.lib.mro-test
  (:require [clojure.edn :as edn]
            [sutra.store.api]
            [clojure.test :refer [deftest is testing]]
            [sutra.lib :as lib]
            [sutra.lib.dsl :refer [defobject deflink defaction ns-ontology]]
            [sutra.lib.store :as store]
            [sutra.lib.test-store :as ts]))

(defobject aircraft
  {:name "Aircraft" :plural "Aircraft" :match-keys [:registration] :title :registration}
  [:registration :string {:name "Registration" :required true}]
  [:open-defects :integer
   {:name    "Open defects"
    :derived {:rollup {:over :defects :agg :count :where [:= :status "open"]}}}])

(defobject defect
  {:name "Defect" :plural "Defects" :match-keys [:defect-id] :title :summary
   :lifecycle {:property    :status
               :states      [:open :deferred]
               :transitions [{:action :defer-defect :from [:open] :to :deferred}]}}
  [:defect-id    :string {:visibility :hidden}]
  [:summary      :string {:name "Summary" :required true}]
  [:severity     :string {:name "Severity" :constraints {:one-of ["minor" "critical"]}}]
  [:status       :string {:name "Status"}]
  [:registration :string {:visibility :hidden}])

(deflink defect-aircraft defect :many-to-one aircraft
  {:forward-api-name :aircraft :forward-name "Aircraft"
   :reverse-api-name :defects  :reverse-name "Defects"
   :fk :registration})

(defaction report-defect
  {:display-name "Report defect" :on aircraft}
  [aircraft  {:type aircraft :name "Aircraft" :required true}
   summary   {:type :string :name "Summary" :required true}
   severity  {:type :string :name "Severity" :required true}
   defect-id {:type :string :exposure :hidden :prefill :uuid}]
  :rules [{:op          :create
           :object-type :defect
           :id          [:get :defect-id]
           :values      {:defect-id    [:get :defect-id]
                         :registration [:get :aircraft :registration]
                         :summary      [:get :summary]
                         :severity     [:get :severity]
                         :status       "open"}}])

(defaction defer-defect
  {:display-name "Defer" :on defect}
  [defect {:type defect :name "Defect" :required true}]
  :submission-criteria [{:test    {:s [:get :defect :severity]}
                         :pred    [:not= :s "critical"]
                         :message "A critical defect cannot be deferred."}])

(def ent (ns-ontology 'sutra.lib.mro-test))

(deftest the-readme-runs
  (let [clock (ts/fixed-clock)
        db    (ts/fresh clock)
        W     (store/install! db (lib/lower ent) {:clock clock})
        ac    (lib/type-rid W :aircraft)
        df    (lib/type-rid W :defect)
        _     (store/put! db ac [{:id "D-AINA" :values {:registration "D-AINA"}}])
        sub!  #(store/submit! db W %1 %2 {:clock clock})
        stat  #(get-in (first (store/read db W df [%])) [:values :status])
        r1    (sub! :report-defect
                    {:aircraft "D-AINA" :summary "Cabin light flickers" :severity "minor"})
        d1    (-> r1 :ops first :payload :id)]
    (testing "report-defect creates an open defect on the aircraft"
      (is (nil? (:error r1)))
      (is (:committed r1))
      (is (= {:registration "D-AINA" :summary "Cabin light flickers" :severity "minor"
              :status "open"}
             (-> (store/read db W df [d1]) first :values (dissoc :defect-id)))))
    (testing "defer-defect moves it to deferred — the lifecycle supplies the rule"
      (is (nil? (:error (sub! :defer-defect {:defect d1}))))
      (is (= "deferred" (stat d1))))
    (testing "the lifecycle refuses a second deferral"
      (is (= :criterion-failed (-> (sub! :defer-defect {:defect d1}) :error :kind))))
    (let [d2 (-> (sub! :report-defect
                       {:aircraft "D-AINA" :summary "Engine fire" :severity "critical"})
                 :ops first :payload :id)]
      (testing "a critical defect cannot be deferred"
        (is (= "A critical defect cannot be deferred."
               (-> (sub! :defer-defect {:defect d2}) :error :message)))
        (is (= "open" (stat d2)) "a refused run commits nothing"))
      (testing "open-defects is one aggregate question to the store: d1 deferred, d2 open"
        (is (= 1 (get-in (first (store/read db W ac ["D-AINA"])) [:values :open-defects]))))
      (testing "the FK link reads both ways, the store deriving it from the foreign key"
        (with-open [s (sutra.store.api/snapshot db {:branch "main"})]
          (let [ask #(sutra.store.api/read s store/system %)
                [a] (ask [:objects ac ["D-AINA"]])
                [d] (ask [:objects df [d1]])]
            (is (= #{d1 d2} (set (map :id (lib/traverse W ask a :defects)))))
            (is (= ["D-AINA"] (map :id (lib/traverse W ask d :aircraft))))))))))

(deftest a-name-keeps-its-rid
  (let [clock (ts/fixed-clock)
        db    (ts/fresh clock)
        W1    (store/install! db (lib/lower ent) {:clock clock})
        W2    (store/install! db (lib/lower ent) {:clock clock})]
    (is (= (lib/type-rid W1 :aircraft) (lib/type-rid W2 :aircraft)))
    (is (= (set (keys (:object-types W1))) (set (keys (:object-types W2)))))))
