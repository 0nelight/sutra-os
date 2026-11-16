(ns sutra-ui.apps.evals.events-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [sutra-ui.api :as api]
            [sutra-ui.apps.evals.events]
            [sutra-ui.state.machine-testing :refer [run step]]
            [sutra-ui.state.staging :as staging]))

(defn- reqs [[_ fx]] (keep (fn [[k a]] (when (= :call k) a)) fx))

(defn- calls [r] (map :call (reqs r)))

(defn- answered
  ([db event v] (answered db event :ok v))
  ([db event k v]
   (let [[s :as r] (step db event)]
     (run s (conj (get (first (reqs r)) k) v)))))

(deftest the-first-suite-is-selected-once-loaded
  (let [s (answered {:sel nil} [:eval-suites/load nil] {:suites [{:name "a"} {:name "b"}]})]
    (is (= "a" (:sel s))))
  (testing "a suite just registered is selected"
    (let [s (answered {:sel "a" :run "r"} [:eval-suites/load "b"] {:suites [{:name "b"}]})]
      (is (= "b" (:sel s)))
      (is (nil? (:run s)))))
  (testing "an abort leaves the list, a failure shows"
    (is (= :loading (:suites (answered {:suites :loading} [:eval-suites/load] :err nil))))
    (is (= {:error "weg"} (:suites (answered {} [:eval-suites/load] :err "weg"))))))

(deftest runs-and-cases-load-once
  (let [[s :as r] (step {:runs {}} [:eval-suites/ensure-runs "s1"])]
    (is (= :loading (get-in s [:runs "s1"])))
    (is (= [[api/get! :eval-results {:id "s1"}]] (calls r))))
  (is (= [] (calls (step {:runs {"s1" []}} [:eval-suites/ensure-runs "s1"]))))
  (is (= [[api/get! :eval-run-cases {:id "r1"}]]
         (calls (step {:cases {}} [:eval-suites/ensure-cases "r1"]))))
  (is (= [] (calls (step {:cases {"r1" :loading}} [:eval-suites/ensure-cases "r1"]))))
  (is (= [{:id 1}] (get-in (answered {:cases {}} [:eval-suites/ensure-cases "r1"]
                                     {:cases [{:id 1}]})
                           [:cases "r1"]))))

(deftest a-run-is-polled-until-its-job-ends
  (let [[s :as r] (step {} [:eval-suites/start {:id "s1" :name "beleg"} {:suite "beleg"}])]
    (is (:busy? (:running s)))
    (is (= [[api/post! :eval-run {:suite "beleg"}]] (calls r)))
    (is (= {:busy? false :err "kaputt"}
           (select-keys (:running (answered {} [:eval-suites/start {:id "s1"} {}] :err "kaputt"))
                        [:busy? :err]))))
  (is (= [[api/get! :eval-job {:id "j"}]]
         (calls (step {} [:eval-suites/job-started {:id "j"}]))))
  (testing "running: progress shows, the next poll waits a second"
    (let [[s [[k a]]] (step {:running {:busy? true}}
                            [:eval-suites/job-polled "j" {:status "running"
                                                         :progress {:done 1 :total 4}}])]
      (is (= {:done 1 :total 4} (get-in s [:running :progress])))
      (is (= [:after 1000 [:eval-suites/job-poll "j"]] [k (:ms a) (:event a)]))))
  (testing "failed: the reason shows"
    (is (= [[:dispatch [:eval-suites/run-failed "kaputt"]]]
           (second (step {} [:eval-suites/job-polled "j" {:status "failed"
                                                          :error "kaputt"}]))))))

(deftest a-finished-run-opens-a-finished-experiment-filters
  (let [[s fx] (step {:running {:suite {:id "s1"} :busy? true}}
                     [:eval-suites/finished {:run-id "r9"}])
        [_ req] (second (second fx))]
    (is (not (:busy? (:running s))))
    (is (= [:eval-suites/load] (second (first fx))))
    (is (= [:runs "s1"] (:path req)))
    (is (= [[:eval-suites/show-result {:run-id "r9"}]] (:then req))))
  (is (= "r9" (:run (first (step {} [:eval-suites/show-result {:run-id "r9"}])))))
  (let [[s] (step {:run "r1"} [:eval-suites/show-result {:experiment {:id "e" :name "x"}
                                                         :runs [1 2]}])]
    (is (= "e" (:experiment s)))
    (is (nil? (:run s)))
    (is (= {:name "x" :runs 2} (:finished s)))))

(deftest a-case-added-to-a-declared-suite-refreshes-the-staged-list
  (let [[s fx] (step {:draft-case {}} [:eval-suites/case-added true])]
    (is (nil? (:draft-case s)))
    (is (some #{[:invoke [staging/refresh-staged!]]} fx)))
  (testing "adding shows busy, a refusal shows why"
    (let [ev [:eval-suites/add-case {:name "s"} {}]]
      (is (:busy? (:case-form (first (step {} ev)))))
      (is (= "nein" (:err (:case-form (answered {} ev :err "nein"))))))))
