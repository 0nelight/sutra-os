(ns sutra-ui.apps.operations.events-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [sutra.kit.remote :as rd]
            [sutra-ui.api :as api]
            [sutra-ui.apps.operations.events :as ev]
            [sutra-ui.state.machine-testing :refer [run step]]))

(defn- asks [[_ fx]] (keep (fn [[k a]] (when (= :call k) a)) fx))

(defn- calls [r] (map :call (asks r)))

(deftest entering-automations-loads-them
  (let [[s :as r] (step {:rows {"a" {:busy true}}} [:ops.automations/enter])
        [{:keys [ok err]}] (asks r)]
    (is (rd/loading? (:automations s)))
    (is (= {} (:rows s)))
    (is (= [[api/get! :automations]] (calls r)))
    (testing "an answer is the list; an aborted request leaves it"
      (is (= [{:id "a"}] (:automations (run s (conj ok {:automations [{:id "a"}]})))))
      (is (rd/loading? (:automations (run s (conj err nil)))))
      (is (rd/failed? (:automations (run s (conj err "kaputt"))))))))

(deftest a-bad-condition-is-said-not-sent
  (let [s       {:automation-form (assoc ev/blank-automation-form :name "n" :on "t"
                                         :action "a" :when "{")}
        [s' fx] (step s [:ops.automation-form/submit])]
    (is (some? (get-in s' [:automation-form :err])))
    (is (empty? fx))))

(deftest a-valid-automation-is-posted
  (let [s (assoc ev/blank-automation-form :name "n" :on "t" :action "a" :every "5000")
        [s' :as r] (step {:automation-form s} [:ops.automation-form/submit])
        [{[f k body] :call :keys [ok err]}] (asks r)]
    (is (get-in s' [:automation-form :busy]))
    (is (= [api/post! :automations] [f k]))
    (is (= {:name "n" :on "t" :effect {:action :a :args {}} :paused false :every-ms 5000}
           body))
    (testing "saved closes the form and reloads"
      (is (nil? (:automation-form (run s' (conj ok {})))))
      (is (some #{[:dispatch [:ops.automations/reload {}]]}
                (second (step s' (conj ok {}))))))
    (testing "a refusal is said in the form"
      (is (= {:busy false :err "nein"}
             (select-keys (:automation-form (run s' (conj err "nein"))) [:busy :err]))))))

(deftest a-run-shows-its-result-and-reloads-a-preview-does-not
  (let [[s :as r] (step {} [:ops.row/run "a"])
        [{:keys [ok]}] (asks r)
        [s' fx] (step s (conj ok {:matched 2}))
        fx (keep identity fx)]
    (is (get-in s [:rows "a" :busy]))
    (is (= {:matched 2 :dry? false} (get-in s' [:rows "a" :result])))
    (is (false? (get-in s' [:rows "a" :busy])))
    (is (= [[:dispatch [:ops.automations/reload {:matched 2}]]] fx)))
  (let [[s :as r] (step {} [:ops.row/preview "a"])
        [{:keys [ok]}] (asks r)]
    (is (empty? (keep identity (second (step s (conj ok {}))))))))

(deftest accepting-an-edited-proposal-sends-only-changed-args
  (let [s {:proposals {"a1" {:cards {"p" {:editing {:x "1" :y "neu"}}}}}}
        [s' :as r] (step s [:ops.card/accept "a1" "p" {:x 1 :y "alt"}])
        [[_ k params body]] (calls r)]
    (is (get-in s' [:proposals "a1" :cards "p" :busy]))
    (is (= :automation-proposal-accept k))
    (is (= {:id "a1" :proposal "p"} params))
    (is (= {:args {:y "neu"}} body)))
  (testing "a failed decision says why and keeps editing"
    (let [s {:proposals {"a1" {:cards {"p" {:editing {} :busy true}}}}}
          [s' fx] (step s [:ops.card/decided "a1" "p" {:status "failed" :error "nein"}])]
      (is (= "nein" (get-in s' [:proposals "a1" :cards "p" :err])))
      (is (= {} (get-in s' [:proposals "a1" :cards "p" :editing])))
      (is (= [[:dispatch [:ops.proposals/load "a1" false]]] fx)))))

(deftest more-proposals-append-to-the-page
  (let [s {:proposals {"a1" {:column :open :items [{:id 1}]}}}
        [{[_ _ _ opts] :call :keys [ok]}] (asks (step s [:ops.proposals/load "a1" true]))
        s' (run s (conj ok {:proposals [{:id 2}] :counts {:open 2}}))]
    (is (= {:limit ev/proposal-page :offset 1} opts))
    (is (= [{:id 1} {:id 2}] (get-in s' [:proposals "a1" :items])))))

(deftest choosing-a-node-kind-clears-the-node
  (let [[s] (step {:health-form (assoc ev/blank-health-form :node-key "x")}
                  [:ops.health-form/field :node-kind "dataset"])]
    (is (= "dataset" (get-in s [:health-form :node-kind])))
    (is (= "" (get-in s [:health-form :node-key])))))

(deftest deleting-asks-first
  (let [[_ [[k {:keys [ok]}]]] (step {} [:ops.check/delete "c" "Zeilen"])]
    (is (= :confirm k))
    (is (= [:ops.check/delete-confirmed "c"] ok))))
