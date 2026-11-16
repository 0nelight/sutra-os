(ns sutra-ui.apps.source-manager.events-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [sutra-ui.api :as api]
            [sutra-ui.apps.source-manager.events :as ev]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.machine-testing :refer [run step]]
            [sutra-ui.state.shell :as shell]))

(defn- calls [[_ fx]] (keep (fn [[k a]] (when (= :call k) a)) fx))

(deftest a-reindex-shows-pending-then-its-answer-or-why-not
  (let [[s :as r] (step {} [:sources/reindex])
        [{:keys [call ok err]}] (calls r)]
    (is (= {:status :pending} (:reindex s)))
    (is (= [api/post! :import-poll] call))
    (is (= {:n 2 :status :ok} (:reindex (run s (conj ok {:n 2})))))
    (is (= {:status :error :msg "weg"} (:reindex (run s (conj err "weg")))))))

(deftest a-jdbc-source-seals-its-credentials-a-local-one-has-none
  (let [form (assoc ev/blank-source :name " lager " :type "postgres" :user "u" :password "")]
    (is (= [{:user "u"} {:name :lager :type :postgres :jdbc-url "jdbc:x"}]
           (ev/create-request form nil "jdbc:x"))))
  (let [form (assoc ev/blank-source :name "ops" :user "u")]
    (is (nil? (first (ev/create-request form nil "jdbc:sqlite:ops.sqlite"))))))

(deftest a-connector-source-splits-config-from-secrets
  (let [fields [{:key :url} {:key :token :kind :secret} {:key :tenant}]
        form   {:name "glpi" :type "glpi" :url "https://g" :token "t" :tenant ""}
        [cred base] (ev/create-request form fields nil)]
    (is (= {:token "t"} cred))
    (is (= {:url "https://g"} (:config base)))
    (is (= :glpi (:type base)))))

(deftest creating-a-source-closes-the-wizard-once-it-is-in
  (let [[{:keys [ok]}] (calls (step {:new-source ev/blank-source :wizard? true}
                                    [:sources.wizard/create nil "jdbc:sqlite:a"]))]
    (is (= [[:dispatch [:sources.wizard/close nil]] [:invoke [data/refresh-sources!]]]
           (take 2 (second (step {} (conj ok nil)))))))
  (let [[s] (step {:wizard? true :wizard-step 2 :ds-test {:status :ok}
                   :new-source {:name "x"}}
                  [:sources.wizard/close])]
    (is (= [false 0 nil ev/blank-source] ((juxt :wizard? :wizard-step :ds-test :new-source) s)))))

(deftest a-connection-test-answer-becomes-a-banner
  (is (= {:status :ok} (ev/test-result {:ok true})))
  (is (= {:status :agent-down :msg "a1"} (ev/test-result {:code "agent-down" :agent "a1"})))
  (is (= {:status :error :msg "nein"} (ev/test-result {:error "nein"})))
  (let [[s :as r] (step {} [:sources.test/source :lager])]
    (is (= {:status :pending} (:ds-test s)))
    (is (= [{:source "lager"}] (drop 3 (:call (first (calls r))))))))

(deftest deleting-a-source-asks-first
  (let [[_ [[k {:keys [ok]}]]] (step {} [:sources.source/delete :lager true])]
    (is (= :confirm k))
    (is (= [:sources.source/delete-confirmed :lager true] ok))
    (let [[{:keys [call ok]}] (calls (step {} ok))]
      (is (= [data/delete-datasource! "lager"] call))
      (is (nil? (:selected-source (run {:selected-source :lager} (conj ok nil))))))))

(deftest a-sync-run-is-busy-until-it-answers
  (let [sync {:kind :object :key :k :target :lieferant}
        [s :as r] (step {} [:sources.sync/run sync])
        [{:keys [call ok err]}] (calls r)]
    (is (= {:busy? true} (get-in s [:sync-run :k])))
    (is (= [api/post! :import-run {:only ["lieferant"]}] call))
    (is (= {:busy? false} (get-in (run s (conj ok {})) [:sync-run :k])))
    (is (= {:busy? false :error "x"} (get-in (run s (conj err "x")) [:sync-run :k])))))

(deftest saving-a-mapping-without-a-source-is-refused
  (let [[_ fx] (step {} [:sources.sync/save-link {:name :l :external nil}])]
    (is (= :invoke (ffirst fx)))
    (is (= shell/fail! (first (second (first fx)))))))

(deftest the-agents-poll-runs-while-the-pane-is-open
  (let [[s fx] (step {} [:sources.agents/enter])]
    (is (:agents-polling? s))
    (is (= [:dispatch [:sources.agents/check]] (first fx)))
    (is (= :after (first (second fx))))
    (is (= 2 (count (second (step s [:sources.agents/tick])))))
    (let [[s fx] (step s [:sources.agents/leave])]
      (is (= [[:cancel :agents-poll]] fx))
      (is (= [] (second (step s [:sources.agents/tick])))))))

(deftest unreachable-agents-are-said
  (let [[{:keys [ok err]}] (calls (step {} [:sources.agents/check]))]
    (is (= :unreachable (:agents (run {} (conj err "x")))))
    (is (= [{:agent "a"}] (:agents (run {} (conj ok {:agents [{:agent "a"}]})))))))

(deftest selecting-a-stream-resets-it-and-opens-its-tail
  (let [[s fx] (step {:streams {:selected "a" :tail [1] :error "x"}}
                     [:sources.stream/select "b"])]
    (is (= "b" (get-in s [:streams :selected])))
    (is (= [] (get-in s [:streams :tail])))
    (is (nil? (get-in s [:streams :error])))
    (is (= [:sources.stream/open "b"] (last fx)))
    (is (= 2 (count (filter #(= :rf/request (first (second %))) fx))))))

(deftest new-records-go-on-top-and-the-tail-is-capped
  (let [db {:streams {:tail (vec (range ev/tail-cap))}}
        [s] (step db [:sources.stream/records "a" [:x :y]])]
    (is (= [:y :x 0] (take 3 (get-in s [:streams :tail]))))
    (is (= ev/tail-cap (count (get-in s [:streams :tail]))))))

(deftest a-flush-is-busy-until-it-answers
  (let [[s :as r] (step {} [:sources.stream/flush "a"])
        [{:keys [ok err]}] (calls r)]
    (is (true? (get-in s [:streams :flushing?])))
    (is (false? (get-in (first (step s (conj ok {}))) [:streams :flushing?])))
    (testing "a failure frees the button and says why"
      (let [[s] (step s (conj err "voll"))]
        (is (false? (get-in s [:streams :flushing?])))
        (is (= "voll" (get-in s [:streams :error])))))))
