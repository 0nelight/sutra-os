(ns ^:api platform.agent-evals-test
  (:require [support.wire :refer [ok?]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.wire.api :as api]
            [support.system :as tsys]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-pod)

(def ^:private schema
  {:projects   {:kasse {}}
   :llm-models {"big" {:in 1.0 :out 2.0 :currency "USD"} "small" {:in 0.1 :out 0.2 :currency "USD"}}
   :objects    {:beleg {:name "Beleg" :primary-key :pk :attributes [[:pk :string] [:betrag :number]]
                        :project :kasse
                        :plural "Belege"}}
   :agents     {:kassier {:model "big" :source :lms :project :kasse
                          :tools [{:kind :object-query :object-type :beleg}]}}})

(defn- until-idle []
  (loop [n 0]
    (let [s (first (:suites (:body (api/get! :agent-tests))))]
      (if (and (seq (:running s)) (< n 200))
        (do (Thread/sleep 50) (recur (inc n)))
        s))))

(deftest test-questions-from-the-assistant
  (doseq [[m r] {"big" [1.0 2.0] "small" [0.1 0.2]}]
    (api/mutate :create-llm-model {:name m :in (first r) :out (second r) :currency "USD"}
                {:stage? true}))
  (is (ok? (api/post! :install {:schema schema})))
  (is (ok? (api/post! :staging)))
  (tsys/provide! :llm/complete
                 (fn [{:keys [caller model]}]
                   {:request-id "r"
                    :message (if (= "eval-judge" caller)
                               {:content "{\"score\": 0.9, \"reason\": \"agrees\"}"}
                               {:content (str "Summe 36,94 (" model ")")})}))

  (testing "a question with the answer it should give becomes a test case of the agent's suite"
    (let [r (api/post! :agent-test-cases
                       {:agent "kassier" :input "Summe Juli?" :reference "36,94 Euro"
                        :must-contain ["36,94"]})]
      (is (ok? r))
      (is (= "kassier-tests" (get-in r [:body :suite]))))
    (is (not (ok? (api/post! :agent-test-cases {:agent "kassier" :input "x"})))
        "a case needs its answer")
    (is (not (ok? (api/post! :agent-test-cases {:agent "nope" :input "x" :reference "y"})))
        "an unknown agent is refused")
    (let [s (first (:suites (:body (api/get! :agent-tests))))]
      (is (= "kassier" (:agent s)))
      (is (= 1 (count (:cases s))))
      (is (= {:reference "36,94 Euro" :answer ["36,94"]} (:expected (first (:cases s)))))))

  (testing "a run on two models side by side lands in the history, each with its answers and grades"
    (is (ok? (api/post! :agent-test-run {:suite "kassier-tests"} {:models ["big" "small"]})))
    (let [s    (until-idle)
          runs (:runs s)
          by   (into {} (map (juxt #(get-in % [:summary :model]) identity)) runs)]
      (is (empty? (:running s)))
      (is (= #{"big" "small"} (set (keys by))) "one run per model")
      (is (= "Summe 36,94 (small)" (get-in by ["small" :summary :cases 0 :answer]))
          "the model asked is the one chosen")
      (is (= 0.9 (get-in by ["big" :summary :by-metric :richter]))
          "the judge's verdict is the grade")
      (is (= 1.0 (get-in by ["big" :summary :by-metric :enthaelt]))
          "the figure it must name is there")
      (is (= "agrees" (-> (some #(when (= "richter" (name (:metric %))) %) (:metrics (by "big")))
                          :detail :detail :scores first :reason)))))

  (testing "a follow-up keeps the conversation before it, and a run replays it to the model"
    (let [seen (atom [])]
      (tsys/provide! :llm/complete
                     (fn [{:keys [caller request]}]
                       (when-not (= "eval-judge" caller) (swap! seen conj (get request "messages")))
                       {:request-id "r"
                        :message (if (= "eval-judge" caller)
                                   {:content "{\"score\": 1, \"reason\": \"ok\"}"}
                                   {:content "Cineplexx Graz"})}))
      (is (ok? (api/post! :agent-test-cases
                          {:agent "kassier" :input "welcher Kinosaal?" :reference "Cineplexx Graz"
                           :history [{:role "user" :content "Wo war ich am 17. Juli?"}
                                     {:role "assistant" :content "In Graz, im Kino."}]})))
      (let [kase (last (:cases (first (:suites (:body (api/get! :agent-tests))))))]
        (is (= "welcher Kinosaal?" (get-in kase [:input :question])))
        (is (= 2 (count (get-in kase [:input :history])))))
      (is (ok? (api/post! :agent-test-run {:suite "kassier-tests"} {:models ["big"]})))
      (until-idle)
      (is (some (fn [msgs] (some #(= "In Graz, im Kino." (get % "content")) msgs)) @seen)
          "the earlier answer reached the model before the follow-up")))

  (testing "an unknown model is refused before anything runs"
    (is (not (ok? (api/post! :agent-test-run {:suite "kassier-tests"} {:models ["nope"]})))))

  (testing "a test case is deleted"
    (let [id (:id (first (:cases (first (:suites (:body (api/get! :agent-tests)))))))]
      (is (ok? (api/delete! :agent-test-case {:suite "kassier-tests" :case id})))
      (is (= 1 (count (:cases (first (:suites (:body (api/get! :agent-tests)))))))))))
