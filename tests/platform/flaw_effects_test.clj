(ns ^:unit platform.flaw-effects-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.compute.clojure :as ct]
            [sutra.kit.effects :as effects]
            [sutra.automation.rules :as rules]))

(def ^:private with-retries #'rules/with-retries)

(deftest retry-repeats-effect
  (testing "an attempt that called the outside and then failed is not repeated"
    (let [calls (atom 0)]
      (is (thrown? Exception
                   (with-retries [0 0] (fn [_]) #(do (effects/call! (fn [] (swap! calls inc)))
                                                    (throw (ex-info "nach dem LLM-Aufruf" {}))))))
      (is (= 1 @calls))))
  (testing "an attempt that failed before any outside call is repeated"
    (let [tries (atom 0)]
      (is (= :ok
             (with-retries [0 0] (fn [_])
               #(if (< (swap! tries inc) 3) (throw (ex-info "vorher" {})) :ok))))
      (is (= 3 @tries)))))

(deftest zombie-effect
  (let [work (effects/work-token)
        run  {:work work :chat-calls (atom 0) :trace (atom []) :trace? false :transform-name :t
              :model-inputs {:m {:source "src" :model "mdl"}}
              :lm-service {:url "http://127.0.0.1:1/never"}}]
    (effects/cancel! work)
    (binding [ct/*run* run]
      (let [e (try (#'ct/chat "m" [{:role "user" :content "hallo"}]) nil
                   (catch clojure.lang.ExceptionInfo e (ex-data e)))]
        (is (= :work-cancelled (:refusal e)) (pr-str e))))
    (is (zero? @(:chat-calls run)) "the timed-out transform reached no model")
    (testing "a token past its deadline refuses as well"
      (is (thrown? clojure.lang.ExceptionInfo
                   (effects/call! (effects/work-token (- (System/currentTimeMillis) 1))
                                  (fn [] :reached)))))))
