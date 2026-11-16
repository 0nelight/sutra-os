(ns ^:unit platform.llm-providers-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as json]
            [sutra.llm.xai :as xai]
            [sutra.types.llm-providers :as llmp]))

(def ^:private adapters (get-in xai/module [:contributes :llm/catalogues]))

(def XAI-BODY
  (json/write-str
   {:models [{:id "grok-4.20-0309-non-reasoning"
              :prompt_text_token_price 12500 :completion_text_token_price 25000}
             {:id "grok-4.5"
              :prompt_text_token_price 20000 :completion_text_token_price 60000}
             {:id "no-price-model"}]}))

(deftest routes-adapter-by-base-url-host
  (is (some? (llmp/adapter-for adapters {:base-url "https://api.x.ai/v1"})) "top-level base-url")
  (is (some? (llmp/adapter-for adapters {:config {:base-url "https://api.x.ai/v1"}}))
      "nested under :config")
  (is (nil?  (llmp/adapter-for adapters {:base-url "https://api.openai.com/v1"}))
      "no adapter yet ⇒ nil"))

(deftest xai-catalog-maps-prices-per-million
  (let [rates (llmp/parse-catalog adapters {:base-url "https://api.x.ai/v1"} XAI-BODY)
        by-id (into {} (map (juxt :id identity) rates))]
    (testing "USD-cents-per-100M ÷ 10000 = $/1M"
      (is (= 1.25 (:in  (by-id "grok-4.20-0309-non-reasoning"))))
      (is (= 2.50 (:out (by-id "grok-4.20-0309-non-reasoning"))))
      (is (= 2.00 (:in  (by-id "grok-4.5"))))
      (is (= 6.00 (:out (by-id "grok-4.5"))))
      (is (= "USD" (:currency (by-id "grok-4.5")))))
    (testing "a model missing a price is dropped, never priced 0"
      (is (nil? (by-id "no-price-model")))
      (is (= 2 (count rates))))))

(deftest unknown-provider-throws-rather-than-silently-empty
  (is (thrown-with-msg? Exception #"no LLM-catalog adapter"
                        (llmp/parse-catalog adapters
                                            {:source-name :openai
                                             :base-url "https://api.openai.com/v1"} "{}"))))
