(ns sutra.types.paths.llm
  (:require [sutra.types.paths.rules :refer [reads writes]]))

(defn- b
  [k]
  (keyword "sutra.foundation.contracts.b" (name k)))

(def endpoints
  {:llm-budget {:url "/api/llm/budget"
                :methods {:get {:effects reads
                                :response (b :llm-budget)}
                          :post {:effects writes
                                 :request (b :llm-budget-in)
                                 :response (b :llm-budget)}}
                :service :source
                :auth :admin
                :reason "reading/setting the daily LLM spend cap"}
   :llm-catalog {:url "/api/llm/catalog"
                 :methods {:get {:effects reads
                                 :response (b :llm-catalog)}}
                 :service :source
                 :auth :admin
                 :reason "reading a language-model provider's catalog"
                 :query {:source :str}}
   :llm-providers {:url "/api/llm/providers"
                   :methods {:get {:effects reads
                                   :response (b :llm-providers)}}
                   :service :source
                   :branch? true
                   :auth :admin
                   :reason "reading the model providers, their keys' presence and the model registry"}
   :llm-provider-models {:url "/api/llm/providers/models"
                         :methods {:get {:effects reads
                                         :response (b :llm-provider-models)}}
                         :service :source
                         :branch? true
                         :query {:source :str}
                         :auth :admin
                         :reason "listing a model provider's models"}
   :llm-provider-test {:url "/api/llm/providers/test"
                       :methods {:post {:effects writes
                                        :request (b :llm-provider-test-in)
                                        :response (b :llm-provider-test)}}
                       :service :source
                       :branch? true
                       :auth :admin
                       :reason "a test call to a model provider, which is billed"}
   :llm-chat {:url "/api/llm/chat"
              :methods {:post {:effects writes
                               :request (b :llm-chat-in)}}
              :deadline :unbounded
              :service :source
              :auth :public}
   :llm-rates {:url "/api/llm/rates"
               :methods {:post {:effects writes
                                :request (b :llm-rates-in)}}
               :service :conductor
               :auth :admin
               :reason "backdating a model rate reprices logged spend"}})
