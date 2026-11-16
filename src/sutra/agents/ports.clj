(ns sutra.agents.ports
  (:require [sutra.kit.system :as sys]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(set! *warn-on-reflection* true)

(defn llm-complete [store] (sys/required (sys/of store) :llm/complete))

(defn llm-embed [store] (sys/required (sys/of store) :llm/relay-embed))

(def ^:dynamic *token*
  nil)

(defn call!
  [store method id m body branch]
  (binding [tx/*exchange*   (sys/required (sys/of store) :http/exchange)
            tx/*auth-token* *token*
            tx/*branch*     branch]
    (case method
      :get    (api/get! id m)
      :post   (api/post! id m body)
      :delete (api/delete! id m))))
