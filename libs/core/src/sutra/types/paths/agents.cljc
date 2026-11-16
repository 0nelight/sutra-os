(ns sutra.types.paths.agents
  (:require [sutra.types.paths.rules :refer [reads writes m-own c-agent]]))

(def endpoints
  {:agent-invoke {:url "/api/agents/:name/invoke"
                  :methods {:post {:effects writes :requires c-agent
                                   :request :a/agent-invoke.post.req
                                   :response :a/agent-invoke.post.res}}
                  :service :platform
                  :branch? true}
   :agents {:url "/api/agents"
            :methods {:get {:effects reads :requires c-agent
                            :response :a/agents.get.res}}
            :service :platform
            :branch? true}
   :agent-threads {:url "/api/agent-threads"
                   :methods {:get {:effects reads :requires m-own
                                   :response :a/agent-threads.get.res}
                             :post {:effects writes :requires c-agent
                                    :response :a/agent-threads.post.res
                                    :request :a/agent-threads.post.req}}
                   :service :platform
                   :branch? true}
   :agent-thread {:url "/api/agent-threads/:id"
                  :methods {:delete {:effects writes :requires m-own
                                     :response :a/agent-thread.delete.res}
                            :get {:effects reads :requires m-own
                                  :response :a/agent-thread.get.res}}
                  :service :platform
                  :branch? true}
   :agent-thread-calls {:url "/api/agent-threads/:id/calls"
                        :methods {:get {:effects reads :requires m-own}}
                        :service :platform
                        :branch? true
                        :query {:run :str}}
   :agent-thread-ask {:url "/api/agent-threads/:id/ask"
                      :methods {:post {:effects writes :requires c-agent}}
                      :service :platform
                      :branch? true}
   :agent-thread-proposal-accept {:url "/api/agent-threads/:id/proposals/:proposal/accept"
                                  :methods {:post {:effects writes :requires c-agent
                                                   :request :a/thread-proposal-accept.req
                                                   :response :a/thread-proposal-accept.res}}
                                  :service :platform
                                  :branch? true}
   :agent-thread-proposal-reject {:url "/api/agent-threads/:id/proposals/:proposal/reject"
                                  :methods {:post {:effects writes :requires c-agent}}
                                  :service :platform
                                  :branch? true}})
