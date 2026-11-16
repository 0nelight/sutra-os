(ns sutra.types.paths.evals
  (:require [sutra.types.paths.rules :refer [reads writes c-agent]]))

(defn- b
  [k]
  (keyword "sutra.foundation.contracts.b" (name k)))

(def endpoints
  {:agent-tests {:url "/api/agent-tests"
                 :methods {:get {:effects reads :requires c-agent
                                 :response (b :agent-tests)}}
                 :service :platform
                 :branch? true}
   :agent-test-cases {:url "/api/agent-tests/cases"
                      :methods {:post {:effects writes :requires c-agent
                                       :request (b :agent-test-case-in)
                                       :response (b :agent-test-case)}}
                      :service :platform
                      :branch? true}
   :agent-test-case {:url "/api/agent-tests/:suite/cases/:case"
                     :methods {:delete {:effects writes :requires c-agent
                                        :response (b :agent-test-deleted)}}
                     :service :platform
                     :branch? true}
   :agent-test-run {:url "/api/agent-tests/:suite/run"
                    :methods {:post {:effects writes :requires c-agent
                                     :request (b :agent-test-run-in)
                                     :response (b :agent-test-run)}}
                    :service :platform
                    :branch? true}
   :doc-intel-sources {:url "/api/doc-intel/sources"
                       :methods {:get {:effects reads :requires {:op :read}
                                       :response (b :doc-intel-sources)}}
                       :service :platform
                       :branch? true
                       :reason "the media a caller may open in Document Intelligence"}
   :doc-intel-items {:url "/api/doc-intel/items"
                     :methods {:get {:effects reads :requires {:op :read}
                                     :response (b :doc-intel-items)}}
                     :service :platform
                     :branch? true
                     :query {:kind :str, :name :str}}
   :doc-intel-extract {:url "/api/doc-intel/extract"
                       :methods {:post {:effects writes :requires {:op :update}
                                        :request (b :doc-intel-extract-in)}}
                       :service :platform
                       :branch? true
                       :reason "one page extracted by one strategy, written nowhere"}
   :doc-intel-evaluate {:url "/api/doc-intel/evaluate"
                        :methods {:post {:effects writes :requires {:op :update}
                                         :request (b :doc-intel-evaluate-in)}}
                        :service :platform
                        :branch? true
                        :reason "a judge model's scores for one page's extraction"}
   :doc-intel-deploy {:url "/api/doc-intel/deploy"
                      :methods {:post {:effects writes :requires {:op :update}
                                       :request (b :doc-intel-deploy-in)
                                       :response (b :doc-intel-deployed)}}
                      :service :platform
                      :branch? true
                      :reason "a strategy committed as a transform template onto a branch of a code repository"}})
