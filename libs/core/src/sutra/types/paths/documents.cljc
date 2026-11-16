(ns sutra.types.paths.documents
  (:require [sutra.types.paths.rules :refer [reads writes m-onto c-read c-app c-doc]]))

(defn- b
  [k]
  (keyword "sutra.foundation.contracts.b" (name k)))

(def endpoints
  {
   :document-export {:url "/api/documents/:id/export"
                     :methods {:post {:effects writes :requires c-doc
                                      :request (b :document-export-in)
                                      :response (b :rendered)}}
                     :deadline :unbounded
                     :service :platform
                     :branch? true}
   :document-sheet {:url "/api/documents/:id/sheet"
                    :methods {:post {:effects writes :requires c-doc
                                     :request (b :document-sheet-in)
                                     :response (b :document-sheet)}}
                    :service :platform
                    :branch? true}
   :document-widget {:url "/api/document-widget"
                     :methods {:post {:effects writes :requires c-read
                                      :request (b :document-widget-in)
                                      :response (b :document-widget)}}
                     :service :platform
                     :branch? true}
   :document-of {:url "/api/documents/:id"
                 :methods {:delete {:effects writes
                                    :requires c-doc
                                    :response (b :document-deleted)}
                           :get {:effects reads :requires c-doc
                                 :response (b :document)}
                           :put {:effects writes :requires c-doc
                                 :request (b :document-in)
                                 :response (b :document-saved)}}
                 :service :platform
                 :branch? true}
   :document-live {:url "/api/documents/:id/live"
                   :methods {:get {:effects writes :requires c-doc}}
                   :service :platform
                   :branch? true
                   :query {:client :long}}
   :document-regenerate {:url "/api/documents/:id/regenerate"
                         :methods {:post {:effects writes :requires c-doc
                                          :request (b :document-regenerate-in)
                                          :response (b :document-saved)}}
                         :service :platform
                         :branch? true}
   :document-restore {:url "/api/documents/:id/restore"
                      :methods {:post {:effects writes :requires c-doc
                                       :request (b :document-restore-in)
                                       :response (b :document-saved)}}
                      :service :platform
                      :branch? true}
   :document-templatize {:url "/api/documents/:id/templatize"
                         :methods {:post {:effects writes :requires c-doc
                                          :request (b :document-templatize-in)}}
                         :service :platform
                         :branch? true}
   :document-version {:url "/api/document-versions/:id/:seq"
                      :methods {:get {:effects reads :requires c-doc
                                      :response (b :document-version)}}
                      :service :platform
                      :branch? true}
   :document-versions {:url "/api/documents/:id/versions"
                       :methods {:get {:effects reads :requires c-doc
                                       :response (b :document-versions)}}
                       :service :platform
                       :branch? true}
   :documents {:url "/api/documents"
               :methods {:get {:effects reads :requires c-doc
                               :response (b :documents)}
                         :post {:effects writes :requires c-doc
                                :request (b :document-in)
                                :response (b :document-saved)}}
               :service :platform
               :branch? true
               :query {:template :kw}}
   :documents-linked {:url "/api/documents/linked/:object-type/:id"
                      :methods {:get {:effects reads :requires c-doc
                                      :response (b :documents)}}
                      :service :platform
                      :branch? true}
   :template-generate {:url "/api/templates/:name/generate"
                       :methods {:post {:effects writes
                                        :requires {:op :read, :on [:templates :name]}
                                        :request (b :template-generate-in)
                                        :response (b :document-saved)}}
                       :service :platform
                       :branch? true}
   :template-of {:url "/api/templates/:name"
                 :methods {:get {:effects reads :requires {:op :read, :on [:templates :name]}
                                 :response (b :template)}}
                 :service :platform
                 :branch? true}
   :template-render {:url "/api/templates/:name/render"
                     :methods {:post {:effects writes
                                      :requires {:op :read, :on [:templates :name]}
                                      :request (b :template-render-in)
                                      :response (b :rendered)}}
                     :service :platform
                     :branch? true}
   :templates {:url "/api/templates"
               :methods {:get {:effects reads :requires m-onto
                               :response (b :templates)}}
               :service :platform
               :branch? true}
   :use-case-plan {:url "/api/use-case/:id/plan"
                   :methods {:post {:effects writes :requires c-doc
                                    :request (b :use-case-plan-in)
                                    :response (b :use-case-plan)}}
                   :service :platform
                   :branch? true}
   :use-case-apply {:url "/api/use-case/:id/apply"
                    :methods {:post {:effects writes :requires {:op :update}}}
                    :audit :schema
                    :service :platform
                    :branch? true}
   :my-application-document {:url "/api/my-application/:rid/document"
                             :methods {:post {:effects writes :requires c-app
                                              :request (b :app-document-in)}}
                             :service :platform
                             :branch? true}})
