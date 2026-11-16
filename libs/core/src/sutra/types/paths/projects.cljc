(ns sutra.types.paths.projects
  (:require [sutra.types.paths.rules :refer [reads writes m-onto c-kompass]]))

(defn- c
  [k]
  (keyword "sutra.foundation.contracts.c" (name k)))

(def endpoints
  {:kompass-annotations {:url "/api/kompass/annotations"
                         :methods {:get {:response (c :kompass-annotations)
                                         :effects reads :requires m-onto}
                                   :post {:request (c :kompass-annotation-in)
                                          :response (c :kompass-annotated)
                                          :effects writes :requires {:op :update}}}
                         :service :platform
                         :branch? true}
   :kompass-export {:url "/api/kompass/export"
                    :methods {:get {:effects reads :requires m-onto}}
                    :deadline :unbounded
                    :service :platform
                    :query {:project :str, :repository :str, :format :str}}
   :kompass-folders {:url "/api/kompass/folders"
                     :request (c :kompass-folder-in)
                     :response (c :rid-ref)
                     :methods {:post {:effects writes :requires c-kompass}}
                     :service :platform
                     :branch? true}
   :kompass-node-move {:url "/api/kompass/nodes/:rid/move"
                       :request (c :kompass-move-in)
                       :response (c :kompass-moved)
                       :methods {:post {:effects writes :requires c-kompass}}
                       :service :platform
                       :branch? true}
   :kompass-node-of {:url "/api/kompass/nodes/:rid"
                     :response (c :deleted-ref)
                     :methods {:delete {:effects writes :requires c-kompass}}
                     :service :platform
                     :branch? true}
   :kompass-node-rename {:url "/api/kompass/nodes/:rid/rename"
                         :request (c :kompass-rename-in)
                         :response (c :kompass-renamed)
                         :methods {:post {:effects writes :requires c-kompass}}
                         :service :platform
                         :branch? true}
   :kompass-index {:url "/api/kompass/index"
                   :response (c :kompass-index)
                   :methods
                   {:get {:effects reads
                          :requires {:custom "each project's rights are the caller's own answer"}}}
                   :service :platform
                   :branch? true
                   :reason "what the files view shows beside the schema"}
   :kompass-favorites {:url "/api/kompass/favorites"
                       :request (c :kompass-favorite-in)
                       :response (c :kompass-favorite)
                       :methods {:post {:effects writes
                                        :requires {:custom "a favorite belongs to who sets it"}}}
                       :service :platform
                       :reason "the caller's own favorites"}
   :kompass-portfolios {:url "/api/kompass/portfolios"
                        :request (c :kompass-portfolio-in)
                        :response (c :rid-ref)
                        :methods {:post {:effects writes :requires {:op :update}}}
                        :service :platform
                        :reason "groupings of projects"}
   :kompass-portfolio {:url "/api/kompass/portfolios/:rid"
                       :methods {:delete {:response (c :deleted-ref)
                                          :effects writes :requires {:op :update}}
                                 :put {:request (c :kompass-portfolio-in)
                                       :response (c :rid-ref)
                                       :effects writes :requires {:op :update}}}
                       :service :platform
                       :reason "one portfolio"}
   :kompass-collections {:url "/api/kompass/collections"
                         :request (c :kompass-collection-in)
                         :response (c :rid-ref)
                         :methods {:post {:effects writes :requires {:op :update}}}
                         :service :platform
                         :reason "curated groups of files (atlas/data-catalog.md)"}
   :kompass-collection {:url "/api/kompass/collections/:rid"
                        :methods {:delete {:response (c :deleted-ref)
                                           :effects writes :requires {:op :update}}
                                  :put {:request (c :kompass-collection-in)
                                        :response (c :rid-ref)
                                        :effects writes :requires {:op :update}}}
                        :service :platform
                        :reason "one collection"}
   :kompass-files {:url "/api/kompass/files"
                   :methods
                   {:get {:response (c :kompass-files)
                          :effects reads
                          :requires {:custom "each row answers to its project or file role"}}
                    :post {:request (c :kompass-upload-in)
                           :response (c :kompass-file)
                           :effects writes
                           :requires {:custom "the target project is in the body: judged there"}}}
                   :service :platform
                   :reason "files uploaded as they are"}
   :kompass-file {:url "/api/kompass/files/:id"
                  :methods {:delete
                            {:response (c :deleted-ref)
                             :effects writes
                             :requires {:custom "the file's project or role: judged on its row"}}
                            :get
                            {:effects reads
                             :requires {:custom "the file's project or role: judged on its row"}}}
                  :service :platform
                  :reason "one uploaded file, its bytes"}
   :kompass-tabular {:url "/api/kompass/tabular"
                     :request (c :kompass-tabular-in)
                     :response (c :kompass-tabular)
                     :methods
                     {:post {:effects writes
                             :requires {:custom "reads the caller's own bytes and stores nothing"}}}
                     :service :platform
                     :reason "a table file read into columns and rows, for a dataset upload"}
   :kompass-file-grants {:url "/api/kompass/file-grants"
                         :audit :access
                         :methods
                         {:get {:response (c :kompass-grants)
                                :effects reads
                                :requires {:custom "the file's owner: judged on the ref"}}
                          :post {:request (c :kompass-grant-in)
                                 :response (c :kompass-grants)
                                 :effects writes
                                 :requires {:custom "the file's owner: judged on the ref"}}}
                         :service :platform
                         :query {:ref :str}
                         :reason "roles on one file"}
   :kompass-share-link {:url "/api/kompass/share-link"
                        :request (c :kompass-share-link-in)
                        :response (c :kompass-grants)
                        :methods {:post {:effects writes
                                         :requires {:custom "the file's owner: judged on the ref"}}}
                        :service :platform
                        :reason "a file's link turned on or off"}
   :kompass-share-redeem {:url "/api/kompass/share-link/:token"
                          :response (c :kompass-redeemed)
                          :methods {:post {:effects writes
                                           :requires {:custom "holding the link is what grants"}}}
                          :service :platform
                          :reason "visiting a shared link grants its role"}
   :kompass-project-sharing {:url "/api/kompass/project-sharing/:name"
                             :request (c :kompass-on-in)
                             :response (c :kompass-sharing)
                             :methods {:post {:effects writes
                                              :requires {:op :manage-permissions
                                                         :on [:projects :name]}}}
                             :service :platform
                             :reason "whether a project takes roles on single files"}
   :kompass-project-imports {:url "/api/kompass/project-imports/:name"
                             :request (c :kompass-imports-in)
                             :response (c :kompass-imports)
                             :methods {:post {:effects writes
                                              :requires {:op :manage-permissions
                                                         :on [:projects :name]}}}
                             :service :platform
                             :branch? true
                             :reason "what a project declares it uses from others"}
   :kompass-nodes {:url "/api/kompass/nodes"
                   :response (c :kompass-nodes)
                   :methods {:get {:effects reads :requires c-kompass}}
                   :service :platform
                   :branch? true}
   :kompass-place {:url "/api/kompass/place"
                   :request (c :kompass-place-in)
                   :response (c :kompass-placed)
                   :methods {:post {:effects writes :requires c-kompass}}
                   :service :platform
                   :branch? true}
   :project-deps {:url "/api/projects/deps"
                  :response (c :project-deps)
                  :methods {:get {:effects reads
                                  :requires m-onto}}
                  :service :platform
                  :branch? true}
   :projects {:url "/api/projects"
              :response (c :projects)
              :methods {:get {:effects reads :requires m-onto}}
              :service :platform
              :branch? true}
   :repositories {:url "/api/repositories"
                  :response (c :repositories)
                  :methods {:get {:effects reads :requires m-onto}}
                  :service :platform
                  :branch? true}})
