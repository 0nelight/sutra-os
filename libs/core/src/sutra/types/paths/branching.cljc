(ns sutra.types.paths.branching
  (:require [sutra.types.paths.rules
             :refer [reads writes m-own m-ask m-instance m-branch c-inbox
                     c-branch c-proposal c-access c-repo]]))

(def endpoints
  {:branch {:url "/api/branches/:name"
            :methods {:delete {:effects writes
                               :requires c-branch
                               :response :a/branch.delete.res}}
            :service :platform}
   :branch-status {:url "/api/branches/:name/status"
                   :methods
                   {:post {:effects writes
                           :requires c-branch
                           :request :a/branch-status.post.req
                           :response :a/branch-status.post.res}}
                   :service :platform}
   :branch-changes {:url "/api/branches/:name/changes"
                    :methods {:get {:effects reads :requires m-branch
                                    :response :a/branch-changes.get.res}}
                    :service :platform
                    :reason "the resources a branch changes against the branch it merges into, and its open proposal (the branch taskbar)"}
   :branch-resources {:url "/api/branches/:name/resources"
                      :methods {:post {:effects writes :requires c-branch
                                       :request :a/branch-resources.post.req
                                       :response :a/branch-resources.post.res}}
                      :service :platform
                      :reason "take resources off a branch: each returns to what its target holds"}
   :branch-rebase {:url "/api/branches/:name/rebase"
                   :methods {:post {:effects writes :requires c-branch}}
                   :service :platform
                   :reason "move a branch onto the head of the branch it merges into, each conflict decided (mine | theirs)"}
   :branch-policy {:url "/api/branches/:name/policy"
                   :audit :access
                   :methods {:get {:effects reads
                                   :response :a/branch-policy.get.res}
                             :post {:effects writes
                                    :response :a/branch-policy.post.res
                                    :request :a/branch-policy.post.req}}
                   :service :platform
                   :auth :admin
                   :reason "branch protection policy"}
   :protections {:url "/api/protections"
                 :audit :access
                 :methods {:get {:effects reads :requires m-instance
                                 :response :a/protections.get.res}
                           :post {:effects writes :requires c-access
                                  :response :a/protections.post.res
                                  :request :a/protections.post.req}}
                 :service :platform
                 :reason "which resources change on the trunk only through a proposal"}
   :access-requests {:url "/api/access/requests"
                     :methods {:get {:effects reads :requires c-access
                                     :response :a/access-requests.get.res}
                               :post {:effects writes :requires m-ask
                                      :response :a/access-requests.post.res
                                      :request :a/access-requests.post.req}}
                     :service :platform
                     :reason "requests for a project role or a group membership"
                     :query {:inbox :bool, :status :str}}
   :access-request {:url "/api/access/requests/:id"
                    :methods {:delete {:effects writes :requires c-access
                                       :response :a/access-request.delete.res}
                              :get {:effects reads :requires c-inbox
                                    :response :a/access-request.get.res}}
                    :service :platform
                    :reason "one access request"}
   :access-request-approve {:url "/api/access/requests/:id/approve"
                            :audit :access
                            :methods {:post {:effects writes :requires c-access
                                             :response :a/access-request-approve.post.res
                                             :request :a/access-request-approve.post.req}}
                            :service :platform
                            :reason "approve an access request's tasks you may decide"}
   :access-request-reject {:url "/api/access/requests/:id/reject"
                           :audit :access
                           :methods {:post {:effects writes :requires c-access
                                            :response :a/access-request-reject.post.res
                                            :request :a/access-request-reject.post.req}}
                           :service :platform
                           :reason "reject an access request"}
   :default-project {:url "/api/projects/default"
                     :methods {:get {:effects reads :requires m-instance
                                     :response :a/default-project.get.res}
                               :post {:effects writes :requires c-access
                                      :response :a/default-project.post.res
                                      :request :a/default-project.post.req}}
                     :service :platform
                     :reason "the project new resources go to when a request names none"}
   :project-policy {:url "/api/projects/:name/policy"
                    :audit :access
                    :methods {:get {:effects reads :requires c-access}
                              :post {:effects writes :requires c-access
                                     :response :a/project-policy.post.res
                                     :request :a/project-policy.post.req}}
                    :service :platform
                    :reason "a project's own policy: protect all it holds, its review rules"}
   :branch-roles {:url "/api/branches/:name/roles"
                  :audit :access
                  :methods {:get {:effects reads :requires m-branch
                                  :response :a/branch-roles.get.res}
                            :post {:effects writes :requires c-branch}}
                  :service :platform}
   :branch-sweep {:url "/api/branch-sweep"
                  :methods {:post {:effects writes
                                   :response :a/branch-sweep.post.res
                                   :request :a/branch-sweep.post.req}}
                  :service :platform
                  :auth :admin
                  :reason "the branch retention sweep"}
   :dataset-retention {:url "/api/dataset-retention"
                       :methods {:get {:effects reads}
                                 :post {:effects writes}}
                       :service :platform
                       :auth :admin
                       :query {:dataset :str}
                       :reason "Retention over datasets: the plan (GET), mark and sweep (POST)"}
   :retention-policies {:url "/api/retention-policies"
                        :methods
                        {:get {:effects reads}
                         :post {:effects writes}}
                        :service :platform
                        :auth :admin
                        :reason "the recommended and custom dataset retention policies"}
   :retention-policy {:url "/api/retention-policies/:name"
                      :methods {:delete
                                {:effects writes}}
                      :service :platform
                      :auth :admin
                      :reason "remove a custom dataset retention policy"}
   :branches {:url "/api/branches"
              :methods {:get {:effects reads :requires m-branch
                              :response :a/branches.get.res}
                        :post {:effects writes :requires m-branch
                               :response :a/branches.post.res
                               :request :a/branches.post.req}}
              :service :platform
              :query {:closed :bool, :machine :bool}}
   :proposal {:url "/api/proposals/:id"
              :methods {:delete {:effects writes
                                 :requires c-proposal
                                 :response :a/proposal.delete.res}
                        :get {:effects reads :requires m-branch
                              :response :a/proposal.get.res}}
              :service :platform}
   :proposal-approve {:url "/api/proposals/:id/approve"
                      :methods {:post {:effects writes
                                       :requires c-proposal
                                       :response :a/proposal-approve.post.res
                                       :request :a/proposal-approve.post.req}}
                      :service :platform}
   :proposal-ask-review {:url "/api/proposals/:id/ask-review"
                         :methods {:post {:effects writes :requires c-proposal
                                          :request :a/proposal-ask-review.post.req
                                          :response :a/proposal-ask-review.post.res}}
                         :service :platform}
   :proposal-comment {:url "/api/proposals/:id/comments/:comment"
                      :methods {:delete {:effects writes :requires c-proposal
                                         :response :a/proposal-comment.delete.res}}
                      :service :platform}
   :proposal-comments {:url "/api/proposals/:id/comments"
                       :methods {:get {:effects reads :requires m-branch}
                                 :post {:effects writes :requires m-branch
                                        :response :a/proposal-comments.post.res
                                        :request :a/proposal-comments.post.req}}
                       :service :platform}
   :proposal-merge {:url "/api/proposals/:id/merge"
                    :methods {:post {:effects writes :requires c-proposal
                                     :response :a/proposal-merge.post.res}}
                    :service :platform}
   :proposal-reject {:url "/api/proposals/:id/reject"
                     :methods
                     {:post {:effects writes
                             :requires c-proposal
                             :response :a/proposal-reject.post.res
                             :request :a/proposal-reject.post.req}}
                     :service :platform}
   :proposal-tasks {:url "/api/proposals/:id/tasks"
                    :methods {:get {:effects reads :requires m-branch
                                    :response :a/proposal-tasks.get.res}}
                    :service :platform}
   :proposals {:url "/api/proposals"
               :methods {:get {:effects reads :requires m-branch
                               :response :a/proposals.get.res}
                         :post {:effects writes :requires c-proposal
                                :response :a/proposals.post.res
                                :request :a/proposals.post.req}}
               :service :platform
               :query {:status :str, :target :str, :source :str, :brief :str}}
   :repo-checks {:url "/api/repositories/:rid/checks"
                 :methods {:get {:effects reads :requires c-repo
                                 :response :a/repo-checks.get.res}}
                 :service :platform
                 :query {:commit :str}}
   :repo-platform-files {:url "/api/repositories/:rid/platform-files"
                         :methods {:get {:effects reads :requires c-repo}}
                         :service :platform}
   :repo-tree {:url "/api/repositories/:rid/tree"
               :methods {:get {:effects reads :requires c-repo}}
               :service :platform
               :branch? true}
   :repo-file {:url "/api/repositories/:rid/file"
               :methods {:get {:effects reads :requires c-repo}}
               :service :platform
               :branch? true
               :query {:path :str}}
   :repo-clone-token {:url "/api/repositories/:rid/clone-token"
                      :audit :access
                      :methods {:post {:effects writes :requires c-repo
                                       :request :a/repo-clone-token.post.req
                                       :response :a/repo-clone-token.post.res}}
                      :service :platform}
   :repo-commit {:url "/api/repositories/:rid/commits"
                 :methods {:post {:effects writes
                                  :requires {:op :update, :on [:repositories :rid]}}}
                 :service :platform
                 :branch? true}
   :staging-rebase {:url "/api/staging-rebase"
                    :methods {:post {:effects writes :requires m-own
                                     :response :a/staging-rebase.post.res
                                     :request :a/staging-rebase.post.req}}
                    :service :actions
                    :branch? true}})
