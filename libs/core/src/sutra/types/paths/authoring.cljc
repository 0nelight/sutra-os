(ns sutra.types.paths.authoring
  (:require [sutra.types.paths.rules :refer [reads writes m-onto m-counts c-schema c-stage]]))

(def endpoints
  {:action-type-by {:url "/api/schema/action-type/:name"
                    :methods {:delete {:effects writes
                                       :requires {:op :delete, :on [:actions :name]}
                                       :response :a/action-type-by.delete.res}}
                    :service :oms}
   :resource-file {:url "/api/resources/:section/:name/file"
                   :methods {:get {:effects reads
                                   :requires {:op :read, :on [{:param :section} :name]}}
                             :post {:effects writes
                                    :requires {:op :update, :on [{:param :section} :name]}}}
                   :service :platform
                   :branch? true}
   :resource-versions {:url "/api/versions/:section/:name"
                       :methods {:get {:effects reads
                                       :requires {:op :read, :on [{:param :section} :name]}}}
                       :service :platform
                       :branch? true}
   :resource-version {:url "/api/versions/:section/:name/releases/:version"
                      :methods {:get {:effects reads
                                      :requires {:op :read, :on [{:param :section} :name]}}}
                      :service :platform
                      :branch? true}
   :resource-versions-publish {:url "/api/versions/:section/:name/publish"
                               :methods {:post {:effects writes
                                                :requires {:op :update
                                                           :on [{:param :section} :name]}
                                                :request :a/resource-versions-publish.post.req
                                                :response :a/resource-versions-publish.post.res}}
                               :service :platform
                               :branch? true}
   :resource-versions-restore {:url "/api/versions/:section/:name/restore"
                               :methods {:post {:effects writes
                                                :requires {:op :update
                                                           :on [{:param :section} :name]}}}
                               :service :platform
                               :branch? true}
   :projections {:url "/api/projections"
                 :methods {:get {:effects reads :requires m-onto
                                 :response :a/projections.get.res}}
                 :service :platform
                 :branch? true}
   :projection-files {:url "/api/projections/:name/files"
                      :methods {:get {:effects reads :requires m-onto
                                      :response :a/projection-files.get.res}
                                :post {:effects writes :requires {:op :update}
                                       :response :a/projection-files.post.res
                                       :request :a/projection-files.post.req}}
                      :service :platform
                      :branch? true
                      :query {:rehome :bool, :texts :str, :project :str}}
   :at-object-type {:url "/api/at/:commit/schema/object-type/:type"
                    :methods {:get {:effects reads
                                    :requires {:op :read, :on [:object-types :type]}}}
                    :service :oms
                    :branch? true}
   :datasource {:url "/api/datasources/:name"
                :audit :access
                :methods {:delete {:effects writes :requires {:op :delete, :on [:sources :name]}}}
                :service :source
                :branch? true}
   :datasource-adopt {:url "/api/datasources/:name/adopt"
                      :methods {:post {:effects writes
                                       :requires {:op :update, :on [:sources :name]}}}
                      :service :oms
                      :branch? true}
   :install {:url "/api/install"
             :audit :schema
             :methods {:post {:effects writes :requires c-stage
                              :request :a/install.post.req
                              :response :a/install.post.res}}
             :service :oms
             :branch? true}
   :package-install {:url "/api/packages/install"
                     :methods {:post {:effects writes :requires {:op :update}
                                      :request :a/package-install.post.req
                                      :response :a/package-install.post.res}}
                     :service :oms
                     :branch? true}
   :package-upgrade {:url "/api/packages/upgrade"
                     :methods {:post {:effects writes :requires {:op :update}
                                      :request :a/package-upgrade.post.req
                                      :response :a/package-upgrade.post.res}}
                     :service :oms
                     :branch? true}
   :packages {:url "/api/packages"
              :methods {:get {:effects reads
                              :requires {:op :read}
                              :response :a/packages.get.res}}
              :service :oms
              :branch? true}
   :products {:url "/api/products"
              :audit :schema
              :methods {:get {:effects reads :requires {:op :read}}
                        :post {:effects writes :requires {:op :update}}}
              :service :oms
              :branch? true}
   :product {:url "/api/products/:product"
             :methods {:get {:effects reads :requires {:op :read}}}
             :service :oms}
   :product-release {:url "/api/products/:product/releases/:version"
                     :audit :schema
                     :methods {:post {:effects writes :requires {:op :update}}}
                     :service :oms}
   :product-bundle {:url "/api/products/:product/releases/:version/bundle"
                    :methods {:get {:effects reads :requires {:op :read}}}
                    :service :oms}
   :product-imports {:url "/api/product-imports"
                     :audit :schema
                     :methods {:post {:effects writes :requires {:op :update}}}
                     :service :oms}
   :product-trust {:url "/api/product-trust"
                   :audit :access
                   :methods {:get {:effects reads :requires {:op :manage-permissions}}}
                   :service :oms}
   :product-install {:url "/api/products/:product/install"
                     :audit :schema
                     :methods {:post {:effects writes :requires {:op :update}}}
                     :service :oms
                     :branch? true}
   :product-installations {:url "/api/product-installations"
                           :methods {:get {:effects reads :requires {:op :read}}}
                           :service :oms}
   :product-installation {:url "/api/product-installations/:id"
                          :audit :schema
                          :methods {:post {:effects writes :requires {:op :update}}}
                          :service :oms}
   :product-installation-upgrade {:url "/api/product-installations/:id/upgrade"
                                  :audit :schema
                                  :methods {:post {:effects writes :requires {:op :update}}}
                                  :service :oms
                                  :branch? true}
   :product-installation-uninstall {:url "/api/product-installations/:id/uninstall"
                                    :audit :schema
                                    :methods {:post {:effects writes :requires {:op :update}}}
                                    :service :oms
                                    :branch? true}
   :project-rename {:url "/api/schema/project/:name/rename"
                    :methods {:post {:effects writes
                                     :requires {:op :manage-permissions, :on [:projects :name]}
                                     :request :a/project-rename.post.req
                                     :response :a/project-rename.post.res}}
                    :service :platform}
   :repository-rename {:url "/api/schema/repository/:name/rename"
                       :methods {:post {:effects writes
                                        :requires {:op :update, :on [:repositories :name]}}}
                       :service :platform}
   :schema {:url "/api/schema"
            :methods {:get {:effects reads :requires m-onto
                            :response :a/schema.get.res}}
            :service :oms}
   :schema-backing {:url "/api/schema/backing"
                    :methods {:get {:effects reads :requires m-onto
                                    :response :a/schema-backing.get.res}}
                    :service :oms
                    :branch? true}
   :schema-by-rid {:url "/api/schema/by-rid/:rid"
                   :methods {:get {:effects reads :requires m-onto}}
                   :service :oms}
   :schema-sections {:url "/api/schema/sections"
                     :methods {:get {:effects reads :requires m-onto
                                     :response :a/schema-sections.get.res}}
                     :service :oms
                     :query {:sections :str}}
   :schema-describe {:url "/api/schema/describe"
                     :methods {:get {:effects reads :requires m-onto
                                     :response :a/schema-describe.get.res}}
                     :service :oms
                     :branch? true
                     :query {:names :str, :project :str, :no-count :bool}}
   :schema-lint {:url "/api/schema/lint"
                 :methods {:get {:effects reads :requires m-onto
                                 :response :a/schema-lint.get.res}}
                 :service :oms
                 :branch? true}
   :schema-name-suggest {:url "/api/schema/name-suggest"
                         :methods {:get {:effects reads :requires m-onto
                                         :response :a/schema-name-suggest.get.res}}
                         :service :oms
                         :branch? true
                         :query {:name :trim :kind :kw :site :kw}}
   :schema-sql-check {:url "/api/schema/sql-check"
                      :methods {:post {:effects reads :requires {:op :update}
                                       :request :a/schema-sql-check.post.req
                                       :response :a/schema-sql-check.post.res}}
                      :service :oms
                      :branch? true}
   :schema-problems {:url "/api/schema/problems"
                     :methods {:get {:effects reads :requires m-onto
                                     :response :a/schema-problems.get.res}}
                     :service :oms
                     :branch? true}
   :schema-prune-orphans {:url "/api/schema/prune-orphans"
                          :methods {:post {:effects writes}}
                          :service :oms
                          :auth :admin
                          :reason "pruning orphan tables"
                          :branch? true
                          :query {:dry-run :bool}}
   :set-status {:url "/api/schema/:kind/:name/status"
                :methods {:post {:effects writes
                                 :requires {:op :update, :on [{:param :kind} :name]}}}
                :service :oms}
   :snapshot {:url "/api/snapshot"
              :methods {:get {:effects reads :requires m-counts
                              :response :a/snapshot.get.res}}
              :service :oms
              :query {:since :trim}}
   :transform-rows {:url "/api/transforms/:name/rows"
                    :methods {:get {:effects reads
                                    :requires {:op :read, :on [[:transforms :datasets] :name]}
                                    :response :a/transform-rows.get.res}}
                    :service :oms
                    :branch? true
                    :query {:as-of :str}}
   :transform-spec {:url "/api/transforms/:name/spec"
                    :methods {:get {:effects reads :requires {:op :read, :on [:transforms :name]}}}
                    :service :oms
                    :branch? true
                    :query {:as-of :str}}
   :trash {:url "/api/trash"
           :methods {:get {:effects reads :requires {:op :read}
                           :response :a/trash.get.res}}
           :service :platform
           :branch? true}
   :trash-restore {:url "/api/trash/restore"
                   :methods {:post {:effects writes :requires c-schema
                                    :request :a/trash-restore.post.req
                                    :response :a/trash-restore.post.res}}
                   :service :platform
                   :branch? true}})
