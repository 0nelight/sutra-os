(ns sutra.apps.http.apps
  (:require [sutra.kit.refusal :as refusal]
            [sutra.kit.attempt :as attempt]
            [sutra.catalog.lookup :as lookup]
            [clojure.string :as str]
            [sutra.capps.layout :as lay]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.apps.http.delivery :refer [app-data-projection type-page
                                                    app-inputs layout-vars request-url
                                                    request-held request-more request-vars
                                                    shown-object-queries
                                                    url-input-vars]]
            [sutra.http.responses :refer [ok negotiated]]
            [sutra.catalog.grants :as grants]
            [sutra.store.world :as world]
            [sutra.types.schema :as sch]
            [sutra.objects.api :as objects]
            [sutra.apps.http.actions :refer [app-action-evaluated app-action-forms
                                                   app-action-intents app-action-params
                                                   app-action-set app-optioned-actions
                                                   my-application-action-params
                                                   my-application-submit]]
            [sutra.apps.http.files :refer [application-file list-applications
                                                 rename-application save-application-file]]
            [sutra.apps.http.query :refer [my-application-aggregate my-application-around
                                                 my-application-data-freshness
                                                 my-application-edit-history
                                                 my-application-function my-application-objects
                                                 my-application-series]]
            [sutra.apps.http.generate :refer [my-application-generate]]
            [sutra.apps.http.comments :refer [my-application-comments]]
            [sutra.apps.http.resources :refer [my-application-linked-resources
                                               my-application-resources]]
            [sutra.apps.http.scenarios :refer [my-application-scenario-actions
                                                     my-application-scenario-apply
                                                     my-application-scenario-create
                                                     my-application-scenario-drop
                                                     my-application-scenario-duplicate
                                                     my-application-scenario-rebuild
                                                     my-application-scenario-seal
                                                     my-application-scenarios]]
            [sutra.apps.http.scope :refer [app-scope rid-str]]
            [sutra.apps.http.session :refer [application-preview scenario-at scenario-schema
                                                   with-app]]
            [sutra.apps.http.transfer :refer [my-application-blob my-application-export
                                                    my-application-upload
                                                    my-application-upload-status]]
            [sutra.apps.http.view :refer [app-row-want app-with-properties
                                                around-answer my-application-facets
                                                my-application-search my-application-users
                                                object-view-standard
                                                prune-layout-visibility scope-tables
                                                standard-around? with-view-reach]]
            [sutra.apps.http.home-page :refer [home-page my-home set-default-workspace
                                               set-home-page]]
            [sutra.apps.http.workspaces :refer [app-label my-applications my-workspace
                                                      my-workspace-around my-workspace-search
                                                      my-workspaces save-workspace-file
                                                      workspace-editable workspace-file]]))

(set! *warn-on-reflection* true)

(defn- app-description
  [auth schema scope a]
  (sch/column
   schema [::description a auth]
   (fn [schema]
     (merge
      (scope-tables auth schema (with-view-reach auth schema scope))
      {:name             (:application-name a)
       :display-name     (app-label a)
       :icon             (:icon a)
       :description      (:description a)
       :object-type      (:object-type a)
       :layout           (prune-layout-visibility (:layout a) schema auth)
       :data-inputs      (app-inputs schema a)
       :live             (:live a)
       :action-params    (app-action-params auth schema a)
       :action-forms     (app-action-forms auth schema a)
       :action-intents   (app-action-intents auth schema a)
       :action-evaluated (app-action-evaluated auth schema a)
       :default-action   (some-> (first (app-action-set auth schema a)) :action ->kw)
       :action-optioned  (app-optioned-actions auth schema a)
       :source           (:source a)}))))

(defn- preloaded-around
  [ctx store schema want shown]
  (into []
        (keep (fn [{:keys [rid query form-factor]}]
                (let [rows (objects/objects-matching store schema rid (assoc query :limit 2)
                                                     {:want want})
                      row  (when (= 1 (count rows)) (first rows))
                      t    (some-> (or (:object-type-ref row) (:object-type row)) name)
                      spec {:type t :id (some-> (:id row) str)
                            :standard (standard-around? ctx t form-factor)}
                      [status answer] (when t (around-answer ctx spec "application"))]
                  (when (= 200 status) {:spec spec :answer answer}))))
        shown))

(defn my-application
  [req]
  (with-app req {:seg :query :preview? true}
    (fn [{:keys [rid schema branch a seg sb scope store auth version]}]
      (let [sent (request-vars req)
            from-url (when-not sent (url-input-vars schema a (request-url req)))
            vals (or sent from-url)
            parts (lay/answer-wants (get-in req [:query-params "data"]))
            data? (or (contains? parts :rows)
                      (not (lay/data-layout? (:layout a)))
                      (boolean (some (comp :default-agg val) (layout-vars a))))
            desc? (contains? parts :descriptor)
            with (app-with-properties a vals)
            want (app-row-want auth schema scope a)
            branch-store  (scenario-at store branch sb)
            bsch (scenario-schema store schema sb)
            ctx  {:schema schema :branch branch :seg seg :sb sb :scope scope :store store
                  :auth auth}
            around (when (and data? from-url)
                     (future (preloaded-around ctx branch-store bsch want
                                               (shown-object-queries schema a vals))))
            held (let [h (request-held req)]
                   (when (and h sent (= with (app-with-properties a h))) h))
            data (when data? (app-data-projection branch-store bsch a with want vals held
                                                  (request-more req)))]
        (negotiated req
                    (cond-> {:scenario seg}
                      desc?
                      (assoc :application
                             (assoc (app-description auth schema scope a)
                                    :id      rid
                                    :viewer  {:role (get-in req [:principal :role])}
                                    :version version))
                      data?
                      (merge {:objects (if-let [ot (:object-type a)]
                                         (type-page branch-store bsch ot want) [])
                              :data    data})
                      (and data? from-url)
                      (assoc :data-vars from-url
                             :around (attempt/or-answer [] @around))))))))

(def ^:private usage-window-days 30)

(defn object-type-usage
  [{{:keys [type]} :path-params store :sutra/store auth :sutra/auth schema :sutra/schema
    branch :sutra/branch}]
  (let [rid    (some->> type ->kw (lookup/->type-rid schema :object-types))
        spec   (get-in schema [:object-types rid])]
    (if-not spec
      (refusal/refused :refusal.apps/unknown-object-type {:type type})
      (do
        (grants/authorize-read! auth spec rid)
        (let [editors (world/edit-usage (world/at store (or branch "main")) rid usage-window-days)
              binds?  (fn [a] (contains? (:type (app-scope schema a)) rid))
              apps    (for [[arid a] (:applications schema)
                            :when (and (not (:form-factor a)) (binds? a))]
                        {:id arid :name (some-> (:application-name a) name)
                         :display-name (app-label a) :status (some-> (:status a) name)})
              views   (for [[vrid v] (:applications schema)
                            :when (and (:form-factor v)
                                       (= rid
                                          (lookup/->type-rid schema :object-types
                                                             (:object-type v))))]
                        {:id vrid :name (some-> (:application-name v) name)
                         :form-factor (some-> (:form-factor v) name)})
              wss     (for [[wrid w] (:workspaces schema)
                            :let [its (for [p (:home w) it (:items p)] it)]
                            :when (some #(and (= :object-type (:kind %))
                                              (= rid (rid-str (:object-type %))))
                                        its)]
                        {:id wrid :name (some-> (:workspace-name w) name)})]
          (ok {:object-type   (name (:object-type-name spec))
               :window-days   usage-window-days
               :writes        (reduce + 0 (map :writes editors))
               :editors       (mapv #(update % :writes long) editors)
               :reads-counted false
               :applications  (vec (sort-by (comp str/lower-case str :display-name) apps))
               :object-views  (vec (sort-by :name views))
               :workspaces    (vec (sort-by :name wss))}))))))

(def module
  {:name        :apps
   :contributes {:http/routes
                 {:application-file
                  {:get application-file :post save-application-file}
                  :application-preview               {:post application-preview}
                  :application-rename                {:post rename-application}
                  :applications                      {:get list-applications}
                  :my-application                    {:get my-application}
                  :my-application-action-params      {:post my-application-action-params}
                  :my-application-aggregate          {:post my-application-aggregate}
                  :my-application-objects            {:post my-application-objects}
                  :my-application-around             {:post my-application-around}
                  :my-application-search             {:post my-application-search}
                  :my-application-facets             {:post my-application-facets}
                  :my-application-users              {:post my-application-users}
                  :my-application-blob               {:get my-application-blob}
                  :my-application-data-freshness     {:post my-application-data-freshness}
                  :my-application-edit-history       {:post my-application-edit-history}
                  :my-application-export             {:post my-application-export}
                  :my-application-function           {:post my-application-function}
                  :my-application-scenario           {:delete my-application-scenario-drop}
                  :my-application-scenario-actions   {:get my-application-scenario-actions}
                  :my-application-scenario-apply     {:post my-application-scenario-apply}
                  :my-application-scenario-duplicate {:post my-application-scenario-duplicate}
                  :my-application-scenario-rebuild   {:post my-application-scenario-rebuild}
                  :my-application-scenario-seal      {:post my-application-scenario-seal}
                  :my-application-scenarios
                  {:get my-application-scenarios :post my-application-scenario-create}
                  :my-application-comments           {:post my-application-comments}
                  :my-application-generate           {:post my-application-generate}
                  :my-application-resources          {:post my-application-resources}
                  :my-application-linked-resources   {:post my-application-linked-resources}
                  :my-application-series             {:post my-application-series}
                  :my-application-submit             {:post my-application-submit}
                  :my-application-upload             {:post my-application-upload}
                  :my-application-upload-status      {:get my-application-upload-status}
                  :my-applications                   {:get my-applications}
                  :my-workspace                      {:get my-workspace}
                  :my-workspace-around               {:post my-workspace-around}
                  :my-workspace-search               {:get my-workspace-search}
                  :my-workspaces                     {:get my-workspaces}
                  :my-home                           {:get my-home}
                  :home-page                         {:get home-page :post set-home-page}
                  :default-workspace-set             {:post set-default-workspace}
                  :object-type-usage                 {:get object-type-usage}
                  :object-view-standard              {:post object-view-standard}
                  :workspace-editable                {:get workspace-editable}
                  :workspace-file
                  {:get workspace-file :post save-workspace-file}}}})
