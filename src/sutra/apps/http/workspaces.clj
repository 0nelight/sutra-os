(ns sutra.apps.http.workspaces
  (:require [sutra.kit.refusal :as refusal]
            [sutra.types.versions :as vers]
            [clojure.string :as str]
            [sutra.objects.api :as objects]
            [sutra.capps.variables :as vars]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.parser.lint.support :as lint-support]
            [sutra.parser.project-emit :as pemit]
            [sutra.apps.http.standard-view :refer [app-row]]
            [sutra.http.endpoint :as ep]
            [sutra.authoring.http.oms :as oms]
            [sutra.http.responses :refer [ok negotiated]]
            [sutra.catalog.grants :as grants]
            [sutra.projects.api :as cp]
            [sutra.types.schema :as sch]
            [sutra.types.shape :as tshape]
            [sutra.apps.http.files :refer [named-entry]]
            [sutra.apps.http.scope :refer [app-scope editable-projects granted-app in-scope
                                                 resource-access rid-str workspace-rights]]
            [sutra.apps.http.session :refer [app-body]]
            [sutra.apps.http.view :refer [app-read scope-tables scoped-around
                                                with-view-reach]]))

(set! *warn-on-reflection* true)

(defn app-label
  [a]
  (or (not-empty (:display-name a))
      (let [t (get-in a [:layout :header :title])] (when (string? t) (not-empty t)))
      (some-> (:application-name a) name)))

(defn workspace-label
  [w]
  (or (not-empty (:display-name w)) (some-> (:workspace-name w) name)))

(defn- card-description
  [s]
  (when s
    (let [s (str s)
          n (dec lint-support/app-description-limit)]
      (if (> (count s) lint-support/app-description-limit)
        (str (str/trimr (subs s 0 n)) "…")
        s))))

(defn- workspaces-claiming
  [schema]
  (reduce (fn [m [wrid w]]
            (reduce (fn [m arid] (update m arid (fnil conj #{}) wrid))
                    m
                    (keep identity (concat (:apps w) (:anchored w) [(:landing w)]))))
          {} (:workspaces schema)))

(defn- app-card
  [rid a]
  {:id           rid
   :name         (:application-name a)
   :display-name (app-label a)
   :icon         (:icon a)})

(defn my-applications
  [{auth :sutra/auth schema :sutra/schema}]
  (let [claimed (workspaces-claiming schema)
        entries (for [[rid a] (:applications schema)
                      :let  [access (resource-access auth schema a)]
                      :when (and access
                                 (vers/published? a)
                                 (:promoted a)
                                 (not (:form-factor a)))]
                  (assoc (app-card rid a)
                         :owner       (:owner a)
                         :thumbnail   (:thumbnail a)
                         :tags        (vec (:tags a))
                         :collection  (:collection a)
                         :workspaces  (vec (sort (get claimed rid)))
                         :locked      (= :locked access)
                         :description (card-description (:description a))))
        unlisted (vec (for [[rid a] (:applications schema)
                            :let  [access (resource-access auth schema a)]
                            :when (and (= :open access)
                                       (vers/published? a)
                                       (not (:promoted a))
                                       (not (:form-factor a)))]
                        (app-card rid a)))
        by-coll (group-by :collection entries)
        label-key (fn [e] [(str/lower-case (str (:display-name e))) (str (:name e))])
        section (fn [crid apps]
                  (let [c (get-in schema [:collections crid])]
                    {:id    crid
                     :name  (or (:collection-name c) (some-> crid name))
                     :icon  (:icon c)
                     :order (:order c)
                     :apps  (vec (sort-by label-key apps))}))]
    (ok {:applications (vec (sort-by label-key entries))
         :unlisted     unlisted
         :collections  (vec (sort-by (juxt #(or (:order %) 1000) :name)
                                     (for [[crid apps] by-coll :when crid]
                                       (section crid apps))))})))

(defn- object-set-target
  [auth schema rid]
  (when-let [s (get-in schema [:object-sets rid])]
    (when (resource-access auth schema s)
      (let [root (get-in schema [:object-types (:root s)])]
        (when (and root (= :open (resource-access auth schema root)))
          {:id          rid
           :name        (:object-set-name s)
           :root        (:root s)
           :description (card-description (:description s))})))))

(defn- favorite-items
  [store auth schema]
  (vec (for [ref  (cp/favorites store (grants/current-user-id auth))
             :let [[sect nm] (str/split (str ref) #"/" 2)
                   kind (case sect
                          "applications" :app
                          "object-types" :object-type
                          "object-sets"  :object-set
                          nil)
                   [rid] (when (and kind (seq nm))
                           (named-entry schema (keyword sect) nm))]
             :when rid]
         (case kind
           :app         {:kind :app :app rid}
           :object-type {:kind :object-type :object-type rid}
           :object-set  {:kind :object-set :object-set rid}))))

(defn- project-object-set-items
  [schema w]
  (vec (for [[rid s] (sort-by (comp str :object-set-name val) (:object-sets schema))
             :when (or (nil? (:project w)) (= (:project w) (:project s)))]
         {:kind :object-set :object-set rid})))

(defn my-workspaces
  [{auth :sutra/auth schema :sutra/schema store :sutra/store}]
  (let [card   (fn [rid]
                 (when-let [a (get-in schema [:applications rid])]
                   (when (resource-access auth schema a)
                     (assoc (app-card rid a) :description (card-description (:description a))))))
        open?  (fn [rid] (some? (card rid)))
        module (fn [rid]
                 (let [a (get-in schema [:applications rid])]
                   (when (vers/published? a)
                     (when-let [c (card rid)]
                       (let [ins (vars/object-inputs (get-in a [:layout :vars]))]
                         (when (seq ins) (assoc c :inputs ins)))))))
        type?  (fn [rid]
                 (when-let [t (get-in schema [:object-types rid])]
                   (when (resource-access auth schema t)
                     {:id rid :name (:object-type-name t)})))
        item   (fn [nav-out? it]
                 (case (:kind it)
                   :app         (when-let [c (card (:app it))]
                                  (assoc it :target c))
                   (:object-type :object)
                   (when-let [t (type? (:object-type it))]
                     (assoc it :target t))
                   :object-set  (when-let [t (object-set-target auth schema (:object-set it))]
                                  (assoc it :target t))
                   :link        (when nav-out? it)
                   nil))
        favs   (delay (favorite-items store auth schema))
        filled (fn [w p]
                 (case (:widget p)
                   :favorites   @favs
                   :object-sets (project-object-set-items schema w)
                   (:items p)))
        panels (fn [w]
                 (let [nav-out? (not (false? (:nav-out w)))]
                   (vec (for [p (:home w)
                              :let [its (vec (keep (partial item nav-out?) (filled w p)))]
                              :when (or (seq its) (:widget p))]
                          (assoc p :items its)))))
        visible (for [[rid w] (:workspaces schema)
                      :when (resource-access auth schema w)
                      :let  [apps (vec (keep card (:apps w)))]]
                  {:id       rid
                   :name     (:workspace-name w)
                   :display-name (workspace-label w)
                   :icon     (:icon w)
                   :subtitle (:subtitle w)
                   :logo     (:logo w)
                   :logo-size (:logo-size w)
                   :description (card-description (:description w))
                   :order    (:order w)
                   :promoted (boolean (:promoted w))
                   :rights   (workspace-rights auth schema w)
                   :nav-out  (not (false? (:nav-out w)))
                   :landing  (when (open? (:landing w)) (:landing w))
                   :landing-params (when (open? (:landing w)) (:landing-params w))
                   :apps     apps
                   :anchored (vec (keep card (:anchored w)))
                   :discoverable (vec (keep module (:discoverable w)))
                   :home     (panels w)})]
    (ok {:workspaces (vec (sort-by (juxt #(or (:order %) 1000) :name) visible))})))

(defn workspace-editable
  [{auth :sutra/auth schema :sutra/schema}]
  (let [rows   (for [[rid w] (:workspaces schema)
                     :let  [rights (workspace-rights auth schema w)]
                     :when (:edit rights)]
                 {:id       rid
                  :name     (:workspace-name w)
                  :icon     (:icon w)
                  :subtitle (:subtitle w)
                  :promoted (boolean (:promoted w))
                  :project  (:project w)
                  :rights   rights})]
    (ok {:workspaces (vec (sort-by :name rows))
         :projects   (editable-projects auth schema)
         :can-create (boolean (seq (editable-projects auth schema)))})))

(def workspace-file
  (ep/handler :workspace-file
              {:respond (fn [{:keys [auth schema name]}]
                          (if-let [[rid spec] (named-entry schema :workspaces name)]
                            (let [named (-> (pemit/names-resolved
                                             (sch/rid-name-index schema) :workspaces {rid spec})
                                            (get rid))
                                  rights (workspace-rights auth schema spec)]
                              (if (:view rights)
                                (ok {:name name
                                     :file (tshape/->author :workspaces named {})
                                     :rights rights
                                     :projects (editable-projects auth schema)})
                                (refusal/refused :refusal.apps/no-access-to-this-workspace {})))
                            (ok {:name name :file nil
                                 :rights {:view true
                                          :edit (boolean (seq (editable-projects auth schema)))
                                          :promote
                                          (:promote (workspace-rights auth schema {}))}
                                 :projects (editable-projects auth schema)})))}))

(defn save-workspace-file
  [{{:keys [name]} :path-params :keys [body-params] auth :sutra/auth schema :sutra/schema
    :as req}]
  (let [file    (:file body-params)
        [_ live] (named-entry schema :workspaces name)
        rights  (workspace-rights auth schema (or live (select-keys file [:project])))
        nm      (keyword name)]
    (cond
      (not (map? file))
      (refusal/refused :refusal.apps/save-needs-a-file-map-2 body-params)

      (not (:edit rights))
      (refusal/refused :refusal.apps/no-edit-access-to-workspace {} {:name name})

      (and (not= (boolean (:promoted file)) (boolean (:promoted live)))
           (not (:promote rights)))
      (refusal/refused :refusal.apps/promoting-a-workspace-is-a {} {:name name})

      :else
      (oms/install-preset
       (assoc req :body-params
              {:schema {:workspaces {nm (assoc file :name (clojure.core/name nm))}}})))))

(defn- workspace-app-rids
  [w]
  (into #{} (remove nil?)
        (concat (:apps w) (:anchored w) [(:landing w)]
                (for [p (:home w) it (:items p) :when (= :app (:kind it))] (:app it)))))

(defn- workspace-scope
  [auth schema w]
  (let [apps  (keep #(granted-app auth schema %) (workspace-app-rids w))
        items (for [p (:home w)
                    it (if (= :object-sets (:widget p))
                         (project-object-set-items schema w)
                         (:items p))
                    :let [t (case (:kind it)
                              (:object-type :object) (:object-type it)
                              :object-set (:root (object-set-target auth schema
                                                                    (:object-set it)))
                              nil)]
                    :when (and t
                               (= :open (some->> (get-in schema [:object-types t])
                                                 (resource-access auth schema))))]
                (rid-str t))]
    {:type        (into (set items) (mapcat (comp :type #(app-scope schema %))) apps)
     :object-type (into (set items) (mapcat (comp :object-type #(app-scope schema %))) apps)}))

(defn- with-workspace
  [req f]
  (let [store     (:sutra/store req)
        auth   (:sutra/auth req)
        rid    (get-in req [:path-params :rid])
        schema (:sutra/schema req)
        w      (some (fn [[k w]] (when (= rid (rid-str k)) w)) (:workspaces schema))]
    (if (and w (resource-access auth schema w))
      (f {:rid rid :schema schema :branch (:sutra/branch req) :w w
          :scope (workspace-scope auth schema w)
          :body (app-body req) :store store :auth auth})
      (negotiated req 404 {:error "workspace not found"}))))

(defn my-workspace
  [req]
  (with-workspace req
    (fn [{:keys [rid schema scope w auth]}]
      (negotiated req
                  {:workspace
                   (-> (scope-tables auth schema
                                     (with-view-reach auth schema scope))
                       (update :standard-views update-vals #(assoc % :actions []))
                       (assoc :id       rid
                              :name     (:workspace-name w)
                              :display-name (workspace-label w)
                              :api-root (str "/api/my-workspace/" rid)
                              :viewer   {:role (get-in req [:principal :role])}))}))))

(defn my-workspace-around
  [req]
  (with-workspace req
    #(scoped-around req % "workspace")))

(defn my-workspace-search
  [req]
  (with-workspace req
    (fn [{:keys [schema branch scope store auth]}]
      (let [qp     (:query-params req)
            q      (some-> (get qp "q") str str/trim not-empty)
            t      (some-> (get qp "type") str str/trim not-empty)
            oset   (some-> (get qp "set") str str/trim not-empty)
            limit  (-> (or (some-> (get qp "limit") str parse-long) 50) (max 1) (min 200))
            offset (-> (or (some-> (get qp "offset") str parse-long) 0) (max 0))
            hidden (grants/hidden-object-types auth)
            only   (when t (in-scope auth schema scope :object-type t))
            types  (if t
                     (if only [only] [])
                     (sort (remove hidden (:object-type scope))))]
        (cond
          (and (nil? q) (nil? t))
          (negotiated req 400 {:error "q or type is required"})

          (and t (nil? only))
          (negotiated req 404 {:error (str "no object type " (pr-str t) " in this workspace")})

          :else
          (app-read store branch nil
                    (fn [store sch]
                      (let [groups (for [rid types
                                         :let [ot   (get-in sch [:object-types rid])
                                               nm   (:object-type-name ot)
                                               read (objects/run-object-query
                                                     store sch
                                                     (cond-> {:find nm :limit limit :offset offset
                                                              :count? true}
                                                       (and t oset) (assoc :object-set
                                                                           (keyword oset))
                                                       q (assoc :keywords q)
                                                       (:title-key ot)
                                                       (assoc :order-by (->kw (:title-key ot)))))]
                                         :when (or t (pos? (:count read)))]
                                     {:type       (name nm)
                                      :rows       (mapv #(app-row rid %) (:rows read))
                                      :count      (:count read)
                                      :offset     offset
                                      :prominent?
                                      (= :prominent (some-> (:visibility ot) name keyword))})]
                        (negotiated
                         req
                         {:groups (mapv #(dissoc % :prominent?)
                                        (sort-by (juxt (comp not :prominent?) :count)
                                                 groups))})))))))))
