(ns sutra.access.authority
  (:require [sutra.kit.attempt :as attempt]
            [clojure.set :as set]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [sutra.catalog.system-datasets :as sysds]
            [sutra.access.principals :as pr]
            [sutra.store.api :as api]
            [sutra.store.world :as world]
            [sutra.store.seat :as seat]
            [sutra.kit.datalog :as dl]
            [sutra.types.authority :as policy]
            [sutra.types.granular :as granular]
            [sutra.types.ctx :as tctx]))

(set! *warn-on-reflection* true)

(defn- marking-objects
  [s schema]
  (when-let [marking-t (lookup/rid-of schema :object-types :marking)]
    (api/read s grants/system-authority [:objects marking-t])))

(defn marking-names
  [s schema]
  (when-let [ms (marking-objects s schema)]
    (into {} (map (fn [m] [(:id m) (keyword (get-in m [:values :name]))])) ms)))

(defn classification
  [s schema]
  (into {} (for [{{:keys [name category rank]} :values} (marking-objects s schema)
                 :when (and name category (some? rank))]
             [(keyword name) {:category (str category) :rank (long rank)}])))

(defn all-markings
  [store branch]
  (world/with-snapshot (world/at store (or branch "main"))
    (fn [s] (set (vals (marking-names s (api/read s nil [:schema/world {:derived? true}])))))))

(defn usable-schema
  [store branch]
  (seat/branch-schema (world/at store branch)))

(def membership-links
  [:member-of :subgroup-of])

(defn memberships
  [s schema who]
  (when-let [ts (and who (seq (keep #(lookup/rid-of schema :link-types %) membership-links)))]
    (loop [edges #{} frontier #{(str who)} seen #{}]
      (if (empty? frontier)
        edges
        (let [es   (for [t ts
                         l (api/read s grants/system-authority [:links t {:from (vec frontier)}])]
                     [(str (:from-object-id l)) (str (:to-object-id l))])
              seen (into seen frontier)]
          (recur (into edges es) (into #{} (comp (map second) (remove seen)) es) seen))))))

(def live-branch
  tctx/default-branch)

(defn- user-values
  [s schema user-id]
  (when-let [user-t (and user-id (lookup/rid-of schema :object-types :user))]
    (:values (first (api/read s grants/system-authority [:objects user-t [user-id]])))))

(defn read-subject
  [s schema user-id]
  (let [id->name  (marking-names s schema)
        cleared-t (lookup/rid-of schema :link-types :cleared-for)
        values    (user-values s schema user-id)]
    {:clears      (when (and id->name cleared-t user-id)
                    (into #{} (keep #(id->name (:to-object-id %)))
                          (api/read s grants/system-authority
                                    [:links cleared-t {:from [user-id]}])))
     :memberships (memberships s schema user-id)
     :role        (some-> (:rolle values) str)
     :disabled?   (true? (:disabled values))
     :attributes  values}))

(defn policy-subject
  ([user-id facts] (policy-subject user-id facts nil))
  ([user-id facts extra]
   (merge (select-keys facts
                       [:memberships :organizations :clears :classification :disabled? :attributes])
          {:who user-id}
          extra)))

(defn read-authority
  [store branch user-id schema]
  (merge
   (let [live (usable-schema store live-branch)]
     (world/with-snapshot (world/at store live-branch)
       (fn [s] (read-subject s live user-id))))
   (world/with-snapshot (world/at store branch)
     (fn [s]
       (let [id->name (marking-names s schema)]
         {:registry       (when (and id->name (lookup/rid-of schema :link-types :cleared-for))
                            (set (vals id->name)))
          :classification (classification s schema)})))))

(def admin-context
  {:role "admin" :clears grants/all :denied #{} :ops grants/all :proj-ops grants/all
   :hidden #{}})

(defn- closed-context
  [store branch]
  {:role nil :clears #{} :denied (all-markings store branch)
   :ops #{} :proj-ops {} :hidden nil})

(defn context-for
  [store {:keys [user-id branch username]}]
  (let [b      (or branch tctx/default-branch)
        schema (usable-schema store b)]
    (tctx/request
     (merge
      {:user-id user-id :username username :branch b :schema schema}
      (if (= username grants/admin-username)
        admin-context
        (try
          (let [{:keys [registry clears role] :as subject} (read-authority store b user-id schema)
                rolekw  (some-> role keyword)
                roles   (grants/roles schema)
                db      (grants/policy-database schema (policy-subject user-id subject))
                blocked (policy/blocked db user-id)
                covered (into #{} (map '?m) (dl/query db [:covers (str user-id) '?m]))
                orgs    (into #{} (map '?o) (dl/query db [:inside (str user-id) '?o]))]
            {:role     role
             :clears   (or clears #{})
             :denied   (set/difference (into (or registry #{}) (keys (:markings schema))) covered)
             :ops      (if (and rolekw (contains? roles rolekw))
                         (grants/role-operations rolekw roles)
                         #{})
             :proj-ops (policy/granted-operations db user-id
                                                  (into (set (keys (:projects schema)))
                                                        (concat (keys (:spaces schema))
                                                                (keys (:organizations schema)))))
             :blocked  blocked
             :subject  (granular/subject
                        {:id            user-id
                         :username      username
                         :groups        (map second (:memberships subject))
                         :markings      covered
                         :organizations orgs
                         :attributes    (:attributes subject)})
             :hidden   (into (if (= role "consumer") (set (sysds/system-type-rids schema)) #{})
                             (filter #(contains? (:object-types schema) %))
                             blocked)})
          (catch Throwable t
            (attempt/rethrow-fatal! t)
            (println "[authority] ERROR deriving authority for user" user-id
                     "on branch" b "— failing closed:" (.getMessage t))
            (closed-context store b))))))))

(defn of-principal
  [store branch username]
  (when-let [p (pr/fetch store username)]
    (tctx/authority (context-for store
                                 {:user-id (:user-object-id p) :username username
                                  :branch branch}))))

(defn owns
  [store branch who]
  (cond
    (grants/auth-off? store)                      (constantly true)
    (= grants/admin-username (str who))              (constantly true)
    :else
    (grants/owner-of (tctx/authority (context-for store {:user-id (str who) :branch branch})))))

(defn approver
  [store branch who]
  (if (or (grants/auth-off? store) (= grants/admin-username (str who)))
    (constantly {:owner? true :editor? true :groups #{}})
    (let [ctx    (context-for store {:user-id (str who) :branch branch})
          auth   (tctx/authority ctx)
          own    (grants/owner-of auth)
          ops    (grants/project-operations auth)
          groups (set (get-in ctx [:subject :groups]))]
      (fn [project]
        {:owner?  (boolean (own project))
         :editor? (boolean (or (= grants/all ops) (contains? (get ops project) :update)))
         :groups  groups}))))

(defn granted?
  [auth schema a]
  (or (grants/all-projects? auth)
      (grants/resource-granted? schema (grants/project-operations auth) a)))

(defn granted-resources
  [auth schema section]
  (for [[rid a] (get schema section)
        :when (granted? auth schema a)]
    [rid a]))

(defn- user-ids
  [store]
  (into {} (keep (fn [{:keys [username user-object-id]}]
                   (when user-object-id [(str user-object-id) username])))
        (pr/all-rows store)))

(defn usernames-by-object-id
  [store]
  (user-ids store))

(defn username-of
  [store principal]
  (get (user-ids store) (str principal) (str principal)))

(defn- group-names*
  [s schema]
  (let [g-t (lookup/rid-of schema :object-types :group)]
    (into {} (for [g (when g-t (api/read s grants/system-authority [:objects g-t]))
                   :let [n (get-in g [:values :name])] :when n]
               [(str (:id g)) (str n)]))))

(defn- group-graph
  [s schema by-user-id]
  (let [read  #(api/read s grants/system-authority %)
        named (group-names* s schema)
        edges (fn [k from->name]
                (for [t (keep #(lookup/rid-of schema :link-types %) [k])
                      l (read [:links t {}])
                      :let [a (from->name (str (:from-object-id l)))
                            g (named (str (:to-object-id l)))]
                      :when (and a g)]
                  [g a]))]
    (as-> (zipmap (vals named) (repeat {})) gs
      (reduce (fn [gs [g sub]] (update-in gs [g :groups] (fnil conj #{}) sub))
              gs (edges :subgroup-of named))
      (reduce (fn [gs [g u]] (update-in gs [g :users] (fnil conj #{}) u))
              gs (edges :member-of by-user-id)))))

(defn group-directory
  [store]
  (let [main   (world/at store live-branch)
        schema (seat/branch-schema main)
        users  (user-ids store)]
    (world/with-snapshot main #(group-graph % schema users))))

(defn group-names
  [store]
  (let [main (world/at store live-branch)]
    (world/with-snapshot main #(group-names* % (seat/branch-schema main)))))

(defn user-emails
  [store usernames]
  (let [main   (world/at store live-branch)
        schema (seat/branch-schema main)
        user-t (lookup/rid-of schema :object-types :user)
        wanted (set (map str usernames))
        ids    (into {} (filter (comp wanted val)) (user-ids store))]
    (if (and user-t (seq ids))
      (world/with-snapshot
       main
       (fn [s]
         (into {} (keep (fn [o] (when-let [e (get-in o [:values :email])]
                                  [(ids (str (:id o))) (str e)])))
               (api/read s grants/system-authority [:objects user-t (vec (keys ids))]))))
      {})))
