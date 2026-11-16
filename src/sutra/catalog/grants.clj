(ns sutra.catalog.grants
  (:require [sutra.kit.refusal :as refusal]
            [clojure.set :as set]
            [clojure.string]
            [sutra.types.authority :as authority]
            [sutra.types.schema :as sch]))

(set! *warn-on-reflection* true)

(def all
  :all)

(defn- grant
  [auth]
  (or auth {}))

(defn- allowing
  [auth k]
  (let [v (get (grant auth) k)]
    (cond (= all v) all
          (some? v) v
          :else     (if (= :project-operations k) {} #{}))))

(defn read-rights
  [store] (:authority store))

(defn actor
  [store] (:authority store))

(defn request-authority
  [store] (:authority store))

(defn source-authority
  [store] (:authority store))

(defn auth-off?
  [store] (nil? (:authority store)))

(defn acting-user [auth] (:user (grant auth)))

(defn acting-username [auth] (:username (grant auth)))

(defn acting-for
  [store] (acting-user (actor store)))

(defn principal
  [store] {:principal (some-> (acting-for store) str)})

(defn clearances
  [auth] (allowing auth :clearances))

(defn denied-markings [auth] (or (:denied-markings (grant auth)) #{}))

(defn hidden-object-types [auth] (or (:hidden-object-types (grant auth)) #{}))

(defn blocked-resources
  [auth] (or (:blocked-resources (grant auth)) #{}))

(defn operations
  [auth] (allowing auth :operations))

(defn project-operations
  [auth] (allowing auth :project-operations))

(defn all-projects?
  [auth] (= all (project-operations auth)))

(defn owner-of
  [auth]
  (let [granted (project-operations auth)]
    (if (= all granted)
      (constantly true)
      (fn [project] (contains? (get granted project) :manage-permissions)))))

(def system-authority
  {:system? true :operations all :project-operations all :clearances all})

(defn stored
  [auth]
  (when auth
    (reduce (fn [a k] (if (some? (get a k)) a (assoc a k all)))
            auth [:operations :project-operations :clearances])))

(defn- meet-axis [a b inter]
  (cond (= all a) b
        (= all b) a
        :else     (inter a b)))

(defn meet
  [a b]
  (let [a (grant a) b (grant b)]
    (-> a
        (assoc :operations (meet-axis (operations a) (operations b) set/intersection)
               :clearances (meet-axis (clearances a) (clearances b) set/intersection)
               :project-operations
               (meet-axis (project-operations a) (project-operations b)
                          (fn [pa pb]
                            (into {} (for [[p ops] pa
                                           :let [both (set/intersection ops (get pb p #{}))]
                                           :when (seq both)]
                                       [p both]))))
               :denied-markings (set/union (denied-markings a) (denied-markings b))
               :hidden-object-types (set/union (hidden-object-types a) (hidden-object-types b))
               :blocked-resources (set/union (blocked-resources a) (blocked-resources b)))
        (cond-> (not (and (:system? a) (:system? b))) (dissoc :system?))
        (cond-> (or (:subject a) (:subject b)) (assoc :subject (or (:subject a) (:subject b)))))))

(defn cleared?
  [clearances markings]
  (or (= all clearances)
      (empty? markings)
      (every? (set clearances) (set markings))))

(def standard-roles
  {:ontology-finder {:operations #{:discover}}
   :ontology-viewer {:includes :ontology-finder
                     :operations #{:read}}
   :ontology-editor {:includes :ontology-viewer
                     :operations #{:create :update}}
   :ontology-owner  {:includes :ontology-editor
                     :operations #{:delete :apply-marking :remove-marking
                                   :manage-markings :manage-permissions}}
   :consumer        {:operations #{}}})

(defn roles
  [schema]
  (merge standard-roles (:roles schema)))

(defn role-operations
  ([role] (role-operations role standard-roles))
  ([role roles]
   (if-let [{:keys [includes operations]} (get roles role)]
     (into (set operations) (when includes (role-operations includes roles)))
     #{})))

(defn- policy-base
  [schema]
  (authority/schema-database (authority/schema-facts schema) (authority/role-facts (roles schema))))

(defn schema-policy
  [schema]
  (sch/column schema ::policy-base policy-base))

(defn access-delta
  [before after]
  (authority/access-delta (schema-policy (or before {})) (schema-policy (or after {}))))

(defn policy-database
  [schema subject]
  (authority/with-subject (sch/column schema ::policy-base policy-base)
    (authority/subject-facts subject)))

(defn held-operations
  [schema subject]
  (authority/granted-operations (policy-database schema subject)
                                (:who subject)
                                (set (keys (:projects schema)))))

(defn project-grants
  ([schema principal] (project-grants schema principal nil))
  ([schema principal memberships]
   (held-operations schema {:who principal :memberships memberships})))

(defn op->operation
  [op]
  (case op
    (:create :create-link)        :create
    (:set :update :compute-on)          :update
    (:delete-object :delete-link) :delete
    :apply-marking                      :apply-marking
    :remove-marking                     :remove-marking
    nil))

(defn effective-operations
  [auth project ignore-inherited?]
  (let [ops (operations auth)
        per (project-operations auth)]
    (cond (= all ops)                                 all
          (or (nil? project) ignore-inherited?)       ops
          (= all per)                                 all
          :else                                       (into ops (get per project)))))

(defn decide
  [auth {:keys [op project ignore-inherited resource]}]
  (let [eff (effective-operations auth project ignore-inherited)]
    (cond
      (and resource (contains? (blocked-resources auth) resource)) :marking
      (and op (not (or (= all eff) (contains? eff op))))           :operation)))

(defn allows?
  [auth project ignore-inherited? op]
  (nil? (decide auth {:op op :project project :ignore-inherited ignore-inherited?})))

(defn authorize!
  [auth {:keys [op project ignore-inherited resource] :as asked} what]
  (case (decide auth asked)
    nil        nil
    :marking   (refusal/refuse! :refusal.catalog/marking-not-cleared
                                {:sutra/acl-denied true :resource resource})
    :operation (refusal/refuse! :refusal.catalog/acl-denied
                                {:sutra/acl-denied true :type resource :operation op
                                 :operations (some-> (effective-operations auth project
                                                                           ignore-inherited)
                                                     vec)}
                                {:op-name (name op) :what what})))

(defn project-allows?
  [auth project op]
  (allows? auth project false op))

(defn read-allowed?
  [auth project ignore-inherited?]
  (allows? auth project ignore-inherited? :read))

(defn authorize-read!
  [auth spec type-rid]
  (authorize! auth {:op :read :project (:project spec) :ignore-inherited (:ignore-inherited spec)
                    :resource type-rid}
              (or (:object-type-name spec)
                  (when (clojure.string/blank? (str type-rid))
                    (str "no type given, from "
                         (->> (.getStackTrace (Thread/currentThread))
                              (map str)
                              (filter #(re-find #"^sutra\." %))
                              (remove #(re-find #"^sutra\.catalog\.grants" %))
                              (take 5)
                              (interpose " ← ")
                              (apply str))))
                  type-rid)))

(defn allows-read?
  [auth spec type-rid]
  (nil? (decide auth {:op :read :project (:project spec)
                      :ignore-inherited (:ignore-inherited spec) :resource type-rid})))

(defn current-user-id
  [authority]
  (or (some-> (:user authority) name) "local"))

(def admin-username "admin")

(defn username-or-admin
  [store]
  (or (acting-username (actor store))
      (when (and store (auth-off? store)) admin-username)))

(defn resource-granted?
  [schema granted spec]
  (boolean
   (when-let [p (:project spec)]
     (contains? granted (if (string? p) p (sch/rid-of (sch/index schema) :projects p))))))
