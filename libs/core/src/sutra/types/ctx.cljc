(ns sutra.types.ctx)

(def default-branch "main")

(defn request
  [{:keys [user-id username branch schema role clears denied ops proj-ops hidden blocked
           subject]}]
  {:user-id  user-id
   :username username
   :branch   (or branch default-branch)
   :schema   schema
   :role     role
   :clears   clears
   :denied   denied
   :ops      ops
   :proj-ops proj-ops
   :hidden   hidden
   :blocked  blocked
   :subject  subject})

(defn branch
  [ctx]
  (or (:branch ctx) default-branch))

(defn authority
  [ctx]
  (cond-> {:user                (:user-id ctx)
           :username            (:username ctx)
           :clearances          (:clears ctx)
           :denied-markings     (:denied ctx)
           :hidden-object-types (or (:hidden ctx) #{})
           :blocked-resources   (or (:blocked ctx) #{})
           :operations          (:ops ctx)
           :project-operations  (:proj-ops ctx)}
    (:subject ctx) (assoc :subject (:subject ctx))))
