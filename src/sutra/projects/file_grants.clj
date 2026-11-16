(ns sutra.projects.file-grants
  (:require [sutra.catalog.grants :as grants]
            [sutra.store.control :as ctl]))

(set! *warn-on-reflection* true)

(def ^:private role-ops
  {:viewer #{:read}
   :editor #{:read :update}
   :owner  #{:read :update :delete :manage-permissions}})

(def roles (set (keys role-ops)))

(defn- principals-of
  [auth]
  (into #{(grants/current-user-id auth)} (map str) (get-in auth [:subject :groups])))

(defn- stronger [a b] (if (> (count (role-ops a)) (count (role-ops b))) a b))

(defn held
  [store auth]
  (reduce (fn [m {:keys [ref role]}]
            (let [role (keyword role)] (update m ref #(if % (stronger % role) role))))
          {}
          (ctl/list store :file-grants {:principal (principals-of auth)})))

(defn allows?
  [shares ref op]
  (contains? (role-ops (get shares ref)) op))

(defn may?
  [store auth ref op]
  (allows? (held store auth) ref op))

(defn grants-on [store ref] (ctl/list store :file-grants {:ref ref}))

(defn link-of [store ref] (first (ctl/list store :share-links {:ref ref})))

(defn link-by-token [store token] (ctl/get store :share-links {:token (str token)}))

(defn sharing-on?
  [store project]
  (boolean (:file-grants (ctl/get store :project-settings {:project (str project)}))))

(defn projects-sharing [store]
  (into #{} (comp (filter :file-grants) (map :project)) (ctl/list store :project-settings {})))

(defn- deleting [t where] (fn [store _] (ctl/deleting store t where)))

(defn- updating [t where sets] (fn [store at] (ctl/updating store t where sets at)))

(defn grant!
  [store {:keys [ref principal role via by project]}]
  (let [where {:ref ref :principal (str principal)}]
    (ctl/commit! store
                 [(fn [s at]
                    (ctl/swapping s :file-grants where
                                  (constantly (assoc where :role role :via (or via :direct)
                                                     :granted-by by :project project))
                                  at))])
    nil))

(defn revoke! [store ref principal]
  (ctl/commit! store [(deleting :file-grants {:ref ref :principal (str principal)})])
  nil)

(defn relink!
  [store ref link]
  (ctl/commit! store
               (cond-> [(deleting :share-links {:ref ref})]
                 link (conj (fn [s _]
                              {:records [(ctl/entry s :share-links
                                                    {:token (:token link) :ref ref
                                                     :role (:role link) :created-by (:by link)
                                                     :project (:project link)})]}))))
  nil)

(defn set-sharing!
  [store project on?]
  (let [p (str project)]
    (ctl/commit! store
                 (cond-> [(fn [s at]
                            (ctl/swapping s :project-settings {:project p}
                                          #(assoc % :project p :file-grants (boolean on?)) at))]
                   (not on?) (conj (deleting :file-grants {:project p})
                                   (deleting :share-links {:project p}))))
    nil))

(defn moved!
  [store ref project]
  (ctl/commit! store
               (if (and project (not (sharing-on? store project)))
                 [(deleting :file-grants {:ref ref}) (deleting :share-links {:ref ref})]
                 [(updating :file-grants {:ref ref} {:project project})
                  (updating :share-links {:ref ref} {:project project})]))
  nil)
