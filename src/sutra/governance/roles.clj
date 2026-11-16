(ns sutra.governance.roles
  (:require [sutra.store.control :as ctl]
            [clojure.string :as str]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.branch-policy :refer [actor-name owner-refusal owner-role]]))

(set! *warn-on-reflection* true)

(defn branch-roles
  [store branch]
  (mapv #(update % :granted-at str)
        (ctl/list store :branch-roles {:branch branch} {:order [[:granted-at :asc]]})))

(defn owners
  [store branch]
  (mapv :principal
        (ctl/list store :branch-roles {:branch branch :role owner-role} {:cols [:principal]})))

(defn assign-role!
  ([store branch principal] (assign-role! store branch principal owner-role))
  ([store branch principal role]
   (if (or (str/blank? (str branch)) (str/blank? (str principal)))
     {:error "assign-role! needs a branch and a principal"}
     (do (ctl/upsert! store :branch-roles
                      {:branch (str branch) :principal (str principal) :role (name role)
                       :granted-by (actor-name store) :granted-at ctl/now})
         {:branch (str branch) :principal (str principal) :role (name role)}))))

(defn revoke-role!
  [store branch principal]
  (let [where {:branch (str branch) :principal (str principal)}
        [r] (ctl/commit!
             store [(fn [store _t]
                   (let [os (ctl/list store :branch-roles {:branch (str branch) :role owner-role})]
                     (if (and (some #(= (str principal) (:principal %)) os)
                              (= 1 (count os)))
                       {:result {:error (str "'" principal "' is the last owner of " branch
                                             " — assign another owner before revoking this one")}}
                       (let [{:keys [records]} (ctl/deleting store :branch-roles where)]
                         {:records (into records
                                         (keep #(when (not= (str principal) (:principal %))
                                                  (ctl/expect store :branch-roles
                                                              {:branch (str branch)
                                                               :principal (:principal %)})))
                                         os)
                          :result  {:branch (str branch) :revoked (str principal)}}))))])]
    r))

(defn owner-block
  [store branch act admin?]
  (owner-refusal (owners store branch) (actor-name store) branch act
                 (or admin? (grants/auth-off? store))))
