(ns sutra.store.migrations.type-identity
  (:require [sutra.store.access.scope :as ctx]
            [sutra.store.control :as ctl]
            [sutra.store.seat :as seat]
            [sutra.store.storage.instance-config :as ic]
            [sutra.store.world :as world]))

(set! *warn-on-reflection* true)

(def ^:private done-key "migrations.type-identity")

(defn- move-type!
  [store branch table nm rid]
  (let [where {:branch branch :object-type nm}
        k     #(select-keys % [:pk-value])
        rows  (ctl/list store table where)
        have  (into #{} (map k) (ctl/list store table {:branch branch :object-type rid}))]
    (ctl/insert-many! store table
                      (map #(assoc % :object-type rid) (remove (comp have k) rows))
                      {:upsert? true})
    (ctl/delete-all! store table where)
    (count rows)))

(defn- rids-by-name
  [schema]
  (into {} (for [[rid ot] (:object-types schema) :when (:object-type-name ot)]
             [(name (:object-type-name ot)) rid])))

(defn- move-branch!
  [store branch]
  (let [store (ctx/at store branch)
        rids  (rids-by-name (seat/branch-schema store))]
    (reduce + 0
            (for [[table names] [[:index-times (map :object-type
                                                    (ctl/list store :index-times
                                                              {:branch branch}))]
                                 [:import-keys (map first
                                                    (ctl/counts store :import-keys
                                                                {:branch branch}
                                                                :object-type))]]
                  nm names
                  :let [rid (get rids nm)]
                  :when rid]
              (move-type! store branch table nm rid)))))

(defn move!
  [store]
  (when-not (ic/get-cfg store done-key)
    (let [moved (into {}
                      (map (fn [{b :name}] [b (move-branch! store b)]))
                      (world/branches store))]
      (ic/set-cfg! store done-key "true")
      moved)))
