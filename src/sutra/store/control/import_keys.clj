(ns sutra.store.control.import-keys
  (:require [sutra.store.control :as ctl]))

(set! *warn-on-reflection* true)

(def ^:private whole-type-read-at
  500)

(defn new-keys
  [held rows]
  (remove (fn [[_ pk uuid]] (= (get held pk) (str uuid))) rows))

(defn- held-keys
  [store branch obj-type]
  (into {} (map (juxt :pk-value :uuid))
        (ctl/list store :import-keys {:branch (or branch ctl/trunk) :object-type obj-type})))

(defn- worth-writing
  [store branch rows]
  (mapcat (fn [[obj-type of-type]]
            (cond->> of-type
              (<= whole-type-read-at (count of-type))
              (new-keys (held-keys store branch obj-type))))
          (group-by first rows)))

(defn write-import-keys! [store branch rows]
  (ctl/insert-many! store :import-keys
                    (map (fn [[obj-type pk-val uuid]]
                           {:branch branch :object-type obj-type :pk-value pk-val :uuid uuid})
                         (worth-writing store branch rows))
                    {:upsert? true}))

(defn import-key-counts [store branch]
  (mapv (fn [[t n]] {:object-type t :n n})
        (ctl/counts store :import-keys {:branch (or branch ctl/trunk)} :object-type)))

(defn write-index-times! [store branch object-types run-id]
  (ctl/insert-many! store :index-times
                    (map (fn [t]
                           {:branch branch :object-type t :indexed-at ctl/now
                            :run-id (some-> run-id str)})
                         object-types)
                    {:upsert? true}))

(defn index-times [store branch]
  (into {} (map (juxt :object-type :indexed-at))
        (ctl/list store :index-times {:branch (or branch ctl/trunk)})))
