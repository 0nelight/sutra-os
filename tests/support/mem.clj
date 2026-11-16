(ns support.mem
  (:require [sutra.store.pg.dataset-store :as dstore :refer
             [capabilities delete-pks! truncate! write-scope]]
            [sutra.store.control.collections :as collections]
            [sutra.store.mem :as store-mem]
            [sutra.store.world :as world]))

(defn store
  ([] (store #(java.util.Date.)))
  ([clock]
   (world/store (store-mem/store store-mem/empty-seed {:collections collections/all}) "main"
                {:clock clock})))

(defn- unsupported! [what caps]
  (throw (ex-info (str "this dataset store cannot " what)
                  {:missing what :capabilities caps})))

(defn- mem-tbl [state scope table]
  (get-in @state [:scopes (str scope) (str table)]))

(defn- mem-live [state scope table]
  (vals (:rows (mem-tbl state scope table))))

(defrecord MemDataset [state]
  dstore/DatasetTables
  (create-table! [_ scope table cols]
    (swap! state assoc-in [:scopes (str scope) (str table)]
           {:cols (into {} (map (juxt (comp str :name) :type)) cols)
            :rows {} :history [] :sig 0})
    nil)
  (drop-table! [_ scope table]
    (swap! state update-in [:scopes (str scope)] dissoc (str table)) nil)
  (add-column! [_ scope table col t]
    (swap! state assoc-in [:scopes (str scope) (str table) :cols (str col)] t) nil)
  (retype-column! [_ scope table col _from to]
    (swap! state assoc-in [:scopes (str scope) (str table) :cols (str col)] to) nil)
  (rename-column! [_ scope table from to]
    (swap! state update-in [:scopes (str scope) (str table) :cols]
           (fn [m] (-> m (assoc (str to) (get m (str from))) (dissoc (str from))))) nil)
  (drop-column! [_ scope table col]
    (swap! state update-in [:scopes (str scope) (str table) :cols] dissoc (str col)) nil)
  (non-null-count [_ scope table col]
    (count (filter #(some? (get % (str col))) (mem-live state scope table))))
  (ensure-versioned! [_ _scope _table] nil)
  (columns [_ scope table] (set (keys (:cols (mem-tbl state scope table)))))
  (column-types [_ scope table] (:cols (mem-tbl state scope table)))
  (table-exists? [_ scope table] (some? (mem-tbl state scope table)))
  (qualify [_ scope table] (str scope "." table))
  (tables [_ branch mode]
    (let [own (set (keys (get-in @state [:scopes (str branch)])))]
      (case mode
        :own own
        :visible (into own (keys (get-in @state [:scopes "main"]))))))

  dstore/DatasetRows
  (insert-rows! [_ scope table _cols rows]
    (let [v (:seq (swap! state update :seq inc))]
      (swap! state update-in [:scopes (str scope) (str table)]
             (fn [t] (-> t
                         (update :rows into (map (fn [r] [(get r "id" (str (random-uuid))) r]))
                                 rows)
                         (assoc :sig v))))
      (count rows)))
  (merge-rows! [_ scope table pk _cols rows {:keys [full-refresh?]}]
    (let [v    (:seq (swap! state update :seq inc))
          keep (set (map #(str (get % pk)) rows))]
      (swap! state update-in [:scopes (str scope) (str table)]
             (fn [t]
               (let [rows' (reduce (fn [m r] (update m (str (get r pk)) merge r))
                                   (:rows t) rows)]
                 (assoc t :sig v
                        :rows (if full-refresh?
                                (select-keys rows' keep)
                                rows')))))
      (count rows)))
  (rows-differ? [_ scope table pk _cols rows]
    (let [have (:rows (mem-tbl state scope table))]
      (boolean (some (fn [r]
                       (let [k (str (get r pk))]
                         (not= (select-keys (get have k) (keys r)) r)))
                     rows))))
  (delete-pks! [_ scope table _pk pks]
    (let [v (:seq (swap! state update :seq inc))]
      (swap! state update-in [:scopes (str scope) (str table)]
             (fn [t] (-> t (update :rows #(apply dissoc % (map str pks))) (assoc :sig v))))
      (count pks)))
  (retain-pks! [_ scope table _pk pks]
    (let [v    (:seq (swap! state update :seq inc))
          keep (set (map str pks))]
      (swap! state update-in [:scopes (str scope) (str table)]
             (fn [t] (-> t (update :rows select-keys keep) (assoc :sig v))))
      nil))
  (truncate! [_ scope table]
    (let [v (:seq (swap! state update :seq inc))]
      (swap! state update-in [:scopes (str scope) (str table)]
             (fn [t] (-> t (assoc :rows {}) (assoc :sig v))))
      nil))
  (count-rows [_ scope table] (count (mem-live state scope table)))
  (select-rows [_ scope table {:keys [as-of ids]}]
    (when as-of (unsupported! "read as-of" #{:time-travel}))
    (let [rows (mem-live state scope table)]
      (if ids
        (let [want (set (map str ids))]
          (vec (filter #(contains? want (str (get % "internal_uuid"))) rows)))
        (vec rows))))
  (select-pks [_ scope table _pk] (mapv str (keys (:rows (mem-tbl state scope table)))))

  dstore/DatasetVersions
  (snapshot [_] (:seq @state))
  (next-snapshot! [_] (:seq (swap! state update :seq inc)))
  (as-of [s _scope _table _snap] (unsupported! "read as-of" (capabilities s)))
  (table-delta [_ {:keys [schema table]}]
    {:changed (set (keys (:rows (mem-tbl state schema table)))) :removed #{}})
  (content-signature [_ scope table] (:sig (mem-tbl state scope table)))
  (had-rows-at? [_ scope table _snap] (boolean (seq (mem-live state scope table))))
  (ever-written? [_ scope table] (some? (mem-tbl state scope table)))
  (restore-to! [s _scope _table _snap] (unsupported! "roll back" (capabilities s)))

  dstore/DatasetBranches
  (read-scope [_ branch table]
    (if (get-in @state [:scopes (str branch) (str table)]) (str branch) "main"))
  (write-scope [_ branch] (str branch))
  (fork-table! [_ branch table]
    (let [b (str branch) t (str table)]
      (boolean
       (when (and (not= "main" b)
                  (not (get-in @state [:scopes b t]))
                  (get-in @state [:scopes "main" t]))
         (swap! state assoc-in [:scopes b t] (get-in @state [:scopes "main" t]))
         true))))
  (settle-read-view! [_ _branch _table] nil)
  (create-branch! [_ branch]
    (swap! state update :scopes update (str branch) #(or % {})) nil)
  (drop-branch! [_ branch] (swap! state update :scopes dissoc (str branch)) nil)
  (retract! [s branch table pk ids]
    (let [sc (write-scope s branch)]
      (when (mem-tbl state sc table) (delete-pks! s sc table pk ids))))
  (retract-all! [s branch table]
    (let [sc (write-scope s branch)]
      (when (mem-tbl state sc table) (truncate! s sc table))))
  (retract-inherited! [_ _branch _table] false)

  dstore/DatasetOps
  (capabilities [_] #{:snapshot-reserve :branches})
  (table-health [_ _scope _table] nil)
  (store-health [_] nil)

  dstore/StoreBackup
  (backup! [s _label] (unsupported! "back up" (capabilities s)))
  (backups [s] (unsupported! "list restore points of" (capabilities s)))
  (restore-store-to! [s _snap] (unsupported! "roll back" (capabilities s)))
  (restore-store-to! [s _snap _scopes] (unsupported! "roll back" (capabilities s))))

(defn mem-dataset
  ([] (mem-dataset {}))
  ([scopes] (->MemDataset (atom {:seq 0 :scopes (merge {"main" {}} scopes)}))))
