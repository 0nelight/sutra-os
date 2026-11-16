(ns sutra.store.storage.pg-store
  (:require [clojure.string :as str]
            [next.jdbc :as jdbc]
            [sutra.types.naming :as naming]
            [sutra.store.control.collections :as collections]
            [sutra.store.control.tables :as cc]
            [sutra.catalog.system-datasets :as sysds]
            [sutra.store.pg.records :as pg-records]
            [sutra.store.pg.tables :as pg-tables]
            [sutra.types.world-rules :as rules]
            [sutra.store.pg.control-tables :as ct]
            [sutra.store.pg :as store-pg]
            [sutra.store.core :as core]
            [sutra.store.storage.heads :as heads]
            [sutra.store.storage.module-dbs :as module-dbs]
            [sutra.store.storage.pg :as pg]
            [sutra.ref.clock :as clock]))

(set! *warn-on-reflection* true)

(def ^:private specs (ct/specs cc/tables))

(def records
  (merge
   (into {}
         (for [{:keys [coll key-cols auto]} cc/all
               :let [spec (get specs (keyword (name coll)))]]
           [coll
            {:table (:table spec) :key-cols key-cols :auto auto :codec (:codec spec)
             :text (into #{}
                         (keep (fn [[a t]]
                                 (when (and (re-find #"(?i)^(VARCHAR|TEXT|CHAR)" (str t))
                                            (not (contains? (:codec spec) a)))
                                   a)))
                         (:cols spec))
             :generated (into #{}
                              (keep (fn [[a t]]
                                      (when (and (not= a auto)
                                                 (re-find #"GENERATED ALWAYS" (str t))) a)))
                              (:cols spec))}]))
   (into {} (for [{:keys [coll key-cols dataset]} cc/ledger]
              [coll
               {:table    (str "\"" (naming/dataset-table dataset) "\"") :key-cols key-cols
                :text     (into (set key-cols)
                                (keep (fn [{:keys [attribute-name attribute-type]}]
                                        (when (#{:string :text} attribute-type)
                                          (keyword (str/replace (name attribute-name) "_" "-")))))
                                (get-in sysds/system-datasets [dataset :columns]))}]))))

(defn store
  [pools opts]
  (doto (store-pg/store {:datasource pools :records records}
                        (merge (rules/store-opts clock/system) opts))
    core/adopt!))

(defn ledger-indexes
  [{:keys [coll dataset index order]}]
  (let [text   (get-in records [coll :text])
        col    (fn [a] (cond-> (str "\"" (str/replace (name a) "-" "_") "\"")
                         (contains? text a) (str " COLLATE \"C\"")))
        [oa od] (first order)
        tail   (when oa (str (col oa) (if (= :desc od) " DESC NULLS FIRST" " NULLS LAST")))
        table  (naming/dataset-table dataset)]
    (for [ix index
          :let [a (if (vector? ix) (first ix) ix)]]
      [(str table "_" (str/replace (name a) "-" "_") "_ix")
       (if (or (vector? ix) (nil? tail) (= a oa)) (col a) (str (col a) ", " tail))])))

(defn- holding-bootstrap-lock
  [ds f]
  (with-open [c (jdbc/get-connection ds)]
    (jdbc/execute-one! c ["SELECT pg_advisory_lock(hashtextextended('store_bootstrap', 0))"])
    (try (f)
         (finally
           (jdbc/execute-one!
            c ["SELECT pg_advisory_unlock(hashtextextended('store_bootstrap', 0))"])))))

(defn- bootstrap!
  [pools moved]
  (let [ds (pools nil)]
    (holding-bootstrap-lock
     ds
     #(do (ct/install! ds (remove (comp moved :name) cc/tables) cc/renamed-tables)
          (doseq [rid (sysds/control-resident-rids)]
            (pg-tables/ensure-kernel-table! ds rid
                                            (get-in sysds/system-datasets [rid :columns])))
          (pg-records/count-writes! ds (into {} (remove (comp moved keyword name key))
                                             records))))
    (future
      (doseq [l cc/ledger]
        (pg-tables/ensure-kernel-indexes! ds (:dataset l) (ledger-indexes l))))))

(defn- module-db
  [config owned m url]
  (let [colls  (module-dbs/colls-of owned m)
        names  (into #{} (map (comp keyword name)) colls)
        base   (pg/settings config)
        own    (update (get-in base [:lanes :control]) :pool-name str ":" m)
        pools  (pg/pools (assoc base :url url
                                :lanes {:control own (module-dbs/lane m) own}))
        ds     (pools nil)]
    (holding-bootstrap-lock
     ds #(do (ct/install! ds (filter (comp names :name) cc/tables) [])
             (pg-records/count-writes! ds (select-keys records colls))))
    (doto (store-pg/store {:datasource pools :records (select-keys records colls)}
                          (merge (rules/store-opts clock/system)
                                 {:collections (filter (comp colls :coll) collections/all)}))
      core/adopt!)))

(defn- open
  [config pools]
  (let [owned  (module-dbs/owners)
        routes (module-dbs/check! owned (module-dbs/routes config))
        moved  (into #{} (comp (mapcat #(module-dbs/colls-of owned %)) (map (comp keyword name)))
                     (keys routes))
        db     (atom nil)]
    (fn []
      (or @db
          (locking db
            (or @db
                (do (bootstrap! pools moved)
                    (reset! db (module-dbs/routed
                                (store pools (assoc heads/store-opts :collections collections/all))
                                owned
                                (into {} (for [[m url] routes]
                                           [m (module-db config owned m url)])))))))))))

(def module
  {:name :store-pg
   :requires [:pg/ds]
   :opens {:store/db (fn [config {pools :pg/ds}] (open config pools))}})
