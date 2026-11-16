(ns sutra-ui.state.data
  (:require [sutra-ui.state.db :as db]
            [sutra-ui.state.requests :as rq]
            [sutra.kit.remote :as rd]
            [sutra-ui.api :as api]
            [sutra-ui.state.shell :as shell]))

(defonce
  datasources (db/cursor [:data :datasources] rd/loading))

(defn datasource-map [] (rd/value-or @datasources {}))

(defn datasources-here? [] (rd/value? @datasources))

(defn datasources-view
  []
  (rd/list-view (rd/fmap #(sort-by (comp name key) %) @datasources)))

(defonce sources        (db/cursor [:data :sources] {}))

(defonce import-plan    (db/cursor [:data :import-plan] {:objects {} :links {}}))

(defonce
  import-status (db/cursor [:data :import-status] rd/loading))

(defonce
  object-counts (db/cursor [:data :object-counts] rd/loading))

(defonce
  runs (db/cursor [:data :runs] rd/loading))

(defn run-list
  []
  (rd/value-or @runs []))

(defonce runs-kind-filter  (db/cursor [:data :runs-kind-filter] nil))

(defonce ds-introspection (db/cursor [:data :ds-introspection] {:tables {} :columns {}}))

(defn source-health
  [src-kw]
  (get-in @sources [src-kw :health]))

(defn source-unhealthy?
  [src-kw]
  (when-let [status (:status (source-health src-kw))]
    (not= "healthy" status)))

(defn record-source-health!
  [src-kw health]
  (when (and src-kw health)
    (db/edit! sources update src-kw
              (fn [s] (assoc (or s {:source-name src-kw}) :health health)))))

(defonce runs-window-hours (db/cursor [:data :runs-window-hours] nil))

(defonce
  audit-entries (db/cursor [:data :audit-entries] rd/loading))

(defonce
  datasets (db/cursor [:data :datasets] rd/loading))

(defonce dataset-preview-data (db/cursor [:data :dataset-preview-data] nil))

(defonce schema-problems (db/cursor [:data :schema-problems] nil))

(def refresh-object-counts
  (db/load [:data :object-counts] [api/get! :object-counts]
           {:pick #(or (:by-type %) {}) :report "object-counts"}))

(defn refresh-import-status! []
  (db/load! [:data :import-status] [api/get! :import-status] {:report "import-status"}))

(defn refresh-object-counts! [] (db/send! (second refresh-object-counts)))

(defn- runs-query [d]
  {:kind  (:runs-kind-filter d)
   :hours (:runs-window-hours d)
   :limit (when (:runs-window-hours d) 2000)})

(db/reg :sutra.data/refresh-runs
        (fn [db [_ reply]]
          (let [q (runs-query (:data db))]
            {:fx [(db/load [:data :runs] [api/list-runs q]
                           {:pick (comp vec :runs) :dedup [:runs q] :report "runs"}
                           reply)]})))

(db/reg :sutra.data/runs-filter
        (fn [db [_ k v]]
          {:db (assoc-in db [:data k] v) :fx [[:dispatch [:sutra.data/refresh-runs]]]}))

(defn refresh-runs!
  []
  (db/promise! [:sutra.data/refresh-runs]))

(defn set-runs-window! [hours] (db/send! [:sutra.data/runs-filter :runs-window-hours hours]))

(defn set-runs-kind-filter! [k] (db/send! [:sutra.data/runs-filter :runs-kind-filter k]))

(defn refresh-audit! []
  (db/load! [:data :audit-entries] [api/get! :audit] {:pick (comp vec :audit) :report "audit"}))

(defn refresh-datasets!
  []
  (db/load! [:data :datasets] [api/get! :datasets]
            {:pick #(or (:datasets %) {}) :report "datasets"}))

(db/reg :sutra.data/preview
        (fn [db [_ id]]
          (rq/request (assoc-in db [:data :dataset-preview-data] {:id id :loading? true})
                      {:ask  [api/get! :dataset-preview {:id id}]
                       :path [:data :dataset-preview-data]
                       :else [[:sutra.data/failed :dataset-preview-data {:id id}]]})))

(db/reg :sutra.data/failed
        (fn [db [_ k m sentence]] {:db (assoc-in db [:data k] (assoc m :error sentence))}))

(defn load-dataset-preview! [id] (db/send! [:sutra.data/preview id]))

(db/reg :sutra.data/refresh-schema-problems
        (fn [db _]
          (rq/request db {:ask  [api/get! :schema-problems] :path [:data :schema-problems]
                          :else [[:sutra.data/failed :schema-problems {}]]})))

(defn refresh-schema-problems! [] (db/send! [:sutra.data/refresh-schema-problems]))

(defn- invalidate-introspection
  [introspection ds-key]
  (let [{:keys [tables columns]} introspection]
    (assoc introspection
           :tables  (dissoc tables ds-key)
           :columns (into {} (remove #(= ds-key (first (key %))) columns)))))

(db/reg :sutra.data/write-datasource
        (fn [db [_ c ds-key what reply]]
          (rq/request db {:ask c :then [[:sutra.data/datasource-written ds-key]] :report what}
                      reply)))

(db/reg :sutra.data/datasource-written
        (fn [db [_ ds-key resp]]
          {:db (update db :data #(-> %
                                     (assoc :datasources (or (:datasources resp) {}))
                                     (update :ds-introspection invalidate-introspection ds-key)))}))

(defn upsert-datasource!
  [body]
  (db/promise! [:sutra.data/write-datasource [api/post! :datasources body]
                (some-> body :name keyword) "upsert-datasource"]))

(defn delete-datasource!
  [nm]
  (db/promise! [:sutra.data/write-datasource [api/delete! :datasource {:name nm}]
                (keyword nm) "delete-datasource"]))

(defn create-datasource-table!
  [ds-key body]
  (shell/with-loading (api/post! :datasource-tables {:name ds-key} body)))

(db/reg :sutra.data/introspected
        (fn [db [_ path v]] {:db (assoc-in db (into [:data :ds-introspection] path) v)}))

(defn- introspect!
  [ask path label rows]
  (-> (ask)
      (.then (fn [resp]
               (let [v (vec (rows resp))]
                 (db/send! [:sutra.data/introspected path v])
                 v)))
      (.catch (fn [e]
                (when-not (:connection-error (:body (ex-data e)))
                  (shell/fail! label e))
                (throw e)))))

(defn fetch-ds-tables!
  [ds-key]
  (introspect! #(api/get! :datasource-tables {:name ds-key}) [:tables ds-key]
               (str "tables(" (clojure.core/name ds-key) ")") :tables))

(defn fetch-ds-columns!
  [ds-key table]
  (introspect! #(api/get! :datasource-columns {:name ds-key :table table})
               [:columns [ds-key table]]
               (str "columns(" (clojure.core/name ds-key) "." table ")") :columns))

(defn- by-source [sources] (into {} (map (juxt :source-name identity)) sources))

(defn adopt-snapshot
  [d snap]
  (assoc d
         :datasources (or (:datasources snap) {})
         :sources (by-source (:sources snap))
         :import-plan (or (:import-plan snap) {:objects {} :links {}})))

(defn snapshot-failed
  [d sentence]
  (cond-> d sentence (update :datasources rd/after-failure sentence)))

(db/reg :sutra.data/refresh-sources
        (fn [db _]
          (rq/request db {:ask  [api/get! :sources] :path [:data :sources]
                          :pick (comp by-source :sources) :report "sources"})))

(defn refresh-sources! [] (db/send! [:sutra.data/refresh-sources]))

(db/reg :sutra.data/refresh-import-plan
        (fn [db _]
          (rq/request db {:ask  [api/get! :import-plan] :path [:data :import-plan]
                          :pick #(or (:plan %) {:objects {} :links {}}) :report "import-plan"})))

(defn refresh-import-plan! [] (db/send! [:sutra.data/refresh-import-plan]))

(defn ensure-ds-tables!
  [ds-key]
  (when (and ds-key
             (not (get-in @ds-introspection [:tables ds-key]))
             (not (contains? (::tables-loading @ds-introspection) ds-key)))
    (db/edit! ds-introspection update ::tables-loading
              (fnil conj #{}) ds-key)
    (-> (fetch-ds-tables! ds-key)
        (.finally
         (fn []
           (db/edit! ds-introspection update ::tables-loading
                     (fnil disj #{}) ds-key))))))

(defn ds-columns-loading?
  [ds-key table]
  (contains? (::columns-loading @ds-introspection) [ds-key table]))

(defn ensure-ds-columns!
  [ds-key table]
  (when (and ds-key (seq table)
             (not (get-in @ds-introspection [:columns [ds-key table]]))
             (not (ds-columns-loading? ds-key table)))
    (db/edit! ds-introspection update ::columns-loading
              (fnil conj #{}) [ds-key table])
    (-> (fetch-ds-columns! ds-key table)
        (.finally
         (fn []
           (db/edit! ds-introspection update ::columns-loading
                     (fnil disj #{}) [ds-key table]))))))

(defn reload-ds-tables!
  [ds-key]
  (db/edit! ds-introspection update :tables dissoc ds-key)
  (fetch-ds-tables! ds-key))
