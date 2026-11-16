(ns sutra.store.access.datasets
  (:require [sutra.kit.attempt :as attempt]
            [sutra.kit.anomaly :as anomaly]
            [sutra.kit.refusal :as refusal]
            [sutra.store.access.row-shape :as row-shape]
            [sutra.catalog.grants :as grants]
            [sutra.store.control :as ctl]
            [sutra.store.control.tables :as cc]
            [sutra.catalog.system-datasets :as sysds]
            [sutra.types.naming :as naming]
            [clojure.string :as str]
            [sutra.foundation.encoding :as enc]
            [sutra.datasets.api :as datasets]
            [sutra.types.coltype :as ctype]
            [sutra.types.column :as column]
            [sutra.types.health-metric :as hm]
            [sutra.types.sql-ident :as sqli]
            [sutra.types.value-types :as vt]
            [sutra.ref.value :as rv]))

(set! *warn-on-reflection* true)

(defn- driver [store]
  (or (:datasets store)
      (anomaly/raise! :fault "the store value names no datasets driver (:datasets/db)"
                             {:capability :datasets/db})))

(defn- branch [store] (str (or (:branch store) "main")))

(def ^:private pk-key (keyword naming/pk-col))
(def ^:private markings-key (keyword naming/markings-col))

(defn- addr
  [store t]
  {:branch (branch store) :dataset (str t) :pk naming/pk-col})

(defn- ds-addr
  [store ds]
  (cond-> (assoc (addr store (naming/dataset-table ds)) :rid ds)
    (sysds/control-resident? ds)
    (assoc :control (cond-> {:model (sysds/branch-model ds)}
                      (seq (sysds/branch-key ds)) (assoc :key (vec (sysds/branch-key ds)))))))

(defn- refused? [e] (some? (:store/refusal (ex-data e))))

(defn snapshot-id
  [token]
  (when (some? token) (parse-long (str token))))

(defn addressable?
  [token]
  (some? (snapshot-id token)))

(defn current-version
  [store]
  (datasets/version (driver store) (addr store "*")))

(defn ensure-tables!
  [store decls]
  (when (seq decls)
    (datasets/layout! (driver store) (branch store) {:old {} :new decls})))

(defn ensure-datasets!
  [store decls]
  (ensure-tables! store (update-keys decls naming/dataset-table)))

(defn inspect
  [store decls]
  (datasets/inspect (driver store) (branch store) decls))

(defn drop-scratch!
  [store names]
  (doseq [n names] (datasets/drop-dataset! (driver store) (assoc (addr store n) :scratch? true)))
  (vec names))

(defn drop-datasets!
  [store names]
  (doseq [n names] (datasets/drop-dataset! (driver store) (addr store n)))
  (vec names))

(defn- landed
  [store ds {:keys [landing] :as answer}]
  (when-let [on-landing (and landing (:on-landing store))]
    (on-landing ds landing))
  (dissoc answer :landing))

(defn write-rows!
  ([store ds mode rows] (write-rows! store ds mode rows nil))
  ([store ds mode rows producer]
   (when (nil? (:authority store))
     (anomaly/raise! :fault (str "a write of dataset " ds " names its authority (the request's, or"
                                 " sutra.store.access.scope/as-system)")
                            {:dataset ds}))
   (landed store ds
           (datasets/write! (driver store) (ds-addr store ds)
                            (cond-> {:kind (if (#{:snapshot :replace} mode) :snapshot :update)
                                     :pk naming/pk-col
                                     :rows (mapv #(row-shape/->row naming/pk-col %) (vec rows))}
                              (= :replace mode) (assoc :whole-row? true)
                              producer (assoc :meta
                                              {:producer
                                               (if (keyword? producer) (name producer)
                                                   (str producer))}))))))

(defn write-system-rows!
  [store ds mode rows]
  (write-rows! (assoc store :authority grants/system-authority) ds mode rows))

(defn write-edges!
  ([store ds edges] (write-edges! store ds edges true))
  ([store ds edges full-refresh?]
   (landed store ds
           (datasets/write! (driver store) (ds-addr store ds)
                            {:kind (if full-refresh? :snapshot :update)
                             :pk naming/pk-col :fork? false
                             :rows (mapv (fn [{:keys [id from-id to-id]}]
                                           {naming/pk-col (str id)
                                            "from_id" (str from-id) "to_id" (str to-id)})
                                         (vec edges))}))))

(defn delete-rows!
  [store ds ids]
  (when (seq ids)
    (datasets/write! (driver store) (ds-addr store ds)
                     {:kind :delete :pk naming/pk-col :ids (vec ids)})))

(defn truncate!
  [store ds]
  (datasets/write! (driver store) (ds-addr store ds) {:kind :delete :pk naming/pk-col :all? true}))

(defn merge-key-id
  [t-kw merge-key]
  (fn [row] (enc/transform-row-id t-kw (mapv #(get row %) merge-key))))

(defn- reject-duplicate-keys!
  [ds rows]
  (let [duplicate-ids (into #{} (comp (filter #(> (val %) 1)) (map key))
                            (frequencies (map :id rows)))]
    (when (seq duplicate-ids)
      (refusal/refuse! :refusal.store/duplicate-primary-key
                       {:error :duplicate-primary-key
                        :dataset ds
                        :duplicates (count duplicate-ids)
                        :sample (:values (first (filter #(duplicate-ids (:id %)) rows)))}
                       {:rows (count rows)}))))

(defn land-rows!
  [store ds mode row-id rows & [producer]]
  (let [{tombs true live false} (group-by (comp boolean row-shape/tombstone?) rows)
        id    (or row-id (fn [_] (str (java.util.UUID/randomUUID))))
        dead  (when row-id (mapv row-id tombs))
        keyed (mapv (fn [row] {:id (id row) :values (row-shape/strip-deletion-key row)}) live)]
    (when row-id (reject-duplicate-keys! ds keyed))
    (write-rows! store ds mode keyed producer)
    (when row-id (delete-rows! store ds dead))
    {:written (count keyed) :deleted (count dead)}))

(defn write-scratch!
  [store nm header rows]
  (datasets/write! (driver store) (assoc (addr store nm) :scratch? true)
                   {:kind :snapshot :pk naming/pk-col
                    :rows (vec (map-indexed (fn [i row]
                                              (into {naming/pk-col (str i)}
                                                    (map vector header (map #(some-> % str) row))))
                                            rows))}))

(defn write-scratch-rows!
  [store nm columns rows]
  (datasets/write! (driver store) (assoc (addr store nm) :scratch? true)
                   {:kind :snapshot :pk naming/pk-col :columns (vec columns) :rows (vec rows)}))

(defn dataset-names
  [store prefix]
  (datasets/list-datasets (driver store) (branch store) {:prefix prefix}))

(defn sample
  [store nm limit]
  (let [rs   (mapv #(dissoc % pk-key markings-key)
                   (datasets/read (driver store) (addr store nm) {:limit limit}))
        cols (vec (sort (map name (distinct (mapcat keys rs)))))]
    {:columns cols :rows (mapv (fn [r] (mapv #(get r (keyword %)) cols)) rs)}))

(defn- kebab-col [c] (keyword (str/replace (name c) "_" "-")))

(declare ^:private read-driver)

(defn- ledger-rows
  [store ds]
  (when-let [{:keys [coll]} (some #(when (= (str ds) (:dataset %)) %) cc/ledger)]
    (mapv (fn [r] (update-keys r #(keyword (str/replace (name %) "-" "_"))))
          (ctl/list store (keyword (name coll)) {:branch (:branch store)}))))

(defn- read-at
  [store ds at]
  (or (when (nil? at) (ledger-rows store ds))
      (read-driver store ds at)))

(defn- read-driver
  [store ds at]
  (when (and (some? at) (nil? (snapshot-id at)))
    (refusal/refuse! :refusal.store/not-a-snapshot {:dataset ds :as-of at} {:snapshot at}))
  (datasets/read (driver store) (ds-addr store ds)
                 (cond-> {} (some? at) (assoc :at (snapshot-id at)))))

(defn rows
  ([store ds] (rows store ds nil))
  ([store ds at]
   (mapv (fn [r]
           {:id (get r pk-key) :values (update-keys (dissoc r pk-key markings-key) kebab-col)})
         (read-at store ds at))))

(defn marked-rows
  [store ds]
  (mapv (fn [r]
          {:id       (get r pk-key)
           :markings (get r markings-key)
           :values   (update-keys (dissoc r pk-key markings-key) kebab-col)})
        (read-at store ds nil)))

(defn past-rows
  [store ds]
  (let [d  (driver store)
        a  (ds-addr store ds)
        hz (or (try (datasets/horizon d a)
                    (catch clojure.lang.ExceptionInfo e (if (refused? e) nil (throw e)))) 0)
        vs (->> (datasets/history d a) (keep :version) (filter #(>= % hz)) distinct sort vec)]
    (into []
          (mapcat (fn [[lo hi]]
                    (try
                      (let [{:keys [removed changed replaced?]} (datasets/delta d a lo hi)
                            ids (distinct (concat removed changed))]
                        (cond replaced? (datasets/read d a {:at lo})
                              (seq ids) (datasets/read d a {:at lo :ids (vec ids)})))
                      (catch clojure.lang.ExceptionInfo e (if (refused? e) nil (throw e))))))
          (map vector vs (rest vs)))))

(defn preview-rows
  [store ds at columns]
  (let [public (into {} (map (fn [[declared k]] [k declared]))
                     (naming/column-aliases (map :attribute-name columns)))]
    (mapv (fn [r] {:id (get r pk-key)
                   :values (update-keys (dissoc r pk-key markings-key) (fn [k] (get public k k)))})
          (read-at store ds at))))

(defn table-count
  [store ds]
  (datasets/count (driver store) (ds-addr store ds)))

(defn table-pks
  [store ds]
  (mapv (comp str pk-key)
        (datasets/read (driver store) (ds-addr store ds) {:columns [naming/pk-col]})))

(defn rows-with
  [store ds cols]
  (mapv (fn [r] {:id (str (get r pk-key)) :values (dissoc r pk-key)})
        (datasets/read (driver store) (ds-addr store ds)
                       {:columns (into [naming/pk-col] (map name) cols)})))

(defn row-values
  ([store table-name array-keys] (row-values store table-name array-keys {}))
  ([store table-name array-keys opts]
   (row-shape/project-row-values array-keys
                                 (datasets/read (driver store) (addr store table-name) opts))))

(defn content-signature
  [store t]
  (some-> (first (datasets/history (driver store) (addr store t) {:limit 1})) :version str))

(defn transactions
  [store ds]
  (datasets/history (driver store) (ds-addr store ds) {:typed? true}))

(defn horizon [store ds] (datasets/horizon (driver store) (ds-addr store ds)))

(defn below-horizon?
  [store table-name low]
  (when-let [lo (snapshot-id low)]
    (< lo (datasets/horizon (driver store) (addr store table-name)))))

(defn retire-below! [store ds v] (datasets/retire-below! (driver store) (ds-addr store ds) v))

(defn expire!
  [store ds through]
  (let [{:keys [version rows] :as r} (datasets/expire! (driver store) (ds-addr store ds) through)]
    (when (pos? (long rows))
      (ctl/commit! store [[:insert :retention-expiries
                           [{:branch (branch store) :table-name (str (naming/dataset-table ds))
                             :version version :rows rows}]]]))
    r))

(defn sweep-past!
  [store ds grace-ms]
  (datasets/sweep! (driver store) (ds-addr store ds) {:grace-ms grace-ms}))

(defn removed-pks
  [store ds low]
  (when-let [lo (snapshot-id low)]
    (try (let [d (driver store) a (ds-addr store ds)]
           (when (seq (datasets/read d a {:at lo :limit 1}))
             (vec (:removed (datasets/delta d a lo (current-version store))))))
         (catch clojure.lang.ExceptionInfo e (when-not (refused? e) (throw e))))))

(defn- window!
  [table-name low high]
  (let [lo (snapshot-id low) hi (snapshot-id high)]
    (when (or (nil? lo) (nil? hi))
      (refusal/refuse! :refusal.store/delta-not-a-snapshot
                       {:table (str table-name) :low low :high high}
                       {:version (if (nil? lo) low high)}))
    [lo hi]))

(defn window-delta
  [store table-name low high]
  (let [[lo hi] (window! table-name low high)
        {:keys [added changed removed]}
        (datasets/delta (driver store) (addr store table-name) lo hi)]
    {:touched (vec (distinct (concat changed added)))
     :removed (vec removed)}))

(defn added-ids [store table-name low high] (:touched (window-delta store table-name low high)))

(defn empty-at?
  [store table-name low high]
  (let [[lo hi] (window! table-name low high)]
    (try (datasets/delta (driver store) (addr store table-name) lo hi)
         false
         (catch clojure.lang.ExceptionInfo e
           (if (= :absent-at-low (:reason (ex-data e))) true (throw e))))))

(defn only-added?
  [store table-name low high]
  (let [[lo hi] (window! table-name low high)
        {:keys [added changed removed replaced?]}
        (datasets/delta (driver store) (addr store table-name) lo hi)]
    (and (not replaced?) (empty? (remove added changed)) (empty? removed))))

(defn only-added-past-retention?
  [store table-name low high]
  (or (only-added? store table-name low high)
      (let [[lo hi] (window! table-name low high)
            cuts    (->> (ctl/list store :retention-expiries
                                   {:branch (branch store) :table-name (str table-name)})
                         (map (comp long :version))
                         (filter #(and (> % lo) (<= % hi)))
                         sort)]
        (and (some? (seq cuts))
             (every? (fn [[a b]] (or (>= a b) (only-added? store table-name a b)))
                     (map vector (cons lo cuts) (concat (map dec cuts) [hi])))))))

(defn retained-floor
  [store table-name]
  (datasets/horizon (driver store) (addr store table-name)))

(defn overwritten-ids
  [store ds ids]
  (filterv (into #{} (map str) (table-pks store ds)) (map str ids)))

(defn added-values
  [store table-name low high array-keys]
  (let [ids (added-ids store table-name low high)]
    (if (empty? ids)
      []
      (row-shape/project-row-values array-keys
                                    (datasets/read (driver store) (addr store table-name)
                                                   {:ids ids})))))

(defn query
  ([store inputs sql] (query store inputs sql []))
  ([store inputs sql params]
   (mapv #(update-keys % kebab-col)
         (datasets/query (driver store)
                         {:inputs (update-vals inputs (partial ds-addr store)) :sql sql
                          :params (vec params)}))))

(defn query-tables
  [store inputs sql {:keys [params explain?]}]
  (datasets/query (driver store)
                  (cond-> {:inputs (update-vals inputs (fn [{:keys [table ids columns] :as in}]
                                                         (if (contains? in :columns)
                                                           {:columns columns}
                                                           (cond-> (addr store table)
                                                             (contains? in :ids)
                                                             (assoc :ids (vec ids))))))
                           :sql sql :params (vec params)}
                    explain? (assoc :explain? true))))

(defn answers-in-place?
  [store dialect]
  (contains? (get-in (datasets/profile (driver store)) [:query :dialects])
             (keyword (name dialect))))

(defn- shape-columns
  [columns]
  (into [{:name naming/pk-col :type (ctype/col-type :varchar)}]
        (comp (remove #(= naming/pk-col (column/sql-name (:attribute-name %))))
              (map (fn [{:keys [attribute-name attribute-type]}]
                     {:name (column/sql-name attribute-name)
                      :type (ctype/for-store (vt/col-type-of attribute-type))})))
        columns))

(defn- table-input
  [store alias table ids rows?]
  (let [d       (driver store)
        a       (addr store table)
        columns (datasets/columns d a)]
    (when (empty? columns)
      (refusal/refuse! :refusal.store/query-input-missing
                       {:datasets/missing-input alias}
                       {:alias alias :table table}))
    {:columns (mapv (fn [[n t]] {:name n :type t}) columns)
     :rows    (if (or (not rows?) (and ids (empty? ids)))
                []
                (datasets/read d a (cond-> {} ids (assoc :ids (vec ids)))))}))

(defn runner-inputs
  [store inputs {:keys [rows?]}]
  (into {}
        (map (fn [[alias {:keys [table ids columns] :as in}]]
               [alias (if (contains? in :columns)
                        {:columns (shape-columns columns) :rows []}
                        (table-input store alias table ids rows?))]))
        inputs))

(defn high-water
  [store ds column]
  (try (some-> (first (query store {"t" ds}
                             (str "SELECT max(" (sqli/quote-ident (name column)) ") AS m FROM t")))
               :m)
       (catch Exception e (attempt/rethrow-fatal! e) nil)))

(defn- metric-of-rows
  [store rs col metric]
  (let [k (when col (kebab-col col))]
    (hm/of-rows metric rs (when k (map #(rv/value % k) rs)) col
                (ctl/instant store))))

(defn scalar-metric
  [store ds col metric]
  (if (sysds/control-resident? ds)
    (metric-of-rows store (rows store ds) col metric)
    (if-let [q (hm/sql metric "t" col)]
      (try (some-> (first (datasets/query (driver store) {:inputs {"t" (ds-addr store ds)} :sql q}))
                   vals
                   first)
           (catch clojure.lang.ExceptionInfo e (if (refused? e) nil (throw e))))
      (metric-of-rows store [] col metric))))

(defn scalar-query
  [store ds q landed]
  (when (sysds/control-resident? ds)
    (refusal/refuse! :refusal.store/control-resident-dataset {:dataset ds}))
  (some-> (first (datasets/query (driver store)
                                 {:inputs (into {(naming/dataset-table ds) (ds-addr store ds)}
                                                (map (fn [t]
                                                       [t (assoc (addr store t) :scratch? true)]))
                                                landed)
                                  :sql q}))
          vals first))

(defn rollback-to!
  [store ds token]
  (let [snap   (or (snapshot-id token)
                   (refusal/refuse! :refusal.store/not-a-snapshot
                                    {:dataset ds :token token}
                                    {:snapshot token}))
        before (datasets/count (driver store) (ds-addr store ds))]
    (datasets/restore! (driver store) (ds-addr store ds) snap)
    {:rows-before before :rows-after (datasets/count (driver store) (ds-addr store ds))}))
