(ns sutra.sources.writeback-check
  (:require [sutra.types.transform-io :as tio]
            [clojure.string :as str]
            [sutra.kit.refusal :as refusal]
            [sutra.types.finding :as finding]
            [sutra.sources.protocols :as api]))

(set! *warn-on-reflection* true)

(defn- ci-set [xs] (into #{} (keep #(some-> % str str/upper-case)) xs))

(defn- finding
  [severity table code params]
  (finding/finding code severity :sources table
                   {:params params :message (refusal/message code params)}))

(defn- column-findings
  [table have named]
  (for [c (distinct (remove nil? named))
        :when (not (have (str/upper-case c)))]
    (finding :error table :refusal.writeback-check/column-missing {:column c :table table})))

(defn- pk-attribute-findings
  [table {:keys [pk column-map object-type]} attributes]
  (let [fold   #(str/upper-case (str/replace (str %) "-" "_"))
        mapped (into {} (map (fn [[k v]] [(fold k) (str/upper-case v)])) column-map)]
    (when (and object-type (seq attributes)
               (not-any? #(= (str/upper-case pk) (get mapped (fold %) (fold %))) attributes))
      [(finding :error table :refusal.writeback-check/pk-no-attribute
                {:pk pk :object-type (name object-type)})])))

(defn- pk-findings
  [table pk pk-cols]
  (cond
    (nil? pk-cols) nil
    (empty? pk-cols)
    [(finding :warn table :refusal.writeback-check/no-primary-key {:table table :pk pk})]
    (not ((ci-set pk-cols) (str/upper-case pk)))
    [(finding :warn table :refusal.writeback-check/pk-not-primary-key
              {:table table :pk pk :primary-key (str/join "," pk-cols)})]))

(defn findings
  [{:keys [table allow columns pk-cols triggers syncs attributes]}]
  (let [have (ci-set (map :name columns))
        typ  (into {} (map (juxt (comp str/upper-case str :name) (comp str/lower-case str :type)))
                   columns)
        {:keys [pk writable-columns insertable-columns identity delete-guard column-map log
                column-values actor-columns]} allow
        writable (ci-set writable-columns)
        actors   (mapcat val actor-columns)]
    (vec
     (concat
      (column-findings table have (concat [pk identity] writable-columns insertable-columns
                                          delete-guard (vals column-map) actors))
      (pk-attribute-findings table allow attributes)
      (pk-findings table pk pk-cols)
      (for [{nm :name :keys [def disabled]} triggers
            :when (and (not disabled) (not (re-find #"(?i)SET\s+NOCOUNT\s+ON" (str def))))]
        (finding :warn table :refusal.writeback-check/trigger-row-count {:trigger nm}))
      (for [c writable-columns
            :let [t (typ (str/upper-case c))]
            :when (and t (re-find #"datetime|smalldatetime" t))]
        (finding :warn table :refusal.writeback-check/datetime-in-condition
                 {:column c :type t}))
      (for [{:keys [transform query]} syncs
            [_ col] (re-seq #"(?i)(?:ISNULL|COALESCE)\s*\(\s*(?:\[?\w+\]?\.)?\[?(\w+)\]?" query)
            :when (writable (str/upper-case col))]
        (finding :warn table :refusal.writeback-check/sync-masks-value
                 {:transform transform :column col}))
      (when (= :none log)
        [(finding :warn table :refusal.writeback-check/log-none {:table table})])
      (for [c ["OPEN_USER" "CURRENTUSER"]
            :when (and (have c)
                       (not ((ci-set (concat writable-columns insertable-columns
                                             (keys column-values) actors))
                             c)))]
        (finding :warn table :refusal.writeback-check/actor-column-unset {:column c}))))))

(defn- object-type-of
  [schema ot]
  (some (fn [[rid spec]]
          (when (= (name ot) (some-> (:object-type-name spec) name)) [rid spec]))
        (:object-types schema)))

(defn- sync-queries
  [schema ot]
  (let [rid   (first (object-type-of schema ot))
        names (set (keep identity [ot rid (name ot) (some-> rid str)]))
        hits? (fn [o] (contains? names (:object-type o)))]
    (for [[_ t] (:transforms schema)
          :let [outs (tio/outputs-of t)
                q    (or (get-in t [:source :query]) (:query t))]
          :when (and (string? q) (some hits? outs))]
      {:transform (name (:transform-name t)) :query q})))

(defn- attributes
  [schema ot]
  (map (comp name :attribute-name) (:attribute-list (second (object-type-of schema ot)))))

(def ^:private plain-table #"^[A-Za-z_][A-Za-z0-9_]*(\.[A-Za-z_][A-Za-z0-9_]*)?$")

(defn allowlisted
  [source-name spec table]
  (let [wb  (:writeback spec)
        key (some #(when (= (str/upper-case (name %)) (str/upper-case (str table))) %)
                  (keys wb))]
    (cond
      (nil? key)
      (let [allowed (vec (sort (map name (keys wb))))]
        (refusal/refuse! :refusal.writeback-check/table-not-allowlisted
                         {:source (name source-name) :table (str table) :allowed allowed}
                         {:allowed (str/join ", " allowed)}))
      (not (re-matches plain-table (name key)))
      (refusal/refuse! :refusal.writeback-check/table-not-plain {:table (name key)})
      :else [(name key) (get wb key)])))

(defn- catalog-rows
  [s sql]
  (try (api/query s sql 200)
       (catch InterruptedException e (throw e))
       (catch Exception e
         (binding [*out* *err*]
           (println "writeback check: catalog query refused, that check is left out:"
                    (ex-message e)))
         nil)))

(defn- mssql-catalog
  [s table]
  (let [bare (last (str/split table #"\."))]
    {:pk-cols  (some->> (str "SELECT kcu.COLUMN_NAME AS col FROM"
                             " INFORMATION_SCHEMA.TABLE_CONSTRAINTS tc"
                             " JOIN INFORMATION_SCHEMA.KEY_COLUMN_USAGE kcu"
                             " ON tc.CONSTRAINT_NAME = kcu.CONSTRAINT_NAME"
                             " AND tc.TABLE_NAME = kcu.TABLE_NAME"
                             " WHERE tc.CONSTRAINT_TYPE = 'PRIMARY KEY'"
                             " AND tc.TABLE_NAME = '" bare "'")
                        (catalog-rows s)
                        (mapv #(str (or (:col %) (get % "col")))))
     :triggers (mapv (fn [r] {:name (:name r) :def (:def r)
                              :disabled (contains? #{true 1 "1" "true"} (:disabled r))})
                     (catalog-rows
                      s (str "SELECT t.name AS name, OBJECT_DEFINITION(t.object_id) AS def,"
                             " t.is_disabled AS disabled FROM sys.triggers t"
                             " WHERE t.parent_id = OBJECT_ID('" table "')")))}))

(defn check
  [schema source-name spec s table]
  (let [[key allow]  (allowlisted source-name spec table)
        columns      (api/columns s key)
        mssql?       (= :mssql (keyword (:type spec)))
        {:keys [pk-cols triggers]} (when mssql? (mssql-catalog s key))
        ot           (:object-type allow)]
    {:source      (name source-name)
     :table       key
     :object-type ot
     :findings    (findings {:table key :allow allow :columns columns
                             :pk-cols pk-cols :triggers triggers
                             :syncs (when ot (sync-queries schema ot))
                             :attributes (when ot (attributes schema ot))})
     :checked     {:columns (count columns) :triggers (when mssql? (count triggers))
                   :primary-key pk-cols}}))
