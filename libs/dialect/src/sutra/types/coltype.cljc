(ns sutra.types.coltype
  (:require [sutra.kit.anomaly :as anomaly]
            [clojure.string :as str]))

(def kinds
  {:boolean   :none
   :tinyint   :none
   :smallint  :none
   :int       :none
   :bigint    :none
   :float     :none
   :double    :none
   :decimal   :scale
   :varchar   :length
   :text      :none
   :date      :none
   :timestamp :precision
   :json      :none})

(def ^:private bare
  (into {} (map (fn [k] [k {:kind k}])) (keys kinds)))

(defn col-type
  ([kind] (get bare kind))
  ([kind params]
   (when-let [shape (get kinds kind)]
     (merge {:kind kind}
            (case shape
              :none      nil
              :length    (when-let [n (:length params)] {:length n})
              :precision (when-let [p (:precision params)] {:precision p})
              :scale     (when-let [p (:precision params)]
                           (cond-> {:precision p}
                             (:scale params) (assoc :scale (:scale params)))))))))

(def ^:private spellings
  {"boolean" :boolean, "bool" :boolean
   "tinyint" :tinyint
   "smallint" :smallint, "int2" :smallint
   "int" :int, "integer" :int, "int4" :int
   "bigint" :bigint, "int8" :bigint
   "float" :float, "real" :float, "float4" :float
   "double" :double, "double precision" :double, "float8" :double
   "decimal" :decimal, "numeric" :decimal, "number" :decimal
   "varchar" :varchar, "character varying" :varchar, "char varying" :varchar
   "nvarchar" :varchar, "char" :varchar, "nchar" :varchar, "bpchar" :varchar
   "text" :text, "longtext" :text, "mediumtext" :text, "clob" :text
   "date" :date
   "timestamp" :timestamp, "datetime" :timestamp
   "timestamp without time zone" :timestamp
   "timestamp with time zone" :timestamp
   "json" :json, "jsonb" :json})

(def ^:private parametric-re
  #"(?i)^\s*([a-z][a-z0-9 ]*?)\s*(?:\(\s*(\d+)\s*(?:,\s*(\d+)\s*)?\))?\s*$")

(defn parse
  [t]
  (when (some? t)
    (let [[_ name p s] (re-matches parametric-re (str t))]
      (when-let [kind (get spellings (some-> name str/lower-case
                                             (str/replace #"\s+" " ")))]
        (col-type kind {:length    (when (and p (#{:varchar} kind)) (parse-long p))
                        :precision (when p (parse-long p))
                        :scale     (when s (parse-long s))})))))

(defn- params-str [& xs]
  (let [xs (remove nil? xs)]
    (when (seq xs) (str "(" (str/join "," xs) ")"))))

(def ^:private render-tables
  {:postgres {:boolean "boolean" :tinyint "smallint" :smallint "smallint"
              :int "integer" :bigint "bigint" :float "real"
              :double "double precision" :decimal "numeric"
              :varchar "character varying" :text "text" :date "date"
              :timestamp "timestamp without time zone" :json "json"}
   :mssql    {:boolean "bit" :tinyint "tinyint" :smallint "smallint"
              :int "int" :bigint "bigint" :float "real"
              :double "float" :decimal "decimal(38,10)"
              :varchar "nvarchar(max)" :text "nvarchar(max)" :date "date"
              :timestamp "datetime2" :json "nvarchar(max)"}})

(def ^:private drops
  {:postgres #{:precision}
   :mssql    #{:precision :length :scale}})

(defn for-store
  [{:keys [kind] :as t}]
  (case kind
    (:text :json :varchar) (col-type :varchar)
    :timestamp             (col-type :timestamp)
    t))

(defn render
  [{:keys [kind precision scale length] :as t} dialect]
  (when-not (contains? kinds kind)
    (anomaly/raise! :fault (str "no column type to render: " (pr-str t))
                           {:col-type t :known (set (keys kinds))}))
  (let [table (or (get render-tables dialect)
                  (anomaly/raise! :fault (str "unknown SQL dialect " (pr-str dialect))
                                         {:dialect dialect :known (set (keys render-tables))}))
        base  (or (get table kind) (str/upper-case (name kind)))
        shape (get kinds kind)]
    (str base
         (when-not (contains? (get drops dialect) shape)
           (case shape
             :length    (params-str length)
             :precision (params-str precision)
             :scale     (params-str precision scale)
             nil)))))

(defn label
  [{:keys [kind precision scale length]}]
  (str (name kind)
       (case (get kinds kind)
         :length    (params-str length)
         :precision (params-str precision)
         :scale     (params-str precision scale)
         nil)))

(defn holds?
  [have want]
  (boolean
   (and have want
        (= (:kind have) (:kind want))
        (if (= :decimal (:kind have))
          (>= (or (:scale have) 0) (or (:scale want) 0))
          true))))

(defn holds-text?
  [have want]
  (holds? (parse have) (parse want)))
