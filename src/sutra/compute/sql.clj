(ns sutra.compute.sql
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [next.jdbc :as jdbc]
            [next.jdbc.prepare :as prepare]
            [next.jdbc.result-set :as rs]
            [sutra.foundation.settings :as settings]
            [sutra.kit.refusal :as refusal]
            [sutra.kit.sql :as kit-sql]
            [sutra.kit.read-only-sql :as read-only]
            [sutra.types.coltype :as ctype]
            [sutra.types.languages :as langs]
            [sutra.types.sql-ident :as sqli])
  (:import [java.sql Connection DriverManager]
           [org.sqlite SQLiteConnection SQLiteLimits]))

(set! *warn-on-reflection* true)

(def ^:private double-quoted (partial kit-sql/quote-with "\"" "\""))

(def ^:private dialects
  {:sqlite   {:quoted double-quoted :stage-name identity :create "CREATE TABLE"
              :embedded? true :seal-sql "PRAGMA query_only = ON"}
   :postgres {:quoted double-quoted :stage-name identity :create "CREATE TEMP TABLE"
              :typed? true :url-env "SUTRA_SQL_RUNNER_POSTGRES_URL" :guarded? true
              :seal-sql "SET TRANSACTION READ ONLY"}
   :mssql    {:quoted (partial kit-sql/quote-with "[" "]") :stage-name #(str "#" %)
              :seal-sql nil
              :create "CREATE TABLE" :typed? true :url-env "SUTRA_SQL_RUNNER_MSSQL_URL"
              :guarded? true :aliased? true}})

(defn- value-kind [v]
  (cond (boolean? v)                 :boolean
        (integer? v)                 :bigint
        (decimal? v)                 :decimal
        (float? v)                   :double
        (instance? java.sql.Date v)  :date
        (inst? v)                    :timestamp
        :else                        :text))

(def ^:private numeric-kinds #{:bigint :decimal :double})

(defn- kind-of-values [vs]
  (let [kinds (into #{} (comp (remove nil?) (map value-kind)) vs)]
    (cond (empty? kinds)                   :text
          (= 1 (count kinds))              (first kinds)
          (every? numeric-kinds kinds)     :decimal
          :else                            :text)))

(defn- row-value [row column]
  (get row (str/lower-case column)))

(defn- typed-column [rows {:keys [name type] :as column}]
  (cond-> column
    (nil? type) (assoc :type (ctype/col-type (kind-of-values (map #(row-value % name) rows))))))

(defn- column-def [dialect {:keys [name type]}]
  (let [{:keys [quoted typed?]} (dialects dialect)]
    (str (quoted name) (when typed? (str " " (ctype/render type dialect))))))

(defn create-sql [dialect alias columns]
  (let [{:keys [quoted stage-name create]} (dialects dialect)]
    (str create " " (quoted (stage-name alias))
         " (" (str/join ", " (map #(column-def dialect %) columns)) ")")))

(defn insert-sql [dialect alias columns]
  (let [{:keys [quoted stage-name]} (dialects dialect)]
    (str "INSERT INTO " (quoted (stage-name alias))
         " (" (str/join ", " (map (comp quoted :name) columns)) ")"
         " VALUES (" (str/join ", " (repeat (count columns) "?")) ")")))

(defn- stage-reference [dialect alias]
  (let [{:keys [quoted stage-name]} (dialects dialect)]
    (str (quoted alias) " AS (SELECT * FROM " (quoted (stage-name alias)) ")")))

(defn- under-aliases [dialect aliases query]
  (let [cte (str/join ", " (map #(stage-reference dialect %) aliases))]
    (if (re-find #"(?i)^\s*;?\s*with\b" query)
      (str/replace-first query #"(?i)^\s*;?\s*with\b" (str "WITH " cte ","))
      (str "WITH " cte " " query))))

(defn query-sql [dialect aliases query]
  (let [{:keys [quoted aliased?]} (dialects dialect)
        rendered (sqli/render-marks query quoted)]
    (cond->> rendered aliased? (under-aliases dialect aliases))))

(defn stage-value [dialect {:keys [type]} v]
  (let [untyped? (not (:typed? (dialects dialect)))]
    (cond (nil? v)                     nil
          (keyword? v)                 (name v)
          (coll? v)                    (json/write-str v)
          (and untyped? (boolean? v))  (if v 1 0)
          (and untyped? (decimal? v))  (double v)
          (and untyped? (inst? v))     (str (.toInstant ^java.util.Date v))
          (and (#{:text :varchar :json} (:kind type)) (not (string? v))) (str v)
          :else                        v)))

(defn- stage-input! [^Connection c dialect alias {:keys [columns rows]}]
  (let [rows    (map #(update-keys % (comp str/lower-case name)) rows)
        columns (mapv #(typed-column rows %) columns)]
    (with-open [s (.createStatement c)]
      (.execute s (create-sql dialect alias columns)))
    (when (seq rows)
      (with-open [ps (jdbc/prepare c [(insert-sql dialect alias columns)])]
        (prepare/execute-batch!
         ps
         (mapv (fn [row]
                 (mapv #(stage-value dialect % (row-value row (:name %))) columns))
               rows))))))

(defn- runner-url [dialect]
  (some-> (:url-env (dialects dialect)) System/getenv))

(defn runnable?
  [language]
  (let [dialect (langs/dialect language)]
    (or (:embedded? (dialects dialect)) (some? (runner-url dialect)))))

(defn- open-sqlite ^Connection []
  (let [c (DriverManager/getConnection "jdbc:sqlite::memory:")]
    (.setLimit ^SQLiteConnection c SQLiteLimits/SQLITE_LIMIT_ATTACHED 0)
    c))

(defn- open-connection ^Connection [language dialect]
  (if (:embedded? (dialects dialect))
    (open-sqlite)
    (let [url (or (runner-url dialect)
                  (refusal/refuse! :refusal.foreign/no-runner {:language language}))]
      (doto (DriverManager/getConnection url) (.setAutoCommit false)))))

(defn- seal! [^Connection c dialect]
  (when-let [sql (:seal-sql (dialects dialect))]
    (jdbc/execute! c [sql])))

(defn- plain-value [v]
  (condp instance? v
    java.sql.Array (mapv plain-value (.getArray ^java.sql.Array v))
    Byte           (int v)
    Short          (int v)
    v))

(defn- execute-query [^Connection c sql seconds]
  (mapv #(update-vals % plain-value)
        (jdbc/execute! c [sql] {:builder-fn rs/as-unqualified-lower-maps :timeout seconds})))

(defn- under-deadline [^Connection c dialect seconds f]
  (if (:embedded? (dialects dialect))
    (let [guard (future (Thread/sleep (* 1000 (long seconds)))
                        (.interrupt (.getDatabase ^SQLiteConnection c)))]
      (try (f) (finally (future-cancel guard))))
    (f)))

(defn run-query!
  [language {:keys [query inputs]}]
  (let [dialect (langs/dialect language)
        seconds (settings/value :source-query-seconds)]
    (when (:guarded? (dialects dialect)) (read-only/assert-read-only-sql! query dialect))
    (with-open [c (open-connection language dialect)]
      (try
        (doseq [[alias input] inputs] (stage-input! c dialect alias input))
        (when-not (.getAutoCommit c) (.commit c))
        (seal! c dialect)
        (under-deadline c dialect seconds
                        #(execute-query c (query-sql dialect (keys inputs) query) seconds))
        (finally
          (when-not (.getAutoCommit c) (.rollback c)))))))
