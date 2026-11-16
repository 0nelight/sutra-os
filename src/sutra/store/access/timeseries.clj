(ns sutra.store.access.timeseries
  (:require [sutra.kit.refusal :as refusal]
            [sutra.store.access.datasets :as dsets]
            [sutra.catalog.lookup :as lookup]
            [clojure.string :as str]
            [sutra.types.sql-ident :as sqli]
            [sutra.types.timeseries :as ts]))

(set! *warn-on-reflection* true)

(defn series-attribute
  [schema type-kw prop]
  (get (:by-name (lookup/attrs-index schema
                                     (or (lookup/rid-of schema :object-types type-kw) type-kw)))
       prop))

(defn- sync-rid
  [schema {:keys [sync property]}]
  (let [rid (lookup/rid-of schema :datasets sync)]
    (when-not rid
      (refusal/refuse! :refusal.store/series-sync-not-dataset {:property property :sync sync}))
    (when-let [problem (ts/explain-sync sync (get-in schema [:datasets rid :columns]))]
      (refusal/refuse! :refusal.store/series-unreadable (assoc problem :property property)))
    rid))

(defn compile-fetch
  ([norm table] (compile-fetch norm table sqli/default-dialect))
  ([{:keys [bucket series-ids from until] :as norm} table dialect]
  (let [{sid :sql} (:series ts/aliases)
        {tsx :sql} (:time ts/aliases)
        {val :sql} (:value ts/aliases)
        col       (fn [role] (str "s." (sqli/quote-ident (name (ts/column-of role)))))
        time-sql  (ts/time-expr dialect norm (col :time))
        value-sql (ts/value-expr norm (col :value))
        ids       (vec series-ids)
        wheres    (cond-> [(str (col :series) " IN ("
                                (str/join ", " (repeat (count ids) "?")) ")")]
                    from  (conj (str (col :time) " >= ?"))
                    until (conj (str (col :time) " < ?")))
        sql (str "SELECT " (col :series) " AS " (sqli/quote-ident sid)
                 ", " time-sql " AS " (sqli/quote-ident tsx)
                 ", " value-sql " AS " (sqli/quote-ident val)
                 " FROM " (sqli/quote-ident table) " s"
                 " WHERE " (str/join " AND " (map #(str "(" % ")") wheres))
                 (when bucket
                   (str " GROUP BY " (col :series) ", " time-sql))
                 " ORDER BY " (sqli/quote-ident sid) " ASC, " (sqli/quote-ident tsx) " ASC")]
    [sql (into ids (remove nil?) [from until])])))

(defn fetch
  [store schema type-kw req]
  (let [attr (series-attribute schema type-kw (:property req))]
    (when-not attr
      (refusal/refuse! :refusal.store/no-property {:property (:property req) :object-type type-kw}))
    (let [norm (ts/normalize-fetch attr req)
          ids  (:series-ids norm)
          rid  (sync-rid schema norm)]
      (ts/check-series-count! (count ids))
      (let [series (if (empty? ids)
                     []
                     (let [[sql params] (compile-fetch norm "series")]
                       (ts/->series (dsets/query store {"series" rid} sql params))))
            n (ts/point-count series)]
        (ts/check-point-count! n)
        (-> (select-keys norm [:property :unit :bucket :rollup :sync])
            (assoc :series series :points n))))))
