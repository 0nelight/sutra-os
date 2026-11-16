(ns sutra.types.health-metric
  (:require [sutra.kit.time :as time]
            [sutra.types.table :as tbl])
  (:import (java.time Instant)))

(set! *warn-on-reflection* true)

(defn- hours-between
  [^Instant t ^Instant now]
  (quot (- (.toEpochMilli now) (.toEpochMilli t)) 3600000))

(def kinds
  {:row-count
   {:rider [[:>= :min 1]]
    :sql  (fn [{:keys [table]}] (str "SELECT COUNT(*) AS n FROM " table))
    :rows (fn [{:keys [rows]}] (count rows))}

   :approximate-unique
   {:column? true :rider [[:>= :min 1]]
    :sql  (fn [{:keys [table col]}] (str "SELECT COUNT(DISTINCT " col ") AS n FROM " table))
    :rows (fn [{:keys [cells]}] (count (set cells)))}

   :freshness
   {:column? true :float? true :rider [[:<= :within-hours]]
    :sql  (fn [{:keys [table col]}]
            (str "SELECT FLOOR(EXTRACT(EPOCH FROM"
                 " ((now() AT TIME ZONE 'UTC')"
                 " - CAST((SELECT MAX(" col ") FROM " table ") AS TIMESTAMP)))"
                 " / 3600) AS n"))
    :rows (fn [{:keys [cells now]}]
            (some-> (some->> cells (keep time/->instant) seq
                             (apply max-key #(.toEpochMilli ^Instant %)))
                    (hours-between now)))}

   :null-percentage
   {:column? true :float? true :rider [[:<= :max 0]]
    :sql  (fn [{:keys [table col cast]}]
            (str "SELECT (SUM(CASE WHEN " col " IS NULL OR " cast " = ''"
                 " THEN 1 ELSE 0 END) * 100.0 / GREATEST(COUNT(*), 1)) AS n FROM "
                 table))
    :rows (fn [{:keys [rows cells]}]
            (double (/ (* 100 (count (filter #(or (nil? %) (= "" (str %))) cells)))
                       (max 1 (count rows)))))}

   :sum
   {:column? true :float? true :rider [[:>= :min] [:<= :max]]
    :sql  (fn [{:keys [table col]}]
            (str "SELECT COALESCE(SUM(CAST(" col " AS NUMERIC)), 0) AS n FROM " table))
    :rows (fn [{:keys [cells]}]
            (reduce + 0.0 (keep #(cond (number? %) (double %)
                                       (string? %) (parse-double %))
                                cells)))}

   :reconcile {:reconcile? true :float? true :rider [[:<= :tolerance 0]]}

   :custom {:custom? true :rider [[:<= :max] [:>= :min 0]]}
   :schema {:pending? true}
   :expectation {:build? true}
   :build-status {:pending? true}})

(defn kind
  [k]
  (get kinds (keyword k)))

(defn answerable?
  [k col]
  (let [row (kind k)]
    (boolean (and (:sql row) (or col (not (:column? row)))))))

(defn sql
  [k table col]
  (when (answerable? k col)
    ((:sql (kind k)) {:table table
                      :col   (when col (str \" col \"))
                      :cast  (when col (str "CAST(\"" col "\" AS VARCHAR)"))})))

(defn of-rows
  [k rows cells col now]
  (when (answerable? k col)
    ((:rows (kind k)) {:rows rows :cells cells :now now})))

(defn unknown
  [k]
  (tbl/unknown :unknown-health-kind "health check kind" kinds (keyword k)))
