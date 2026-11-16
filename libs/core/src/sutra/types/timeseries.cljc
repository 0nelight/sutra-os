(ns sutra.types.timeseries
  (:require [clojure.string :as str]
            [sutra.kit.refusal :as refusal]
            [sutra.types.aggregate :as agg]
            [sutra.types.table :as tbl]))

(def sync-columns
  {:series {:column :series_id :role :key
            :types #{:string :text :keyword}
            :doc "Which series a row belongs to. The value a TSP's own column
                  holds on the object — the join, and the reason one sync can
                  serve every TSP of every type."}
   :time   {:column :ts :role :time
            :types #{:date :timestamp :instant}
            :doc "When the quantity was measured. Bucketed by
                  `sutra.types.aggregate/bucket-units`, so a series and an aggregate
                  bucket the same instant into the same label."}
   :value  {:column :value :role :value
            :types #{:number :decimal :double :float :integer :long :short :byte
                     :string :text}
            :doc "The measured quantity. A string value is a CATEGORICAL series
 and can be read raw but not rolled up."}})

(def sync-column-names
  (into #{} (map (comp :column val)) sync-columns))

(defn column-of
  [role]
  (get-in sync-columns [role :column]))

(defn series-property?
  [{:keys [attribute-type]}]
  (= :timeseries attribute-type))

(defn series-spec
  [attr]
  (get-in attr [:opts :series]))

(defn explain-property
  [attr]
  (when (series-property? attr)
    (let [{:keys [sync rollup unit] :as spec} (series-spec attr)
          nm (:attribute-name attr)]
      (or
       (when (nil? spec)
         {:problem :missing-series-spec :attribute nm
          :message (str "property " (pr-str nm) " is a :timeseries but declares no"
                        " :series {:sync <dataset>} — a series property whose sync"
                        " is unknown holds a series ID nothing can resolve")})
       (when-not (keyword? sync)
         {:problem :bad-sync :attribute nm :sync sync
          :message ":series :sync names a sync DATASET (a keyword)"})
       (when (and (some? rollup) (not (contains? agg/agg-fns rollup)))
         (assoc (tbl/unknown :unknown-rollup ":rollup" agg/agg-fns rollup)
                :attribute nm :rollup rollup))
       (when (and (some? unit) (not (string? unit)))
         {:problem :bad-unit :attribute nm :unit unit
          :message ":series :unit is a display string"})))))

(def max-points
  50000)

(def max-series
  1000)

(defn check-series-count!
  [n]
  (when (> n max-series)
    (refusal/refuse! :refusal.types/too-many-series {:series n :limit max-series})))

(defn explain-fetch
  [attr {:keys [bucket rollup series-ids from until]}]
  (let [declared (:rollup (series-spec attr))
        eff      (or rollup declared)
        nm       (:attribute-name attr)]
    (or
     (when-not (series-property? attr)
       {:problem :not-a-series-property :attribute nm
        :type (:attribute-type attr)
        :message (str "property " (pr-str nm) " is " (pr-str (:attribute-type attr))
                      ", not a :timeseries — only a time-series property has "
                      "values over time to fetch")})
     (explain-property attr)
     (when (and (some? bucket) (not (contains? agg/bucket-units bucket)))
       (assoc (tbl/unknown :unknown-bucket-unit "bucket unit" agg/bucket-units bucket)
              :bucket bucket))
     (when (and (some? rollup) (not (contains? agg/agg-fns rollup)))
       (assoc (tbl/unknown :unknown-rollup "rollup" agg/agg-fns rollup)
              :rollup rollup))
     (when (and (some? bucket) (nil? eff))
       {:problem :rollup-required :bucket bucket :attribute nm
        :message (str "a bucketed read of " (pr-str nm) " needs a :rollup — "
                      "declare one on the property (:series {:rollup :sum}) or "
                      "name one in the request. There is no default: summing a "
                      "level series and averaging a flow series are both wrong, "
                      "and only the series knows which it is")})
     (when (and (nil? bucket) (some? rollup))
       {:problem :rollup-without-bucket :rollup rollup
        :message (str ":rollup folds points into BUCKETS — without :bucket there "
                      "is nothing to fold. Drop it, or ask for buckets")})
     (when (and (some? series-ids) (not (sequential? series-ids)))
       {:problem :bad-series-ids
        :message ":series-ids is a collection of series identifiers"})
     (when (and (some? from) (some? until) (neg? (compare (str until) (str from))))
       {:problem :empty-window :from from :until until
        :message (str ":until (" (pr-str until) ") is before :from (" (pr-str from)
                      ") — the window [from, until) is empty")}))))

(defn normalize-fetch
  [attr req]
  (when-let [problem (explain-fetch attr req)]
    (refusal/refuse! :refusal.types/malformed-series-read problem {:detail (:message problem)}))
  (let [{:keys [sync rollup unit]} (series-spec attr)]
    {:property   (:attribute-name attr)
     :bucket     (:bucket req)
     :rollup     (when (:bucket req) (or (:rollup req) rollup))
     :unit       unit
     :sync       sync
     :from       (:from req)
     :until      (:until req)
     :series-ids (some-> (:series-ids req) vec)}))

(def aliases
  {:series {:sql "sid" :key :sid}
   :time   {:sql "tsx" :key :tsx}
   :value  {:sql "val" :key :val}})

(defn time-expr
  [dialect {:keys [bucket]} col]
  (if bucket
    (agg/group-sql dialect {:kind :bucket :unit bucket} col)
    col))

(defn value-expr
  [{:keys [rollup]} col]
  (if rollup (agg/agg-sql {:fn rollup} col) col))

(defn ->series
  [rows]
  (let [{:keys [series time value]} aliases
        {:keys [order points]}
        (reduce (fn [{:keys [order points] :as acc} row]
                  (let [sid (get row (:key series))
                        pt  [(get row (:key time)) (get row (:key value))]]
                    (-> acc
                        (assoc :order (if (contains? points sid) order (conj order sid)))
                        (assoc :points (update points sid (fnil conj []) pt)))))
                {:order [] :points {}}
                rows)]
    (mapv (fn [sid] {:series-id sid :points (get points sid)}) order)))

(defn point-count [series] (transduce (map (comp count :points)) + 0 series))

(defn check-point-count!
  [n]
  (when (> n max-points)
    (refusal/refuse! :refusal.types/too-many-points {:points n :limit max-points})))

(defn base-type
  [t]
  (if (vector? t) (first t) t))

(defn explain-sync
  [ds columns]
  (let [by-name (into {} (map (juxt :attribute-name (comp base-type :attribute-type)))
                      columns)
        missing (into (sorted-set) (remove by-name) sync-column-names)]
    (or
     (when (seq missing)
       {:problem :sync-missing-columns :dataset ds :missing missing
        :message (str "dataset " (pr-str ds) " is used as a time-series sync but "
                      "has no " (str/join ", " (map name missing)) " column — a "
                      "sync carries exactly " (pr-str (vec (sort sync-column-names)))
                      " (extra columns are fine)")})
     (some (fn [[role {:keys [column types]}]]
             (let [t (by-name column)]
               (when-not (contains? types t)
                 {:problem :sync-column-type :dataset ds :column column :type t
                  :message (str "sync " (pr-str ds) "'s " (name column) " column is "
                                (pr-str t) " — the " (name role) " column must be one of "
                                (pr-str (vec (sort types))))})))
           sync-columns))))
