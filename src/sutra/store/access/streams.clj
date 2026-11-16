(ns sutra.store.access.streams
  (:refer-clojure :exclude [reset!])
  (:require [sutra.store.access.scope :as ctx]
            [sutra.kit.refusal :as refusal]
            [sutra.store.control :as ctl]
            [sutra.types.column :as column]
            [sutra.store.access.datasets :as dsets]
            [sutra.catalog.lookup :as lookup]
            [sutra.catalog.nodes :as nodes]
            [sutra.types.naming :as naming]
            [clojure.core :as core]
            [clojure.string :as str]
            [sutra.foundation.encoding :as enc]
            [sutra.kit.pubsub :as pubsub]
            [sutra.streams.api :as s]
            [sutra.types.stream :as strm]))

(set! *warn-on-reflection* true)

(defn stream-of
  [schema name-kw]
  (when-let [rid (lookup/rid-of schema :datasets (keyword (core/name name-kw)))]
    (let [entry (get-in schema [:datasets rid])]
      (when-let [spec (strm/normalize (:stream entry))]
        {:name    (or (:dataset-name entry) (keyword (core/name name-kw)))
         :rid     rid
         :spec    spec
         :columns (into [] (keep :attribute-name) (:columns entry))
         :entry   entry
         :buffer  (naming/stream-table rid)}))))

(defn streams
  [schema]
  (into []
        (keep (fn [[rid entry]]
                (when (:stream entry)
                  (stream-of schema (or (:dataset-name entry) rid)))))
        (:datasets schema)))

(defn require-stream!
  [schema name-kw]
  (or (stream-of schema name-kw)
      (refusal/refuse! :refusal.store/no-stream {:stream (core/name name-kw)})))

(defn- addr [store stream] {:branch (str (:branch store)) :stream (:buffer stream)})

(def consumers [:archive :index])

(defn- cursor-key [store consumer stream]
  {:branch (str (:branch store)) :stream (core/name (:name stream)) :consumer (core/name consumer)})

(defn cursor-state [store consumer stream]
  (ctl/get store :stream-cursors (cursor-key store consumer stream)))

(defn cursors
  [store consumer stream]
  (into {} (map (fn [[p o]] [(long p) (long o)])) (:offsets (cursor-state store consumer stream))))

(defn set-cursors!
  ([store consumer stream offsets] (set-cursors! store consumer stream offsets nil))
  ([store consumer stream offsets dead-letters]
   (if (empty? dead-letters)
     (ctl/upsert! store :stream-cursors (assoc (cursor-key store consumer stream)
                                               :offsets offsets :last-run-at ctl/now))
     (let [k (cursor-key store consumer stream)]
       (ctl/plan! store
                  (cons (fn [store t]
                          (ctl/swapping store :stream-cursors k
                                        (fn [old] (assoc (merge old k) :offsets offsets
                                                         :last-run-at ctl/now))
                                        t))
                        (for [{:keys [partition offset] :as d} dead-letters
                              :let [dk {:branch (str (:branch store))
                                        :stream (core/name (:name stream))
                                        :partition (long partition) :offset (long offset)}]]
                          (fn [store t]
                            (ctl/swapping store :stream-dead-letter dk
                                          (fn [_]
                                            (merge (select-keys d [:object-type :record :reason])
                                                   dk {:at ctl/now}))
                                          t)))))))
   offsets))

(defn dead-letters
  [store stream limit]
  (ctl/list store :stream-dead-letter
            {:branch (str (:branch store)) :stream (core/name (:name stream))}
            {:limit limit}))

(defonce ^:private subscribers (pubsub/registry))

(defn- topic [branch stream-name] [(str branch) (core/name stream-name)])

(defn subscribe! [branch stream-name sink]
  (pubsub/subscribe! subscribers (topic branch stream-name) sink))

(defn subscriber-count [branch stream-name]
  (pubsub/count subscribers (topic branch stream-name)))

(defn drop-subscribers! [] (pubsub/clear! subscribers))

(defn- too-big? [record]
  (> (count (.getBytes (pr-str record) "UTF-8")) strm/max-record-bytes))

(defn publish-problems
  [{:keys [spec columns]} records]
  (into []
        (comp (map-indexed
               (fn [i r]
                 (let [ps (cond-> (strm/record-problems spec columns r)
                            (and (map? r) (too-big? r))
                            (conj (str "the record is larger than " strm/max-record-bytes
                                       " bytes — streams operate per row and cap"
                                       " the row to keep transfers low-latency")))]
                   (when (seq ps) {:index i :problems ps}))))
              (remove nil?))
        records))

(defn entries
  [spec records t]
  (into [] (map-indexed (fn [i r] {:partition (strm/partition-for spec r (hash [i r]))
                                   :key       (some-> (strm/key-of spec r) str)
                                   :at        t
                                   :record    r}))
        records))

(defn publish!
  [store schema name-kw records]
  (let [{:keys [spec] :as stream} (require-stream! schema name-kw)
        problems (publish-problems stream records)
        nm       (core/name (:name stream))]
    (when (seq problems)
      (refusal/refuse! :refusal.store/stream-batch-rejected
                       {:stream nm :rejected problems}
                       {:rejected-count (count problems) :records (count records)}))
    (let [placed (s/append! (:streams store) (addr store stream)
                            (entries spec records (ctl/clock store)))]
      (pubsub/publish! subscribers (topic (:branch store) (:name stream))
                       {:stream nm :records (mapv (fn [r o] (assoc o :record r)) records placed)})
      {:stream nm :published (count placed) :offsets placed})))

(defn end-offsets
  [store schema name-kw]
  (let [{:keys [spec] :as stream} (require-stream! schema name-kw)]
    (into (sorted-map)
          (merge (zipmap (range (max 1 (long (or (:partitions spec) 1)))) (repeat 0))
                 (s/ends (:streams store) (addr store stream))))))

(defn- wire [r] (update r :at str))

(defn records
  [store schema name-kw {:keys [partition start-offset limit]}]
  (let [p (long (or partition 0))]
    (mapv wire (s/read (:streams store) (addr store (require-stream! schema name-kw))
                       {:from {p (long (or start-offset 0))} :partition p
                        :limit (long (or limit 200))}))))

(defn buffered-since
  [store stream cursor limit]
  (s/read (:streams store) (addr store stream) {:from cursor :limit limit}))

(defn- cold->record
  [columns]
  (let [by-stored (into {} (map (fn [c] [(keyword (str/replace (column/sql-name c) "_" "-")) c]))
                        columns)]
    (fn [{:keys [values]}]
      (reduce-kv (fn [m k v] (assoc m (get by-stored k k) v)) {} values))))

(defn view
  [store schema name-kw limit]
  (let [{:keys [spec rid columns] :as stream} (require-stream! schema name-kw)
        cold (mapv (cold->record columns) (dsets/rows store rid))
        hot  (mapv :record (buffered-since store stream (cursors store :archive stream) 10000))]
    {:stream (core/name (:name stream))
     :cold   (count cold)
     :hot    (count hot)
     :rows   (vec (take (or limit 200) (strm/current-view spec (into cold hot))))}))

(defn- row-id-of
  [spec rid {:keys [record partition offset]}]
  (if (strm/cdc? spec)
    (enc/transform-row-id rid (mapv #(get record %) (strm/dedup-columns spec)))
    (enc/transform-row-id rid ["archive" (str partition ":" offset)])))

(defn trim!
  [store stream]
  (s/trim! (:streams store) (addr store stream)
           {:below  (s/floor (map #(cursors store % stream) consumers))
            :before (java.util.Date. (- (ctl/millis store)
                                        (long (or (get-in stream [:spec :retention-ms]) 0))))}))

(defn archive!
  [store schema name-kw]
  (let [{:keys [spec rid] :as stream} (require-stream! schema name-kw)
        cur   (cursors store :archive stream)
        batch (buffered-since store stream cur 10000)
        nm    (core/name (:name stream))]
    (if (empty? batch)
      {:stream nm :archived 0 :deleted 0 :cursors cur}
      (let [{dead true live false} (group-by #(strm/deleted? spec (:record %)) batch)
            rows     (if (strm/cdc? spec)
                       (mapv (fn [r] {:id (row-id-of spec rid {:record r}) :values r})
                             (strm/current-view spec (map :record live)))
                       (mapv (fn [e] {:id (row-id-of spec rid e) :values (:record e)}) live))
            dead-ids (when (strm/cdc? spec) (mapv #(row-id-of spec rid %) dead))
            next-cur (s/advance cur batch)]
        (when (seq rows) (dsets/write-rows! (ctx/as-system store) rid :update rows))
        (when (seq dead-ids) (dsets/delete-rows! store rid dead-ids))
        (set-cursors! store :archive stream next-cur)
        (trim! store stream)
        {:stream nm :archived (count rows) :deleted (count (or dead-ids [])) :cursors next-cur}))))

(defn reset!
  [store schema name-kw]
  (let [{:keys [rid] :as stream} (require-stream! schema name-kw)]
    (when (nodes/producer-of schema rid)
      (refusal/refuse! :refusal.store/stream-not-resettable {:stream (core/name name-kw)}))
    (s/reset! (:streams store) (addr store stream))
    (when (pos? (long (dsets/table-count store rid))) (dsets/truncate! store rid))
    (doseq [c (conj consumers :sync)]
      (ctl/delete! store :stream-cursors (cursor-key store c stream)))
    {:stream (core/name (:name stream)) :reset true}))

(defn status
  [store schema name-kw]
  (let [{:keys [spec rid] :as stream} (require-stream! schema name-kw)
        {:keys [depth last-at]} (s/stats (:streams store) (addr store stream))]
    {:stream             (core/name (:name stream))
     :rid                rid
     :spec               spec
     :columns            (:columns stream)
     :partitions         (:partitions spec)
     :cdc?               (strm/cdc? spec)
     :end-offsets        (end-offsets store schema name-kw)
     :last-record-at     (some-> last-at str)
     :buffered           (reduce + 0 (vals depth))
     :depth-by-partition depth
     :archived-rows      (dsets/table-count store rid)
     :subscribers        (subscriber-count (:branch store) (:name stream))
     :cursors            {:archive (cursor-state store :archive stream)
                          :index   (cursor-state store :index stream)
                          :sync    (cursor-state store :sync stream)}
     :dead-letter        (mapv #(update % :at str) (dead-letters store stream 50))
     :dead-letter-count  (ctl/count store :stream-dead-letter
                                    {:branch (str (:branch store))
                                     :stream (core/name (:name stream))})}))
