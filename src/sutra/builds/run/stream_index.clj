(ns sutra.builds.run.stream-index
  (:require [sutra.kit.refusal :as refusal]
            [sutra.store.world :as world]
            [sutra.store.control :as ctl]
            [sutra.types.column :as column]
            [sutra.store.access.streams :as streams]
            [sutra.store.access.watch :as watch]
            [sutra.streams.api :as s]
            [sutra.catalog.backing :as backing]
            [clojure.string :as str]
            [sutra.store.seat :as seat]
            [sutra.catalog.grants :as grants]
            [sutra.sources.api :as sources]
            [sutra.catalog.values :as values]
            [sutra.builds.ports :as ports]
            [sutra.foundation.encoding :as enc]
            [sutra.builds.plan.restrictions :as restrict]
            [sutra.builds.run.monitor :as bm]
            [sutra.types.stream :as strm]
            [sutra.builds.plan.outputs :as outputs]
            [sutra.catalog.streaming :as streaming]
            [sutra.catalog.lookup :as lookup]
            [sutra.store.api :as api]
            [sutra.store.control.collections :as collections]
            [sutra.kit.noted :as noted]))

(set! *warn-on-reflection* true)

(def ^:private index-batch 5000)

(defn- ms-of
  [ts]
  (cond
    (nil? ts) nil
    (instance? java.sql.Timestamp ts) (.getTime ^java.sql.Timestamp ts)
    :else (try (.getTime ^java.util.Date ts) (catch ClassCastException _ nil))))

(defn archive-due?
  [store stream now-ms]
  (let [last-run (ms-of (:last-run-at (streams/cursor-state store :archive stream)))]
    (or (nil? last-run)
        (>= (- now-ms last-run)
            (long (get-in stream [:spec :archive-every-ms] 60000))))))

(defn stream-backed-types
  [schema stream]
  (into []
        (keep (fn [[ot-rid _]]
                (let [backing (backing/object-backing schema ot-rid)]
                  (when (= (str (:dataset backing)) (str (:rid stream)))
                    [ot-rid backing]))))
        (:object-types schema)))

(defn- values-of
  [record column-map attrs]
  (let [explicit (reduce-kv (fn [m attr-kw col-kw] (assoc m attr-kw (get record col-kw)))
                            {} (or column-map {}))]
    (reduce (fn [m attr-kw]
              (if (or (nil? attr-kw) (contains? m attr-kw))
                m
                (let [col (column/attr->col attr-kw)]
                  (cond
                    (contains? record col)     (assoc m attr-kw (get record col))
                    (contains? record attr-kw) (assoc m attr-kw (get record attr-kw))
                    :else                      m))))
            explicit (or attrs []))))

(defn- staged-rows
  [schema stream ot-rid {:keys [primary-key column-map]} entries]
  (let [sspec (:spec stream)
        attrs (outputs/attr-names (get-in schema [:object-types ot-rid]))]
    (mapv (fn [{:keys [record] :as entry}]
            (let [values (values-of record column-map attrs)
                  pk     (get values primary-key)]
              {:pk pk
               :values values
               :deleted? (strm/deleted? sspec record)
               :entry (select-keys entry [:partition :offset :record])
               :id (when (some? pk) (enc/object-id ot-rid pk))}))
          entries)))

(defn- dead-letter [ot-rid reason {:keys [entry]}]
  (assoc entry :object-type (str ot-rid) :reason reason))

(defn- note-rejects!
  [stream ot-rid rejected]
  (when (seq rejected)
    (binding [*out* *err*]
      (println (str "[stream-index] " (:stream-name stream) " → " ot-rid ": "
                    (count rejected) " record(s) refused by the data restrictions"
                    " and NOT indexed — " (str/join "; " (take 3 rejected)))))))

(defn- refused-batch
  [ot-rid resolved fatal]
  (let [why (restrict/describe (first fatal))]
    {:ops []
     :rejects (mapv restrict/describe fatal)
     :result {:object-type ot-rid :indexed 0 :retired 0
              :error why
              :dead-letters (mapv #(dead-letter ot-rid why %) resolved)}}))

(defn- indexed-batch
  [ot-rid resolved dead vs]
  (let [why  (into {} (keep (fn [v] (when (:row v) [(:row v) (restrict/describe v)]))) vs)
        bad  (sort (keys why))
        kept (into [] (keep-indexed (fn [i r] (when-not (contains? why i) r))) resolved)]
    {:ops (into (vec (for [{:keys [id values]} kept :when id]
                       {:op :create-object :type ot-rid
                        :payload {:id (str id) :values values}}))
                (for [{:keys [id]} dead :when id]
                  {:op :delete-object :type ot-rid :payload {:id (str id)}}))
     :rejects (mapv (fn [i] (str "pk=" (pr-str (:pk (nth resolved i))))) bad)
     :result {:object-type ot-rid
              :indexed (count kept)
              :retired (count dead)
              :rejected (count bad)
              :dead-letters (mapv (fn [i] (dead-letter ot-rid (get why i) (nth resolved i)))
                                  bad)}}))

(defn- in-time-order
  [ts-attr live]
  (if ts-attr
    (sort-by #(or (streaming/timestamp-ms (get-in % [:values ts-attr])) Long/MIN_VALUE) live)
    live))

(defn index-decision
  ([schema stream ot-rid backing entries]
   (index-decision schema stream ot-rid backing entries {}))
  ([schema stream ot-rid {pk-col :primary-key :as backing} entries {:keys [ts-attr stored]}]
   (if (nil? pk-col)
     {:ops [] :rejects []
      :result {:object-type ot-rid :indexed 0 :retired 0
               :error (str "no primary key on the backing of " ot-rid
                           " — a changelog record identifies its object by primary key,"
                           " so without one there is nothing for an update to update")}}
     (let [rows (staged-rows schema stream ot-rid backing entries)
           {dead true live false} (group-by (comp boolean :deleted?) rows)
           resolved0 (vec (vals (reduce (fn [m r] (assoc m (:pk r) r)) (array-map)
                                        (in-time-order ts-attr live))))
           late (if ts-attr
                  (streaming/out-of-order ts-attr
                                          (mapv #(update % :id str) resolved0)
                                          (update-keys (or stored {}) str))
                  #{})
           resolved (filterv #(not (contains? late (str (:id %)))) resolved0)
           vs (restrict/violations {:schema schema :object-type ot-rid :primary-key pk-col}
                                   (mapv #(select-keys % [:pk :values]) resolved))]
       (cond-> (if-let [fatal (seq (remove :row vs))]
                 (refused-batch ot-rid resolved fatal)
                 (indexed-batch ot-rid resolved dead vs))
         (seq late) (assoc-in [:result :out-of-order] (count late)))))))

(def ^:private mark-coll (:coll collections/stream-index-marks))

(defn- mark-key [store ot-rid stream]
  [(str (:branch store)) (str ot-rid) (str (:rid stream))])

(defn unmarked
  [mark entries]
  (filterv (fn [{:keys [partition offset]}]
             (> (long offset) (long (get mark partition -1))))
           entries))

(defn advanced-mark
  [mark entries]
  (reduce (fn [m {:keys [partition offset]}] (update m partition (fnil max -1) offset))
          (or mark {}) entries))

(defn- stored-timestamps
  [store ot-rid attr ids]
  (when (seq ids)
    (into {}
          (keep (fn [o] (when-some [v (get-in o [:values attr])] [(str (:id o)) v])))
          (world/with-snapshot store
            (fn [s] (api/read s api/unrestricted [:objects ot-rid (vec (distinct ids))]))))))

(defn- index-type!
  [store schema stream ot-rid backing entries]
  (let [{:keys [direct? consistency source-timestamp]} (streaming/settings schema ot-rid)
        once?   (not= :at-least-once consistency)
        k       (mark-key store ot-rid stream)
        mark    (when once?
                  (world/with-snapshot store (fn [s] (api/read s nil [:records/get mark-coll k]))))
        fresh   (if once? (unmarked mark entries) entries)
        ts-attr (when direct? source-timestamp)
        stored  (when (and ts-attr (seq fresh))
                  (stored-timestamps store ot-rid ts-attr
                                     (keep :id (staged-rows schema stream ot-rid backing fresh))))
        {:keys [ops rejects result]}
        (index-decision schema stream ot-rid backing fresh {:ts-attr ts-attr :stored stored})]
    (note-rejects! stream ot-rid rejects)
    (world/index! store ops
                  {:origin  (when direct? :direct)
                   :records (when (and once? (seq fresh) (seq ops))
                              [{:coll mark-coll :key k
                                :expect (if (some? mark) mark api/absent)
                                :value (advanced-mark mark fresh)}])})
    (when (seq ops)
      (watch/notify! (:branch store) ot-rid
                     {:at  (ctl/stamp store)
                      :ids (mapv (comp :id :payload) ops)}))
    (cond-> result
      (< (count fresh) (count entries)) (assoc :already-indexed (- (count entries)
                                                                  (count fresh))))))

(defn shape-entries
  [fn-name entries rows]
  (when-not (= (count entries) (count rows))
    (throw (ex-info (str "stream shape " (name fn-name) " answered " (count rows)
                         " rows for " (count entries) " records — it maps them one to one")
                    {:shape fn-name})))
  (mapv #(assoc %1 :record %2) entries rows))

(defn- shaped
  [store schema stream batch]
  (if-let [f (get-in stream [:spec :shape])]
    (let [deletion (strm/deletion-column (:spec stream))
          shape    (shape-entries f batch (ports/run-query-function store schema f
                                                                    {:records
                                                                     (mapv :record batch)}))]
      (cond->> shape
        deletion (mapv (fn [before after]
                         (assoc-in after [:record deletion] (get-in before [:record deletion])))
                       batch)))
    batch))

(defn index!
  [store schema stream]
  (let [cur     (streams/cursors store :index stream)
        batch   (streams/buffered-since store stream cur index-batch)
        types   (stream-backed-types schema stream)]
    (when (seq batch)
      (let [results (when (seq types)
                      (let [entries (shaped store schema stream batch)]
                        (mapv (fn [[ot-rid backing]]
                                (index-type! store schema stream ot-rid backing entries))
                              types)))]
        (streams/set-cursors! store :index stream (s/advance cur batch)
                              (into [] (mapcat :dead-letters) results))
        {:stream (:stream-name stream)
         :consumed (count batch)
         :types (mapv #(dissoc % :dead-letters) results)}))))

(defn pulled-cursor
  [{:keys [cursor]} from rows]
  (reduce max from (keep cursor rows)))

(defn- publish-pulled!
  [store schema stream rows]
  (when (seq rows)
    (streams/publish! store schema (:name stream) (vec rows))
    (count rows)))

(defn- pull-query!
  [store schema stream {:keys [query] :as sync} read!]
  (let [from (get (streams/cursors store :sync stream) 0 0)
        rows (read! [query from])]
    (when-let [n (publish-pulled! store schema stream rows)]
      (streams/set-cursors! store :sync stream {0 (pulled-cursor sync from rows)})
      n)))

(defn cdc-step
  [table lsn {:keys [min_lsn max_lsn]}]
  (cond
    (or (nil? max_lsn) (nil? min_lsn) (re-matches #"0x0+" min_lsn))
    (refusal/refuse! :refusal.builds/cdc-not-enabled {:table table})

    (nil? lsn)
    {:step :snapshot :to (if (neg? (compare max_lsn min_lsn)) min_lsn max_lsn)}

    (neg? (compare lsn min_lsn))
    (refusal/refuse! :refusal.builds/changelog-retention-passed
                     {:table table :lsn lsn :min-lsn min_lsn})

    (neg? (compare lsn max_lsn))
    {:step :changes :from lsn :to max_lsn}

    :else {:step :idle}))

(defn- pull-cdc!
  [store schema {:keys [spec columns] :as stream} {:keys [cdc]} read!]
  (let [deletion (or (get-in spec [:primary-key :deletion-column])
                     (refusal/refuse! :refusal.builds/cdc-sync-incomplete {:stream (:name stream)}))
        instance (sources/cdc-instance cdc)
        lsn      (get-in (streams/cursor-state store :sync stream) [:offsets :lsn])
        {:keys [step from to]} (cdc-step (:table cdc) lsn
                                         (first (read! (sources/cdc-bounds-stmt instance))))]
    (when-not (= :idle step)
      (let [n (publish-pulled! store schema stream
                               (read! (if (= :snapshot step)
                                        (sources/cdc-snapshot-stmt cdc columns deletion)
                                        (sources/cdc-changes-stmt instance columns deletion
                                                                  from to))))]
        (streams/set-cursors! store :sync stream {:lsn to})
        n))))

(defn pull!
  [store schema {:keys [spec] :as stream}]
  (when-let [{:keys [source cdc] :as sync} (:sync spec)]
    (let [src   ((ports/foreign-datasources store :cleared!)
                 (grants/source-authority store)
                 (or (get (values/sources->datasources schema) source)
                     (refusal/refuse! :refusal.builds/stream-unknown-source {:source source})))
          read! (fn [[sql & params]]
                  ((ports/foreign-datasources store :read-rows) src sql params))]
      (if cdc
        (if (sources/cdc-capable? (:type src))
          (pull-cdc! store schema stream sync read!)
          (refusal/refuse! :refusal.builds/source-no-cdc {:source source} {:type (:type src)}))
        (pull-query! store schema stream sync read!)))))

(defn- iso [t] (some-> (ms-of t) java.time.Instant/ofEpochMilli str))

(defn liveness
  [store schema name-kw]
  (let [stream (streams/require-stream! schema name-kw)
        index  (streams/cursor-state store :index stream)]
    (vec (for [[ot-rid _] (stream-backed-types schema stream)
               :let [{:keys [direct? consistency source-timestamp]}
                     (streaming/settings schema ot-rid)
                     edit (first (world/edit-history store ot-rid {:limit 1}))]]
           (cond-> {:object-type      (str ot-rid)
                    :direct?          direct?
                    :consistency      consistency
                    :data-at          (iso (:last-run-at index))}
             source-timestamp (assoc :source-timestamp source-timestamp)
             edit             (assoc :latest-edit-at (iso (:edited-at edit))))))))

(defn derive!
  [store schema {:keys [spec] :as stream}]
  (when-let [{:keys [from function]} (:derive spec)]
    (let [source (streams/require-stream! schema (keyword (name from)))
          cur    (streams/cursors store :derive stream)
          batch  (streams/buffered-since store source cur index-batch)]
      (when (seq batch)
        (let [rows (ports/run-query-function store schema (keyword (name function))
                                             {:records (mapv :record batch)})]
          (when (seq rows)
            (streams/publish! store schema (:name stream) (vec rows)))
          (streams/set-cursors! store :derive stream (s/advance cur batch) [])
          (count rows))))))

(defonce ^:private failing (atom {}))

(defonce ^:private last-pull (atom {}))

(defn sync-due?
  [stream last-at now]
  (let [every (get-in stream [:spec :sync :every-ms])]
    (or (nil? every) (nil? last-at) (>= (- now last-at) every))))

(defn- pull-when-due!
  [store schema now {nm :stream-name :as stream}]
  (when (sync-due? stream (get @last-pull nm) now)
    (swap! last-pull assoc nm now)
    (pull! store schema stream)))

(defn- tick-stream!
  [store schema now building? stream]
  (let [nm (:stream-name stream)]
    (try
      (let [pulled  (pull-when-due! store schema now stream)
            derived (derive! store schema stream)
            idx     (index! store schema stream)
            arc    (when (and (not building?) (archive-due? store stream now))
                     (streams/archive! store schema (:name stream)))]
        (swap! failing dissoc nm)
        (when (or pulled derived idx (and arc (pos? (+ (:archived arc 0) (:deleted arc 0)))))
          (cond-> {:stream nm}
            pulled  (assoc :pulled pulled)
            derived (assoc :derived derived)
            idx    (assoc :indexed idx)
            arc    (assoc :archived arc))))
      (catch InterruptedException e (throw e))
      (catch Exception e
        (let [why (.getMessage e)]
          (when (not= why (get @failing nm))
            (swap! failing assoc nm why)
            (println (str "[stream] " nm " stalled: " why))))
        nil))))

(defn tick!
  [store _branch]
  (let [schema (seat/branch-schema store)
        now    (ctl/millis store)
        building? (bm/any-build-active? store)]
    (into []
          (keep #(tick-stream! store schema now building?
                               (assoc % :stream-name (name (:name %)))))
          (streams/streams schema))))

(defn- wake-here! []
  (when-let [poke (ports/stream-wake)] (poke))
  nil)

(defn wake!
  [store]
  (wake-here!)
  (when-let [broadcast (ports/cluster-broadcast store)]
    (noted/quietly "stream-index: wake not broadcast, the tick picks it up"
                     (broadcast :stream/wake {})))
  nil)

(defn direct-writes
  [store obj-type]
  (let [schema (seat/branch-schema store)
        ot     (or (when (contains? (:object-types schema) obj-type) obj-type)
                   (some->> obj-type name keyword (lookup/rid-of schema :object-types)))
        {:keys [direct? stream source-timestamp]} (when ot (streaming/settings schema ot))]
    (when direct?
      (cond-> {:origin :direct :stream (str stream)}
        source-timestamp (assoc :source-timestamp source-timestamp)))))

(def module
  {:name :stream-index
   :provides {:objects/direct-writes direct-writes}
   :contributes {:cluster/listeners {:stream/wake (fn [_] (wake-here!))}}
   :jobs [{:name  :stream-index :every :stream-ms
           :tick  (seat/on-branch
                   "stream"
                   (fn [store branch]
                     (doseq [moved (tick! store branch)]
                       (println (str "[stream] " (:stream moved) " "
                                     (pr-str (dissoc moved :stream)))))))}]})
