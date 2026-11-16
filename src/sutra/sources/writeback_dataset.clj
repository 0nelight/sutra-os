(ns sutra.sources.writeback-dataset
  (:require [sutra.store.access.scope :as ctx]
            [sutra.catalog.grants :as grants]
            [sutra.types.column :as column]
            [clojure.string :as str]
            [sutra.sources.datasources :as ds]
            [sutra.store.access.run-log :as run-log]
            [sutra.store.control :as ctl]
            [sutra.catalog.values :as values]
            [sutra.catalog.lookup :as lookup]
            [sutra.kit.refusal :as refusal]
            [sutra.sources.writeback :as wb]))

(set! *warn-on-reflection* true)

(defn- declared-target
  [schema obj-name]
  (let [want (name obj-name)]
    (first
     (for [[src-nm src] (values/sources->datasources schema)
           [table cfg]  (:writeback src)
           :when (= want (some-> (:object-type cfg) name))]
       {:source-id src-nm :source src :table table :cfg cfg}))))

(defn- attr->foreign
  [cfg attr]
  (let [local (name (column/attr->col attr))
        m     (:column-map cfg)]
    (name (or (get m local) (get m (keyword local)) local))))

(defn- declared-writeback
  [schema obj-kw]
  (let [oid      (or (lookup/rid-of schema :object-types obj-kw) obj-kw)
        obj-name (or (get-in schema [:object-types oid :object-type-name]) obj-kw)
        {:keys [source-id source table cfg]} (declared-target schema obj-name)]
    (when-not table
      (refusal/refuse! :refusal.sources/writeback-not-enabled
                       {:object-type obj-kw
                        :hint
                          "no :writeback allowlist entry on any source names this object type"}))
    (let [attrs      (map :attribute-name (get-in schema [:object-types oid :attribute-list]))
          column-map (into {} (for [a attrs] [a (attr->foreign cfg a)]))
          by-col     (into {} (for [[a c] column-map] [(str/lower-case c) a]))
          attr-of    (fn [c] (some->> c name str/lower-case (get by-col)))
          writable   (set (map name (:writable-columns cfg)))
          insertable (set (map name (:insertable-columns cfg)))
          pk-col     (some-> (:pk cfg) name)
          pk-attr    (attr-of pk-col)
          mk         (:match-key cfg)
          match-cols (when mk (mapv name (if (sequential? mk) mk [mk])))]
      (when-not pk-attr
        (refusal/refuse! :refusal.sources/writeback-pk-unknown
                         {:object-type obj-kw :pk pk-col :columns (vec (sort (keys by-col)))}))
      {:source      source
       :source-id   source-id
       :datasource  source-id
       :table       table
       :pk-col      pk-col
       :pk-attr     pk-attr
       :pk-result   pk-col
       :pk-columns  (:pk-columns cfg)
       :query       (:query cfg)
       :column-map  column-map
       :writable    writable
       :insertable  insertable
       :identity-col (:identity cfg)
       :match-key   match-cols
       :match-attrs (mapv attr-of match-cols)
       :on-duplicate (some-> (:on-duplicate cfg) keyword)
       :number-attr (attr-of (let [a (:autonumber cfg)]
                               (:into (if (sequential? a) (first a) a))))
       :mode        (or (:mode cfg) :jdbc)
       :webhook-id  (:webhook-id cfg)
       :identity-path (:identity-path cfg)
       :export-transform (:export-transform cfg)
       :deletable?  (boolean (:deletable? cfg))
       :delete-guard (:delete-guard cfg)
       :timestamp-attr (:timestamp-attr cfg)
       :writable-attrs (->> column-map
                            (keep (fn [[attr col]]
                                    (when (and (not= attr pk-attr)
                                               (contains? writable (name col)))
                                      attr)))
                            set)
       :strategy    (or (:source-of-record cfg) :apply-user-edits)})))

(defn- writeback-target
  [schema obj-kw store]
  (assoc (declared-writeback schema obj-kw)
         :connect   (ctx/foreign)
         :authority (grants/request-authority store)))

(defn preview
  ([schema obj-kw obj-id] (preview schema obj-kw obj-id {}))
  ([schema obj-kw obj-id {:keys [proposed store seen]}]
   (let [tgt      (writeback-target schema obj-kw store)
         _        (when (nil? proposed)
                    (refusal/refuse! :refusal.sources/writeback-object-missing
                                     {:object-type obj-kw :object-id obj-id}))
         pk-val   (get proposed (:pk-attr tgt))
         live     (wb/read-live-row tgt pk-val)
         ts-attr  (:timestamp-attr tgt)
         changes  (into {}
                        (for [a (:writable-attrs tgt)]
                          [a {:live     (get live a)
                              :proposed (get proposed a)
                              :changes? (not= (get live a) (get proposed a))}]))
         only     (ctx/object-edited-attrs store obj-kw obj-id)
         verdict  (wb/conflict-verdict (:strategy tgt)
                                       {:timestamp-attr ts-attr
                                        :ts-live  (get live ts-attr)
                                        :ts-known (get proposed ts-attr)})]
     {:object-type obj-kw :object-id obj-id
      :source-id (:source-id tgt) :table (:table tgt) :pk-value pk-val
      :strategy (:strategy tgt)
      :edited   only
      :write-spec (wb/reshape tgt proposed live only seen)
      :seen     (into {} (for [a (:writable-attrs tgt)] [a (get live a)]))
      :live-present? (some? live)
      :changes changes
      :verdict verdict})))

(defn- with-who
  [spec actor correlation]
  (cond-> spec
    actor       (assoc :actor actor)
    correlation (assoc :correlation correlation)))

(defn- live-row
  [tgt obj-kw values what]
  (let [pk-val (get values (:pk-attr tgt))]
    (when (nil? pk-val)
      (refusal/refuse! :refusal.sources/writeback-no-source-key
                       {:object-type obj-kw :pk-attr (:pk-attr tgt)}
                       {:what what}))
    [pk-val (wb/read-live-row tgt pk-val)]))

(defn- outcome
  [tgt spec res]
  {:pushed spec :result res :table (:table tgt) :source-id (:source-id tgt)})

(defn- push-logged!
  [store log-kind tgt spec gate entry]
  (let [res (wb/push! tgt spec gate)
        e   (run-log/action-entry log-kind
                                  (assoc entry :source-id (:source-id tgt) :table (:table tgt)
                                         :actor (:actor spec) :correlation (:correlation spec)
                                         :result res)
                                  nil nil)]
    (if-let [session (:write-session tgt)]
      (ds/hold-log! session e)
      (run-log/append! store [e]))
    res))

(defn confirm!
  ([schema obj-kw obj-id] (confirm! schema obj-kw obj-id {}))
  ([schema obj-kw obj-id
    {:keys [proposed force? actor correlation effect-gate store write-session seen]}]
   (when-not (or seen force?)
     (refusal/refuse! :refusal.sources/writeback-confirm-unseen
                      {:object-type obj-kw :object-id obj-id}))
   (let [tgt (assoc (writeback-target schema obj-kw store) :write-session write-session)
         pv  (preview schema obj-kw obj-id {:proposed proposed :store store :seen seen})]
     (when-not (or force? (get-in pv [:verdict :allow?]))
       (refusal/refuse! :refusal.sources/writeback-blocked
                        {:verdict (:verdict pv) :object-id obj-id}
                        {:why (get-in pv [:verdict :reason])}))
     (when (empty? (:edited pv))
       (refusal/refuse! :refusal.sources/writeback-no-edits
                        {:object-type obj-kw :object-id obj-id :table (:table tgt)}))
     (when (empty? (get-in pv [:write-spec :set]))
       (refusal/refuse! :refusal.sources/writeback-already-written
                        {:object-type obj-kw :object-id obj-id :table (:table tgt)}))
     (let [spec (with-who (:write-spec pv) actor correlation)
           res  (push-logged! store :writeback-push tgt spec effect-gate
                              {:object-type obj-kw :object-id obj-id
                               :pk-value (:pk-value pv)
                               :set (get-in pv [:write-spec :set])
                               :changes (:changes pv)
                               :strategy (:strategy tgt)
                               :forced (boolean force?)})]
       {:pushed spec :result res :verdict (:verdict pv)}))))

(defn push-values!
  [schema obj-kw values {:keys [actor correlation only effect-gate store write-session]}]
  (let [tgt           (assoc (writeback-target schema obj-kw store) :write-session write-session)
        [pk-val live] (live-row tgt obj-kw values "writeback")]
    (when-not live
      (refusal/refuse! :refusal.sources/writeback-row-missing
                       {:object-type obj-kw :table (:table tgt) :pk-value pk-val}))
    (let [spec (with-who (wb/reshape tgt values live only) actor correlation)]
      (if (empty? (:set spec))
        (outcome tgt nil :no-change)
        (outcome tgt spec (push-logged! store :writeback-push tgt spec effect-gate
                                        {:object-type obj-kw :pk-value pk-val
                                         :set (:set spec) :expect (:expect spec)}))))))

(def claim-ttl-ms
  (* 10 60 1000))

(defn- match-value
  [tgt values]
  (let [vs (mapv #(get values %) (:match-attrs tgt))]
    (when (and (seq vs) (not-any? #(str/blank? (str %)) vs))
      vs)))

(defn- claim-where
  [tgt match-val]
  {:source (name (:source-id tgt)) :target-table (str (:table tgt))
   :match-value (str/join "|" (map (comp str/trim str) match-val))})

(defn- release-claim!
  [store tgt match-val]
  (when (and store match-val)
    (ctl/commit! store [(fn [store _]
                          (ctl/deleting store :writeback-claims (claim-where tgt match-val)))])
    nil))

(defn- claim!
  [store tgt match-val]
  (let [where (claim-where tgt match-val)
        stale? (fn [{:keys [claimed-at]}]
                 (< (+ (.getTime ^java.util.Date claimed-at) claim-ttl-ms)
                    (ctl/millis store)))]
    (some? (first (ctl/commit! store [(fn [store at]
                                      (ctl/swapping store :writeback-claims where
                                                     (fn [old]
                                                       (when (or (nil? old) (stale? old)) {}))
                                                     at))])))))

(defn- created
  [tgt base insert-spec res]
  (case (:mode tgt)
    :jdbc
    (let [attr-of (into {} (for [[a c] (:column-map tgt)] [(name c) a]))]
      (merge base {:identity    (:identity res) :deduped? false
                   :number      (:number res)
                   :number-attr (when (:number res) (:number-attr tgt))
                   :drawn-attrs (into {} (for [[c v] (:drawn res)
                                               :let [a (get attr-of (name c))]
                                               :when a]
                                           [a v]))
                   :insert-spec insert-spec :result res}))
    :webhook
    (merge base {:identity (:identity res) :deduped? false
                 :insert-spec insert-spec :result (:result res)})))

(defn create!
  ([schema obj-kw values] (create! schema obj-kw values {}))
  ([schema obj-kw values
    {:keys [on-duplicate actor correlation effect-gate store write-session]}]
   (let [tgt         (assoc (writeback-target schema obj-kw store) :write-session write-session)
         base        {:object-type obj-kw :table (:table tgt)
                      :source-id (:source-id tgt) :mode (:mode tgt)
                      :pk-attr (:pk-attr tgt)}
         match-val   (match-value tgt values)
         claimed?    (or (nil? match-val) (claim! store tgt match-val))
         existing    (when match-val (wb/find-by-match-key tgt match-val))
         on-dup      (or on-duplicate (:on-duplicate tgt) :bind)]
     (when-not (or claimed? existing)
       (refusal/refuse! :refusal.sources/writeback-create-in-flight
                        (assoc base :conflict true :match-key (:match-key tgt)
                               :match-value match-val)))
     (if existing
       (if (= :abort on-dup)
         (refusal/refuse! :refusal.foreign/duplicate-row
                          (assoc base :match-key (:match-key tgt) :match-value match-val
                                 :existing (get existing (:pk-attr tgt)))
                          {:match (str/join ", " (map #(str %1 " " (pr-str %2))
                                                      (:match-key tgt) match-val))})
         (merge base {:identity (get existing (:pk-attr tgt))
                      :deduped? true :existing existing}))
       (let [insert-spec (with-who (wb/reshape-insert tgt values) actor correlation)]
         (try
           (created tgt base insert-spec
                    (case (:mode tgt)
                      :jdbc    (wb/push! tgt insert-spec effect-gate)
                      :webhook (wb/push-create-webhook! tgt insert-spec effect-gate)
                      (refusal/refuse! :refusal.sources/writeback-unknown-mode
                                       base
                                       {:mode (:mode tgt)})))
           (catch Throwable t
             (release-claim! store tgt match-val)
             (throw t))))))))

(defn delete!
  ([schema obj-kw values] (delete! schema obj-kw values {}))
  ([schema obj-kw values {:keys [actor correlation effect-gate store write-session]}]
   (let [tgt           (assoc (writeback-target schema obj-kw store) :write-session write-session)
         [pk-val live] (live-row tgt obj-kw values "writeback delete")]
     (if-not live
       (outcome tgt nil :already-gone)
       (let [spec (with-who (wb/reshape-delete tgt values live) actor correlation)
             res  (outcome tgt spec (push-logged! store :writeback-delete tgt spec effect-gate
                                                  {:object-type obj-kw :pk-value pk-val
                                                   :expect (:expect spec)}))]
         (release-claim! store tgt (match-value tgt live))
         res)))))

(def module
  {:name     :writeback-dataset
   :provides {:writeback/create create!
              :writeback/update push-values!
              :writeback/delete delete!}})
