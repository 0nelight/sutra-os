(ns sutra.builds.import.nodes
  (:require [sutra.kit.refusal :as refusal]
            [sutra.projects.api :as projects]
            [sutra.store.edits :as edits]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.access.row-shape :as row-shape]
            [sutra.catalog.backing :as backing]
            [sutra.catalog.transforms :as transforms]
            [sutra.catalog.outputs :as catalog-outputs]
            [sutra.types.world-rules :as rules]
            [sutra.types.naming :as naming]
            [sutra.catalog.changes :as changes]
            [sutra.foundation.encoding :as enc]
            [sutra.builds.import.coerce :as coerce]
            [sutra.builds.plan.inputs :as inputs]
            [sutra.builds.import.materialize :as mat]
            [sutra.builds.plan :as plan]
            [sutra.builds.plan.restrictions :as restrict]
            [sutra.builds.plan.expectations :as expect]
            [sutra.types.column :as column]
            [sutra.builds.plan.outputs :as outputs]))

(set! *warn-on-reflection* true)

(defn- map-columns
  [row column-map attrs]
  (let [explicit (reduce-kv (fn [m attr-kw col-kw]
                              (assoc m attr-kw (get row col-kw)))
                            {} column-map)]
    (reduce (fn [m attr-kw]
              (if (or (nil? attr-kw) (contains? m attr-kw))
                m
                (let [col (column/attr->col attr-kw)]
                  (cond
                    (contains? row col)     (assoc m attr-kw (get row col))
                    (contains? row attr-kw) (assoc m attr-kw (get row attr-kw))
                    :else                   m))))
            explicit (or attrs []))))

(defn- canon-col
  [k]
  (column/attr->col (if (keyword? k) k (keyword (str k)))))

(defn- expected-output-cols
  [schema obj-type column-map]
  (let [attrs   (outputs/attr-names (get-in schema [:object-types obj-type]))
        fk-cols (for [lt-kw (keys (:link-types schema))
                      :let  [{:keys [fk-type fk-attribute]} (rules/fk-link-spec schema lt-kw)]
                      :when (and fk-attribute (= obj-type fk-type))]
                  fk-attribute)]
    (into #{(canon-col :is_deleted)}
          (map canon-col)
          (concat attrs fk-cols (vals (or column-map {}))))))

(defn- dropped-output-cols
  [schema obj-type column-map row]
  (let [expected (expected-output-cols schema obj-type column-map)]
    (into #{} (remove #(contains? expected (canon-col %))) (keys row))))

(defn- source-name
  [obj-type from]
  (or (:datasource from) (:dataset from) (get-in from [:compute :transform-name]) obj-type))

(defn- staged-rows
  [schema obj-type {:keys [primary-key column-map transforms]} rows]
  (let [attrs     (outputs/attr-names (get-in schema [:object-types obj-type]))
        nil-blank (fn [m] (reduce-kv (fn [acc k v] (assoc acc k (if (= v "") nil v))) {} m))]
    (mapv (fn [row]
            (let [vals (-> row (map-columns column-map attrs)
                           (coerce/apply-transforms transforms) nil-blank)]
              {:raw row :tombstone? (row-shape/tombstone? row) :values vals
               :pk (get vals primary-key)}))
          rows)))

(defn- timestamp-attr
  [schema obj-type entry]
  (edits/timestamp-attr (or (:edit-conflict entry)
                            (get-in schema [:object-types obj-type :edit-conflict]))))

(defn- stamp
  [ts-attr values]
  {:at (when ts-attr (get values ts-attr)) :attrs (vec (keys values))})

(defn- import-one-source
  [{:keys [store schema run-id] :as ctx} obj-type resolved?
   {:keys [from primary-key column-map output-name] :as entry}]
  (when-not primary-key
    (refusal/refuse! :refusal.builds/object-import-no-key {:object-type obj-type}))
  (let [rows   (->> (inputs/read-source-rows ctx from output-name)
                    (expect/checked-output ctx from output-name))
        src    (source-name obj-type from)
        ts     (timestamp-attr schema obj-type entry)
        staged (staged-rows schema obj-type entry rows)]
    (when-let [compute (:compute from)]
      (when-let [ds (:dataset (backing/object-backing schema obj-type))]
        (inputs/check-strict-append! ctx compute ds
                                     (keep #(when-not (:tombstone? %)
                                              (enc/object-id obj-type (:pk %)))
                                           staged))))
    (when-let [t-name (get-in from [:compute :transform-name])]
      (when-let [row0 (first rows)]
        (inputs/note! ctx :dropped-cols t-name
                      (dropped-output-cols schema obj-type column-map row0))))
    (restrict/check-rows! {:schema schema :object-type obj-type
                           :primary-key primary-key :source src}
                          staged)
    (mapv (fn [{:keys [tombstone? values pk]}]
            (let [eid (when resolved? (mat/resolve! store schema obj-type values))]
              (when (and eid (not tombstone?))
                (mat/record-provenance! store eid values src run-id))
              {:id (or eid (enc/object-id obj-type pk)) :pk pk :values values
               :stamps [(stamp ts values)] :source src :tombstone? tombstone?}))
          staged)))

(defn- import-object-node
  [{:keys [store schema run-id] :as ctx} obj-type entry]
  (let [entries    (plan/entries entry)
        resolved?  (outputs/resolved-object-type? schema obj-type)
        all-rows   (into []
                         (mapcat #(import-one-source ctx obj-type resolved? %))
                         entries)
        {live false tombs true} (group-by (comp boolean :tombstone?) all-rows)
        final-rows (if resolved?
                     (vec (mat/consolidate-rows live (mat/survivorship-spec schema obj-type)))
                     live)
        live-ids   (set (map :id final-rows))
        retr       (vals (reduce
                          (fn [m {:keys [id source]}]
                            (let [surv (when resolved?
                                         (projects/retract-provenance! store id source run-id)
                                         (mat/survived-after-retraction
                                          schema obj-type id
                                          (projects/latest-provenance-by-source store id)))]
                              (assoc m id {:id id :survived surv})))
                          {} (remove #(live-ids (:id %)) tombs)))
        del-ids    (into [] (comp (filter #(nil? (:survived %))) (map :id)) retr)
        upd-rows   (into [] (keep #(when (:survived %)
                                     {:id (:id %) :values (:survived %)})) retr)]
    {:obj-decls (-> (mapv (fn [{:keys [id values stamps]}]
                            (changes/create-object id obj-type values stamps))
                          final-rows)
                    (into (map (fn [{:keys [id values]}]
                                 (changes/update-object id obj-type values)))
                          upd-rows)
                    (into (map (fn [id] (changes/delete-object id obj-type))) del-ids))
     :pk-index  {obj-type (into {} (map (juxt :pk :id)) all-rows)}
     :rows      {obj-type (into (vec final-rows) upd-rows)}}))

(defn- retract-supplement
  [values cols]
  (reduce (fn [m c] (if (contains? values c) m (assoc m c nil))) values cols))

(defn- merges-into-output?
  [{:keys [full-refresh?] :as ctx} {{t :compute} :from}]
  (and t (transforms/incremental? t) (transforms/modify-write? t)
       (not full-refresh?) (not (inputs/snapshot-run? ctx t))))

(defn- merge-supplement-into-rows
  [existing-rows held-rows supp-rows primary-key keep-absent?]
  (let [_      (restrict/check-duplicate-pks!
                supp-rows
                {:what "Supplement"
                 :hint (str "A §7b supplement merges its columns onto one row per PK."
                            " The join resolved a FK-side row to more than one target —"
                            " deduplicate to one row per key IN the supplement query with"
                            " a deterministic tiebreak (the pipeline, not a resolve-time"
                            " coin-flip).")})
        by-pk  (into {} (map (juxt :pk :values)) supp-rows)
        cols   (disj (into #{} (mapcat (comp keys :values)) supp-rows) primary-key)
        merge1 (fn [{:keys [pk values] :as row}]
                 (if-let [add (get by-pk pk)]
                   (assoc row :values (merge values add))
                   (cond-> row
                     (not keep-absent?) (assoc :values (retract-supplement values cols)))))
        moved  (keep (fn [{:keys [values] :as row}]
                       (let [m (merge1 (cond-> row
                                         (not keep-absent?) (assoc :values
                                                                   (apply dissoc values cols))))]
                         (when (some #(not= (get values %) (get-in m [:values %])) cols) m)))
                     held-rows)
        merged (into (mapv merge1 existing-rows) moved)]
    {:merged  merged
     :touched (if (seq cols) merged [])}))

(defn- supplement-stamp
  [supp-rows primary-key]
  (let [by-pk (into {} (map (juxt :pk (comp first :stamps))) supp-rows)
        gone  {:at nil :attrs (vec (disj (into #{} (mapcat (comp keys :values)) supp-rows)
                                         primary-key))}]
    (fn [{:keys [pk] :as row}] (update row :stamps (fnil conj []) (get by-pk pk gone)))))

(defn- staging-backing
  [schema obj-type]
  (let [b (backing/object-backing schema obj-type)]
    (when-not (or (:authored? b)
                  (= (naming/dataset-table (:dataset b)) (naming/obj-table obj-type)))
      b)))

(defn- held-rows
  [{:keys [store schema]} acc obj-type run-rows primary-key]
  (when-let [{:keys [dataset]} (when-not (= :snapshot (get-in acc [:landed obj-type :mode]))
                                 (staging-backing schema obj-type))]
    (let [attrs (outputs/attr-names (get-in schema [:object-types obj-type]))
          ran   (into #{} (map (comp str :id)) run-rows)]
      (into []
            (keep (fn [{:keys [id values]}]
                    (when-not (or (ran (str id))
                                  (row-shape/tombstone?
                                   {row-shape/deletion-key (:is-deleted values)}))
                      (let [vs (select-keys values attrs)]
                        {:id id :pk (get vs primary-key) :values vs}))))
            (mat/staged-source-rows store schema dataset)))))

(defn- import-supplement-node
  [ctx acc obj-type spec]
  (let [pk        (:primary-key spec)
        run-rows  (get (:rows acc) obj-type [])
        supp-rows (remove :tombstone? (import-one-source ctx obj-type false spec))
        {:keys [merged touched]} (merge-supplement-into-rows
                                  run-rows (held-rows ctx acc obj-type run-rows pk) supp-rows pk
                                  (merges-into-output? ctx spec))
        stamped   (if (seq touched) (supplement-stamp supp-rows pk) identity)]
    {:obj-decls (mapv (fn [row]
                        (let [{:keys [id values stamps]} (stamped row)]
                          (changes/update-object id obj-type values stamps)))
                      touched)
     :rows      {obj-type (mapv stamped merged)}}))

(defn coalesce-object-decls
  [decls]
  (let [obj-key (fn [{:keys [kind verb type row]}]
                  (when (and (= :objects kind) (#{:create :update} verb)) [type (:id row)]))]
    (:out (reduce (fn [{:keys [out at] :as acc} d]
                    (let [k (obj-key d) i (get at k)]
                      (if (and i (= :update (:verb d)))
                        (update-in acc [:out i :row]
                                   (fn [{:keys [stamps] :as r}]
                                     (cond-> (update r :values merge (get-in d [:row :values]))
                                       (or stamps (get-in d [:row :stamps]))
                                       (assoc :stamps (into (vec stamps)
                                                            (get-in d [:row :stamps]))))))
                        (cond-> (update acc :out conj d)
                          k (assoc-in [:at k] (count out))))))
                  {:out [] :at {}}
                  decls))))

(defn endpoints
  [acc]
  {:rows      (merge (:fresh-rows acc) (:rows acc))
   :pks       (merge (:fresh-pk-index acc) (:pk-index acc))
   :writeback (:writeback-index acc)})

(defn- endpoint-index
  [{:keys [pks writeback]} t]
  (or (get pks t) (get writeback t)))

(defn- import-join-table-conn
  [read-rows {:keys [from from-type from-key-column to-type to-key-column]} ends]
  (let [rows     (read-rows from)
        from-map (endpoint-index ends from-type)
        to-map   (endpoint-index ends to-type)]
    (when (and from-map to-map)
      (into []
            (keep (fn [row]
                    (let [fk (get row from-key-column)
                          tk (get row to-key-column)
                          fid (get from-map fk)
                          tid (get to-map tk)]
                      (when (and fid tid)
                        {:from-id fid :to-id tid}))))
            rows))))

(defn- import-resolve-by-shared-property-conn
  [read-rows conn-type
   {:keys [from from-type to-type from-key-column match-column shared-property]}
   ends]
  (let [pairs   (read-rows from)
        from-ix (or (endpoint-index ends from-type)
                    (throw (ex-info "resolve-by-shared-property: from-type has no imported rows"
                                    {:link conn-type :from-type from-type})))
        to-ix   (into {}
                      (keep (fn [{:keys [id values]}]
                              (when-let [v (get values shared-property)]
                                [v id])))
                      (or (get (:rows ends) to-type)
                          (throw (ex-info "resolve-by-shared-property: to-type has no imported rows"
                                          {:link conn-type :to-type to-type}))))]
    (into []
          (keep (fn [row]
                  (let [fid (get from-ix (get row from-key-column))
                        tid (get to-ix (get row match-column))]
                    (when (and fid tid) {:from-id fid :to-id tid}))))
          pairs)))

(defn- import-link-node
  [read-rows conn-type spec ends]
  (let [pairs (case (or (:strategy spec) :join-table)
                :join-table
                (import-join-table-conn read-rows spec ends)
                :resolve-by-shared-property
                (import-resolve-by-shared-property-conn read-rows conn-type spec ends)
                :polymorphic
                (into [] (mapcat #(import-join-table-conn read-rows % ends)) (:sources spec))
                (refusal/refuse! :refusal.builds/link-import-strategy
                                 {:link conn-type :strategy (:strategy spec)}))]
    {:link-decls (mapv (fn [{:keys [from-id to-id]}] (changes/connect conn-type from-id to-id))
                       pairs)
     :conn-pairs {conn-type pairs}}))

(defn- endpoint-value-index
  [rows]
  (let [raw (persistent!
             (reduce
              (fn [m {:keys [id values]}]
                (reduce-kv
                 (fn [m _ v]
                   (if (or (nil? v) (coll? v))
                     m
                     (let [prev (get m v ::absent)]
                       (cond
                         (= prev ::absent) (assoc! m v id)
                         (= prev id)       m
                         :else             (assoc! m v ::ambiguous)))))
                 m values))
              (transient {}) rows))
        clean (into {} (remove (comp #{::ambiguous} val)) raw)]
    {:index clean :ambiguous (- (count raw) (count clean))}))

(defn writeback-endpoint-types
  [links pk-index]
  (into #{} (comp (mapcat (juxt :from-type :to-type))
                  (remove nil?)
                  (remove #(contains? pk-index %)))
        (vals links)))

(defn writeback-index
  [t rows]
  (let [{:keys [index ambiguous]} (endpoint-value-index rows)]
    (when (pos? (long ambiguous))
      (println
       (format "  ⚠ endpoint index for %s: %d value(s) identify more than one row — left unresolved rather than resolved to an arbitrary one"
               (name t) ambiguous)))
    index))

(defn seed-writeback-endpoints
  [rows-of links pk-index]
  (into {} (map (fn [t] [t (writeback-index t (rows-of t))]))
        (writeback-endpoint-types links pk-index)))

(defn- stage!
  [store schema obj-type rows]
  (when-let [{:keys [dataset snapshot?]} (staging-backing schema obj-type)]
    (let [mode (if snapshot? :snapshot :append)]
      (dsets/write-rows! store dataset mode rows)
      {:staged {obj-type dataset}
       :landed {obj-type {:mode mode :rows rows}}})))

(defn- one-to-one-violations
  [{:keys [fk-side from-type to-type fk-attribute]} ends]
  (let [fk-t    (if (= :from fk-side) from-type to-type)
        pk-t    (if (= :from fk-side) to-type from-type)
        pk-ix   (or (endpoint-index ends pk-t) {})
        matched (keep (fn [{:keys [values]}]
                        (let [v (get values fk-attribute)]
                          (when (and (some? v) (contains? pk-ix v)) v)))
                      (get (:rows ends) fk-t))]
    (into [] (keep (fn [[v n]] (when (> n 1) [v n]))) (frequencies matched))))

(defn- check-one-to-one!
  [conn-type spec ends]
  (let [dups (one-to-one-violations spec ends)]
    (when (seq dups)
      (refusal/refuse! :refusal.builds/one-to-one-duplicates
                       {:link conn-type
                        :duplicate-count (count dups)
                        :duplicate-sample (vec (take 5 (map first dups)))}
                       {:fk (:fk-attribute spec)}))
    (count dups)))

(defn nodes-of
  [schema {:keys [objects links supplements]}]
  (let [producer (fn [kind target] (:producer (backing/backing-of schema kind target)))
        name->tk (into {} (for [[k t] (:transforms schema), o (catalog-outputs/outputs-of t)]
                            [(:name o) k]))]
    (concat
     (for [[ot entry] objects
           :let [authored? (:authored? (backing/object-backing schema ot))]]
       {:kind :object :key ot :spec entry
        :t-kw (when-not authored? (producer :object-type ot))
        :level (when authored? -1)})
     (for [[ot entries] supplements, entry entries]
       {:kind :supplement :key ot :spec entry :t-kw (get name->tk (:output-name entry))})
     (for [[ct spec] links
           :let [fk? (= :endpoint-fk (:strategy spec))
                 one-to-one? (= :one-to-one (:cardinality spec))]
           :when (or (not fk?) one-to-one?)]
       {:kind :link :key ct :spec spec :t-kw (producer :link-type ct) :fk? fk?
        :withholds (when fk? [(:from-type spec) (:to-type spec)])}))))

(defn- staged
  [{:keys [store schema]} ot delta]
  (let [rows (get-in delta [:rows ot])]
    {:delta (merge delta (stage! store schema ot rows)) :rows-out (count rows)}))

(defn object-job
  [{:keys [ctx] :as env} _ {ot :key entry :spec}]
  (staged env ot (import-object-node ctx ot entry)))

(defn supplement-job
  [{:keys [ctx] :as env} acc {ot :key entry :spec}]
  (staged env ot (import-supplement-node ctx acc ot entry)))

(defn link-job
  [{:keys [ctx]} acc {ct :key spec :spec fk? :fk?}]
  (if fk?
    (do (check-one-to-one! ct spec (endpoints acc))
        {:delta {} :rows-out 0})
    (let [read #(expect/checked-output ctx % nil (inputs/read-source-rows ctx % nil))
          d    (import-link-node read ct spec (endpoints acc))]
      {:delta d :rows-out (count (get-in d [:conn-pairs ct]))})))

(defn without-failed-types
  [schema acc fs]
  (let [failed   (into #{} (keep :job) fs)
        rejected (into #{} (keep :t-kw) (:restriction-failures acc))
        gone     (-> #{}
                     (into (comp (filter #(= :supplement (:kind %))) (keep :key)) fs)
                     (into (comp (mapcat :withholds) (remove nil?)) fs)
                     (into (filter #(failed (:producer (backing/backing-of schema :object-type %))))
                           (keys (:rows acc)))
                     (into
                      (filter #(rejected (:producer (backing/backing-of schema :object-type %))))
                      (keys (:object-types schema))))]
    (cond-> (assoc acc :withheld gone)
      (seq gone)
      (-> (update :obj-decls #(into [] (remove (fn [d] (gone (:type d)))) %))
          (update :rows #(apply dissoc % gone))
          (update :pk-index #(apply dissoc % gone))))))
