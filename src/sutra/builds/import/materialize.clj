(ns sutra.builds.import.materialize
  (:require [sutra.kit.coll :as coll]
            [sutra.kit.refusal :as refusal]
            [sutra.sources.api :as sources]
            [sutra.builds.ports :as ports]
            [sutra.store.access.scope :as ctx]
            [sutra.catalog.grants :as grants]
            [sutra.kit.coerce :refer [->name-kw]]
            [sutra.kit.digest :as digest]
            [sutra.kit.err :as err]
            [sutra.types.languages :as langs]
            [sutra.types.attribute :as attribute]
            [clojure.string :as str]
            [sutra.types.naming :as naming]
            [sutra.catalog.outputs :as outputs]
            [sutra.catalog.lookup :as lookup]
            [sutra.types.kinds :as kinds]
            [sutra.types.table :as tbl]
            [sutra.projects.api :as projects]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.access.row-shape :as row-shape]
            [sutra.store.access.substrate :as sub]
            [sutra.store.edits :as edits]
            [sutra.store.world :as world]
            [sutra.types.column :as column]
            [sutra.types.rid :as rid]
            [sutra.types.virtual :as virtual]
            [sutra.parser.params :as params]
            [sutra.kit.noted :as noted]
            [sutra.store.control :as ctl]
            [sutra.kit.uuid :as uuid]
            [sutra.ref.value :as rv]
            [sutra.builds.plan.outputs :refer
             [identity-keys-for]])
  (:import [java.util UUID]))

(set! *warn-on-reflection* true)

(declare consolidate-rows er-namespace ext-rows materialize-source! mint-entity-id
         present-identifiers record-provenance! resolve! resolve-input-table run-transform! sanitize
         short-sha source-rank staging-table-name survivorship-spec write-staging-table!)

(defn- short-sha [^String s]
  (subs (digest/hex (digest/digest "SHA-1" s)) 0 16))

(defn- sanitize [s]
  (str/replace (str s) #"[^A-Za-z0-9_]" "_"))

(defn- staging-table-name
  [{:keys [datasource table query]}]
  (str "s_" (sanitize (name datasource)) "_"
       (cond
         table (sanitize table)
         query (str "q" (short-sha query))
         :else (refusal/refuse! :refusal.builds/datasource-input-incomplete
                                {:datasource datasource}))))

(defn- ext-rows
  [connect spec input-entry]
  (when (:external-transform spec)
    (refusal/refuse! :refusal.builds/external-transform-source
                     {:type (:type spec) :external-transform true}
                     {:type (name (:type spec))}))
  (sources/with-source [s connect spec]
    (let [rows   (cond
                   (sources/one-file? spec)     (sources/scan s (or (:table input-entry) "csv"))
                   (:query input-entry)         (sources/read s (:query input-entry))
                   (:table input-entry)         (sources/scan s (:table input-entry))
                   :else (refusal/refuse! :refusal.builds/datasource-input-incomplete
                                          {:datasource (:datasource input-entry)}))
          header (vec (or (some-> (first rows) keys (->> (map name)) vec) []))]
      [header (mapv (fn [r] (mapv #(get r (keyword %)) header)) rows)])))

(defn- write-staging-table!
  [store tname header rows]
  (dsets/write-scratch! store tname (mapv sanitize header) rows))

(defn materialize-source!
  [store _schema datasources cache input-entry]
  (let [tname (staging-table-name input-entry)]
    (when-not (contains? @cache tname)
      (let [ds-kw (:datasource input-entry)
            spec  ((ports/foreign-datasources store :cleared!)
                   (grants/source-authority store)
                   (or (get datasources ds-kw)
                       (refusal/refuse! :refusal.builds/inputs-unknown-datasource
                                        {:datasource ds-kw})))
            [header rows] (ext-rows (ctx/connect (grants/source-authority store)) spec input-entry)]
        (write-staging-table! store tname header rows)
        (swap! cache assoc tname true)))
    tname))

(defn virtual-staging-table
  [ds]
  (str "v_" (sanitize (name (:dataset-name ds)))))

(defn stage-virtual!
  [store cache ds read!]
  (let [tname (virtual-staging-table ds)]
    (when-not (contains? @cache tname)
      (let [rows   (read!)
            header (or (not-empty (mapv (comp name virtual/column-key :attribute-name)
                                        (:columns ds)))
                       (vec (sort (into #{} (comp (mapcat keys) (map name)) rows))))]
        (write-staging-table! store tname header
                              (mapv (fn [r] (mapv #(get r (keyword %)) header)) rows))
        (swap! cache assoc tname true)))
    tname))

(defn column-rows
  [objects]
  (mapv (fn [{:keys [id values]}]
          {:id id
           :values (reduce-kv (fn [m a v] (assoc m (column/attr->col a) v))
                              {}
                              (dissoc values (keyword naming/markings-col)
                                      (keyword (str/replace naming/markings-col "_" "-"))))})
        objects))

(defn derived-attrs
  [ot-spec]
  (:derived (attribute/of ot-spec)))

(defn- stored-column-rows
  [schema ot objects]
  (let [spec    (get-in schema [:object-types (or (lookup/rid-of schema :object-types ot) ot)])
        derived (map (comp column/attr->col first) (derived-attrs spec))]
    (mapv #(update % :values (fn [v] (apply dissoc v derived))) (column-rows objects))))

(defn object-rows
  [reads schema ot]
  (stored-column-rows schema ot (sub/select reads {:root ot})))

(defn under-edits
  ([source edits current] (under-edits source edits current nil))
  ([source edits current ts-attr]
   (let [by-id (into {} (map (juxt (comp str :id) :values)) source)]
     (into []
           (keep (fn [id]
                   (let [e  (get edits id)
                         vs (get by-id id)
                         r  (edits/resolve-object
                             e vs (when e (edits/source-times ts-attr {:values vs})))]
                     (cond (not (:visible? r))  nil
                           (:follows-source? r) {:id id :values (:values r)}
                           :else (or (get current id) {:id id :values (:values r)})))))
           (distinct (concat (map (comp str :id) source) (keys edits)))))))

(defn staged-source-rows
  [store schema dataset]
  (let [arr (row-shape/array-keys-for store schema (naming/dataset-table dataset)
                                      #(keyword (str/replace (name %) "_" "-")))]
    (mapv #(update % :values (partial row-shape/decode-array-vals arr))
          (dsets/rows store dataset))))

(defn staged-object-rows
  [store schema ot dataset]
  (let [reads   (sub/reader store)
        current (coll/index-by (comp str :id) (sub/select reads {:root ot}))]
    (stored-column-rows schema ot (under-edits (staged-source-rows store schema dataset)
                                               (world/edited store ot) current
                                               (edits/timestamp-attr
                                                (get-in schema [:object-types ot
                                                                :edit-conflict]))))))

(def landing-prefix
  "l_")

(defn landing
  [schema ot at-sec suffix]
  (let [t    (sanitize (subs (naming/obj-table ot) 2))
        spec (get-in schema [:object-types (or (lookup/rid-of schema :object-types ot) ot)])]
    {:name    (str landing-prefix (subs t 0 (min 40 (count t))) "_" (Long/toString (long at-sec) 36)
                   "_" suffix)
     :columns (into []
                    (comp (remove #(attribute/derived-expression (attribute/normalize %)))
                          (map #(select-keys % [:attribute-name :attribute-type])))
                    (:attribute-list spec))}))

(def landing-max-age-sec
  (* 6 3600))

(defn stale-landings
  [names now-sec max-age]
  (filterv (fn [n]
             (when-let [[_ at] (re-find #"_([0-9a-z]+)_[0-9a-f]{8}$" (str n))]
               (and (str/starts-with? (str n) landing-prefix)
                    (< (Long/parseLong at 36) (- now-sec max-age)))))
           names))

(defn- land-objects!
  ([store schema landing* ot] (land-objects! store schema landing* ot nil))
  ([store schema landing* ot staged-dataset]
   (or (get @landing* ot)
       (let [{tname :name :keys [columns]} (landing schema ot
                                                    (quot (ctl/millis store) 1000)
                                                    (subs (str (random-uuid)) 0 8))]
         (vswap! landing* assoc ot tname)
         (dsets/write-scratch-rows! store tname columns
                                    (mapv (fn [{:keys [id values]}]
                                            (assoc values naming/pk-col (str id)))
                                          (if staged-dataset
                                            (staged-object-rows store schema ot staged-dataset)
                                            (object-rows (sub/reader store) schema ot))))
         tname))))

(defn drop-landed!
  [store landed]
  (doseq [t (vals @landed)]
    (noted/quietly "landing drop" (dsets/drop-scratch! store [t]))))

(defn with-landing
  [store schema f]
  (let [landed (volatile! {})]
    (try (f (partial land-objects! store schema landed))
         (finally (drop-landed! store landed)))))

(defn resolve-input-table
  [schema staged input-entry land]
  (case (kinds/physical (:kind input-entry))
    :staged          (staging-table-name input-entry)
    :transform-table (naming/trans-table (outputs/transform-input-key schema input-entry))
    :landed          (let [ot (:object-type input-entry)]
                       (if-let [ds (get staged ot)]
                         (land ot ds)
                         (land ot)))
    :dataset-table
    (let [ds-rid (lookup/rid-of schema :datasets (kinds/input-ref-val input-entry))
          ds     (get-in schema [:datasets ds-rid])]
      (if (virtual/virtual? ds)
        (virtual-staging-table ds)
        (naming/dataset-table ds-rid)))))

(defn shape-of
  ([schema ot] {:columns (:columns (landing schema ot 0 ""))})
  ([schema ot _staged-dataset] (shape-of schema ot)))

(defn- store-inputs
  [added-ids input->phys added]
  (into {}
        (map (fn [[alias phys]]
               (let [bnd (get added (keyword alias))
                     ids (when (and bnd (:low bnd))
                           (if (and (dsets/snapshot-id (:low bnd))
                                    (dsets/snapshot-id (:high bnd)))
                             (try (added-ids (str phys) (:low bnd) (:high bnd))
                                  (catch clojure.lang.ExceptionInfo e
                                    (if (:store/refusal (ex-data e)) ::whole []))
                                  (catch InterruptedException e (throw e))
                                  (catch Exception e (noted/note "input delta" e) []))
                             []))]
                 [alias (cond-> (if (map? phys) phys {:table (str phys)})
                          (and ids (not= ::whole ids)) (assoc :ids ids))])))
        input->phys))

(defn- plan-inputs
  [added-ids schema {:keys [inputs incremental query]
                     ::keys [prev-table prev-empty? added staged]} land]
  (let [input->phys (into {} (map (fn [[alias-kw entry]]
                                    [(name alias-kw)
                                     (resolve-input-table schema staged entry land)]))
                          inputs)
        prev        (when (and incremental prev-table (re-find #"(?i)\bprevious\b" (str query)))
                      (if (vector? prev-table)
                        (resolve-input-table schema nil
                                             {:kind :object-type :object-type (second prev-table)}
                                             land)
                        prev-table))]
    (cond-> (store-inputs added-ids (cond-> input->phys prev (assoc "previous" prev)) added)
      (and prev prev-empty?) (assoc-in ["previous" :ids] []))))

(defn- in-place?
  [store language]
  (dsets/answers-in-place? store (langs/dialect language)))

(defn run-query!
  [store language inputs query]
  (if (in-place? store language)
    (dsets/query-tables store inputs query {})
    ((ports/compute-runner store :run-query!)
     language {:query query :inputs (dsets/runner-inputs store inputs {:rows? true})})))

(defn run-transform!
  [store schema {:keys [transform-name query language] :as compute}]
  (when-not query
    (refusal/refuse! :refusal.builds/sql-transform-no-query {:transform transform-name}))
  (with-landing store schema
    (fn [land]
      (run-query! store (->name-kw language)
                  (plan-inputs (partial dsets/added-ids store) schema compute land)
                  query))))

(def ^:private output-tables
  (tbl/per-key "materialize/output-tables" kinds/output-kinds
               {:dataset            (fn [o] (naming/dataset-table (:dataset o)))
                :object-type        (fn [o] [:object-type (:object-type o)])
                #{:link-type :media-set :export} nil}))

(defn output-table [o] (tbl/on output-tables (:kind o) o))

(defn- previous-table
  [{:keys [output outputs]}]
  (some-> (or output (first outputs)) output-table))

(defn- sql-exception
  [^Throwable e]
  (err/cause #(instance? java.sql.SQLException %) e))

(defn- explain-query!
  [store language on-store query]
  (if (in-place? store language)
    (dsets/query-tables store on-store query {:explain? true})
    ((ports/compute-runner store :run-query!)
     language {:query query :inputs (dsets/runner-inputs store on-store {:rows? false})})))

(defn- explain-on-store
  [store language query on-store]
  (try
    (explain-query! store language on-store query)
    {:state :ok}
    (catch InterruptedException e (throw e))
    (catch Exception e
      (let [^java.sql.SQLException se (sql-exception e)
            missing (:datasets/missing-input (ex-data e))]
        (cond
          missing {:state :unknown :missing [missing]}
          (and se (not (str/starts-with? (str (.getSQLState se)) "08")))
          {:state :refused :detail (ex-message se)}
          :else {:state :unknown :detail (ex-message e)})))))

(defn explain-transform
  [store schema spec]
  (let [t (params/substitute-transform spec)
        language (->name-kw (:language t))
        prev (when (:incremental t) (previous-table t))
        unresolved (vec (for [[alias entry] (:inputs t)
                              :when (and (not= :datasource (:kind entry))
                                         (not (rid/rid? (kinds/input-ref-val entry))))]
                          (name alias)))]
    (cond
      (seq unresolved)
      {:state :unknown :missing unresolved}

      (not (or (in-place? store language) ((ports/compute-runner store :query-runnable?) language)))
      {:state :refused :detail (str "no runner reaches a " (name language) " engine")}

      :else
      (let [r (try (let [store (ctx/as-system store)]
                     {:store store
                      :on-store (plan-inputs (partial dsets/added-ids store) schema
                                             (cond-> t prev (assoc ::prev-table prev))
                                             (partial shape-of schema))})
                   (catch InterruptedException e (throw e))
                   (catch Exception e {:failed (ex-message e)}))]
        (if (:failed r)
          {:state :unknown :detail (:failed r)}
          (explain-on-store (:store r) language (:query t) (:on-store r)))))))

(def ^:private er-namespace
  (UUID/fromString "6ba7b811-9dad-11d1-80b4-00c04fd430c8"))

(defn mint-entity-id
  [object-type id-kind id-value]
  (str (uuid/v5 er-namespace
                (str (name object-type) "/" (name id-kind) "/" id-value))))

(defn present-identifiers
  [schema object-type id-map]
  (vec (keep (fn [k] (when-let [v (get id-map k)] [k v]))
             (identity-keys-for schema object-type))))

(defn resolve!
  [store schema object-type id-map]
  (let [present (present-identifiers schema object-type id-map)]
    (when (seq present)
      (let [known (->> present
                       (keep (fn [[k v]] (projects/crosswalk-lookup store object-type k v)))
                       distinct vec)
            entity-id (cond
                        (empty? known)
                        (let [[k v] (first present)] (mint-entity-id object-type k v))

                        (= 1 (count known)) (first known)

                        :else
                        (let [survivor (first known)]
                          (projects/crosswalk-redirect! store object-type known survivor)
                          survivor))]
        (doseq [[k v] present]
          (projects/crosswalk-put! store object-type k v entity-id))
        entity-id))))

(defn record-provenance!
  [store entity-id values source run-id]
  (projects/write-provenance! store
                              (for [[prop v] values :when (some? v)]
                                {:entity-id entity-id :property prop :value v
                                 :source source :run-id run-id})))

(defn- source-rank
  [order]
  (into {} (map-indexed (fn [i s] [s i]) order)))

(defn consolidate-rows
  [rows survivorship]
  (let [order  (or (:order survivorship)
                   (vec (distinct (map :source rows))))
        by-prop (:by-property survivorship)
        rank-for (fn [prop] (source-rank (or (get by-prop prop) order)))]
    (for [[id grp] (group-by :id rows)]
      (let [grp   (vec grp)
            props (distinct (mapcat (comp keys :values) grp))
            won   (for [p props
                        :let [r (rank-for p)
                              cands (for [[i row] (map-indexed vector grp)
                                          :let [v (rv/value row p)]
                                          :when (some? v)]
                                      [(get r (:source row) Long/MAX_VALUE) v i])]
                        :when (seq cands)]
                    (let [[_ v i] (apply min-key first cands)] [p v i]))]
        {:id id :pk (:pk (first grp))
         :values (into {} (map (fn [[p v]] [p v])) won)
         :stamps (vec (for [[i ws] (group-by #(nth % 2) won)]
                        {:at (:at (first (:stamps (grp i)))) :attrs (mapv first ws)}))}))))

(defn survivorship-spec
  [schema object-type]
  (get-in schema [:object-types object-type :survivorship]))

(defn survived-after-retraction
  [schema object-type entity-id contributions]
  (let [rows   (for [[src cs] (group-by :source contributions)]
                 {:id entity-id :source src
                  :values (into {} (map (juxt :property :value)) cs)})
        merged (first (consolidate-rows rows (survivorship-spec schema object-type)))]
    (not-empty (:values merged))))

(defn sweep-landings!
  [store]
  (doseq [b (map :name (world/branches store))
          :let [store (ctx/at store b)]
          t (stale-landings (dsets/dataset-names store landing-prefix)
                            (quot (ctl/millis store) 1000) landing-max-age-sec)]
    (try (dsets/drop-scratch! store [t])
         (println "[landings] dropped" t "on" b "— left by a read that did not end")
         (catch InterruptedException e (throw e))
         (catch Exception e (println "[landings] could not drop" t "on" b "—" (ex-message e))))))

(def module
  {:name     :materialize
   :provides {:sql/explain explain-transform}
   :jobs     [{:name :landings :every "SUTRA_LANDINGS_MS" :tick sweep-landings! :default 3600000}]})
