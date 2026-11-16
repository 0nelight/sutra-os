(ns sutra.store.lib
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.types.column :as column]
            [sutra.types.attribute :as attribute]
            [sutra.types.object-set :as os]
            [clojure.string :as str]
            [sutra.store.tx-vector :as tv]
            [sutra.ref.objectset :as ros]
            [sutra.types.aggregate :as agg]
            [sutra.types.entities :as ents]
            [sutra.types.granular :as granular]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]
            [sutra.types.tree :as tree]
            [sutra.ref.expr :as rexpr]
            [sutra.types.world-rules :as rules]
            [sutra.kit.uuid :as uuid]))

(set! *warn-on-reflection* true)

(def ^:private cardinality-ids
  {:one-to-one   {:cols [:from :to] :sorted? true}
   :many-to-one  {:cols [:from]     :sorted? false}
   :one-to-many  {:cols [:to]       :sorted? false}
   :many-to-many {:cols [:from :to] :sorted? true}})

(defn pair-id
  [sig L from to]
  (let [{:keys [cols sorted?]} (or (cardinality-ids (get-in sig [:links L :cardinality]))
                                   (cardinality-ids :many-to-many))
        row   {:from (str from) :to (str to)}
        parts (if sorted? (sort (map row cols)) (mapv row cols))]
    (str (uuid/v5 uuid/dns-namespace (str (name L) "/" (str/join "|" parts))))))

(defn- attr-type [sig T a]
  (some #(when (= a (:name %)) (:type %))
        (or (get-in sig [:types T :attrs]) (get-in sig [:interfaces T :props]))))

(defn fk-pairs
  [sig L fs ps]
  (let [{:keys [holder target side attr target-attr]} (get-in sig [:links L :fk])
        want   (attr-type sig holder attr)
        array? (and (vector? want) (= :array (first want)))
        elem   (if array? (second want) want)
        same?  (fn [x y have] (and (some? x) (some? y)
                                   (if (and have (not= elem have)) (= (str x) (str y))
                                       (rexpr/same-value? x y))))
        holds? (fn [fv pv have] (if array? (some #(same? % pv have) fv) (same? fv pv have)))
        p-cols (if target-attr
                 [[target-attr (attr-type sig target target-attr)]]
                 (for [{:keys [name type]} (or (get-in sig [:types target :attrs]) [])
                       :when (and (= elem type) (not (sequential? type)))]
                   [name type]))]
    (for [f fs
          :let [fv (get-in f [:values attr])]
          :when (some? fv)
          p ps
          :when (some (fn [[a have]] (holds? fv (get-in p [:values a]) have)) p-cols)]
      (case side
        :to [(str (:id p)) (str (:id f))]
        [(str (:id f)) (str (:id p))]))))

(defn fk-links
  [sig L fs ps]
  (for [[from to] (distinct (fk-pairs sig L fs ps))]
    {:id (pair-id sig L from to) :from from :to to}))

(defn denied-markings
  [authority]
  (into #{} (map name) (:denied-markings authority)))

(defn visible?
  [denied row]
  (or (empty? denied) (not-any? #(contains? denied (name %)) (:markings row))))

(defn hidden-type?
  [authority type-ref]
  (contains? (:hidden-object-types authority) type-ref))

(def policy-exempt? granular/exempt?)

(defn- supplement-properties
  [idx T]
  (vec (for [[_ _ out] (sch/outputs-for idx :object-type T)
             :when (and (= :supplement (:role out)) (seq (:column-map out)))]
         (set (keys (:column-map out))))))

(defn row-guard
  [W authority T]
  (when-not (policy-exempt? authority)
    (when-let [security (not-empty (get-in W [:object-types T :security]))]
      (let [denied   (denied-markings authority)
            idx      (sch/index W)
            security (cond-> security
                       (seq (:mandatory-controls security))
                       (update :mandatory-controls granular/datasource-scoped
                               (supplement-properties idx T)))
            subj     (granular/with-org-names (or (:subject authority) (granular/subject {}))
                                              #(some-> (sch/name-of idx %) name))]
        (granular/object-guard security subj #(contains? denied (name %)))))))

(defn guarded
  [g]
  (if-let [{:keys [admit? redact]} g]
    (fn [rows] (into [] (comp (filter (comp admit? :values)) (map #(update % :values redact)))
                     rows))
    identity))

(defn rid-of
  [W section x]
  (when (some? x)
    (if (contains? (get W section) x) x (sch/rid-of (sch/index W) section x))))

(defn key-collisions
  [W tx-vector holders]
  (let [writes  (filter tv/store-write? tx-vector)
        deleted (into #{} (comp (filter #(= :delete-object (:op %))) (map (comp str :id :payload)))
                      writes)]
    (vec
     (for [[type creates] (group-by :type (filter #(= :create-object (:op %)) writes))
           :let [ot (or (rid-of W :object-types type) type)
                 {props :properties} (rules/object-key W ot)]
           :when (seq props)
           :let [keyed  (for [c creates
                              :let [t (mapv #(get-in c [:payload :values %]) props)]
                              :when (every? some? t)]
                          [t (str (get-in c [:payload :id]))])
                 twice  (for [[t n] (frequencies (map first keyed)) :when (> n 1)]
                          {:key t :taken-by :same-submission})
                 mine   (into #{} (map second) keyed)
                 live   (for [{:keys [id key]} (when (seq keyed)
                                                 (holders type props
                                                          (vec (distinct (map first keyed)))))
                              :when (not (deleted id))
                              :when (not (mine id))]
                          {:key key :taken-by id})
                 nm     (or (get-in W [:object-types ot :object-type-name]) type)]
           c (concat twice live)]
       (assoc c :object-type (keyword (rid/->name nm)) :properties props)))))

(defn key-refusal
  [collisions]
  (when-let [{:keys [object-type properties key taken-by]} (first collisions)]
    [(str (name object-type) " " (pr-str (zipmap properties key)) " exists already"
          (if (= :same-submission taken-by)
            " — this submission creates it twice"
            (str " (object " taken-by ")"))
          ". An object is its key: creating a second one with the same key is"
          " refused. Edit the existing object instead, or choose another "
          (str/join "/" (map name properties)) ".")
     {:rule :object-key-taken
      :object-type object-type :key (zipmap properties key)
      :taken-by taken-by :collisions (vec collisions)}]))

(defn diff-paths
  [old new]
  (let [ss  (filter #(not= (get old %) (get new %)) ents/sections)
        ch  (tree/changes (select-keys old ss) (select-keys new ss))
        put (fn [acc k] (reduce (fn [acc [[s] {e :key v :value}]] (assoc-in acc [s k e] v))
                                acc (get ch k)))
        d   (reduce put {} [:added :removed :changed])]
    (into {}
          (for [s ents/sections :when (contains? d s)]
            [s (merge {:added {} :removed {} :changed {}} (get d s))]))))

(defn check-paging!
  [{:keys [offset limit]}]
  (when (and (some? offset) (nil? limit))
    (anomaly/raise! :incorrect
                    (str ":offset without :limit is an unbounded read — "
                         "a page needs a size, not just a start.")
                    {:offset offset}))
  (when (and (some? offset) (neg? offset))
    (anomaly/raise! :incorrect
                    ":offset must not be negative"
                    {:offset offset}))
  (when (and (some? limit) (neg? limit))
    (anomaly/raise! :incorrect
                    ":limit must not be negative"
                    {:limit limit})))

(defn qt-aggregate
  [qt]
  (some-> (agg/normalize {:aggs (:aggregate qt) :group-by (:group-by qt)})
          (assoc :window (select-keys qt [:limit :offset]))))

(defn check-bucket-count!
  [n]
  (when (> n agg/max-buckets)
    (anomaly/raise! :incorrect
                    (str "aggregation exceeded " agg/max-buckets " buckets (" n
                         ") — group by a coarser bucket, or filter the set first")
                    {:buckets n :limit agg/max-buckets})))

(defn refuse-aggregate-order!
  [order-by result-keys]
  (anomaly/raise! :incorrect
                  (str "cannot order a grouped aggregate by " (pr-str order-by)
                       " — order by one of its own result keys: "
                       (pr-str (vec (sort result-keys))))
                  {:order-by order-by :keys (set result-keys)}))

(defn derived-clears
  [W0 W]
  (vec (for [[T spec] (:object-types W)
             :let [names #(set (map first (:derived (attribute/of %))))]
             a (remove (names (get-in W0 [:object-types T])) (names spec))]
         [:clear-attribute T a])))

(defn spelling-renames
  [W0 W]
  (vec (for [[T spec] (:object-types W)
             :let [names (fn [s] (set (map :attribute-name (:attribute-list s))))
                   old   (names (get-in W0 [:object-types T]))
                   new   (names spec)
                   gone  (group-by column/attr->col (remove new old))]
             b (sort (remove old new))
             :let [[a & more] (get gone (column/attr->col b))]
             :when (and a (empty? more))]
         [:rename-attribute T a b])))

(defn plan
  [W ir]
  (let [hop (fn [T link dir]
              (select-keys (ros/resolve-hop {} W T {:link link :dir dir}) [:link :dir :far :near]))]
    (os/rewrite-bottom-up
     (fn [{:keys [op in] :as n}]
       (case op
         (:scan :static) (assoc n :type (:root n))
         :search-around  (let [h (hop (:type in) (:link n) (:dir n))] (merge n h {:type (:far h)}))
         :with           (cond-> (merge n (hop (:type in) (:link n) (:dir n)) {:type (:type in)})
                           (:far-in n) (update :far-in #(plan W %)))
         (:union :intersect :subtract) (assoc n :type (ros/type-of {} W n))
         :filter (cond-> (assoc n :type (:type in))
                   (seq (:include n))
                   (update :include
                           (partial mapv
                                    #(merge %
                                            (select-keys (hop (:type in) (:link %) (:dir %))
                                                         [:link :dir])))))
         :aggregate (assoc n :type (:type in)
                           :sides (into {}
                                        (for [t (:group-by (agg/normalize n))
                                              :when (= :link (:kind t))]
                                          [(:name t)
                                           (select-keys (ros/resolve-side {} W (:type in) (:link t))
                                                        [:link :dir :far])])))
         (assoc n :type (:type in))))
     (if (:op ir) ir (os/qt->ir ir)))))
