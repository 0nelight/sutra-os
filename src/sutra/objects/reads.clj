(ns sutra.objects.reads
  (:require [sutra.kit.coll :as coll]
            [sutra.kit.noted :as noted]
            [sutra.kit.refusal :as refusal]
            [sutra.foundation.ops :as op]
            [sutra.store.access.counts :as counting]
            [sutra.store.access.run-log :as run-log]
            [sutra.store.access.scope :as ctx]
            [sutra.store.access.substrate :as sub]
            [sutra.catalog.link-ends :as link-ends]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [clojure.string :as str]
            [sutra.objects.views :as views]
            [sutra.store.kept :as kept]
            [sutra.types.rid :as rid]))

(set! *warn-on-reflection* true)

(defn- staged-rows
  [store]
  (op/query {:store store} :actions/staged {:branch (:branch store)}))

(defn- refuse-oversized-read!
  [obj-type n cap]
  (refusal/refuse! :refusal.objects/read-capped
                   {:object-type obj-type
                    :objects     n
                    :max         cap}
                   {:type-name (name obj-type)}))

(defn objects-of-type
  ([store schema obj-type] (objects-of-type store schema obj-type nil))
  ([store schema obj-type {:keys [limit derived? derived-extra want max-objects]}]
   (let [iface? (lookup/iface? schema obj-type)]
     (grants/authorize-read! (grants/read-rights store)
                             (get-in schema [(if iface? :interfaces :object-types) obj-type])
                             obj-type)
     (when (and max-objects (not limit) (not iface?))
       (when-let [n (try (sub/count-type store obj-type)
                         (catch clojure.lang.ExceptionInfo e
                           (noted/note (str "size guard of " obj-type " skipped") e)))]
         (when (> n max-objects) (refuse-oversized-read! obj-type n max-objects))))
     (cond-> (if (or limit iface?)
               (sub/query store (sub/query-request
                                 nil (cond-> {:root obj-type}
                                       limit (assoc :limit limit))))
               (sub/read-type store obj-type))
       derived? (as-> rows (views/attach-derived (sub/reader store) schema obj-type rows
                                                 want derived-extra))))))

(defn objects-matching
  [store schema obj-type query {:keys [derived-extra want]}]
  (let [find (or (get-in schema [:object-types obj-type :object-type-name])
                 (get-in schema [:interfaces obj-type :interface-name])
                 obj-type)
        rows (:rows (views/run-object-query store schema (assoc query :find find :want want)))]
    (cond-> rows
      (seq derived-extra)
      (as-> rs (views/attach-derived (sub/reader store) schema obj-type rs want derived-extra)))))

(defn- query-find [schema obj-type]
  (or (get-in schema [:object-types obj-type :object-type-name])
      (get-in schema [:interfaces obj-type :interface-name])
      obj-type))

(defn page-matching
  [store schema obj-type query {:keys [derived-extra want]}]
  (let [q    (assoc query :find (query-find schema obj-type) :want want)
        rows (:rows (views/run-object-query store schema q))
        full (and (:limit query) (>= (count rows) (:limit query)))]
    {:rows  (cond-> rows
              (seq derived-extra)
              (as-> rs (views/attach-derived (sub/reader store) schema obj-type rs want
                                             derived-extra)))
     :total (if full
              (:count (views/run-object-query store schema
                                              (-> q (dissoc :limit :offset :order-by :sort-dir)
                                                  (assoc :count-only? true))))
              (+ (or (:offset query) 0) (count rows)))}))

(defn aggregate-matching
  [store schema obj-type query agg]
  (-> (views/run-object-query store schema
                              (assoc query :find (query-find schema obj-type)
                                     :aggs [(assoc agg :name :v)]))
      :aggregations first :metrics :v))

(defn count-matching
  [store schema obj-type query]
  (let [find (or (get-in schema [:object-types obj-type :object-type-name])
                 (get-in schema [:interfaces obj-type :interface-name])
                 obj-type)]
    (:count (views/run-object-query store schema (assoc query :find find :count-only? true)))))

(defn objects-by-ids
  [query schema obj-type ids]
  (when (seq ids)
    (:rows (query {:find  (or (get-in schema [:object-types obj-type :object-type-name])
                              obj-type)
                   :ids   (vec ids)
                   :limit (count ids)}))))

(defn- link-rows
  [store L {:keys [from-ids to-ids limit] :as filters}]
  (if (or (and (contains? filters :from-ids) (empty? from-ids))
          (and (contains? filters :to-ids)   (empty? to-ids))
          (and limit (not (pos? limit))))
    []
    (let [tos  (some->> to-ids (map str) set)
          rows (sub/read-links store L (cond from-ids {:from (mapv str from-ids)}
                                             to-ids   {:to (vec tos)}
                                             :else    nil))]
      (cond->> (for [l rows :when (or (nil? tos) (nil? from-ids) (contains? tos (:to-object-id l)))]
                 (cond-> {:id (:id l) :link-type-ref L :from-id (:from-object-id l)
                          :to-id (:to-object-id l)}
                   (:markings l) (assoc :markings (:markings l))))
        limit  (take limit)
        :always vec))))

(defn links-of-type
  ([store schema conn-type] (links-of-type store schema conn-type {}))
  ([store schema conn-type filters]
   (grants/authorize-read! (grants/read-rights store)
                           (get-in schema [:link-types conn-type]) conn-type)
   (link-rows store conn-type filters)))

(defn all-links
  ([store schema] (all-links store schema {}))
  ([store schema {:keys [limit] :as filters}]
   (when-not (grants/allows? (grants/read-rights store) nil false :read)
     (refusal/refuse! :refusal.objects/all-links-read-denied
                      {:sutra/acl-denied true :operation :read}))
   (loop [[L & more] (sort-by str (keys (:link-types schema)))
          acc        []]
     (if (or (nil? L) (and limit (>= (count acc) limit)))
       acc
       (recur more
              (into acc
                    (link-rows store L
                               (cond-> filters limit (assoc :limit (- limit (count acc)))))))))))

(defn- type-name [schema section k]
  (clojure.core/name (or (get-in schema
                                 [section k
                                  (if (= :object-types section) :object-type-name :link-type-name)])
                         k)))

(defn- store-counts
  [store schema missing]
  (let [p (counting/plan schema missing)]
    (counting/tally p (sub/counts store p))))

(defn tableless
  [schema {:keys [missing]}]
  (let [names (fn [section ks] (vec (sort (map #(type-name schema section %) ks))))
        t     {:object-types (names :object-types (:object-types missing))
               :link-types   (names :link-types (:link-types missing))}]
    (when (or (seq (:object-types t)) (seq (:link-types t))) t)))

(defn app-snapshot
  [store schema]
  (let [branch (:branch store)
        tail   (run-log/action-log-tail store 1)
        {:keys [layout counts]}
        (sub/with-read
          store
          (fn [store]
            (let [layout (sub/layout store)]
              {:layout layout
               :counts (store-counts store schema (:missing layout))})))]
    {:branch          branch
     :counts          (select-keys counts [:objects :links])
     :max-action-seq  (or (:seq (first tail)) 0)
     :integrity       (tableless schema layout)}))

(defn object-counts
  [store schema]
  (let [branch (:branch store)]
    (sub/with-read
      store
      (fn [store]
        (let [layout (sub/layout store)
              counts (store-counts store schema (:missing layout))
              names  (fn [ks]
                       (vec (sort (for [k ks]
                                    (type-name schema
                                               (if (get-in schema [:object-types k]) :object-types
                                                   :link-types) k)))))]
          (cond-> {:branch  branch
                   :counts  (select-keys counts [:objects :links])
                   :by-type (:by-type counts)}
            (not= "main" branch)
            (assoc :branch-index {:local     (names (:local layout))
                                  :inherited (names (:inherited layout))})))))))

(defn- read-schema
  [store]
  (kept/reading store (:branch store)))

(defn- ->obj-row
  [{:keys [id values]} staged-ids]
  {:id id :values values :staged? (contains? staged-ids id)})

(def ^:private around-cap
  10000)

(defn- refuse-large-around!
  [seeds conns]
  (when (< around-cap (count conns))
    (refusal/refuse! :refusal.objects/around-too-large
                     {:kind :around-too-large :links (count conns) :cap around-cap}
                     {:seeds seeds})))

(defn seed-link-types
  [reach types]
  (sort-by str (into #{} (mapcat reach) types)))

(defn around-links
  [link-types seed-set read]
  (vec (distinct (for [L link-types
                       f [:from-ids :to-ids]
                       l (read L {f (vec seed-set)})]
                   l))))

(defn neighbors
  ([store view seed-ids types] (neighbors store view seed-ids types nil))
  ([store {:keys [read live?]} seed-ids types {:keys [links-only? among?]}]
   (when (empty? types)
     (refusal/refuse! :refusal.objects/around-needs-types {:seeds (count seed-ids)}))
   (let [seed-set        (into #{} (map str) seed-ids)
         store              (ctx/at store read)
         schema      (read-schema store)
         staged-ids  (if live?
                       (into #{} (keep #(get-in % [:args :id])) (staged-rows store))
                       #{})
         ct-index    (:link-types schema)
         type-rids   (for [t types
                           :let [T (lookup/->type-rid schema :object-types t)]]
                       (if (get-in schema [:object-types T])
                         T
                         (refusal/refuse! :refusal.objects/unknown-type {:type t})))
         conns       (around-links (seed-link-types #(link-ends/reaching schema %) type-rids)
                                   seed-set #(link-rows store %1 %2))
         conns       (cond->> conns
                       among? (filterv #(and (seed-set (str (:from-id %)))
                                             (seed-set (str (:to-id %))))))
         _           (when-not links-only? (refuse-large-around! (count seed-set) conns))
         conns-out   (mapv (fn [c]
                             {:id            (:id c)
                              :link-type-ref (:link-type-ref c)
                              :from-id       (:from-id c)
                              :to-id         (:to-id c)
                              :staged?       (contains? staged-ids (:id c))})
                           conns)
         nbr->type   (reduce
                      (fn [m c]
                        (let [ct (get ct-index (:link-type-ref c))
                              f  (str (:from-id c)), t (str (:to-id c))]
                          (cond-> m
                            (not (seed-set f)) (assoc f (:from-object-type ct))
                            (not (seed-set t)) (assoc t (:to-object-type ct)))))
                      {}
                      conns)
         objs-by-type
         (into {}
               (keep (fn [[type-ref ids]]
                       (when-not links-only?
                         (let [found (coll/index-by :id (sub/read-objects store type-ref ids))]
                           (when-let [rows (seq (keep #(when-let [o (found %)]
                                                         (->obj-row {:id % :values (:values o)}
                                                                    staged-ids))
                                                      ids))]
                             [type-ref (vec rows)])))))
               (update-vals (group-by val nbr->type) #(mapv key %)))
         objs-by-type (reduce-kv (fn [m rid rows]
                                   (assoc m rid (views/attach-derived (sub/reader store)
                                                                      schema rid rows)))
                                 {}
                                 objs-by-type)]
     {:objects objs-by-type :links conns-out})))

(defn search-objects
  ([store view q] (search-objects store view q nil 50))
  ([store {:keys [read live?]} q type-token limit]
   (let [needle (some-> q str str/lower-case str/trim)
         store     (ctx/at store read)]
     (if (str/blank? needle)
       {:objects {}}
       (let [
             schema     (read-schema store)
             staged-ids (if live?
                          (into #{} (keep #(get-in % [:args :id])) (staged-rows store))
                          #{})
             only-type  (when type-token
                          (if (rid/rid? type-token) type-token
                              (lookup/rid-of schema :object-types (keyword type-token))))
             types      (cond->> (keys (:object-types schema))
                          only-type (filter #(= % only-type)))
             groups (->> (for [t types
                               :let [s   (get-in schema [:object-types t])
                                     vis (some-> (:visibility s) name keyword)]
                               :when (and (or only-type (not= :hidden vis))
                                          (grants/read-allowed?
                                           (grants/read-rights store) (:project s)
                                           (:ignore-inherited s)))
                               :let [spec {:find (:object-type-name s) :keywords needle}
                                     {:keys [rows]} (views/run-object-query
                                                     store schema (assoc spec :limit limit))]
                               :when (seq rows)
                               :let [{:keys [count]} (views/run-object-query
                                                      store schema (assoc spec :count-only? true))]]
                           {:type t :count count :rows (mapv #(->obj-row % staged-ids) rows)
                            :prominent? (= :prominent vis)})
                         (sort-by (juxt (comp not :prominent?) :count))
                         (mapv #(dissoc % :prominent?)))]
         {:objects (into {} (map (juxt :type :rows)) groups)
          :groups  groups})))))
