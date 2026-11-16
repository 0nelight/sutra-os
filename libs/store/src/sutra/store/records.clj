(ns sutra.store.records
  (:refer-clojure :exclude [get list count swap!])
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.store.api :as api]
            [sutra.store.machine :as machine]
            [sutra.store.served :as served]
            [sutra.store.world :as world]))

(set! *warn-on-reflection* true)

(def trunk "main")
(def now ::now)
(defn coalesce [v] [::coalesce v])

(defn- clock ^java.util.Date [store] ((:clock store)))

(defn- typed
  [{:keys [keywords]} m]
  (reduce (fn [m a]
            (let [v (clojure.core/get m a)]
              (cond (string? v) (assoc m a (keyword v))
                    (set? v)    (assoc m a (into #{} (map #(if (string? %) (keyword %) %)) v))
                    :else       m)))
          m (filter #(contains? m %) keywords)))

(defn- scoped [decl m]
  (cond-> (typed decl m) (:branch? decl) (update :branch #(or % trunk))))

(defn- resolve-vals
  [row old t]
  (reduce-kv (fn [m k v]
               (assoc m k (cond (= v now) t
                                (and (vector? v)
                                     (= ::coalesce
                                        (first v)))
                                (if (some? (second v)) (second v) (clojure.core/get old k))
                                :else v)))
             {} row))

(defn- record-key [{:keys [key-cols]} v]
  (if (= 1 (clojure.core/count key-cols)) (clojure.core/get v (first key-cols))
      (mapv #(clojure.core/get v %) key-cols)))

(defn- keyed
  [{:keys [auto]} k v]
  (cond-> v (and auto v) (assoc auto k)))

(defrecord ^:private Deciding [db planned]
  api/Db
  (-snapshot [_ basis] (api/-snapshot db basis))
  (-prepare [_ tx] (api/-prepare db tx))
  (-branch! [_ op] (api/-branch! db op))
  (-read-at [_ basis authority q] (api/-read-at db basis authority q)))

(defn deciding
  [store snap]
  (update store :db #(->Deciding (served/holding % snap) {})))

(defn- ask [store q]
  (world/with-snapshot (world/at store trunk) (fn [s] (api/read s nil q))))

(defn- planned
  [store coll]
  (when (instance? Deciding (:db store))
    (clojure.core/get-in store [:db :planned coll])))

(defn- matches? [where v]
  (every? (fn [[a x]]
            (let [y (clojure.core/get v a)]
              (cond (set? x)                (contains? x y)
                    (nil? x)                (nil? y)
                    (= :records/present x)  (some? y)
                    :else                   (= x y))))
          where))

(defn- over-planned
  [store coll where rows]
  (if-let [p (planned store coll)]
    (concat (remove (fn [[k _]] (contains? p k)) rows)
            (for [[k v] p :when (and v (matches? where v))] [k v]))
    rows))

(defn- refuse-sql! [where]
  (when (contains? where :sql)
    (anomaly/raise! :incorrect
                    "a records where is data: an SQL predicate has no place in it ( A6d)"
                    {:where where})))

(defn list
  ([store decl where] (list store decl where nil))
  ([store {:keys [coll order] :as decl} where
    {:keys [limit offset all-branches? range pick project] o :order}]
   (refuse-sql! where)
   (when (and pick (planned store coll))
     (anomaly/raise! :incorrect "a :pick reads committed records, not a plan's" {:coll coll}))
   (let [o    (first (or o order))
         w    (if all-branches? (typed decl where) (scoped decl where))
         rows (over-planned store coll w
                            (ask store
                                 [:records/list coll
                                  (cond-> {:where w}
                                    o     (assoc :order o)
                                    range (assoc :range range)
                                    pick  (assoc :pick pick)
                                    project (assoc :project (set project))
                                    limit (assoc :limit (+ (long limit) (long (or offset 0)))))]))]
     (cond->> (mapv (fn [[k v]] (keyed decl k v)) rows) offset (drop offset) true vec))))

(defn get
  [store {:keys [coll key-cols fallback] :as decl} where]
  (let [where (scoped decl where)
        one   (fn [w] (if (= (set key-cols) (set (keys w)))
                        (let [k (record-key decl w)
                              p (planned store coll)]
                          (keyed decl k
                                 (if (contains? p k) (clojure.core/get p k)
                                     (ask store [:records/get coll k]))))
                        (first (list store decl w {:limit 1}))))]
    (or (one where)
        (when (and (= :trunk fallback) (not= trunk (:branch where)))
          (one (assoc where :branch trunk))))))

(defn count [store {:keys [coll] :as decl} where]
  (refuse-sql! where)
  (ask store [:records/list coll {:where (scoped decl where) :count? true}]))

(defn written
  [store {:keys [coll]}]
  (ask store [:records/clock coll]))

(defn counts
  [store {:keys [coll] :as decl} where attr]
  (refuse-sql! where)
  (ask store [:records/list coll {:where (scoped decl where) :group-by attr}]))

(defn- touched [{:keys [touch]} row t] (merge row (zipmap touch (repeat t))))

(defn- defaulted
  [{:keys [defaults]} row]
  (merge defaults row))

(defn- fresh-row [decl row t]
  (touched decl (resolve-vals (defaulted decl (scoped decl row)) nil t) t))

(defn entry
  [{:keys [coll auto] :as decl} row t]
  (let [row (fresh-row decl row t)]
    (if auto
      {:coll coll :key api/auto :value row}
      {:coll coll :key (record-key decl row) :value row :expect api/absent})))

(defn- replacing
  [{:keys [coll auto] :as decl} old v]
  (let [k (if old (record-key decl old) (if auto api/auto (record-key decl v)))]
    (cond-> {:coll coll :key k :value v}
      (not= api/auto k) (assoc :expect (or old api/absent)))))

(defn- merged
  [{:keys [merge]} old row]
  (reduce-kv (fn [m k v]
               (case (clojure.core/get merge k :replace)
                 :replace      (assoc m k v)
                 :keep         (if (contains? old k) m (assoc m k v))
                 :coalesce-old (assoc m
                                      k (if (some? (clojure.core/get old k))
                                          (clojure.core/get old k) v))
                 :coalesce-new (assoc m k (if (some? v) v (clojure.core/get old k)))
                 :greatest     (assoc m
                                      k (let [a (clojure.core/get old k)]
                                          (if (and a v (pos? (compare a v))) a v)))))
             (or old {}) row))

(def ^:private actor {:principal "sutra.store.records"})

(defn- commit! [store records]
  (api/transact! (:db store)
                 {:basis (world/head (world/at store trunk)) :actor actor :records records}))

(defn- decide!
  [store decide]
  (:result
   (machine/command! (:db store) {:branch trunk}
                     (fn [snap]
                       (let [{:keys [records] :as d} (decide (deciding store snap) (clock store))]
                         (cond-> (dissoc d :records)
                           (seq records) (assoc :tx {:actor actor :records records})))))))

(defn insert!
  [store {:keys [coll] :as decl} row]
  (let [e  (entry decl row (clock store))
        k' (:key (first (:records (commit! store [e]))))]
    (keyed decl k' (or (ask store [:records/get coll k']) (:value e)))))

(defn expecting
  [{:keys [coll] :as decl} v]
  {:coll coll :key (record-key decl v) :expect v :value v})

(defn expect
  [store decl where]
  (some->> (get store decl where) (expecting decl)))

(defn upsert!
  ([store decl row] (upsert! store decl row nil))
  ([store {:keys [conflict key-cols] :as decl} row {c :conflict}]
   (let [row   (scoped decl row)
         match (select-keys row (or conflict key-cols))
         result (decide! store
                         (fn [store t]
                           (let [old (get store decl match)]
                             (if (and old (= :nothing c))
                               {:result old}
                               (let [v (cond-> (touched decl
                                                        (merged decl old
                                                                (resolve-vals
                                                                 (cond->> row
                                                                   (nil? old) (defaulted decl)) old
                                                                 t)) t)
                                         old (merge (select-keys old key-cols)))]
                                 {:records [(replacing decl old v)] :result v})))))]
     (or (get store decl match) result))))

(defn upserting
  [store {:keys [conflict key-cols touch] :as decl} rows t]
  (let [match (or conflict key-cols)
        mk    (fn [v] (mapv #(clojure.core/get v %) match))
        rows  (vals (into {} (map (juxt mk identity)) (map #(scoped decl %) rows)))
        olds  (if (= 1 (clojure.core/count rows))
                (let [row (first rows)]
                  (if-let [old (get store decl (select-keys row match))] {(mk row) old} {}))
                (into {} (map (juxt mk identity))
                      (list store decl (into {} (for [c match] [c (into #{} (map c) rows)])))))
        bare  #(apply dissoc % touch)]
    {:records (into []
                    (keep (fn [row]
                            (let [old (clojure.core/get olds (mk row))
                                  v
                                  (cond-> (touched decl
                                                   (merged decl old
                                                           (resolve-vals
                                                            (cond->> row
                                                              (nil? old) (defaulted decl)) old t))
                                                   t)
                                    old (merge (select-keys old key-cols)))]
                              (when-not (and old (= (bare old) (bare v)))
                                (replacing decl old v)))))
                    rows)
     :result nil}))

(defn insert-many!
  ([store decl rows] (insert-many! store decl rows nil))
  ([store {:keys [coll auto] :as decl} rows {:keys [upsert?]}]
   (if upsert?
     (doseq [ch (partition-all 500 rows)]
       (decide! store #(upserting %1 decl ch %2)))
     (let [t (clock store)]
       (doseq [ch (partition-all 500 rows)]
         (commit! store (mapv (fn [r] (let [r (fresh-row decl r t)]
                                        {:coll coll :key (if auto api/auto (record-key decl r))
                                         :value r}))
                              ch)))))
   nil))

(defn swapping
  [store decl where f t]
  (let [where (scoped decl where)
        old   (get store decl where)
        v     (some->> (f old) (typed decl))]
    (if (nil? v)
      {:result nil}
      (let [v (touched decl
                       (merge where (resolve-vals (cond->> v (nil? old) (defaulted decl)) old t))
                       t)]
        {:records [(replacing decl old v)] :result v}))))

(defn updating
  [store {:keys [coll] :as decl} where sets opts t]
  (let [olds (list store decl where opts)]
    {:records (mapv (fn [o]
                      {:coll coll :key (record-key decl o) :expect o
                       :value (touched decl
                                       (merge o
                                              (resolve-vals
                                               (typed decl (if (fn? sets) (sets o) sets)) o t)) t)})
                    olds)
     :result  (clojure.core/count olds)}))

(defn rewriting
  [{:keys [coll] :as decl} rows f]
  {:records (mapv (fn [o] {:coll coll :key (record-key decl o) :expect o :value (f o)}) rows)
   :result  (clojure.core/count rows)})

(defn deleting
  [store decl where]
  (rewriting decl (list store decl where) (constantly nil)))

(defn- seeing
  [store records]
  (update-in store [:db :planned]
             (fn [p] (reduce (fn [p {:keys [coll key value]}]
                               (if (= api/auto key) p (assoc-in p [coll key] value)))
                             p records))))

(defn- combined
  [records]
  (let [auto? #(= api/auto (:key %))
        [order by] (reduce (fn [[order by] e]
                             (let [k [(:coll e) (:key e)]]
                               (if (contains? by k)
                                 [order (assoc-in by [k :value] (:value e))]
                                 [(conj order k) (assoc by k e)])))
                           [[] {}] (remove auto? records))]
    (into (mapv by order) (filter auto? records))))

(defn plan!
  [store plans]
  (decide! store (fn [store t]
                   (let [{:keys [records results]}
                         (reduce (fn [{:keys [store records results]} p]
                                   (let [{r :records x :result} (p store t)]
                                     {:store (seeing store r) :records (into records r)
                                      :results (conj results x)}))
                                 {:store store :records [] :results []}
                                 plans)]
                     (cond-> {:result results}
                       (seq records) (assoc :records (combined records)))))))

(defn swap!
  [store decl where f]
  (decide! store #(swapping %1 decl where f %2)))

(defn update!
  ([store decl where sets] (update! store decl where sets nil))
  ([store decl where sets opts]
   (decide! store #(updating %1 decl where sets opts %2))))

(defn delete!
  [store decl where]
  (decide! store (fn [store _] (deleting store decl where))))
