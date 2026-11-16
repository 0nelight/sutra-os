(ns platform-sdk.objectset
  (:require [platform-sdk.aggregate :as agg]
            [platform-sdk.client :as client]
            [platform-sdk.errors :as err]
            [platform-sdk.predicates :as p]
            [platform-sdk.wire :as w]
            [sutra.types.query :as q]))

(defn- then [answer f] #?(:clj (f answer) :cljs (.then answer #(f %))))

(def max-page 10000)
(def max-hops 3)

(def surface-ops q/surface-ops)

(defn- bind [q kvs] (vary-meta q merge kvs))

(defn- index [q] (::index (meta q)))

(defn- type-of [q] (or (::type (meta q)) (:find q)))

(defn- type-spec [q] (get-in (index q) [:object-types (type-of q)]))

(defn client-of [q] (or (::client (meta q)) (client/current)))

(defn on
  [q c]
  (bind q {::client c}))

(defn of
  ([type-kw] {:find type-kw})
  ([c type-kw] (of c type-kw nil))
  ([c type-kw idx] (bind {:find type-kw} {::client c ::index idx ::type type-kw})))

(defn from-set
  ([rid type-kw] {:object-set rid :find type-kw})
  ([c rid type-kw] (bind (from-set rid type-kw) {::client c ::type type-kw}))
  ([c rid type-kw idx] (bind (from-set rid type-kw) {::client c ::type type-kw ::index idx})))

(defn- known-fields
  [spec]
  (into (set (keys (:attributes spec))) (keys (:links spec))))

(defn- check-field!
  [q field what]
  (when-let [spec (type-spec q)]
    (when-not (contains? (known-fields spec) field)
      (err/raise! :usage
                  (str (pr-str field) " is not " what " of " (pr-str (type-of q))
                       " — known: " (pr-str (vec (sort (known-fields spec)))))
                  {:field field :type (type-of q)
                   :known (vec (sort (known-fields spec)))}))))

(defn- check-ops!
  [where]
  (doseq [[field ops] where
          [op v] (when (map? ops) ops)
          :when (and (not (surface-ops op)) (not (map? v)))]
    (err/raise! :usage (str "not a surface operator: " (pr-str op) " on " (pr-str field)
                            " — known: " (pr-str (vec (sort surface-ops))))
                {:field field :op op :ops surface-ops})))

(defn where
  [q m]
  (doseq [field (keys m) f (if (vector? field) field [field])]
    (check-field! q f "an attribute or link"))
  (check-ops! m)
  (update q :where (fnil p/deep-merge {}) m))

(defn ids [q coll] (assoc q :ids (vec coll)))

(defn- check-side!
  [side]
  (when-not (keyword? side)
    (err/raise! :usage (str "a hop names a link side by its API name (a keyword), got "
                            (pr-str side))
                {:link side})))

(defn around
  [q side]
  (check-side! side)
  (let [hops (:around q [])
        spec (type-spec q)
        info (get-in spec [:links side])]
    (when (>= (count hops) max-hops)
      (err/raise! :usage (str "at most " max-hops " hops per query")
                  {:around hops :cap max-hops}))
    (when (and spec (nil? info))
      (err/raise! :usage (str (pr-str side) " is not a link of " (pr-str (type-of q))
                              " — known: " (pr-str (vec (keys (:links spec)))))
                  {:link side :type (type-of q) :known (vec (keys (:links spec)))}))
    (cond-> (assoc q :around (conj hops side))
      info (bind {::type (:far-type info)}))))

(defn order
  ([q attr] (order q attr :asc))
  ([q attr dir]
   (check-field! q attr "an attribute")
   (cond-> (assoc q :order-by attr)
     (= dir :desc) (assoc :sort-dir :desc))))

(defn limit [q n] (assoc q :limit n))

(defn offset [q n] (assoc q :offset n))

(defn page [q size from] (-> q (limit size) (offset from)))

(defn show
  [q fields]
  (doseq [f fields :when (keyword? f)] (check-field! q f "an attribute"))
  (assoc q :show (vec fields)))

(defn accuracy [q mode] (assoc q :accuracy mode))

(defn engine [q e] (assoc q :engine e))

(defn aggregate
  ([q aggs] (aggregate q aggs nil))
  ([q aggs group-by]
   (agg/normalize aggs group-by)
   (doseq [a aggs :when (:attr a)] (check-field! q (:attr a) "an attribute"))
   (cond-> (assoc q :aggs aggs)
     (seq group-by) (assoc :group-by (vec group-by)))))

(defn validate
  [{:keys [limit offset around where] :as q}]
  (when (and offset (nil? limit))
    (err/raise! :usage "offset needs a limit" {:query q}))
  (when (and limit (or (neg? limit) (> limit max-page)))
    (err/raise! :usage (str "limit out of range: " limit) {:limit limit :cap max-page}))
  (when (and offset (neg? offset))
    (err/raise! :usage "offset must not be negative" {:offset offset}))
  (when (> (count around) max-hops)
    (err/raise! :usage (str "at most " max-hops " hops per query") {:cap max-hops}))
  (run! check-side! around)
  (check-ops! where)
  q)

(defn run
  ([q] (run (client-of q) q))
  ([c q] (w/query c (validate q))))

(defn rows
  ([q] (then (run q) :rows))
  ([c q] (then (run c q) :rows)))

(defn one
  ([q] (then (rows (limit q 1)) first))
  ([c q] (then (rows c (limit q 1)) first)))

(defn total
  ([q] (total (client-of q) q))
  ([c q] (then (run c (assoc q :limit 0 :count? true)) :count)))

(defn explain
  ([q] (explain (client-of q) q))
  ([c q] (run c (assoc q :explain? true))))

(defn get-by-key
  ([q v] (get-by-key (client-of q) q v))
  ([c q v]
   (if-let [pk (:primary-key (type-spec q))]
     (one c (where q (p/= pk v)))
     (one c (ids q [v])))))

#?(:clj
   (defn pages
     ([q] (pages q 1000))
     ([q size]
      (let [c (client-of q)]
        (letfn [(step [from]
                  (lazy-seq
                   (let [rs (rows c (page q size from))]
                     (when (seq rs)
                       (cons rs (when (= size (count rs)) (step (+ from size))))))))]
          (step 0))))))

#?(:clj
   (defn objects
     ([q] (objects q 1000))
     ([q size] (sequence cat (pages q size)))))

(defn ->ir
  [{:keys [find where ids around limit offset object-set]} {:keys [types]}]
  (when (or limit offset)
    (err/raise! :usage "a paged set cannot be a member of a set operation" {}))
  (when object-set
    (err/raise! :usage "a set backed by a saved object set has no local IR" {}))
  (let [root (or (get types find)
                 (err/raise! :not-found (str "no rid for object type " (pr-str find))
                             {:type find}))
        base (if ids
               {:op :static :ids (vec ids) :root root}
               {:op :scan :root root})
        filtered (if (seq where)
                   {:op :filter
                    :where (into [:and]
                                 (for [[field ops] (sort-by key where)
                                       [op v] (sort-by key ops)]
                                   (if (surface-ops op)
                                     [op field v]
                                     (err/raise! :usage
                                                 "link sub-filters have no predicate-IR spelling"
                                                 {:field field}))))
                    :in base}
                   base)]
    (reduce (fn [node side]
              (check-side! side)
              {:op :search-around :link side :in node})
            filtered
            around)))

(defn combine [op a b resolve] {:op op :in [(->ir a resolve) (->ir b resolve)]})

(defn- rids-of
  [c q]
  (or (:rids (index q))
      #?(:clj  (client/rids c)
         :cljs (err/raise! :usage "a set operation needs the generated index" {:client c}))))

(defn combine!
  ([op a b] (combine! (client-of a) op a b))
  ([c op a b]
   (then (w/create-object-set! c {:ir (combine op a b (rids-of c a))})
         (fn [{:keys [rid]}]
           (when-not rid
             (err/raise! :server "the temporary object set came back without a rid" {:op op}))
           (from-set c rid (type-of a) (index a))))))

(defn union     [a b] (combine! :union a b))
(defn intersect [a b] (combine! :intersect a b))
(defn subtract  [a b] (combine! :subtract a b))
