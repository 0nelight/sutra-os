(ns sutra.store.model
  (:require [sutra.store.api :as api]
            [sutra.store.semantics :as sem]))

(set! *warn-on-reflection* true)

(defn kv-part [] (sorted-map-by sem/compare-values))

(defn kv-range
  [m {:keys [after upto limit reverse? keys-only?]}]
  (let [m  (cond-> m upto (as-> x (into (kv-part) (subseq x <= upto))))
        xs (if reverse?
             (if after (rsubseq m > after) (rseq m))
             (if after (subseq m > after) (seq m)))]
    (cond->> xs
      limit   (take limit)
      :always (mapv (fn [[k v]] (if keys-only? [k] [k v]))))))

(defn at
  [versions r]
  (if (nil? r)
    (second (peek versions))
    (second (first (filter #(<= (first %) r) (rseq versions))))))

(defn live
  [hist r]
  (into {} (keep (fn [[id vs]] (when-let [v (at vs r)] [id v]))) hist))

(defn record
  [hist rev id v]
  (update hist id (fnil conj []) [rev v]))

(defn- revise
  [nsv rev T f]
  (update-in nsv [:objects T]
             (fn [hist]
               (reduce-kv (fn [m id vs]
                            (let [v (at vs nil) v' (some-> v f)]
                              (if (and v (not= v v')) (record m rev id v') m)))
                          hist (or hist {})))))

(defn renamed [r a b]
  (update r :values #(if (contains? % a) (-> % (dissoc a) (assoc b (get % a))) %)))

(defn dropped [r a] (update r :values dissoc a))

(defn cleared [r a]
  (cond-> r (some? (get-in r [:values a])) (assoc-in [:values a] nil)))

(defn- step
  [nsv rev [k T a b]]
  (case k
    :rename-attribute (revise nsv rev T #(renamed % a b))
    :drop-attribute   (revise nsv rev T #(dropped % a))
    :clear-attribute  (revise nsv rev T #(cleared % a))
    :drop-type        (update nsv :objects dissoc T)
    :drop-link-type   (update nsv :links dissoc T)
    (:retype-attribute :detach-type :detach-link-type :relayout) nsv))

(defn matches
  [{:keys [ids from to ends]}]
  (let [in (fn [k vs] (let [want (set vs)] #(contains? want (k %))))]
    (cond ids  (in :id ids)
          from (in :from from)
          to   (in :to to)
          ends (let [f (in :from ends) t (in :to ends)] #(or (f %) (t %)))
          :else (constantly true))))

(defn own-at
  [versions r]
  (when-let [e (if (nil? r) (peek versions) (first (filter #(<= (first %) r) (rseq versions))))]
    [(second e)]))

(defn detached-at? [detached K as-of]
  (when-let [r (get detached K)] (or (nil? (:rev as-of)) (<= r (:rev as-of)))))

(defn section
  [st ns sec K as-of]
  (let [nsv  (get-in st [:ns ns])
        root (when-not (detached-at? (:detached nsv) K as-of) (:root nsv))
        base (if root (live (get-in st [:ns root sec K]) (:root-rev as-of)) {})]
    (reduce-kv (fn [m id vs]
                 (if-let [[v] (own-at vs (:rev as-of))] (if v (assoc m id v) (dissoc m id)) m))
               base (get-in nsv [sec K]))))

(defn- detach
  [st ns rev sec K]
  (let [nsv  (get-in st [:ns ns])
        root (:root nsv)
        own  (get-in nsv [sec K] {})
        held (reduce-kv (fn [m id v] (if (contains? own id) m (record m rev id v)))
                        own (if root (live (get-in st [:ns root sec K]) nil) {}))]
    (-> st
        (assoc-in [:ns ns sec K] held)
        (update-in [:ns ns :detached] #(merge {K rev} %)))))

(defn normal [x]
  (cond-> (update x :values #(into {} (remove (comp nil? val)) %))
    (empty? (:markings x)) (dissoc :markings)))

(defn same-rows? [a b]
  (let [by-id #(into {} (map (juxt :id normal)) %)
        a (by-id a) b (by-id b)]
    (and (= (count a) (count b)) (sem/same-data? a b))))

(defn empty-state [] {:kv {} :ns {"main" {}} :records {}})

(defn kv-read [st ks]
  (into {} (keep (fn [[space part k :as x]] (some->> (get-in st [:kv [space part] k]) (vector x))))
        ks))

(defn kv-write [st writes]
  (update st :kv
          (fn [kv] (reduce-kv (fn [kv [space part k] v]
                                (update kv [space part]
                                        #(if (some? v) (assoc (or % (kv-part)) k v)
                                             (dissoc (or % (kv-part)) k))))
                              kv writes))))

(defn kv-scan [st space part opts]
  (kv-range (get-in st [:kv [space part]] (kv-part)) opts))

(defn kv-parts [st space]
  (vec (sort (keep (fn [[[s p] kv]] (when (and (= s space) (seq kv)) p)) (:kv st)))))

(defn ns-fork [st from to]
  (let [fv (get-in st [:ns from] {})]
    (assoc-in st [:ns to] (if (:root fv)
                            (select-keys fv [:objects :links :detached :root])
                            {:root from}))))

(defn ns-drop [st ns] (update st :ns dissoc ns))

(defn layout [st {:keys [ns rev]} {:keys [new steps]}]
  (reduce (fn [st [k T :as s]]
            (case k
              :drop-unclaimed   (update-in st [:ns ns]
                                           (fn [nsv]
                                             (-> (or nsv {})
                                                 (update :objects select-keys (keys (:types new)))
                                                 (update :links select-keys (keys (:links new))))))
              :detach-type      (detach st ns rev :objects T)
              :detach-link-type (detach st ns rev :links T)
              (update-in st [:ns ns] (fn [nsv] (step (or nsv {}) rev s)))))
          st steps))

(defn attrs-of [sig T] (map :name (get-in sig [:types T :attrs])))

(defn projected [sig T row]
  (update row :values select-keys (attrs-of sig T)))

(defn rows [st {:keys [ns as-of sig]} T ids]
  (let [m (update-vals (section st ns :objects T as-of) #(projected sig T %))]
    (if (nil? ids) (vec (vals m)) (into [] (keep m) ids))))

(defn links [st {:keys [ns as-of]} L where]
  (filterv (matches where) (vals (section st ns :links L as-of))))

(defn inspect [st {:keys [ns sig]}]
  (let [nsv (get-in st [:ns ns])]
    {:missing   {:object-types [] :link-types []}
     :orphans   (vec (for [[sec held] [[:objects :types] [:links :links]]
                           k (keys (get nsv sec))
                           :when (not (contains? (get sig held) k))]
                       {:name (str k)}))
     :local     (if (:root nsv) (set (keys (:detached nsv))) (set (keys (:objects nsv))))
     :inherited (if (:root nsv) (into #{} (remove (set (keys (:detached nsv)))) (keys (:types sig)))
                    #{})}))

(defn write [st sec {:keys [ns rev]} K xs]
  (update-in st [:ns ns sec K] #(reduce (fn [m x] (record m rev (:id x) x)) (or % {}) xs)))

(defn erase [st sec {:keys [ns rev]} K ids]
  (update-in st [:ns ns sec K] #(reduce (fn [m id] (record m rev id nil)) (or % {}) ids)))

(defn rec-get [st {:keys [ns]} coll k] (get-in st [:records coll ns k]))

(defn rec-put [st {:keys [ns]} coll kvs next-key]
  (reduce (fn [[st ks] [k v]]
            (let [k (if (= api/auto k) (next-key coll) k)]
              [(if (some? v)
                 (assoc-in st [:records coll ns k] v)
                 (update-in st [:records coll ns] dissoc k))
               (conj ks k)]))
          [st []] kvs))

(defn rec-scan [st {:keys [ns]} coll] (vec (get-in st [:records coll ns])))

(defn rec-copy [st coll from to]
  (assoc-in st [:records coll to] (get-in st [:records coll from] {})))
