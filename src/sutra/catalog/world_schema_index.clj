(ns sutra.catalog.world-schema-index
  (:require [sutra.world.db :as kdb]
            [sutra.world.index :as ix]
            [sutra.types.nested :as nested]
            [sutra.world.view :as view]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]))

(set! *warn-on-reflection* true)

(def ^:private named-kinds (filterv rid/name-field-of rid/rid-kinds))

(defn- name-of [db kk k ix]
  (when-some [v (get (kdb/entry db [kk k]) (rid/name-field-of kk))]
    ((get-in sch/name-keys [ix :key]) v)))

(defn- named
  [db kk n]
  (for [[_ k] (kdb/nodes db :by-name-str [kk (ix/nm n)])] [k (kdb/entry db [kk k])]))

(defn- framed
  [idx db]
  (let [W (kdb/world db)]
    (reduce (fn [idx kk] (if (map? (get W kk)) (update idx kk #(or % {})) (dissoc idx kk)))
            idx named-kinds)))

(defn- step-names
  [idx ix spaces db0 db1 touched]
  (framed
   (reduce (fn [idx kk]
             (update idx kk view/recompute
                     (into #{} (comp (mapcat (fn [k] [(name-of db0 kk k ix) (name-of db1 kk k ix)]))
                                     (remove nil?))
                           (get touched kk))
                     #(sch/name-entry spaces nil ix kk % (named db1 kk %))))
           idx named-kinds)
   db1))

(defn- step-rids
  [idx db1 ks]
  (let [es (into {} (map (fn [k] [k (sch/rid-entry #(kdb/entry db1 [% k]) k)])) ks)]
    (cond-> idx
      (seq ks) (-> (update :rid->name view/recompute ks #(first (es %)))
                   (update :rid->section view/recompute ks #(second (es %)))))))

(defn- step-producers
  [producers db0 db1 ks]
  (reduce (fn [p k]
            (let [t0 (kdb/entry db0 [:transforms k]) t1 (kdb/entry db1 [:transforms k])]
              (cond-> p
                (some? t0) (sch/drop-producer k t0)
                (some? t1) (sch/add-producer k t1))))
          producers ks))

(defn- first-key [ks] (first (sort-by str ks)))

(defn- resolver-step
  [res db0 db1 sec ks]
  (let [nf     (rid/name-field-of sec)
        nm-of  (fn [db k] (some-> (get (kdb/entry db [sec k]) nf) name))
        here?  #(some? (kdb/entry db1 [sec %]))
        names  (into #{} (comp (mapcat (fn [k] [(nm-of db0 k) (nm-of db1 k)])) (remove nil?)) ks)
        knames (into #{} (keep #(when (or (keyword? %) (string? %)) (name %))) ks)
        re     (fn [res part ns cands]
                 (cond-> res
                   (seq ns) (update part view/recompute ns #(first-key (cands %)))))]
    (as-> res res
      (update res :keys view/recompute-set ks here?)
      (re res :by-nf names (fn [n] (for [[_ k] (kdb/nodes db1 :by-name-str [sec n])
                                         :when (= n (nm-of db1 k))]
                                     k)))
      (re res :by-k knames (fn [n] (filter here? [(keyword n) n]))))))

(defn- refs-of
  [db0 db1 sec k]
  (let [nf (rid/name-field-of sec)]
    (into #{(ix/nm k)} (keep #(some-> (get (kdb/entry % [sec k]) nf) ix/nm)) [db0 db1])))

(defn- members
  [db0 db1 kind xs]
  (into #{} (for [db [db0 db1] x xs n (kdb/nodes db :members [kind x])] n)))

(defn- spaces-step
  [spaces db0 db1 touched]
  (let [tp      (get touched :projects #{})
        ts      (get touched :spaces #{})
        project (resolver-step (:project spaces) db0 db1 :projects tp)
        sres    (resolver-step (:space-res spaces) db0 db1 :spaces ts)
        via     (into #{} (map second)
                      (members db0 db1 :space (mapcat #(refs-of db0 db1 :spaces %) ts)))
        ps      (into tp via)
        space   (view/recompute (:space spaces) ps
                                #(when-some [e (kdb/entry db1 [:projects %])]
                                   (or (sch/resolve-in sres (:space e)) sch/default-space)))]
    [{:project project :space-res sres :space space} ps]))

(defn- rescoped
  [db0 db1 ps]
  (reduce (fn [m [sec k]] (nested/conj-in m [sec] k))
          {}
          (members db0 db1 :project (mapcat #(refs-of db0 db1 :projects %) ps))))

(defn- step
  [old changes db0 db1]
  (let [touched (into {} (for [kk rid/rid-kinds :let [ks (view/touched changes kk)]
                               :when (seq ks)]
                           [kk ks]))
        tts     (view/touched changes :transforms)]
    (if (and (empty? touched) (empty? tts))
      (-> old (update :name->rid framed db1) (update :name->id framed db1))
      (let [[spaces ps] (if (or (touched :projects) (touched :spaces))
                          (spaces-step (:spaces old) db0 db1 touched)
                          [(:spaces old) #{}])
            names       (merge-with into touched (rescoped db0 db1 ps))]
        (-> old
            (assoc :spaces spaces)
            (update :name->rid step-names :name->rid spaces db0 db1 names)
            (update :name->id step-names :name->id spaces db0 db1 names)
            (step-rids db1 (into #{} cat (vals touched)))
            (update :producers step-producers db0 db1 tts))))))

(def view
  {:id    :sutra.types.schema/index
   :reads (into #{:transforms} rid/rid-kinds)
   :init  (fn [db] (sch/build-index (kdb/world db)))
   :step  step})
