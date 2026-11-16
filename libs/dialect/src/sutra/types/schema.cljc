(ns sutra.types.schema
  (:require [sutra.types.rid :as rid]
            [sutra.types.transform-io :as tio]))

(def default-space :space/default)

(defn- carried
  [m]
  (when-let [c (::carry (meta m))]
    (when (identical? m @(:of c)) c)))

(defn indexed
  [m]
  (if (or (nil? m) (carried m))
    m
    (let [of (volatile! nil)
          m' (vary-meta m assoc ::carry {:of of :cols (atom {})})]
      (vreset! of m')
      m')))

(defn with-views
  [m vs]
  (let [m (indexed m)]
    (when-let [cols (:cols (carried m))]
      (swap! cols into (map (fn [[k v]] [k (delay v)])) vs))
    m))

(defn column
  [m k f]
  (if-let [cols (:cols (carried m))]
    @(or (get @cols k)
         (get (swap! cols (fn [c] (if (contains? c k) c (assoc c k (delay (f m)))))) k))
    (f m)))

(defn- by-str [es] (sort-by (comp str first) es))

(defn resolver-of
  [m section]
  (let [es   (get m section)
        nf   (rid/name-field-of section)
        once (fn [acc n r] (if (contains? acc n) acc (assoc acc n r)))
        es'  (by-str es)]
    {:keys  (set (keys es))
     :by-nf (reduce (fn [acc [r s]] (if-some [n (get s nf)] (once acc (name n) r) acc)) {} es')
     :by-k  (reduce (fn [acc [r _]] (if (or (keyword? r) (string? r)) (once acc (name r) r) acc))
                    {} es')}))

(defn resolve-in
  [{:keys [keys by-nf by-k]} x]
  (when x
    (if (contains? keys x) x (let [n (name x)] (or (get by-nf n) (get by-k n))))))

(defn spaces-of
  [m]
  (let [project (resolver-of m :projects)
        space   (resolver-of m :spaces)]
    {:project   project
     :space-res space
     :space     (into {}
                      (map (fn [[r p]] [r (or (resolve-in space (:space p)) default-space)]))
                      (get m :projects))}))

(defn- scope-in
  [{:keys [project space]} spec]
  (when-let [p (:project spec)]
    (get space (resolve-in project p) default-space)))

(defn- pick
  [scope n hits]
  (if (apply = (map #(nth % 2) hits))
    (second (peek hits))
    (let [one (fn [s]
                (let [xs (filterv #(= s (nth % 2)) hits)]
                  (when (= 1 (count xs)) (second (xs 0)))))]
      (or (when scope (one scope))
          (one nil)
          (rid/ambiguous n (map (fn [[_ v s]] [v s]) hits))))))

(def name-keys
  {:name->rid {:key #(if (string? %) (keyword %) %) :val (fn [r _] r)}
   :name->id  {:key identity                        :val (fn [_ spec] (:id spec))}})

(defn name-entry
  [spaces scope ix kk n entries]
  (let [{kf :key vf :val} (name-keys ix)
        nf                (rid/name-field-of kk)
        hits              (into [] (keep (fn [[r spec]]
                                           (let [v (get spec nf)]
                                             (when (and (some? v) (= n (kf v)))
                                               [n (vf r spec) (scope-in spaces spec)]))))
                                (by-str entries))]
    (when (seq hits) (pick scope n hits))))

(defn- name-index-of
  [m spaces ix]
  (let [scope (::scope (meta m))
        {kf :key vf :val} (name-keys ix)]
    (into {}
          (for [kk    rid/rid-kinds
                :let  [nf (rid/name-field-of kk) entries (get m kk)]
                :when (and nf (map? entries))]
            [kk (persistent!
                 (reduce-kv
                  (fn [acc n hits] (assoc! acc n (pick scope n hits)))
                  (transient {})
                  (reduce
                   (fn [acc [r spec]]
                     (if-some [n (get spec nf)]
                       (let [k (kf n)]
                         (assoc acc k (conj (get acc k []) [k (vf r spec)
                                                            (scope-in spaces spec)])))
                       acc))
                   {} (by-str entries))))]))))

(defn rid-entry
  [entries-of r]
  (reduce (fn [[n s] kk]
            (if-some [e (entries-of kk)]
              [(if-some [v (get e (rid/name-field-of kk))] v n) kk]
              [n s]))
          [nil nil] rid/rid-kinds))

(defn- rid->section-index [m]
  (into {} (for [kk rid/rid-kinds, [r _] (get m kk) :when (some? r)] [r kk])))

(defn- producer-keys
  [t]
  (let [outs (tio/outputs-of t)]
    {:outs (for [out outs :let [target (get out (:kind out))] :when (some? target)]
             [[(:kind out) target] out])
     :datasets (keep :dataset (cons t outs))}))

(defn- sort-producers [v] (vec (sort-by (comp str first) v)))

(defn add-producer
  [idx k t]
  (let [{:keys [outs datasets]} (producer-keys t)]
    (as-> idx idx
      (reduce (fn [idx [ok out]]
                (update-in idx [:by-output ok] #(sort-producers (conj (or % []) [k t out]))))
              idx outs)
      (reduce (fn [idx ds] (update-in idx [:by-dataset ds] (fnil conj #{}) k)) idx datasets))))

(defn drop-producer
  [idx k t]
  (let [{:keys [outs datasets]} (producer-keys t)
        gone (fn [idx path f]
               (let [v (f (get-in idx path))]
                 (if (empty? v)
                   (let [idx (update idx (first path) dissoc (second path))]
                     (if (empty? (get idx (first path))) (dissoc idx (first path)) idx))
                   (assoc-in idx path v))))]
    (as-> idx idx
      (reduce (fn [idx [ok _]]
                (gone idx [:by-output ok] #(vec (remove (fn [[k' _ _]] (= k k')) %))))
              idx (distinct (map (fn [[ok out]] [ok out]) outs)))
      (reduce (fn [idx ds] (gone idx [:by-dataset ds] #(disj % k))) idx datasets))))

(defn- producer-index [m]
  (reduce (fn [idx [k t]] (add-producer idx k t)) {} (by-str (:transforms m))))

(defn build-index
  [m]
  (let [spaces (spaces-of m)]
    {:spaces       spaces
     :name->rid    (name-index-of m spaces :name->rid)
     :name->id     (name-index-of m spaces :name->id)
     :rid->name    (rid/rid->name-index m)
     :rid->section (rid->section-index m)
     :producers    (producer-index m)}))

(defn index [m] (column m ::index build-index))

(defn project-space
  [m p]
  (let [{:keys [project space]} (:spaces (index m))]
    (get space (resolve-in project p) default-space)))

(defn scope-of
  [m spec]
  (scope-in (:spaces (index m)) spec))

(defn in-scope
  [m space]
  (if (nil? space)
    m
    (column m [::in-scope space] #(indexed (vary-meta % assoc ::scope space)))))

(defn name-index [m] (:name->id (index m)))

(defn rid-name-index [m] (:rid->name (index m)))

(defn rid-of
  [idx kinds-key name-kw]
  (rid/resolve-name->rid (:name->rid idx) [kinds-key] name-kw))

(defn name-of [idx r]
  (get-in idx [:rid->name r]))

(defn section-of [idx r]
  (get-in idx [:rid->section r]))

(defn outputs-for
  [idx kind target]
  (get-in idx [:producers :by-output [kind target]] []))

(defn producers-of-dataset [idx ds-rid]
  (get-in idx [:producers :by-dataset ds-rid] #{}))
