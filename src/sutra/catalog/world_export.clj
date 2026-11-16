(ns sutra.catalog.world-export
  (:require [sutra.world.db :as kdb]
            [sutra.world.delta :as d]
            [sutra.world.index :as ix]
            [sutra.world.view :as view]
            [sutra.parser.project-emit :as emit]
            [sutra.types.entities :as ents]
            [sutra.types.rid :as rid]
            [sutra.types.tree :as tree]
            [sutra.types.world-rules :as world-rules]))

(set! *warn-on-reflection* true)

(defn- rid-name
  [db r]
  (reduce (fn [n kk] (if-some [m (some-> (kdb/entry db [kk r]) (get (rid/name-field-of kk)))]
                       m
                       n))
          nil rid/rid-kinds))

(defn- in-section-order
  [db sec ks]
  (if (next ks)
    (let [ks (set ks)] (last (filter ks (keys (get (kdb/world db) sec)))))
    (first ks)))

(defn- lookup [f]
  (reify clojure.lang.ILookup
    (valAt [_ k] (f k))
    (valAt [_ k nf] (let [v (f k)] (if (some? v) v nf)))))

(defn- context
  [db facts inline? deps]
  (let [note!  (fn [dep] (vswap! deps conj dep))
        idx    (lookup (fn [r] (note! [:rid r]) (rid-name db r)))
        named  (fn [sec nf]
                 (lookup (fn [n]
                           (note! [:name sec (ix/nm n)])
                           (let [ks (keep (fn [[_ k]] (when (= n (get (kdb/entry db [sec k]) nf))
                                                        k))
                                          (kdb/nodes db :by-name-str [sec (ix/nm n)]))]
                             (when-some [k (in-section-order db sec ks)]
                               (get (emit/names-resolved idx sec {k (kdb/entry db [sec k])})
                                    k))))))
        fact   (fn [f] (note! [:fact f])
                 (some->> (seq (get facts f)) (map second) (in-section-order db :transforms)))
        tx     (fn [k] (get (emit/names-resolved idx :transforms
                                                 {k (kdb/entry db [:transforms k])})
                            k))
        ext    (fn [kind] (lookup (fn [n] (some-> (fact [:ext kind n]) tx emit/external-source))))]
    {:idx        idx
     :vt-by-name (named :value-types :value-type-name)
     :sp-by-name (named :shared-properties :shared-property-name)
     :producers  (lookup (fn [n] (some-> (fact [:producer n]) tx)))
     :ext-by-obj (ext :object-type)
     :ext-by-ds  (ext :dataset)
     :pipeline?  (fn [p] (note! [:fact [:pipeline p]]) (boolean (seq (get facts [:pipeline p]))))
     :inline?    inline?}))

(def ^:private filed
  {[:by-node] (comp keys :nodes) [:rdeps] :deps [:facts] :facts})

(defn- winner
  [db st a]
  (let [cs (for [n (get-in st [:by-node a])] [n (get-in st [:contrib n :nodes a])])]
    (when (seq cs)
      (let [top  (apply max (map (comp :rank second) cs))
            best (filter #(= top (:rank (second %))) cs)
            [s]  (first (first best))
            ks   (map (comp second first) best)
            e    (dissoc (get-in st [:contrib [s (in-section-order db s ks)] :nodes a]) :rank)]
        (if (and (:vector? e) (next ks))
          (let [ks (set ks)]
            (assoc e :all (vec (for [k (keys (get (kdb/world db) s)) :when (ks k)]
                                 (get-in st [:contrib [s k] :nodes a :value])))))
          e)))))

(defn- recompute
  [db inline? st [sec k :as node]]
  (let [c0 (get-in st [:contrib node])
        e  (kdb/entry db node)
        c  (when (some? e)
             (let [deps  (volatile! #{})
                   facts (ix/retract (:facts st) node (:facts c0))
                   out   (try (emit/node-export (context db facts inline? deps) sec k e)
                              (catch InterruptedException x (throw x))
                              (catch Exception x {:nodes {} :facts #{} :error (ex-message x)}))]
               (assoc out :deps @deps)))]
    [(-> (ix/refile-record st [:contrib] filed node c)
         (update :errors (if (:error c) conj disj) node))
     (ix/changed (:facts c0) (:facts c))]))

(def ^:private first-sections
  {:pipelines 0 :transforms 1})

(defn- settle
  [db inline? st todo]
  (loop [st st queue (sort-by #(get first-sections (first %) 2) todo) seen-authors #{}]
    (if-let [[node & more] (seq queue)]
      (let [authors     (into seen-authors (keys (get-in st [:contrib node :nodes])))
            [st moved]  (recompute db inline? st node)
            authors     (into authors (keys (get-in st [:contrib node :nodes])))
            woken       (distinct (mapcat #(get-in st [:rdeps [:fact %]]) moved))]
        (recur st (sort-by #(get first-sections (first %) 2) (concat more woken)) authors))
      (update st :index view/recompute seen-authors #(winner db st %)))))

(defn- moved-keys
  [changes db0 db1]
  (into #{}
        (mapcat (fn [[sec k :as node]]
                  (let [nf (ix/name-field sec)]
                    (cond-> [[:rid k]]
                      nf (into (for [db [db0 db1]
                                     :let [n (get (kdb/entry db node) nf)]
                                     :when (some? n)]
                                 [:name sec (ix/nm n)]))))))
        (d/nodes changes)))

(defn- row [id inline?]
  {:id    id
   :reads nil
   :init  (fn [db]
            (settle db inline? {:index {} :errors #{}}
                    (for [[sec m] (kdb/world db) :when (map? m) k (keys m)] [sec k])))
   :step  (fn [old changes db0 db1]
            (let [touched (set (d/nodes changes))
                  asked   (mapcat #(get-in old [:rdeps %]) (moved-keys changes db0 db1))]
              (settle db1 inline? old (into touched asked))))})

(def authored
  (row ::authored false))

(def inlined
  (row ::inlined true))

(def rows [authored inlined])

(defn index
  [d id]
  (let [st (or (kdb/view d id) (throw (ex-info "kern db carries no export" {:view id})))]
    (when-let [node (first (sort-by str (:errors st)))]
      (throw (ex-info (str "export of " (pr-str node) ": " (get-in st [:contrib node :error]))
                      {:node node})))
    (:index st)))

(defn units
  [d id]
  (let [st      (or (kdb/view d id) (throw (ex-info "kern db carries no export" {:view id})))
        W       (kdb/world d)
        contrib (fn [sec k] (get-in st [:contrib [sec k]]))
        member  (fn [p] (for [k (sort (map second (get-in st [:facts [:in-pipeline p]])))
                              :let [e (:pipe (contrib :transforms k))]
                              :when e]
                          e))
        views   (reduce (fn [m k] (if-let [s (:view-app (contrib :applications k))]
                                    (if-some [ot (some-> (:object-type s) name keyword)]
                                      (update m ot (fnil conj []) [k s])
                                      m)
                                    m))
                        (array-map) (keys (:applications W)))]
    (vec (concat
          (for [k (keys (:pipelines W)) :let [p (:pipeline (contrib :pipelines k))] :when p]
            (emit/pipeline-unit* p (member (:pipeline-name p))))
          (mapcat #(:units (contrib :templates %)) (keys (:templates W)))
          (mapcat #(:units (contrib :applications %)) (keys (:applications W)))
          (for [[ot vs] views] (emit/view-unit ot vs))))))

(defn- live
  [d]
  (let [W (kdb/world d)]
    (emit/live-lookup
     (fn [n]
       (let [ks (keep (fn [[_ k]] (when (= n (some-> (kdb/entry d [:object-types k])
                                                     :object-type-name name keyword))
                                    k))
                      (kdb/nodes d :by-name-str [:object-types (ix/nm n)]))]
         (when-some [rid (in-section-order d :object-types ks)]
           (let [o  (get-in W [:object-types rid])
                 pk (world-rules/object-primary-key W rid)]
             (cond-> {:attributes (mapv (juxt :attribute-name :attribute-type)
                                        (:attribute-list o))
                      :title      (:title o)
                      :match-keys (vec (:match-keys o))}
               (and pk (not (sequential? pk))) (assoc :primary-key (keyword (name pk)))
               (and (sequential? pk) (= 1 (count pk)))
               (assoc :primary-key (keyword (name (first pk))))))))))))

(defn held
  [d]
  (let [lv    (live d)
        body  (fn [id] (delay (emit/body-of (index d id) lv)))
        auth  (body ::authored)
        inl   (body ::inlined)
        psec  (ents/section->author :projections)
        laid  (delay (cond-> @auth
                       (map? (get @auth psec))
                       (update psec update-vals #(cond-> % (map? %) (dissoc :layout)))))]
    {:laid-body laid
     :laid      (delay (tree/index @laid))
     :units     (delay (units d ::authored))
     :authored  (delay (tree/index @auth))
     :inlined   (delay (tree/index @inl))}))

