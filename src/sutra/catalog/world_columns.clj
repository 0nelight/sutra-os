(ns sutra.catalog.world-columns
  (:require [sutra.catalog.interfaces :as interfaces]
            [sutra.catalog.world-schema-index :as world-schema-index]
            [sutra.catalog.link-ends :as link-ends]
            [sutra.catalog.lookup :as lookup]
            [sutra.catalog.outputs :as outputs]
            [sutra.catalog.transforms :as transforms]
            [sutra.world.db :as kdb]
            [sutra.world.index :as ix]
            [sutra.types.nested :as nested]
            [sutra.world.view :as view]
            [sutra.ref.install :as ri]
            [sutra.types.column :as column]
            [sutra.types.kinds :as kinds]
            [sutra.types.naming :as naming]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]
            [sutra.types.value-types :as vt]))

(set! *warn-on-reflection* true)

(defn- array-cols [attr-list]
  (vec (for [{:keys [attribute-name attribute-type]} attr-list
             :when (vt/json-attribute? attribute-type)]
         (column/sql-name attribute-name))))

(defn- ot-array-cols
  [W anc ot]
  (array-cols (ri/contribute-props W anc (:implements ot) (:attribute-list ot))))

(def ^:private table-facts
  {:transforms   (fn [k _] [(naming/trans-table k)])
   :object-types (fn [k _] [(naming/obj-table k)])})

(defn- table-cols
  [W entry anc owners]
  (let [by (group-by first owners)
        ot (some entry (:object-types by))
        t  (some entry (:transforms by))]
    (cond ot (ot-array-cols W anc ot)
          t  (array-cols (:output-columns t)))))

(defn array-cols-index
  [W]
  (let [anc (:ancestors (interfaces/ifaces W))]
    (into {} (for [[table owners] (view/fact-index table-facts W)]
               [table (table-cols W #(get-in W %) anc owners)]))))

(defn- type-attrs
  [db k]
  (let [ot (kdb/entry db [:object-types k]) i (kdb/entry db [:interfaces k])]
    (get (lookup/attrs-index-of (cond-> {} ot (assoc :object-types {k ot})
                                        i  (assoc :interfaces {k i})))
         k)))

(def attrs
  {:id    :sutra.catalog.lookup/attrs
   :reads #{:interfaces :object-types}
   :init  (fn [db] (lookup/attrs-index-of (kdb/world db)))
   :step  (fn [old changes _ db1]
            (view/recompute old
                            (into (view/touched changes :interfaces)
                                  (view/touched changes :object-types))
                            #(type-attrs db1 %)))})

(defn- here? [db sec k] (some? (kdb/entry db [sec k])))

(defn- put
  [m db sec ks f]
  (view/recompute m ks #(when (here? db sec %) (f %))))

(def ifaces
  {:id    :sutra.catalog.interfaces/ifaces
   :reads #{:interfaces}
   :init  (fn [db] (interfaces/iface-index (kdb/world db)))
   :step  (fn [old changes db0 db1]
            (let [is  (view/interfaces-moved changes db0 db1)
                  anc (put (:ancestors old) db1 :interfaces is #(view/ancestors-of db1 %))]
              {:ancestors anc
               :known     (view/recompute-set (:known old) (view/touched changes :interfaces)
                                              #(here? db1 :interfaces %))
               :effective (put (:effective old) db1 :interfaces is
                               #(ri/effective-interface (kdb/world db1) anc %))}))})

(def tables
  (view/facts ::tables table-facts))

(def array-cols-view
  {:id    ::array-cols
   :reads #{:transforms :object-types :interfaces}
   :init  (fn [db] (array-cols-index (kdb/world db)))
   :step  (fn [old changes db0 db1]
            (let [anc (:ancestors (kdb/view db1 :sutra.catalog.interfaces/ifaces))
                  ts  (kdb/view db1 ::tables)
                  ots (into (view/touched changes :object-types)
                            (for [i (view/interfaces-moved changes db0 db1)
                                  n (kdb/nodes db1 :edges [:implements i])]
                              (second n)))
                  ns  (into (set (map naming/obj-table ots))
                            (map naming/trans-table (view/touched changes :transforms)))]
              (view/recompute old ns
                              #(table-cols (kdb/world db1) (partial kdb/entry db1) anc
                                           (get ts %)))))})

(defn- inputs-of [t] (cons (:input t) (vals (:inputs t))))

(defn listens
  [t]
  (let [ins (inputs-of t) outs (outputs/outputs-of t)]
    (concat
     (for [in ins :when (= :transform (:kind in))] [:t (:transform in)])
     (for [in ins
           :let [s (kinds/input-names-section in)]
           :when (and s (not= :transforms s))]
       [:out (rid/kinds-key->tag s) (kinds/input-ref-val in)])
     (for [in (vals (:inputs t)) :when (= :media-set-items (:kind in))] [:media (:media-set in)])
     (for [in ins :when (= :dataset (:kind in))] [:ds (:dataset in)])
     (for [o outs :when (= :link-type (:kind o))] [:link (:link-type o)])
     (for [o outs :when (transforms/supplements? o)] [:out :object-type (:object-type o)]))))

(defn emits
  [t]
  (let [outs (outputs/outputs-of t)]
    (concat (for [o outs] [:out (:kind o) (get o (:kind o))])
            (for [d (keep :dataset (cons t outs))] [:ds d])
            (for [o (vals (:media-outputs t))] [:media (:media-set o)]))))

(def ^:private referent-at
  {:out 2 :t 1 :ds 1 :media 1 :link 1})

(defn referent
  [[tag :as sig]]
  (let [i (or (referent-at tag) (throw (ex-info "unknown signal" {:signal sig})))]
    [(subvec (vec sig) 0 i) (nth sig i)]))

(defn- signal-index
  [aux db0 db1 ks]
  (reduce (fn [a k]
            (let [t0 (kdb/entry db0 [:transforms k]) t1 (kdb/entry db1 [:transforms k])]
              (-> a
                  (update :listen ix/refile k (when t0 (listens t0)) (when t1 (listens t1)))
                  (update :emit ix/refile k (when t0 (emits t0)) (when t1 (emits t1))))))
          aux ks))

(defn- lookups
  [db aux links?]
  (let [ts (get (kdb/world db) :transforms)
        by #(get-in aux [:emit %] #{})]
    {:producers         (fn [kind target]
                          (when (some? target)
                            (for [u (sort-by str (by [:out kind target]))
                                  :let [t (get ts u)]
                                  o (outputs/outputs-of t)
                                  :when (and (= kind (:kind o)) (= target (get o kind)))]
                              [u t o])))
     :dataset-producers #(if (some? %) (by [:ds %]) #{})
     :media-producers   #(by [:media %])
     :link              (if links? #(kdb/entry db [:link-types %]) (constantly nil))}))

(defn- supplementing
  [db aux]
  (let [ts (get (kdb/world db) :transforms)]
    (fn [ot]
      (into #{}
            (filter (fn [u] (some #(and (transforms/supplements? %) (= ot (:object-type %)))
                                  (outputs/outputs-of (get ts u)))))
            (get-in aux [:emit [:out :object-type ot]])))))

(defn- moved
  [changes aux1 db0 db1 links?]
  (let [tk   (view/touched changes :transforms)
        outs (for [k tk
                   t [(kdb/entry db0 [:transforms k]) (kdb/entry db1 [:transforms k])]
                   :when t
                   f (emits t)]
               f)
        sigs (cond-> (into (into #{} (map #(vector :t %)) tk) outs)
               links? (-> (into (map #(vector :link %)) (view/touched changes :link-types))
                          (into (for [[_ kind o] outs
                                      :when (= :object-type kind)
                                      n (kdb/nodes db1 :edges [:link-end o])]
                                  [:link (second n)]))))]
    (into tk (mapcat #(get-in aux1 [:listen %])) sigs)))

(defn- rewired
  [graph db aux ks links?]
  (let [ts   (get (kdb/world db) :transforms)
        look (lookups db aux links?)
        supp (supplementing db aux)
        g    (reduce (fn [g k]
                       (if-some [t (get ts k)]
                         (let [ds (into #{} (filter #(contains? ts %))
                                        (transforms/deps-with look t))]
                           (-> g
                               (assoc-in [:deps k] ds)
                               (assoc-in [:schedule k] (transforms/schedule-with supp k t ds))))
                         (-> g (update :deps dissoc k) (update :schedule dissoc k))))
                     graph ks)
        r    (reduce (fn [r k] (ix/refile r k (get-in graph [:deps k]) (get-in g [:deps k])))
                     (:rdeps graph) ks)
        r    (view/recompute r (into (set ks) (mapcat #(get-in graph [:deps %])) ks)
                             #(when (contains? ts %) (get r % #{})))]
    (assoc g :rdeps r)))

(defn- trimmed [ls] (if (and (seq ls) (empty? (peek ls))) (recur (pop ls)) ls))

(defn- relaid
  [levels moves]
  (let [out (reduce (fn [m [x from]] (cond-> m from (nested/conj-in [from] x))) {} moves)
        in  (reduce (fn [m [x _ to]] (cond-> m to (update to (fnil conj []) x))) {} moves)
        top (reduce max -1 (keys in))
        ls  (into levels (repeat (max 0 (- (inc top) (count levels))) []))]
    (trimmed (reduce (fn [ls i]
                       (assoc ls i (vec (sort-by str (concat (remove (get out i #{}) (get ls i))
                                                             (get in i))))))
                     ls (distinct (concat (keys out) (keys in)))))))

(defn core-moved
  [s0 s1 down left0 live? ss]
  (let [d     (set (view/reach down ss))
        up    (view/reach (fn [x] (filter #(or (left0 %) (d %)) (concat (s0 x) (s1 x)))) ss)
        gone  (set (view/reach down (concat ss up)))
        z     (into #{} (filter live?) gone)
        leftz (or (:cycle (ri/kahn (into {} (for [x z]
                                               [x (into #{} (keep #(cond (z %) % (left0 %) ::left))
                                                        (s1 x))]))))
                  #{})]
    {:gone gone :z z :leftz leftz :corez (transforms/cycle-core s1 leftz)}))

(defn- relevelled
  [{:keys [level core]} g0 g1 live? ss]
  (let [s0    #(get-in g0 [:schedule %])
        s1    #(get-in g1 [:schedule %])
        down  #(concat (get-in g0 [:rdeps %]) (get-in g1 [:rdeps %]))
        left0 (or (:cycle g0) #{})
        {:keys [gone z leftz corez]} (core-moved s0 s1 down left0 live? ss)
        n     (into #{} (remove corez) z)
        order (apply concat (:levels (ri/kahn (into {} (for [x n]
                                                         [x (into #{} (filter n) (s1 x))])))))
        lv    (reduce (fn [lv x]
                        (assoc lv x (let [ls (keep lv (s1 x))]
                                      (if (seq ls) (inc (apply max ls)) 0))))
                      (apply dissoc level gone) order)
        left1 (into (reduce disj left0 gone) leftz)]
    {:graph (cond-> (assoc g1 :levels (relaid (:levels g0)
                                              (for [x gone :when (not= (level x) (lv x))]
                                                [x (level x) (lv x)])))
              (seq left1)  (assoc :cycle left1)
              (empty? left1) (dissoc :cycle))
     :level lv
     :core  (into (reduce disj core gone) corez)}))

(defn- graph-state-init [links? db]
  (let [W (kdb/world db)
        g (transforms/graph-of (if links? W (select-keys W [:transforms])))
        l (into {} (for [[i ks] (map-indexed vector (:levels g)) k ks] [k i]))]
    {:graph g
     :aux   (signal-index {:listen {} :emit {}} {} db (keys (:transforms W)))
     :level l
     :core  (into #{} (remove l) (:cycle g))}))

(defn- graph-state-of
  [id links?]
  {:id    id
   :reads (cond-> #{:transforms} links? (conj :link-types))
   :init  (partial graph-state-init links?)
   :step  (fn [{:keys [graph aux] :as old} changes db0 db1]
            (let [aux1 (signal-index aux db0 db1 (view/touched changes :transforms))
                  ks   (moved changes aux1 db0 db1 links?)
                  g1   (rewired graph db1 aux1 ks links?)
                  ss   (filter #(not= (get-in graph [:schedule %]) (get-in g1 [:schedule %])) ks)
                  ts   (get (kdb/world db1) :transforms)]
              (assoc (if (seq ss)
                       (relevelled old graph g1 #(contains? ts %) ss)
                       (assoc old :graph g1))
                     :aux aux1)))})

(def graph-state (graph-state-of ::graph-state true))

(def loop-state
  (graph-state-of ::loop-state false))

(def graph
  {:id    :sutra.catalog.transforms/graph
   :reads #{:transforms :link-types}
   :init  (fn [db] (:graph (kdb/view db ::graph-state)))
   :step  (fn [_ _ _ db1] (:graph (kdb/view db1 ::graph-state)))})

(def rows [attrs ifaces tables array-cols-view graph-state graph link-ends/view
           world-schema-index/view])

(def column-fns
  {:sutra.catalog.lookup/attrs      lookup/attrs-index-of
   :sutra.catalog.interfaces/ifaces interfaces/iface-index
   ::array-cols                     array-cols-index
   :sutra.catalog.transforms/graph  transforms/graph-of
   ::link-ends/ends                 #(link-ends/by-end (:link-types %))
   :sutra.types.schema/index        sch/build-index})

(def columns (set (keys column-fns)))

(defn carried
  [d]
  (let [vs (:views d)]
    (into {} (for [id columns :when (contains? vs id)] [id (get vs id)]))))

(defn world-of
  [d]
  (sch/with-views (kdb/world d) (carried d)))
