(ns sutra.world.install
  (:require [sutra.world.db :as db]
            [sutra.world.delta :as d]
            [sutra.world.gate :as gate]
            [sutra.world.index :as ix]
            [sutra.world.view :as view]
            [sutra.ref.err :refer [fail!]]
            [sutra.types.entities :as ents]
            [sutra.types.refs :as refs]
            [sutra.types.table :as tbl]))

(def views (into view/rows gate/views))

(defn of-world
  [W]
  (db/db W views))

(def ^:private sigma-defaults
  {:authorize       (fn [_op _target] nil)
   :system-datasets (fn [] {})
   :fk-link?        (fn [_W _lt] false)})

(defn- sigma [cx k]
  (or (get-in cx [:sigma k]) (get sigma-defaults k)
      (fail! :sigma-missing (str "Σ lacks " k) {:key k})))

(defn- rewrite [sec e resolve] (refs/rewrite-refs (get ents/refs sec) e resolve))

(defn- normalised
  [db sec e]
  (cond-> (rewrite sec e (db/resolver db (db/scope-of db e)))
    (= :actions sec) (as-> a (refs/rewrite-refs ents/action-refs a (db/resolver db nil)))))

(defn- check-edit-conflict!
  [k ot]
  (let [{:keys [strategy timestamp-attr]} (:edit-conflict ot)]
    (when (= :apply-most-recent (some-> strategy keyword))
      (let [a (some #(when (= (some-> timestamp-attr name) (some-> (:attribute-name %) name)) %)
                    (:attribute-list ot))]
        (when-not (= :timestamp (some-> (:attribute-type a) keyword))
          (fail! :edit-conflict-timestamp
                 (str "Object type " (pr-str k) ": :apply-most-recent needs :timestamp-attr"
                      " naming one of its :timestamp attributes"
                      (when timestamp-attr (str ", not " (pr-str timestamp-attr))))
                 {:name k :timestamp-attr timestamp-attr}))))))

(defn- implementing
  [db v]
  (let [v (cond-> v (= [] (:implements v)) (dissoc :implements))]
    (cond-> v
      (seq (:implements v))
      (assoc :attribute-list (gate/contribute-props (get (db/world db) :interfaces)
                                                    (db/view db :ancestors)
                                                    (:implements v) (:attribute-list v))))))

(defn- backing
  [db _cx r ot]
  (if (some? (:dataset ot))
    {}
    (let [ds (db/rid :dataset (db/loc r))]
      {[:object-types r] (assoc ot :dataset ds)
       [:datasets ds]    (merge (cond-> {:id ds :dataset-name (:object-type-name ot)
                                         :kind :object-backing
                                         :columns (vec (:attribute-list ot []))}
                                  (some? (:project ot)) (assoc :project (:project ot)))
                                (db/entry db [:datasets ds]))})))

(defn- uuid3 [s]
  #?(:clj  (str (java.util.UUID/nameUUIDFromBytes (.getBytes ^String s "UTF-8")))
     :cljs (fail! :kern-unsupported "name-based uuids need the JVM" {:of s})))

(defn- named-ds
  [db m nm]
  (into (into #{} (remove #(contains? m %)) (db/nodes db :by-name-str [:datasets (ix/nm nm)]))
        (keep (fn [[[sec :as node] e]]
                (when (and (= :datasets sec) (= (ix/nm nm) (ix/nm (:dataset-name e)))) node)))
        m))

(defn- entry-in [db m node] (if (contains? m node) (get m node) (db/entry db node)))

(defn- put-dataset
  [db m r nm cols t]
  (assoc m [:datasets r]
         (merge (cond-> {:id r :dataset-name nm :kind :transform-output :columns (vec cols)}
                  (some? (:project t)) (assoc :project (:project t)))
                (entry-in db m [:datasets r]))))

(defn- named-outputs
  [db cx m t]
  (reduce (fn [m {:keys [kind dataset]}]
            (if (and (= :dataset kind) dataset (not (db/rid? dataset))
                     (empty? (named-ds db m dataset)))
              (put-dataset db m (db/rid :dataset ((sigma cx :fresh-uuid)))
                           (keyword (name dataset)) [] t)
              m))
          m (concat (some-> (:output t) vector) (:outputs t))))

(defn- output-name
  [db m d nm]
  (if (seq (disj (named-ds db m nm) [:datasets d]))
    (keyword (str (name nm) "-output"))
    nm))

(defn- backed
  [db m tr paths d nm cols]
  (let [t (reduce (fn [t p] (assoc-in t p d)) (get m [:transforms tr]) paths)
        m (assoc m [:transforms tr] t)]
    (put-dataset db m d (output-name db m d nm) cols t)))

(defn- transform-outputs
  [db m tr]
  (let [t  (get m [:transforms tr])
        nm (:transform-name t)
        ds (db/rid :dataset (db/loc tr))]
    (cond
      (:output-columns t)
      (if (:dataset t) m (backed db m tr [[:dataset]] ds nm (:output-columns t)))
      (:output t)
      (if (get-in t [:output :dataset])
        m
        (backed db m tr [[:output :dataset] [:dataset]] ds nm []))
      (seq (:outputs t))
      (let [n (count (:outputs t))]
        (reduce (fn [m i]
                  (let [out (get-in m [[:transforms tr] :outputs i])]
                    (if (:dataset out)
                      m
                      (backed db m tr (cond-> [[:outputs i :dataset]] (zero? i) (conj [:dataset]))
                              (if (= 1 n)
                                ds
                                (db/rid :dataset (uuid3 (str (db/loc tr) "/"
                                                             (name (:name out))))))
                              (:name out) []))))
                m (range n)))
      :else m)))

(defn- outputs
  [db cx r t]
  (transform-outputs db (named-outputs db cx {[:transforms r] t} t) r))

(defn- outputs-edit-conflict!
  [db _r t]
  (doseq [o (or (seq (:outputs t)) (some-> (:output t) vector))
          :when (and (:edit-conflict o) (:object-type o))
          :let [ot (some->> (db/ref-of db #{:object-types} (:object-type o))
                            (vector :object-types) (db/entry db))]]
    (check-edit-conflict! (:object-type o) (assoc ot :edit-conflict (:edit-conflict o)))))

(defn- sql-findings
  [cx db0 db1 k]
  (when-let [f (get-in cx [:sigma :sql-gate])]
    (not-empty (vec (f (db/world db0) (db/world db1) k)))))

(defn- name-taken! [sec k _existing]
  (fail! :name-taken (str "Cannot create " (name sec) " " (pr-str k)
                          ": the name is taken — update it instead")
         {:name k :section sec}))

(defn- transform-exists! [_sec k existing]
  (fail! :transform-exists (str "Cannot create transform " (pr-str k))
         {:name k :existing existing}))

(def ^:private takes
  (tbl/per-key
   "sutra.world.install/takes" ents/rid-sections
   {:object-types {:prepare implementing
                   :check (fn [_db r ot] (check-edit-conflict! r ot))
                   :stands-in backing}
    :transforms   {:taken transform-exists! :check outputs-edit-conflict!
                   :stands-in outputs :findings sql-findings}
    #{:spaces :projects :interfaces :link-types :actions :agents :applications :collections
      :custom-applications :datasets :exports :functions :graphs :media-sets :object-sets
      :object-type-groups :organizations :pipelines :repositories :shared-properties
      :sources :templates :value-types :workspaces} {}}))

(defn- row [sec k] (get-in takes [sec k]))

(defn- names-of [db0 db1 [sec k :as node]]
  (distinct (for [d [db0 db1] :let [e (db/entry d node)] :when e
                  x [k (get e (ix/name-field sec))] :when (some? x)]
              (ix/nm x))))

(defn- dependents
  [db0 db1 nodes]
  (into #{}
        (for [[sec :as node] nodes
              d [db0 db1]
              :let [n (get (db/entry d node) (ix/name-field sec))]
              :when (some? n)
              v (distinct [n (ix/nm n) (keyword (ix/nm n))])
              x (db/nodes db1 :refs-in v)]
          x)))

(defn- scoped-under
  [db0 db1 nodes]
  (let [under    (fn [kind node] (for [n (names-of db0 db1 node)
                                       x (db/nodes db1 :members [kind n])]
                                   x))
        projects (for [node nodes :when (= :spaces (first node)) p (under :space node)] p)]
    (into (set projects)
          (for [node (concat (filter #(= :projects (first %)) nodes) projects)
                m (under :project node)]
            m))))

(defn- changes-to [db m]
  (into [] (mapcat (fn [[node e]] (d/entry-delta node (db/entry db node) e))) m))

(defn- normalise
  [db0 db1 nodes]
  (changes-to db1 (into {}
                        (keep (fn [[s :as n]]
                                (when-let [e (db/entry db1 n)] [n (normalised db1 s e)])))
                        (concat nodes (dependents db0 db1 nodes)
                                (scoped-under db0 db1 nodes)))))

(defn- with-system-datasets [cx db]
  (changes-to db (into {} (map (fn [[ds e]] [[:datasets ds] e])) ((sigma cx :system-datasets)))))

(defn- finished
  [cx db0 c1 db1 [sec k] op r ddl-of]
  (let [nodes (d/nodes c1)
        c2    (normalise db0 db1 nodes)
        db2   (db/db-with db1 c2)
        c3    (with-system-datasets cx db2)
        db3   (db/db-with db2 c3)]
    {:db    db3
     :step-findings (some-> (row sec :findings) (apply [cx db0 db3 k]))
     :delta (d/squash (concat c1 c2 c3))
     :ddl   (ddl-of db3 nodes)
     :event {:op op :target {:type sec :id r}}}))

(def ^:private mode-keys
  [:language :code :query :inputs :input :source :ops :strategy :sources])

(def ^:private output-forms
  {:output [:output :primary-key :column-map :from-key-column :to-key-column :fk-attribute]
   :outputs [:outputs]
   :output-columns [:output-columns :primary-key]})

(defn- mode-flip? [{:keys [value]}]
  (boolean (or (:language value) (:input value) (:source value) (:strategy value))))

(def ^:private clear-table
  (into
   [{:section :sources :clears [:secret-ref] :restatable? false}
    {:section :actions :when #(contains? (:value %) :rules) :clears [:edits]}
    {:section :transforms :when mode-flip? :clears mode-keys}
    {:section :transforms :clears [:primary-key]
     :when #(and (mode-flip? %) (= :dataset (get-in (:merged %) [:output :kind])))}
    {:section :applications :when #(contains? (:value %) :layout)
     :clears [:source :object-type]}]
   (concat
    (for [[section ks] ents/whole-declaration]
      {:section section :when #(:whole-declaration? (:step %)) :clears ks})
    (for [[form ks] output-forms]
      {:section :transforms :when #(contains? (:value %) form)
       :clears (remove (set ks) (mapcat val (dissoc output-forms form)))}))))

(defn- merged
  [old step value]
  (if-not (and old value (not (:whole-entry? step)))
    value
    (let [m (merge old value)
          c {:section (first (:path step)) :value value :merged m :step step}]
      (reduce (fn [m {:keys [section clears restatable?] pred :when :or {restatable? true}}]
                (if (and (= section (:section c)) (or (nil? pred) (pred c)))
                  (reduce (fn [m k] (if (and restatable? (contains? value k)) m (dissoc m k)))
                          m clears)
                  m))
              m clear-table))))

(defn- rid-keyed? [sec] (contains? ents/rid-sections sec))

(defn- own
  [db sec k value]
  (if (rid-keyed? sec)
    (db/own-entry db sec k value)
    (when (db/entry db [sec k]) k)))

(defn- put
  [cx db step]
  (let [[sec k] (:path step)
        value   (:value step)
        rid?    (rid-keyed? sec)
        r       (or (own db sec k value)
                    (if rid? (db/rid (ents/section->tag sec) ((sigma cx :fresh-uuid))) k))
        v       (as-> (if rid?
                        (rewrite sec (assoc value :id r) (db/resolver db (db/scope-of db value)))
                        value) v
                  (merged (db/entry db [sec r]) step v)
                  ((or (row sec :prepare) (fn [_ v] v)) db v))
        _       (doseq [id [r nil]] ((sigma cx :authorize) :set {:type sec :id id}))
        _       (some-> (row sec :check) (apply [db r v]))
        c1      (changes-to db (merge {[sec r] v}
                                      (some-> (row sec :stands-in) (apply [db cx r v]))))]
    (finished cx db c1 (db/db-with db c1) [sec k] :set-schema r (constantly []))))

(defn- create
  [cx db step]
  (let [[sec k] (:path step)]
    (when-let [e (some->> (db/own-entry db sec k (:value step)) (vector sec) (db/entry db))]
      ((or (row sec :taken) name-taken!) sec k e))
    (put cx db step)))

(defn- pruned
  [db sec r]
  (merge
   (when (= :object-types sec)
     (into {} (for [n (db/nodes db :edges [:link-end r]) :when (= :link-types (first n))]
                [n nil])))
   (when (ents/in-role? :carries-datasets sec)
     (into {} (for [ds (into #{(db/rid :dataset (db/loc r))} (keep :dataset)
                             (:outputs (db/entry db [:transforms r])))
                    :when (db/entry db [:datasets ds])]
                [[:datasets ds] nil])))))

(defn- drops
  [cx db0 db1 nodes]
  (let [fk?     (sigma cx :fk-link?)
        held?   (fn [kind t] (seq (db/nodes db1 :tables [kind t])))
        gone    (for [[sec k :as n] nodes :when (nil? (db/entry db1 n))] [sec k (ix/table-name k)])
        kind    (fn [sec] (ix/table-kind sec))
        bulk    (into #{} (for [[sec _ t] gone :when (and t (= :bulk (kind sec)))
                                :when (not (held? :bulk t))]
                            t))
        links   (distinct
                 (for [[sec k t] gone
                       :when (and t (= :link (kind sec)) (not (fk? (db/world db0) k)))
                       :when (not-any? #(not (fk? (db/world db1) (second %)))
                                       (db/nodes db1 :tables [:link t]))]
                   t))
        sets    (distinct
                 (for [[sec _ t] gone
                       :when (and t (#{:bulk :ot} (kind sec)) (not (held? :bulk t))
                                  (not (held? :ot t)) (not (contains? bulk t)))]
                   t))]
    (into (vec (for [[sec k] gone :when (= :graphs sec)]
                 {:op :sweep-graph :graph (or (:id (db/entry db0 [sec k])) k)}))
          (map (fn [t] {:op :drop-table :table t}))
          (concat (map #(str "d_" %) bulk) (map #(str "c_" %) links) (map #(str "d_" %) sets)))))

(def ^:private protected-phases
  #{:active :endorsed})

(defn- delete
  [cx db step]
  (let [[sec k]  (:path step)
        existing (db/own-entry db sec k (:value step))
        e        (some->> existing (vector sec) (db/entry db))]
    (when (contains? protected-phases (:status e))
      (fail! :protected-retire (str "Cannot delete " (name sec) " " (pr-str k) ": it is "
                                    (:status e) " — set its status to :deprecated first")
             {:name k :section sec :status (:status e)}))
    (let [r  (or existing k)
          _  (doseq [id [r nil]] ((sigma cx :authorize) :dissoc {:type sec :id id}))
          c1 (changes-to db (cond-> (pruned db sec r) (own db sec r nil) (assoc [sec r] nil)))]
      (finished cx db c1 (db/db-with db c1) [sec k] :dissoc-schema r
                (partial drops cx db)))))

(def ^:private verbs {:create create :update put :delete delete})

(defn- lifted
  [cx sec r e]
  (first (vals (:transforms ((sigma cx :migrate-external) {sec {r e}})))))

(defn- external
  [cx db step verb]
  (let [[sec]  (:path step)
        es     (get-in step [:value :external-source])
        r1     (verb cx db (update step :value dissoc :external-source))
        db1    (:db r1)
        r      (get-in r1 [:event :target :id])
        t      (lifted cx sec r (assoc (db/entry db1 [sec r]) :external-source es))
        ek     (db/own-entry db1 :transforms (:transform-name t) t)
        rm     (if ek (changes-to db1 {[:transforms ek] nil}) [])
        r2     (put cx (db/db-with db1 rm)
                    (with-meta {:op :set :path [:transforms (or ek (:transform-name t))] :value t}
                      {:verb :update}))]
    (assoc r1
           :db (:db r2)
           :delta (d/squash (concat (:delta r1) rm (:delta r2))))))

(defn run
  [cx db step]
  (let [verb (verbs (:verb (meta step)))]
    (if (and (= :set (:op step)) (some? (get-in step [:value :external-source])))
      (external cx db step verb)
      (verb cx db step))))
