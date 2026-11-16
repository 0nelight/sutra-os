(ns sutra.world.db
  (:require [sutra.world.delta :as d]
            [sutra.world.index :as ix]))

(def ^:private rid-re #"ri\.sutra\.main\.([a-z-]+)\.([0-9A-Za-z_-]+)")

(defn rid? [x] (boolean (and (string? x) (re-matches rid-re x))))

(def loc ix/loc)

(defn rid [tag locator] (str "ri.sutra.main." (name tag) "." locator))

(defn- step-views [rows db0 db1 changes]
  (reduce (fn [db {:keys [id step]}]
            (assoc-in db [:views id] (step (get-in db0 [:views id]) changes db0 db)))
          db1 rows))

(defn db
  ([W] (db W []))
  ([W rows]
   (reduce (fn [db {:keys [id init]}] (assoc-in db [:views id] (init db)))
           {:ents W :idx (ix/init W) :views {} :view-rows (vec rows) :delta []}
           rows)))

(defn world [db] (:ents db))

(defn entry [db node] (get-in (:ents db) node))

(defn value
  [db node]
  (if (d/frame-node? node) (d/frame-value (:ents db) (second node)) (entry db node)))

(defn changes-to
  [db vs]
  (let [fcs (for [[n v] vs :when (d/frame-node? n) :let [a (value db n)] :when (not= a v)]
              [n :value a v])
        ecs (for [[n v] vs :when (not (d/frame-node? n)) c (d/entry-delta n (entry db n) v)]
              c)]
    (d/around fcs ecs)))

(defn view [db id] (get-in db [:views id]))

(defn nodes
  [db index fact]
  (get-in db [:idx index fact] #{}))

(defn keys-of
  [nodes]
  (sort-by str (map second nodes)))

(defn with
  [db changes]
  (let [changes (d/completed (:ents db) (d/squash changes))
        entries (into [] (remove (comp d/frame-node? first)) changes)
        W1      (d/apply-to (:ents db) changes)
        db1     (assoc db
                       :ents W1
                       :idx (ix/step (:idx db) (:ents db) W1 (d/nodes entries))
                       :delta (into (:delta db) changes))]
    {:db-before db
     :db-after  (step-views (:view-rows db) db db1 entries)
     :delta     changes}))

(defn db-with [db changes] (:db-after (with db changes)))

(defn- by-name
  [db sec x]
  (when (some? x)
    (if (contains? (get (:ents db) sec) x)
      x
      (first (keys-of (nodes db :by-name-str [sec (ix/nm x)]))))))

(defn scope-of
  [db e]
  (when-let [p (:project e)]
    (let [pr (by-name db :projects p)
          sp (get-in (:ents db) [:projects pr :space])]
      (or (by-name db :spaces sp) :space/default))))

(defn ref-of
  ([db targets x] (ref-of db targets x nil))
  ([db targets x scope]
   (cond
     (rid? x) x
     (nil? x) nil
     :else
     (let [k (if (string? x) (keyword x) x)]
       (some (fn [t]
               (let [ns (nodes db :by-name [t k])]
                 (if (<= (count ns) 1)
                   (some->> (first ns) (entry db) :id)
                   (let [in (fn [s] (some->> ns
                                             (filter #(= s (scope-of db (entry db %))))
                                             keys-of last (vector t) (entry db) :id))]
                     (or (when scope (in scope))
                         (in nil)
                         (throw (ex-info (str "the name " k
                                              " is given in several spaces — name the space's"
                                              " project")
                                         {:name k :section t})))))))
             targets)))))

(defn resolver
  [db scope]
  (fn [t x] (or (ref-of db t x scope) x)))

(defn own-entry
  [db sec ident value]
  (let [scope (scope-of db value)
        found (ref-of db #{sec} ident scope)
        other (some->> found (vector sec) (entry db) (scope-of db))]
    (when-not (and scope other (not= scope other)
                   (pos? (count (get (:ents db) :spaces))))
      found)))
