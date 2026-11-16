(ns sutra.world.gate.base
  (:require [sutra.world.db :as db]
            [sutra.world.index :as ix]
            [sutra.world.view :as view]))

(defn named [sec k e] (or (get e (ix/name-field sec)) k))

(defn section-keys [sec] (fn [db] (keys (get (db/world db) sec))))

(defn both
  [f db0 db1 xs]
  (into #{} (for [d [db0 db1] x xs y (f d x)] y)))

(defn key-of
  [db sec x]
  (when (some? x)
    (if (contains? (get (db/world db) sec) x)
      x
      (first (db/keys-of (db/nodes db :by-name-str [sec (ix/nm x)]))))))

(defn names-of
  [db sec k]
  (cond-> #{(ix/nm k)}
    (db/entry db [sec k]) (conj (ix/nm (get (db/entry db [sec k]) (ix/name-field sec))))))

(defn ref-forms
  [db sec k]
  (let [n (get (db/entry db [sec k]) (ix/name-field sec))]
    (into #{k} (when (some? n) [n (ix/nm n) (keyword (ix/nm n))]))))

(defn referrers
  [db0 db1 [sec k]]
  (into #{} (for [d [db0 db1] v (into (ref-forms db0 sec k) (ref-forms db1 sec k))
                  node (db/nodes d :refs-in v)]
              node)))

(defn scope-dependents
  [db0 db1 changes]
  (let [names    (fn [sec k] (into (names-of db0 sec k) (names-of db1 sec k)))
        members  (fn [fact] (into (db/nodes db0 :members fact) (db/nodes db1 :members fact)))
        projects (into (view/touched changes :projects)
                       (for [s (view/touched changes :spaces)
                             n (names :spaces s)
                             [_ p] (members [:space n])]
                         p))]
    (into #{} (for [p projects n (names :projects p) node (members [:project n])] node))))

(defn- facts-of [{f :facts} [sec] e] (when (map? e) (distinct (f sec e))))

(defn- by-section
  [{:keys [sections] f :facts}]
  (into {} (for [s sections] [s (fn [_k e] (f s e))])))

(defn facts-view
  [rows]
  {:id    :gate-facts
   :reads (into #{} (mapcat :sections) rows)
   :init  (fn [db]
            (into {} (for [r rows] [(:id r) (view/fact-index (by-section r) (db/world db))])))
   :step  (fn [old changes db0 db1]
            (into {} (for [r rows]
                       [(:id r) (view/refiled (get old (:id r) {}) (by-section r) vector
                                              changes db0 db1)])))})

(defn moved-facts
  [rows db0 db1 nodes]
  (into #{} (for [{:keys [id sections] :as r} rows
                  [sec :as node] nodes
                  :when (contains? sections sec)
                  f (ix/changed (facts-of r node (db/entry db0 node))
                                (facts-of r node (db/entry db1 node)))]
              [[:gate-facts id] f])))

(defn facts
  [db id fact]
  (get-in (db/view db :gate-facts) [id fact] #{}))
