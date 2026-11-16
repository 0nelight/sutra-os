(ns sutra.store.kept
  (:require [sutra.catalog.world-advice :as world-advice]
            [sutra.catalog.world-columns :as kc]
            [sutra.catalog.world-derived :as kd]
            [sutra.catalog.world-export :as world-export]
            [sutra.catalog.world-gates :as world-gates]
            [sutra.catalog.problems :as problems]
            [sutra.world.gate :as gate]
            [sutra.parser.lint :as lint]
            [sutra.catalog.shape :as shape]
            [sutra.catalog.system-datasets :as sysds]
            [sutra.world.db :as kdb]
            [sutra.world.delta :as delta]
            [sutra.world.index :as ix]
            [sutra.world.install :as kinstall]
            [sutra.world.instance :as inst]
            [sutra.ref.clock :as clock]
            [sutra.commits.loader :as lader]
            [sutra.commits.log :as log]
            [sutra.commits.branches :as zweige]
            [sutra.store.api :as api]
            [sutra.store.memo :as memo]
            [sutra.store.world :as world]
            [sutra.types.forms :as forms]
            [sutra.types.membership :as member]
            [sutra.types.rid :as rid]))

(set! *warn-on-reflection* true)

(defn home
  [store b]
  (when (:instance-keys store) (world/instance-home (world/at store b))))

(defn at-head
  [store b rev f]
  (with-open [s (api/snapshot (:db store) (cond-> {:branch (str b)} rev (assoc :rev rev)))]
    (f (:db s) (:h s))))

(defn between
  [d0 d1]
  (into [] (drop (count (:delta d0))) (:delta d1)))

(defn restore-at
  [d src nodes]
  (kdb/changes-to d (into {} (map (juxt identity #(kdb/value src %))) nodes)))

(defn db
  [store b]
  (at-head store b nil #(zweige/head %1 %2 (str b) world-gates/views (home store b))))

(defn steps
  [d W0 W1]
  (let [d0 (log/stepped d W0)] [d0 (log/stepped d0 W1)]))

(defn onto
  [k W]
  {:before k :after (log/stepped k W)})

(defn verdict
  [k N old]
  (apply world-gates/verdict (steps k (or old {}) N)))

(def ^:private change-rules
  (into #{} (keep #(when (= :change (:scope %)) (:id %))) lint/rules))

(defn value-reasons
  [d]
  (problems/reasons
   (into [] (remove #(and (= :lint-gate-clean? (:gate %)) (change-rules (:rule %))))
         (gate/report-of world-gates/rows d))))

(defn reasons
  [k W0 W1]
  (zipmap [:before :after] (map value-reasons (steps k W0 W1))))

(def ^:private advice-dbs
  (atom {}))

(defn- advice-db
  [k b]
  (let [W      (kdb/world k)
        [W' d] (get @advice-dbs (str b))
        d      (cond (identical? W W') d
                     d                 (log/stepped d W)
                     :else             (kdb/db W world-advice/views))]
    (swap! advice-dbs #(assoc (if (< 64 (count %)) {} %) (str b) [W d]))
    d))

(defn advisories
  [k b W0 W1]
  (zipmap [:before :after] (map world-advice/advisories (steps (advice-db k b) W0 W1))))

(defn- kern-cx []
  {:sigma {:fresh-uuid       (:fresh-uuid clock/system)
           :migrate-external shape/migrate-external-source-to-transforms
           :system-datasets  (constantly sysds/system-datasets)
           :sql-gate         (fn [_W0 _W1 _k] nil)}})

(defn step-of
  [W]
  (::step (meta W)))

(defn- stepped
  [before after]
  (with-meta (kdb/world after) {::step {:before before :after after}}))

(defn- whole-steps
  [db W]
  (let [W0 (kdb/world db)]
    (sort-by (comp {:delete 0 :update 1 :create 2} :verb meta)
     (for [s (distinct (concat (keys W0) (keys W)))
          :let [a (get W0 s) b (get W s)]
          :when (and (not (identical? a b)) (or (map? a) (map? b)))
          :let [a (if (map? a) a {}) b (if (map? b) b {})]
          k (distinct (concat (keys a) (keys b)))
          :let [x (get a k) y (get b k)]
          :when (not= x y)]
      (cond
        (nil? y) (with-meta {:op :dissoc :path [s k]} {:verb :delete})
        (nil? x) (with-meta {:op :set :path [s k] :value y} {:verb :create})
        :else    (with-meta {:op :set :path [s k] :value y :whole-entry? true}
                   {:verb :update}))))))

(defn- frame-split
  [db W]
  (let [fcs (delta/frame-delta (kdb/world db) W)]
    ((juxt filterv remove) (fn [[_ _ _ b]] (= {} b)) fcs)))

(defn- framed
  [W]
  (let [W (if (or (contains? W :objects) (contains? W :links)) (shape/unpack-schema W) W)]
    (reduce (fn [W s] (update W s #(or % {})))
            W [:transforms :object-types :link-types :datasets :actions])))

(defn- against
  [d {:keys [op path value] :as st}]
  (let [cur (kdb/entry d path)]
    (cond
      (= :dissoc op)                  (when (some? cur) st)
      (= cur value)                   nil
      (and (some? cur) (= :create (:verb (meta st))))
      (with-meta (assoc st :whole-entry? true) {:verb :update})
      :else                           st)))

(defn whole
  [k W1]
  (let [W1           (framed W1)
        [opens rest] (frame-split k W1)
        cx           (kern-cx)
        d1           (reduce (fn [d st] (if-let [st (against d st)]
                                              (:db (kinstall/run cx d st))
                                              d))
                             (kdb/db-with k opens)
                             (whole-steps k W1))]
    (stepped k (kdb/db-with d1 rest))))

(defn step
  [k W st]
  (stepped k (:db (kinstall/run (kern-cx) (log/stepped k W) st))))

(defn- step-changes
  [{:keys [before after]}]
  (between before after))

(defn held
  [W]
  (::held (meta W)))

(defn- holding
  [d on]
  (with-meta (kdb/world d) {::held d ::on on}))

(defn made-by
  [W b before after]
  (vary-meta W assoc ::held after ::on {:db before :branch (str b)}))

(defn- staged-of
  [W b]
  (let [d                   (held W)
        {:keys [db branch]} (::on (meta W))]
    (when (and d (= (str b) branch) (= W (kdb/world d)))
      {:before db :after d})))

(defn commit!
  [store b W st commit]
  (let [st (or st (step-of W) (staged-of W b) (step-of (whole (db (world/at store b) b) W)))]
    (commit {:changes (step-changes st)})))

(defn world-of
  [d from]
  (holding d (::on (meta from))))

(defn row-delta
  [W0 W1]
  (let [h0 (held W0)
        h1 (held W1)]
    (when (and h0 h1)
      (step-changes {:before (log/stepped h0 W0) :after (log/stepped h1 W1)}))))

(defn- system-datasets-delta
  [d]
  (for [[ds e] sysds/system-datasets
        :let [x (kdb/entry d [:datasets ds])]
        :when (not= x e)
        c (delta/entry-delta [:datasets ds] x e)]
    c))

(defn- staging-db
  [d]
  (let [W0 (kdb/world d)
        W1 (-> (if (seq W0) W0 (merge shape/empty-schema-map W0))
               (update :actions #(or % {}))
               (update :datasets #(or % {})))]
    (kdb/db-with d (delta/around (delta/frame-delta W0 W1) (system-datasets-delta d)))))

(defn staging
  [store b]
  (let [k (db store b)]
    (holding (staging-db k) {:db k :branch (str b)})))

(defonce ^:private reads-memo (atom {}))

(def ^:private reads-spec {::reading {:cap 32} ::export {:cap 8}})

(defn- read-key
  [store b]
  (let [s (world/at store b)]
    (when-let [rev (:rev (world/basis s))]
      [(:db store) (str b) rev
       (when-let [h (home store b)]
         (:rev (world/basis (world/at store h))))])))

(defn- read-db
  [store b]
  (at-head store b nil #(staging-db (zweige/head %1 %2 (str b) kc/rows (home store b)))))

(defn reading
  [store b]
  (kc/world-of (if-let [k (read-key store b)]
              (memo/through reads-memo reads-spec ::reading k #(read-db store b))
              (read-db store b))))

(defn export
  [store b]
  (if-let [k (read-key store b)]
    (memo/through reads-memo reads-spec ::export k #(world-export/held (db store b)))
    (world-export/held (db store b))))

(def ^:private request-rows (into kc/rows kd/rows))

(defn- derived!
  [d b]
  (when-let [ns (seq (kd/unfixed d))]
    (throw (ex-info (str "the head of " b " is not its own derivation")
                    {:branch b :nodes (vec (take 5 ns))}))))

(defn- readable-db
  [d]
  (let [W0  (kdb/world d)
        W1  (-> (or (not-empty (world/without-retired W0)) shape/empty-schema-map)
                (update :datasets #(or % {})))
        app (for [[k a] (kd/lifted-apps d)
                  c (delta/entry-delta [:applications k] (kdb/entry d [:applications k]) a)]
              c)]
    (kdb/db-with d (delta/around (delta/frame-delta W0 W1)
                                 (concat (system-datasets-delta d) app)))))

(defn- request-db
  [store b rev]
  (at-head store b rev
           (fn [sdb h]
             (let [d  (lader/head-db sdb h (str b) request-rows)
                   hm (home store b)
                   hd (when hm (lader/head-db sdb h hm [kd/fixed]))]
               (derived! d b)
               (when hd (derived! hd hm))
               (readable-db (cond-> d hd (inst/overlaid hd)))))))

(defn request-schema
  [store b rev]
  (kc/world-of (request-db store b rev)))

(defn placed
  [db0 db1 project]
  (let [added (filter #(and (nil? (kdb/entry db0 %)) (some? (kdb/entry db1 %)))
                      (distinct (map first (between db0 db1))))]
    (if-not (and project (seq added))
      db1
      (let [W (member/place-added forms/installed (kdb/world db1) project added)]
        (kdb/db-with db1 (mapcat #(delta/entry-delta % (kdb/entry db1 %) (get-in W %))
                                 added))))))

(def ^:private no-event {:target nil :before nil :after nil :actor nil :provenance nil})

(defn staged-step
  [W start st project checked]
  (let [from (if (held W) W start)
        d0   (log/stepped (held from) W)
        r    (kinstall/run (kern-cx) d0 st)
        W1   (holding (placed d0 (:db r) project) (::on (meta from)))]
    (checked W1 (get-in r [:event :target :id]))
    {:world W1 :events [(merge no-event (:event r))]}))

(defn- names-of [x]
  (cond (= delta/absent x) []
        (keyword? x) [x (name x)]
        (string? x)  [x (keyword x)]
        :else        []))

(defn- tokens
  [d cs]
  (let [by-node (group-by first (remove (comp delta/frame-node? first) cs))]
    (into #{}
          (mapcat (fn [[[s k :as n] changes]]
                    (let [nf     (get rid/name-field-of s)
                          fields (fn [i] (into {} (keep #(let [v (nth % i)]
                                                           (when-not (= delta/absent v)
                                                             [(second %) v])))
                                               changes))
                          es     (filter map? [(fields 2) (fields 3) (kdb/entry d n)])]
                      (concat [k]
                              (for [e es [_ v] (ix/ref-values s e)
                                    x (if (coll? v) v [v])
                                    t (names-of x)]
                                t)
                              (when nf (mapcat (comp names-of nf) es))))))
          by-node)))

(defn- mentions? [toks x]
  (boolean (some #(contains? toks %) (tree-seq coll? seq x))))

(defn- invertible? [{:keys [delta data? effects?]}]
  (and (vector? delta) (not data?) (not effects?)))

(defn- builds-on?
  [{:keys [nodes toks]} x]
  (let [cs (:delta x)]
    (boolean (or (and cs (or (some nodes (delta/nodes cs)) (mentions? toks cs)))
                 (mentions? toks [(:args x) (:tx-vector x)])))))

(defn- taint
  ([d cs] (taint {:nodes #{} :toks #{}} d cs))
  ([t d cs] (-> t (update :nodes into (delta/nodes cs)) (update :toks into (tokens d cs)))))

(defn- rebuild [x why] {:form :rebuild :why why :seq (:seq x)})

(defn- why-not
  [x]
  (cond (:data? x) :data-row (:effects? x) :effects :else :no-delta))

(defn- unless-stale
  [f stale]
  (try (f)
       (catch clojure.lang.ExceptionInfo e
         (if (= :stale-change (:ref/kind (ex-data e)))
           (stale)
           (throw e)))))

(defn- turned-back
  [d cs]
  (let [inv (delta/invert cs)
        d1  (kdb/db-with d (remove (comp delta/frame-node? first) inv))
        W   (kdb/world d1)]
    (kdb/db-with d1 (filter (fn [[[_ s] _ _ b]] (or (not= delta/absent b) (empty? (get W s))))
                            (filter (comp delta/frame-node? first) inv)))))

(defn discarding
  [d rows k]
  (let [r (nth rows k)]
    (if-not (invertible? r)
      (rebuild r (why-not r))
      (let [{:keys [dep blocked]}
            (reduce (fn [{:keys [t] :as acc} x]
                      (cond (not (builds-on? t x)) acc
                            (invertible? x)        (-> acc
                                                       (assoc :t (taint t d (:delta x)))
                                                       (update :dep conj x))
                            :else                  (reduced (assoc acc :blocked x))))
                    {:t (taint d (:delta r)) :dep []}
                    (subvec rows (inc k)))]
        (if blocked
          (rebuild blocked (why-not blocked))
          (unless-stale
           #(let [d1 (reduce (fn [d x] (turned-back d (:delta x))) d (rseq (into [r] dep)))]
              (if (seq dep)
                {:form :suffix :db d1 :replay dep}
                {:form :inverse :db d1}))
           #(rebuild r :stale)))))))

(defn moved
  [store parent staging rev]
  (at-head store parent nil
           (fn [sdb h]
             (let [{:keys [theirs base nodes]}
                   (zweige/states sdb h (str staging) (str parent) rev [] (home store staging))]
               (vec (restore-at base theirs nodes))))))

(defn rebasing
  [p moved rows k]
  (let [r    (nth rows k)
        bare (first (remove :delta rows))]
    (cond
      bare                  (rebuild bare :no-delta)
      (not (invertible? r)) (rebuild r (why-not r))
      :else
      (let [kt    (taint p (:delta r))
            mt    (taint p moved)
            lays? (fn [i x] (and (invertible? x)
                                 (not (builds-on? mt x))
                                 (not (and (> i k) (builds-on? kt x)))))
            lay   (fn [d x] (unless-stale #(kdb/db-with d (:delta x)) (constantly nil)))]
        (loop [d p [[i x] & more :as left] (keep-indexed #(when (not= %1 k) [%1 %2]) rows)]
          (if-let [d1 (and x (lays? i x) (lay d x))]
            (recur d1 more)
            {:form :rebase :db d :replay (mapv second left)}))))))
