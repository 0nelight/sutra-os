(ns sutra.store.core
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.store.api :as api]
            [sutra.store.guard :as guard]
            [sutra.store.shadow :as shadow]
            [sutra.types.world-rules :as rules]
            [sutra.store.spi :as spi]
            [sutra.store.core.answers :refer [answer as-of! committed-world overlay-of snapshot-cx]]
            [sutra.store.core.branches :refer [ancestor? bases-ks branch-row clocks-of live!
                                               live-branch root-of]]
            [sutra.store.core.commit :refer [apply! commit-in! dropped-with]]
            [sutra.store.core.engine :refer [->CheckedExecutor begin check-profile! checked-engine
                                             commit! lock! rollback!]]
            [sutra.store.core.kv :refer [branch-k clock-k enc kv-fx kv-get kv-parts kv-read kv-scan
                                         layout-init layout-k log-indexed-k log-kinded-k record-part
                                         rev-k
                                         rev-key schema-logged-k with-layout-stamp]]
            [sutra.store.core.reads :refer [derived fixed-by-basis head-revs kept-since
                                            remember-heads! sig-of through world-at]]
            [sutra.store.core.resources :as res]
            [sutra.store.core.records :refer [in-engine?]]
            [sutra.store.core.schema-log :as schema-log]))

(set! *warn-on-reflection* true)

(defn adopt!
  ([db] (adopt! db ["main"]))
  ([{:keys [engine] :as db} branches]
   (into []
         (filter (fn [b]
                   (let [h        (begin db :read nil)
                         missing? (try (and (nil? (branch-row db h b)) (spi/-ns-exists? engine h b))
                                       (finally (rollback! db h)))]
                     (when missing?
                       (let [w (begin db :write nil)]
                         (try
                           (lock! db w [[:branch b]])
                           (let [fresh? (nil? (kv-get db w (rev-k 0)))]
                             (apply! db w
                                     (cond-> []
                                       fresh? (conj (kv-fx (rev-k 0) {:rev 0 :parents []})
                                                    (kv-fx log-indexed-k true))
                                       (nil? (branch-row db w b))
                                       (conj
                                        (kv-fx (branch-k b)
                                               {:name b :rev 0 :base nil :meta {}})
                                        (kv-fx (schema-logged-k b) 0)
                                        (kv-fx (log-kinded-k b) {:rev 0 :base nil})))))
                           (commit! db w)
                           true
                           (catch Throwable t (rollback! db w) (throw t))))))))
         branches)))

(defrecord Snapshot [db h basis as-of closed? ops ov wo rc]
  api/Snapshot
  (-basis [_] basis)
  (-answer [this authority query]
    (when @closed? (api/refuse! :snapshot-expired "this snapshot is closed"))
    (answer this authority query))
  (-overlay [this {:keys [world migration] more :ops}]
    (when @closed? (api/refuse! :snapshot-expired "this snapshot is closed"))
    (as-of! db as-of :objects)
    (let [b   (:branch basis)
          wo' (if (or world (seq migration))
                {:world     (or world (:world wo) (committed-world db h b as-of (:tick this)))
                 :migration (into (vec (:migration wo)) migration)}
                wo)
          rc' (if (= wo' wo) rc (delay (snapshot-cx db h b (dissoc basis :branch) as-of (:tick this)
                                                    (:layout this) wo')))
          all (into (vec ops) more)]
      (assoc this :ops all :ov (when (seq all) (overlay-of db h @rc' all)) :wo wo' :rc rc')))
  (-close [this]
    (when (and (nil? ops) (compare-and-set! closed? false true)) (rollback! db h))
    this)
  java.io.Closeable
  (close [this] (api/-close this)))

(defn- snapshot
  [db h basis as-of tick lay]
  (map->Snapshot {:db db :h h :basis basis :as-of as-of :closed? (atom false) :tick tick :layout lay
                  :rc (delay (snapshot-cx db h (:branch basis) (dissoc basis :branch) as-of tick lay
                                          nil))
                  :clocks (delay (clocks-of db h (:branch basis)))}))

(defrecord Prepared [db h done? result]
  api/Prepared
  (-commit! [_]
    (if (compare-and-set! done? false true)
      (do (commit! db h) result)
      (api/refused :prepare-expired "this prepared tx was already committed or aborted" {})))
  (-abort! [_]
    (when (compare-and-set! done? false true) (rollback! db h))))

(defn- with-write
  [db f]
  (let [h (begin db :write nil)]
    (try (let [r (f h)] (commit! db h) r)
         (catch Throwable t (rollback! db h) (throw t)))))

(def ^:private compact-page 64)

(defn compact-index-feed!
  [db upto]
  (let [stop  (rev-key upto)
        parts (let [h (begin db :read nil)]
                (try (kv-parts db h :change) (finally (rollback! db h))))
        page! (fn [part after]
                (with-write
                  db
                  (fn [h]
                    (let [kvs (kv-scan db h [:change part]
                                       (cond-> {:upto stop :limit compact-page}
                                         after (assoc :after after)))
                          fx  (for [[k {:keys [origin entries] :as c}] kvs
                                    :when (and (= :index origin) (seq entries))]
                                (kv-fx [:change part k]
                                       (assoc c :entries [] :compacted (count entries))))]
                      (when (seq fx) (apply! db h (vec fx)))
                      {:last (first (last kvs)) :full? (= compact-page (count kvs))
                       :n (count fx)}))))]
    (reduce (fn [acc part]
              (loop [after nil acc acc]
                (let [{:keys [last full? n]} (page! part after)
                      acc (update acc :commits + n)]
                  (if full? (recur last acc) (update acc :types inc)))))
            {:types 0 :commits 0}
            parts)))

(defn- part-copy
  [space to read]
  (for [[k v] read] (kv-fx [space to k] v)))

(defn- part-clear
  [space to read]
  (for [[k _] read] (kv-fx [space to k] nil)))

(defn- plan-branch
  [{:keys [op name from meta expect-rev expect-meta] :as o}
   {:keys [taken parent row sig parts colls parent-heads parent-es parent-kinded] :as in}]
  (case op
    :adopt
    (let [live (when (and taken (not (:retired? taken))) taken)]
      (when-not (= (:rev live) expect-rev)
        (api/refuse! :conflict (str "branch " (pr-str name) (if live " moved" " is not live"))
                     {:branch name :rev (:rev live)}))
      (let [f (:branch from)
            {:keys [effects result]} (plan-branch (assoc o :op :fork) (assoc in :taken nil))
            mark (log-kinded-k name)]
        {:effects (vec (concat (when live [[:ns-drop name]])
                               (remove #(= mark (second %)) effects)
                               (for [space [:log-of :log-kind]
                                     [k v] (get parts [space f])]
                                 (kv-fx [space name k] (assoc v :branch name)))
                               [(kv-fx mark (when (= parent-kinded
                                                     {:rev (:rev parent) :base (:base parent)})
                                              {:rev (:rev parent) :base (:base result)}))]))
         :result result}))
    :fork
    (do (when (and taken (not (:retired? taken)))
          (api/refuse! :conflict (str "branch " (pr-str name) " exists") {:branch name}))
        (when (and (:rev from) (not= (:rev from) (:rev parent)))
          (api/refuse! :as-of-unsupported "a fork starts at the parent's head" {:from from}))
        (let [f   (:branch from)
              row {:name name :rev (:rev parent) :base {:branch f :rev (:rev parent)}
                   :meta (or meta {})}]
          {:effects (vec (concat
                          [[:ns-fork f name {:sig sig :rev (:rev parent)}]]
                          (mapcat (fn [space] (part-clear space name (get parts [space name])))
                                  [:schema :clock :edited :detached :log-of :log-kind :whead
                                   :wlog :whist :wstate :schema-log])
                          (for [[coll engine?] colls :when (not engine?)
                                e (part-clear :record (record-part coll name)
                                              (get parts [:record (record-part coll name)]))]
                            e)
                          (res/fork-effects name f (:rev parent) parent-heads parent-es)
                          (part-copy :clock name (get parts [:clock f]))
                          (part-copy :edited name (get parts [:edited f]))
                          (part-copy :detached name (get parts [:detached f]))
                          (for [[coll engine?] colls
                                e (if engine?
                                    [[:rec-copy coll f name]]
                                    (part-copy :record (record-part coll name)
                                               (get parts [:record (record-part coll f)])))]
                            e)
                          [(kv-fx (branch-k name) row)
                           (kv-fx (schema-logged-k name) (:rev parent))
                           (kv-fx (log-kinded-k name) {:rev (:rev parent) :base (:base row)})]))
           :result row}))
    :retire
    (do (when (not= expect-rev (:rev row))
          (api/refuse! :conflict (str "branch " name " moved") {:branch name :rev (:rev row)}))
        (let [row (assoc row :retired? true)]
          {:effects [[:ns-drop name] (kv-fx (branch-k name) row)] :result row}))
    :set-meta
    (do (when (and (contains? o :expect-meta) (not= expect-meta (:meta row)))
          (api/refuse! :conflict (str "branch " name " meta changed") {:branch name}))
        (let [row (assoc row :meta meta)]
          {:effects [(kv-fx (branch-k name) row)] :result row}))))

(defn- branch-op!
  [{:keys [collections] :as db} {:keys [op name from] :as o}]
  (with-write db
    (fn [h]
      (lock! db h
             (sort-by pr-str
                      (distinct (remove nil?
                                        [[:branch name] (some->> from :branch (vector :branch))]))))
      (let [f     (:branch from)
            W     (when (#{:fork :adopt} op) (world-at db h f))
            kept  (when W (kept-since db h f))
            colls (for [[coll {:keys [scope]}] collections :when (= :branch scope)]
                    [coll (in-engine? db coll)])
            pa    (when (#{:fork :adopt} op) (live-branch db h f))
            root? (and pa (nil? (get-in pa [:base :branch])))
            in    (if (#{:fork :adopt} op)
                    {:taken  (branch-row db h name)
                     :parent pa
                     :sig    (sig-of db (derived db W))
                     :parent-heads (when kept (head-revs db h f))
                     :parent-es (when-not kept (res/entries W))
                     :colls  colls
                     :parent-kinded (when (= :adopt op) (kv-get db h (log-kinded-k f)))
                     :parts  (into {}
                                   (for [[space part] (concat [[:clock f] [:detached f]]
                                                              (when-not root? [[:edited f]])
                                                              [[:schema name] [:clock name]
                                                               [:edited name] [:detached name]
                                                               [:log-of name] [:log-kind name]
                                                               [:wlog name]
                                                               [:whist name] [:wstate name]
                                                               [:schema-log name]]
                                                              (when (= :adopt op)
                                                                [[:log-of f] [:log-kind f]])
                                                              (for [[coll engine?] colls
                                                                    :when (not engine?)
                                                                    b [f name]]
                                                                [:record (record-part coll b)]))]
                                     [[space part] (kv-scan db h [space part] nil)]))}
                    {:row (live-branch db h name)})
            in    (cond-> in
                    (#{:fork :adopt} op)
                    (assoc-in [:parts [:whead name]]
                              (if (kept-since db h name)
                                (for [[id r] (head-revs db h name)] [(enc id) r])
                                (kv-scan db h [:whead name] nil))))
            {:keys [effects result]} (plan-branch o in)]
        (when-let [heads (:parent-heads in)]
          (remember-heads! db name {:at (:rev pa) :entry {:fork {:from f :rev (:rev pa)}}
                                    :revs heads}))
        (apply! db h (-> (cond-> effects (#{:retire :adopt} op) (into (dropped-with db h name)))
                         (with-layout-stamp (str (random-uuid)) false)))
        result))))

(defrecord Db [engine collections derive mint signature now check? memo executors upgrade
               head sig-view held-head]
  api/Db
  (-snapshot [this {b :branch r :rev lane :lane}]
    (let [h (begin this :read lane)]
      (try
        (let [kv   (kv-read this h
                            (list* (branch-k b) (clock-k b [:schema]) (layout-k) (bases-ks this b)))
              row  (live! b (get kv (branch-k b)))
              tick (get kv (clock-k b [:schema]))
              lay  (or (get kv (layout-k)) layout-init)
              head (:rev row)
              [root root-row] (root-of this h b row kv)]
          (cond
            (or (nil? r) (= r head))
            (snapshot this h
                      (cond-> {:branch b :rev head} root-row (assoc :root-rev (:rev root-row))) nil
                      tick lay)

            (ancestor? this h b head r)
            (let [entry (kv-get this h (rev-k r))
                  r0    (when root (if (= root (:branch entry)) r (:root-rev entry)))]
              (snapshot this h (cond-> {:branch b :rev r} r0 (assoc :root-rev r0))
                        (cond-> {:rev r} root (assoc :root-rev (or r0 ::unknown))) tick lay))

            :else (api/refuse! :malformed
                               (str "revision " (pr-str r) " is not on branch " (pr-str b))
                               {:branch b :rev r})))
        (catch Throwable t (rollback! this h) (throw t)))))
  (-prepare [this tx]
    (let [h (begin this :write (:lane tx))
          r (try (anomaly/catching #(commit-in! this h tx))
                 (catch Throwable t (rollback! this h) (throw t)))]
      (if (anomaly/refusal? r)
        (do (rollback! this h) r)
        (->Prepared this h (atom false) r))))
  (-branch! [this op]
    (if (= :repair-schema-log (:op op))
      (schema-log/repair! this (:name op))
      (branch-op! this op)))
  (-read-at [this {b :branch r :rev :as basis} authority [kind :as q]]
    (let [k   [b r q]
          ask #(with-open [^java.io.Closeable s (api/-snapshot this basis)]
                 (api/-answer s authority q))]
      (if-not (fixed-by-basis kind)
        (ask)
        (through memo ::at k ask)))))

(defn db
  ([engine] (db engine {}))
  ([engine {:keys [collections derive mint signature now check? memo executors upgrade
                   head sig-view held-head]
            :or {derive identity mint (fn [_old w] w) signature rules/signature check? true
                 upgrade identity}}]
   (->Db (checked-engine (cond-> engine shadow/*on* shadow/wrap guard/*on* guard/wrap) check?
                         (into {} (keep (fn [{:keys [coll auto]}] (when auto [coll auto])))
                               collections))
         (into {} (map (juxt :coll api/check-collection!)) collections)
         derive
         mint
         signature
         (or now #(java.time.Instant/now))
         check?
         (or memo (atom {}))
         (let [autos (into {} (keep (fn [{:keys [coll auto]}] (when auto [coll auto])))
                           collections)]
           (mapv (fn [{:keys [pushdown profile]}]
                   (let [p (check-profile! check? profile)]
                     {:pushdown (->CheckedExecutor pushdown check? autos p) :profile p}))
                 executors))
         upgrade
         head
         sig-view
         held-head)))
