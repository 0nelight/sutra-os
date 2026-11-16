(ns sutra.store.world
  (:require [sutra.datasets.api :as datasets]
            [sutra.world.delta :as d]
            [sutra.kit.anomaly :as anomaly]
            [sutra.types.entities :as ents]
            [sutra.store.api :as api]
            [sutra.store.core.orakel :as orakel]
            [sutra.store.core.resources :as res]
            [sutra.store.lib :as lib]
            [sutra.store.served :as served]))

(set! *warn-on-reflection* true)

(defrecord Store [db branch clock datasets streams instance-keys instance-home view])

(defn store
  [db b m]
  (map->Store (assoc m :db db :branch (str (or b "main")))))

(defn at
  [store b]
  (assoc store :branch (str (or b "main"))))

(defn viewing
  [store]
  (assoc store :view (atom {})))

(defn- moved!
  [store b basis]
  (when-let [v (:view store)]
    (if basis
      (swap! v assoc (str b) (select-keys basis [:branch :rev]))
      (swap! v dissoc (str b)))))

(defn- try-transact!
  [store tx]
  (anomaly/then (api/try-transact! (:db store) tx)
                (fn [r]
                  (moved! store (get-in r [:basis :branch]) (:basis r))
                  r)))

(defn- transact! [store tx] (anomaly/unwrap! (try-transact! store tx)))

(defn- snapshot ^java.io.Closeable [store rev]
  (api/snapshot (:db store) (cond-> {:branch (str (:branch store))} rev (assoc :rev rev))))

(defn- ask
  ([store q] (ask store nil q))
  ([store rev q] (with-open [s (snapshot store rev)] (api/read s nil q))))

(defn head [store]
  (with-open [s (snapshot store nil)] (api/basis s)))

(defn schema-tick [store]
  (with-open [s (snapshot store nil)] (:tick s)))

(defn pin!
  [store bs]
  (let [b  (str (:branch bs))
        bs (select-keys (assoc bs :branch b) [:branch :rev])]
    (if-let [v (:view store)]
      (get (swap! v #(if (contains? % b) % (assoc % b bs))) b)
      bs)))

(defn context
  [store]
  (let [b   (str (:branch store))
        rev (some-> (:view store) deref (get b) :rev)]
    (cond-> {:branch b}
      (:authority store) (assoc :authority (:authority store))
      rev                (assoc :as-of rev))))

(defn in-context
  [store {:keys [branch authority as-of]}]
  (let [s (cond-> (assoc (dissoc (at store branch) :authority) :view nil)
            authority (assoc :authority authority))]
    (if as-of
      (let [s (viewing s)]
        (pin! s {:branch (:branch s) :rev as-of})
        s)
      s)))

(defn basis
  [store]
  (or (some-> (:view store) deref (get (str (:branch store))))
      (pin! store (head store))))

(defn- read-at [store bs q]
  (api/read-at (:db store) bs nil q))

(defn- ask-fixed
  [store rev q]
  (let [b (str (:branch store))]
    (cond
      rev        (read-at store {:branch b :rev rev} q)
      (:view store) (if-let [bs (get @(:view store) b)]
                      (read-at store bs q)
                      (with-open [s (snapshot store nil)]
                        (let [bs     (select-keys (api/basis s) [:branch :rev])
                              pinned (pin! store bs)]
                          (if (= pinned bs) (api/read s nil q) (read-at store pinned q)))))
      :else      (ask store q))))

(defn with-instance
  [W ks from]
  (reduce-kv (fn [w s k]
               (if (= :all k)
                 (if (contains? from s) (assoc w s (get from s)) (dissoc w s))
                 (let [f (select-keys (get from s) k)
                       r (merge (apply dissoc (get w s {}) k) f)]
                   (if (or (contains? w s) (seq f)) (assoc w s r) w))))
             W ks))

(defn instance-home
  [store]
  (when-let [f (:instance-home store)] (f (str (:branch store)))))

(defn instance-keys
  [store & Ws]
  (when-let [f (:instance-keys store)]
    (reduce ents/union-keys {} (map #(f (or % {})) Ws))))

(defn- overlaid
  [store W read]
  (if-let [h (when (and W (:instance-keys store)) (instance-home store))]
    (let [F (or (read (at store h)) {})]
      (with-instance W (instance-keys store W F) F))
    W))

(def ^:private sections (set ents/sections))

(defn without-retired
  [W]
  (if (map? W)
    (reduce-kv (fn [w s m] (if (and (not (sections s)) (zero? (count m))) (dissoc w s) w)) W W)
    W))

(defn world
  ([store] (world store nil))
  ([store rev]
   (without-retired
    (overlaid store (ask-fixed store rev [:schema/world {:derived? true}]) world))))

(defn authored
  ([store] (authored store nil))
  ([store rev] (without-retired (overlaid store (ask-fixed store rev [:schema/world]) authored))))

(defn history
  ([store] (history store {}))
  ([store opts] (ask-fixed store nil [:schema/history opts])))

(defn- ask-logged
  [store q]
  (try (ask-fixed store nil q)
       (catch clojure.lang.ExceptionInfo e
         (if-not (:schema-log/gap (ex-data e))
           (throw e)
           (do (api/branch! (:db store) {:op :repair-schema-log :name (str (:branch store))})
               (ask-fixed store nil q))))))

(defn schema-commits
  [store opts]
  (ask-logged store [:schema/commits opts]))

(defn resource-history
  [store section k]
  (ask-fixed store nil [:schema/resource-history [section k]]))

(defn changed-resources
  [store rev]
  (let [ids (ask-fixed store nil [:schema/changed (if rev {:after rev} {})])]
    (when (and ids (not (contains? ids [:frame]))) ids)))

(defn branches [store]
  (into [] (remove :retired?) (ask (at store "main") [:branch/list])))

(defn branch [store b]
  (let [r (ask (at store "main") [:branch/get (str b)])] (when (and r (not (:retired? r))) r)))

(defn tips
  [store bs]
  (ask (at store "main") [:branch/tips (mapv str bs)]))

(defn exists? [store b] (or (= "main" (str b)) (some? (branch store b))))

(defn parent [store b]
  (get-in (branch store b) [:base :branch]))

(defn fork!
  ([store parent name] (fork! store parent name {}))
  ([store parent name meta]
   (let [row (api/branch! (:db store)
                          {:op :fork :name (str name) :from {:branch (str parent)} :meta meta})]
     (moved! store name {:branch (str name) :rev (:rev row)})
     row)))

(defn adopt!
  [store name from expect-rev meta]
  (let [row (api/branch! (:db store)
                         (cond-> {:op :adopt :name (str name) :from {:branch (str from)}
                                  :meta (or meta {})}
                           expect-rev (assoc :expect-rev expect-rev)))]
    (moved! store name {:branch (str name) :rev (:rev row)})
    row))

(defn retire!
  [store name]
  (let [row (api/branch! (:db store)
                         {:op :retire :name (str name) :expect-rev (:rev (head (at store name)))})]
    (moved! store name nil)
    row))

(defn removed
  [store depth]
  (let [{:keys [removed] :as w} (ask-logged store [:schema/removed {:limit depth}])]
    (assoc (dissoc w :removed)
           :deleted (vec (for [{[section k] :id :keys [rev value]} removed]
                           {:kind section :rid k :seq rev :spec value})))))

(defn last-changed
  ([store] (last-changed store nil))
  ([store after]
   (ask-logged store [:schema/last-changed (if after {:after after} {})])))

(defn deleted
  [store depth]
  (:deleted (removed store depth)))

(defn with-snapshot
  ([store f] (with-snapshot store nil f))
  ([store rev f]
   (with-open [s (snapshot store rev)]
     (f s))))

(defn holding
  [store b f]
  (with-snapshot (at store b) (fn [s] (f (update store :db served/holding s)))))

(defn as-of
  [store at]
  (cond
    (nil? at)                                   (world store)
    (int? at)                                   (world store at)
    (and (string? at) (re-matches #"\d+" at))   (world store (parse-long at))
    :else (api/refuse! :malformed (str "as-of " (pr-str at) " is not a revision of the store")
                       {:as-of at})))

(defn reparent!
  [store b onto]
  (transact! store
             {:basis (head (at store b)) :actor {:principal "sutra.store.world"} :reparent onto
              :message (str "rebase onto " (:branch onto))}))

(defn heal!
  [store]
  (let [{:keys [missing]} (ask store [:store/layout])]
    (when (some seq ((juxt :object-types :link-types :indexes) missing))
      (transact! store
                 {:basis (head store) :actor {:principal "sutra.store.world"} :message "relayout"
                  :schema {:authored (authored store) :migration [[:relayout]]}}))))

(defn drop-unclaimed!
  [store]
  (transact! store
             {:basis (head store) :actor {:principal "sutra.store.world"} :message "drop unclaimed"
              :schema {:authored (authored store) :migration [[:drop-unclaimed]]}}))

(defn raise-fence!
  [store lease-name epoch]
  (api/transact! (:db store) {:basis (head (at store "main")) :actor {:principal "coord"}
                              :message (str "fence " lease-name " at " epoch)
                              :fences [[:lease (str lease-name) (long epoch)]]}))

(defn edited
  ([store T] (edited store T nil))
  ([store T ids] (with-snapshot store
                   (fn [s]
                     (api/read s api/unrestricted
                               (cond-> [:objects/edited T] ids (conj (vec ids))))))))

(defn warm-history!
  [store]
  (when-let [T (first (keys (:object-types (authored store))))]
    (with-snapshot store
      (fn [s] (api/read s api/unrestricted [:objects/changes T {:upto 0}])))
    nil))

(defn- with-previous
  [es]
  (first
   (reduce (fn [[out seen] {:keys [id op values before] :as e}]
             (let [seen (if (= :create-object op) (dissoc seen id) seen)
                   prev (or before (select-keys (get seen id) (keys values)))]
               [(conj out (assoc e :previous prev))
                (update seen id merge values)]))
           [[] {}]
           es)))

(defn- edit-of-change
  [{:keys [rev id op values previous actor at]}]
  (cond-> {:seq rev :pk id :edited-values values :previous-values previous
           :edited-by (:principal actor) :edited-at at
           :edit-kind (case op :create-object "create" :delete-object "delete" "update")}
    (:action actor)    (assoc :action (:action actor))
    (:action-id actor) (assoc :action-id (:action-id actor))
    (:agent actor)     (assoc :agent (:agent actor))
    (:agent-run actor) (assoc :agent-run (:agent-run actor))))

(defn- edits
  [store T {:keys [pk upto authority]}]
  (let [cs (with-snapshot store
             (fn [s] (api/read s (or authority api/unrestricted)
                               [:objects/changes T
                                (cond-> {:origin :edit}
                                  pk   (assoc :ids [(str pk)])
                                  upto (assoc :upto upto))])))]
    (mapv edit-of-change (rseq (with-previous cs)))))

(defn edit-history
  [store T {:keys [limit] :as opts}]
  (into [] (take (or limit 200)) (edits store T opts)))

(defn- page-token
  [token]
  (when token
    (if-let [[_ upto offset] (re-matches #"(\d+)\.(\d+)" (str token))]
      [(parse-long upto) (parse-long offset)]
      (api/refuse! :malformed (str "page token " (pr-str token) " is not one this read gave")
                   {:token token}))))

(defn- on-props
  [props e]
  (let [vs (select-keys (:edited-values e) props)]
    (when (seq vs)
      (assoc e :edited-values vs :previous-values (select-keys (:previous-values e) props)))))

(defn edit-history-page
  [store T {:keys [limit token oldest-first? kinds editors props] :as opts}]
  (let [[upto offset] (page-token token)
        all     (edits store T (assoc opts :upto upto))
        keep?   (fn [{:keys [edit-kind edited-by]}]
                  (and (or (empty? kinds) (contains? (set kinds) edit-kind))
                       (or (empty? editors) (contains? (set editors) (str edited-by)))))
        matched (cond->> (filter keep? all)
                  (seq props)   (keep (partial on-props (map keyword props)))
                  oldest-first? reverse
                  true          vec)
        offset  (or offset 0)
        page    (into [] (comp (drop offset) (take (or limit 50))) matched)
        reached (+ offset (count page))]
    (cond-> {:history page :total (count matched)}
      (and (seq page) (< reached (count matched)))
      (assoc :next-token (str (or upto (:seq (first all))) "." reached)))))

(defn edit-usage
  [store T days]
  (let [since (java.util.Date. (- (.getTime ^java.util.Date ((:clock store)))
                                  (* (long days) 86400000)))]
    (->> (edit-history store T {:limit Integer/MAX_VALUE})
         (filter #(and (:edited-at %) (.after ^java.util.Date (:edited-at %) since)))
         (group-by :edited-by)
         (map (fn [[p es]]
                {:edited_by p :writes (count es)
                 :last_at (apply max-key #(.getTime ^java.util.Date %) (map :edited-at es))}))
         (sort-by (juxt (comp - :writes) :edited_by))
         vec)))

(defn edit-surface
  [store W]
  (with-snapshot
   store
   (fn [s]
     (reduce (fn [acc [T spec]]
               (let [nm (:object-type-name spec)
                     es (api/read s api/unrestricted [:objects/edited T])]
                 (if (empty? es)
                   acc
                   (-> acc (update :types conj nm)
                       (update :properties into
                               (for [[_ {:keys [attrs]}] es a attrs :when (not= :markings a)]
                                 [nm a]))))))
             {:types #{} :properties #{}}
             (:object-types W)))))

(defn edit-provenance
  [store T {:keys [pk limit authority]}]
  (let [es   (edited store T (when pk [pk]))
        hist (group-by :pk (edit-history store T {:pk pk :limit Integer/MAX_VALUE
                                                  :authority authority}))]
    (->> (for [[id {:keys [created? deleted?]}] es
               :let [h (get hist id)]
               :when (or (nil? authority) (seq h))]
           {:pk id
            :edit_kind  (cond deleted? "delete" created? "create" :else "update")
            :edited_by  (:edited-by (first h))
            :updated_at (:edited-at (first h))
            :values     (reduce (fn [m {:keys [edited-values edited-at]}]
                                  (reduce-kv (fn [m a v]
                                               (if (contains? m a) m
                                                   (assoc m a {:v v :at (str edited-at)}))) m
                                             edited-values))
                                {} h)})
         (sort-by #(some-> ^java.util.Date (:updated_at %) .getTime) #(compare %2 %1))
         (take (or limit Long/MAX_VALUE))
         vec)))

(defn forget-edits!
  [store Ts]
  (transact! store
             {:basis (head store) :actor {:principal "sutra.store.world"} :forget-edits (vec Ts)}))

(defn adopt-edits!
  [store entries]
  (when (seq entries)
    (transact! store
               {:basis (head store) :actor {:principal "sutra.store.world"} :message "adopt edits"
                :adopt-edits (vec entries)})))

(def ^:private index-chunk 5000)

(defn index!
  ([store ops] (index! store ops {}))
  ([store ops {:keys [actor log log-fn origin records]}]
   (let [chunks (partition-all index-chunk ops)
         last-i (dec (count chunks))]
     (into []
           (map-indexed (fn [i ch]
                          (let [entries (cond-> (vec (when log-fn (log-fn ch)))
                                          (and log (= i last-i)) (into log))]
                            (transact! store (cond-> {:basis (head store)
                                                      :origin (or origin :index)
                                                      :actor (or actor {:principal "index"})
                                                      :ops (vec ch)}
                                               (seq entries) (assoc :log entries)
                                               (and (seq records) (= i last-i))
                                               (assoc :records (vec records)))))))
           chunks))))

(defn- seen-head
  [store]
  (some-> (:view store) deref (get (str (:branch store)))))

(defn- migration
  [old w steps]
  (let [named (into #{} (for [[k T a] steps :when (= :rename-attribute k)] [T a]))]
    (-> (vec steps)
        (into (remove (fn [[_ T a]] (named [T a]))) (lib/spelling-renames old w))
        (into (lib/derived-clears old w)))))

(defn- populated? [W] (some #(pos? (count (get W %))) ents/structural-sections))

(defn- wipe-guard!
  [old w opts]
  (when (and (populated? old) (not (populated? w)) (not (:allow-schema-wipe? opts)))
    (anomaly/raise!
     :unprocessable
     "Schema-wipe guard: refusing to overwrite a populated ontology with a structurally-empty schema. Set SUTRA_ALLOW_SCHEMA_WIPE=1 to override."
     {:sutra/schema-wipe-guard true})))

(defn- touched
  [W changes]
  (res/world (dissoc (res/only W (res/ids-of (d/nodes changes))) res/frame-id)))

(defn- whole-schema
  [store {w :world :keys [steps basis prior]}]
  (let [w    (overlaid store w authored)
        seen (seen-head store)
        [head-basis old] (if (and seen prior (not basis))
                           [nil (:authored prior)]
                           (with-snapshot store
                             (fn [s] [(api/basis s) (api/read s nil [:schema/world])])))]
    {:old    old :w w
     :basis  (or basis seen head-basis)
     :fence? (and seen (not basis))
     :schema {:authored w :migration (migration old w steps)}}))

(defn- changes-schema
  [store {:keys [changes steps basis]}]
  (let [[head-basis old] (with-snapshot store
                           (fn [s] [(api/basis s) (api/read s nil [:schema/world])]))
        w                (try (d/apply-to old changes)
                              (catch clojure.lang.ExceptionInfo e
                                (if (= :stale-change (:ref/kind (ex-data e))) old (throw e))))]
    {:old    old :w w
     :basis  (or basis head-basis)
     :schema {:changes   (vec changes)
              :migration (migration (touched old changes) (touched w changes) steps)}}))

(def ^:private decls-states
  (atom {}))

(defn- decls-state
  [{:keys [init]} k tick W]
  (let [e (get @decls-states k)]
    (if (and e (some? tick) (= tick (:tick e))) (:st e) (init W))))

(defn- stepped-decls
  [{:keys [step] :as view} store r W0 W changes]
  (let [{:keys [branch rev]} (:basis r)
        k   [(:db store) branch]
        st0 (decls-state view k (:prior-tick r) W0)
        st  (step st0 W0 W (d/nodes changes))
        ts  (filter #(not= (get-in st0 [:decls %]) (get-in st [:decls %]))
                    (:touched (meta st)))]
    (when orakel/on? (orakel/decls! view W st))
    (swap! decls-states #(assoc (if (and (< 64 (count %)) (not (contains? % k))) {} %)
                                k {:tick rev :st st}))
    [(select-keys (:decls st0) ts) (select-keys (:decls st) ts)]))

(defn try-commit!
  [store {w :world :keys [changes actor dataset-decls dataset-view drop-unclaimed? basis]
          :as opts}]
  (when-not (or basis w changes (:at-head? opts) (seen-head store))
    (throw (ex-info (str "a commit names the basis it was decided at (or :world, or "
                         ":at-head? true when nothing it read can be stale)")
                    {:store/refusal :blind-commit :branch (str (:branch store))})))
  (let [sch (cond changes (changes-schema store opts)
                  w       (whole-schema store opts))]
    (when sch (wipe-guard! (:old sch) (:w sch) opts))
    (let [tx (into {} (remove (comp #(or (nil? %) (and (coll? %) (empty? %))) val))
                   (select-keys opts
                                [:fences :ops :log :records :origin :message :parents :reparent
                                 :retires]))
          r  (try-transact! store
                            (cond-> (assoc tx :basis (or (:basis sch) basis (seen-head store)
                                                         (with-snapshot store api/basis))
                                           :actor (or actor {:principal "sutra.store.world"}))
                              sch            (assoc :schema (:schema sch))
                              (:fence? sch)  (update :fences (fnil conj []) [:schema])))]
      (anomaly/then
       r
       (fn [r]
         (doseq [{:keys [branch]} (:retires opts)] (moved! store branch nil))
         (when-let [d (and sch dataset-decls (:datasets store))]
           (let [[old new] (if (and changes dataset-view)
                             (stepped-decls dataset-view store r (:prior-world r) (:world r)
                                            changes)
                             [(dataset-decls (:prior-world r)) (dataset-decls (:world r))])
                 rev       (get-in r [:basis :rev])]
             (datasets/layout! d (:branch store)
                               {:old  old :new new
                                :head #(if (= rev (:rev (head store)))
                                         new
                                         (dataset-decls (world (assoc store :view nil))))
                                :drop-unclaimed? (boolean drop-unclaimed?)})))
         (dissoc r :world :prior-world :prior-tick))))))

(defn commit!
  [store opts]
  (anomaly/unwrap! (try-commit! store opts)))

(defn install!
  ([store authored] (install! store authored {}))
  ([store authored opts] (commit! store (assoc (dissoc opts :basis) :world authored))))
