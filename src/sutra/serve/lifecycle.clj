(ns sutra.serve.lifecycle
  (:refer-clojure :exclude [reset!])
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.kit.attempt :as attempt]
            [sutra.kit.system :as sys]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [sutra.store.access.scope :as ctx]
            [sutra.store.world :as world]
            [sutra.store.migrations.index-feed :as index-feed]
            [sutra.store.migrations.object-edits :as object-edits]
            [sutra.store.migrations.schema-log :as schema-log]
            [sutra.store.migrations.log-kind :as log-kind]
            [sutra.store.migrations.type-identity :as type-identity]
            [sutra.store.migrations.inherited-edits :as inherited-edits]
            [sutra.store.access.datasets :as dsets]
            [sutra.catalog.signature :as signature]
            [sutra.actions.dispatch :as c]
            [sutra.authoring.install.world :as onto-install]
            [sutra.parser.authoring.standard :as standard]
            [sutra.store.seat :as seat]
            [sutra.governance.merge :as bm]
            [sutra.authoring.preview :as preview]
            [sutra.authoring.projections :as projections]
            [sutra.authoring.placements :as placements]
            [sutra.actions.landing :as landing]
            [sutra.store.storage.instance-config :as ic]
            [sutra.objects.saved-sets :as temp-sets]
            [sutra.access.oauth :as oauth]
            [sutra.access.oidc :as oidc]
            [sutra.foundation.ops :as op]
            [sutra.access.sessions :as sessions]
            [sutra.compute.external :as ext]))

(set! *warn-on-reflection* true)

(defn- auth-upkeep!
  [store]
  (ic/ensure-relay-token! store)
  (oidc/signing-key! store)
  (op/set-peer-secret! (ic/ensure-ops-secret! store))
  (oauth/sweep-expired-codes! store)
  (temp-sets/sweep-expired! store))

(defn- schema-or-degraded
  [store branch warn]
  (try
    (let [s (seat/rebuild-schema-from-storage (ctx/at store branch))]
      (seat/clear-schema-load-error!)
      s)
    (catch Throwable e
      (attempt/rethrow-fatal! e)
      (when warn (warn e))
      (seat/set-schema-load-error! {:message (.getMessage e) :branch branch :data (ex-data e)})
      c/empty-schema)))

(def ^:private adopted-key "projections.standard-adopted")

(defn- adopted
  [store]
  (let [v (some-> (ic/get-cfg store adopted-key) edn/read-string)]
    (if (set? v) v #{})))

(defn- adopt-standard-projections!
  [store branch]
  (let [done (adopted store)
        store   (ctx/as-system (ctx/at store branch) "sutra.authoring")
        w    (world/authored (world/at store branch))
        m    (standard/migrate w done)]
    (when (not= w m)
      (onto-install/install-on-branch! store m))
    (ic/set-cfg! store adopted-key (pr-str (into done (standard/entry-ids standard/standard))))))

(defn timed
  [label f]
  (let [t0 (System/nanoTime)
        r  (f)]
    (println (str "[boot] " label " " (quot (- (System/nanoTime) t0) 1000000) "ms"))
    r))

(defn- reconcile-in-background!
  [store branch]
  (future
    (try
      (let [store (ctx/at store branch)]
        (timed "transform reconcile"
               #(bm/reconcile-after-commit! store landing/after-commit-callbacks nil)))
      (catch Throwable e
        (attempt/rethrow-fatal! e)
        (binding [*out* *err*]
          (println (str "WARNING: reconciling " branch
                        "'s head at boot failed — a reset it owes is made up"
                        " by the next commit or boot. Cause: " (.getMessage e))))))))

(defn- compact-index-feed-in-background!
  [store branch]
  (future
    (try
      (when-let [{:keys [types commits]}
                 (timed "index feed compaction" #(index-feed/compact! (ctx/at store branch)))]
        (println (str "[boot] change feed: entries of " commits " index commit(s) of "
                      types " type(s) dropped")))
      (catch Throwable e
        (attempt/rethrow-fatal! e)
        (binding [*out* *err*]
          (println (str "WARNING: compacting the change feed's index entries failed — the"
                        " next boot tries again. Cause: " (.getMessage e))))))))

(defn- compact-inherited-edits-in-background!
  [store]
  (future
    (try
      (when-let [done (timed "inherited edits" #(inherited-edits/compact! store))]
        (println (str "[boot] inherited edits: " (reduce + 0 (vals done)) " copied entr(ies)"
                      " dropped on " (count done) " branch(es)")))
      (catch Throwable e
        (attempt/rethrow-fatal! e)
        (binding [*out* *err*]
          (println (str "WARNING: dropping the inherited edit copies failed — the next boot"
                        " tries again; reads are unaffected. Cause: " (.getMessage e))))))))

(defn- key-types-by-rid-in-background!
  [store]
  (future
    (try
      (when-let [moved (timed "type identity" #(type-identity/move! store))]
        (println (str "[boot] type identity: " (reduce + 0 (vals moved))
                      " index row(s) moved to their RID")))
      (catch Throwable e
        (attempt/rethrow-fatal! e)
        (binding [*out* *err*]
          (println (str "WARNING: moving index rows to their RID failed — the next boot tries"
                        " again; until then a type not built since shows no time and count."
                        " Cause: " (.getMessage e))))))))

(defn- seed-placements-in-background!
  [store]
  (future
    (try
      (when-not (ic/get-cfg store "migrations.placements-from-tree-2")
        (projections/seed-placements-from-tree! store (world/world store))
        (ic/set-cfg! store "migrations.placements-from-tree-2" "true")
        (println "[boot] placements taken over from the ontology tree"))
      (catch Throwable e
        (attempt/rethrow-fatal! e)
        (binding [*out* *err*]
          (println (str "WARNING: taking over the placements failed — the next boot tries "
                        "again. Cause: " (.getMessage e))))))))

(defn- tidy-sessions-in-background!
  [store]
  (future
    (try
      (let [swept (timed "sessions swept" #(sessions/sweep-expired! store))]
        (println (str "[boot] sessions: " swept " expired or unhashed deleted")))
      (catch Throwable e
        (attempt/rethrow-fatal! e)
        (binding [*out* *err*]
          (println (str "WARNING: tidying the sessions failed — the next boot tries again;"
                        " expired ones are not served meanwhile. Cause: " (.getMessage e))))))))

(defn- index-schema-logs-in-background!
  [store]
  (future
    (try
      (when-let [done (timed "schema log backfill" #(schema-log/index! store))]
        (println (str "[boot] schema log: " (count done) " branch(es) marked, "
                      (reduce + (vals done)) " schema commit(s) indexed")))
      (catch Throwable e
        (attempt/rethrow-fatal! e)
        (binding [*out* *err*]
          (println (str "WARNING: backfilling the schema logs failed — each branch is "
                        "filled on its first read instead. Cause: "
                        (.getMessage e))))))))

(defn- index-log-kinds-in-background!
  [store]
  (future
    (doseq [wait [0 (* 15 60 1000)]]
      (Thread/sleep (long wait))
      (try
        (when-let [done (timed "log kind index" #(log-kind/index! store))]
          (println (str "[boot] log kind index: " (count done) " branch(es) marked, "
                        (reduce + 0 (vals done)) " entr(ies) indexed")))
        (catch Throwable e
          (attempt/rethrow-fatal! e)
          (binding [*out* *err*]
            (println (str "WARNING: indexing the log by kind failed — reads of one kind "
                          "scan the branch's log instead. Cause: " (.getMessage e)))))))))

(defn- sweep-xfer-in-background!
  []
  (future
    (try
      (let [n (timed "xfer sweep" #(ext/sweep-xfer! (* 24 60 60 1000)))]
        (when (pos? n) (println (str "[boot] transfer volume: " n " leftover(s) deleted"))))
      (catch Throwable e
        (attempt/rethrow-fatal! e)
        (binding [*out* *err*]
          (println (str "WARNING: sweeping the transfer volume failed. Cause: "
                        (.getMessage e))))))))

(defn orphan-line
  [branch orphans]
  (when (seq orphans)
    (str "[boot] " branch ": " (count orphans) " table(s) no resource claims — "
         (str/join ", "
                   (for [{:keys [table store rows]} orphans]
                     (str table " (" (name store) ", " rows " rows)")))
         ". Nothing is dropped at boot; preview and drop them with `bin/sutra ontology "
         "prune-orphans`.")))

(defn- report-orphans-in-background!
  [store branch schema]
  (future
    (try
      (some-> (orphan-line branch (preview/orphan-tables (ctx/at store branch) schema)) println)
      (catch Throwable e
        (attempt/rethrow-fatal! e)
        (binding [*out* *err*]
          (println (str "WARNING: the orphan tables of " branch
                        " could not be read at boot. Cause: " (.getMessage e))))))))

(defn start!
  []
  (timed "store" #(ctx/db (sys/process)))
  (let [branch (seat/seated-branch)
        store (ctx/system-store)
        _  (timed "auth upkeep" #(auth-upkeep! store))
        _  (when-not (world/exists? store branch)
             (anomaly/raise! :fault (str "Branch " branch " not seeded; bootstrap broken?")
                                    {:branch branch}))
        schema (timed "schema"
                      #(schema-or-degraded
                        store branch
                        (fn [^Throwable e]
                          (binding [*out* *err*]
                            (println (str "WARNING: schema for branch " branch
                                          " failed to load — booting DEGRADED (empty "
                                          "schema). Cause: " (.getMessage e)))))))]
    (when-not (seat/schema-load-error)
      (try
        (timed "world heal" #(world/heal! (world/at (ctx/at store branch) branch)))
        (timed "dataset tables"
               #(dsets/ensure-tables! (ctx/at store branch) (signature/datasets-of schema)))
        (catch Throwable e
          (attempt/rethrow-fatal! e)
          (binding [*out* *err*]
            (println (str "WARNING: physical schema install failed for branch "
                          branch " — declared tables may be missing or narrow. "
                          "Cause: " (.getMessage e))))))
      (try
        (when-let [n (timed "object edits"
                            #(object-edits/adopt! (world/at (ctx/at store branch) branch)))]
          (println (str "[boot] " n " edit(s) of object_edits adopted on " branch)))
        (catch Throwable e
          (attempt/rethrow-fatal! e)
          (binding [*out* *err*]
            (println (str "WARNING: the edits of object_edits could not be adopted on " branch
                          " — an index run overwrites what they wrote until they are. Cause: "
                          (.getMessage e))))))
      (try
        (when-let [moved (not-empty (timed "placement rows" #(placements/migrate! store)))]
          (println (str "[boot] placement rows of before converted: " moved)))
        (catch Throwable e
          (attempt/rethrow-fatal! e)
          (binding [*out* *err*]
            (println (str "WARNING: converting the placement rows of before failed — their "
                          "placements read as none until a boot converts them. Cause: "
                          (.getMessage e))))))
      (reconcile-in-background! store branch)
      (compact-index-feed-in-background! store branch)
      (index-schema-logs-in-background! store)
      (index-log-kinds-in-background! store)
      (compact-inherited-edits-in-background! store)
      (key-types-by-rid-in-background! store)
      (seed-placements-in-background! store)
      (tidy-sessions-in-background! store)
      (report-orphans-in-background! store branch schema)
      (sweep-xfer-in-background!)
      (try
        (timed "standard projections" #(adopt-standard-projections! store branch))
        (catch Throwable e
          (attempt/rethrow-fatal! e)
          (binding [*out* *err*]
            (println (str "WARNING: the standard projections could not be installed on "
                          branch " — files they declare are not read until they are. Cause: "
                          (.getMessage e)))))))
    {:branch branch}))

(defn reload-schema!
  [store]
  (schema-or-degraded store (seat/seated-branch) nil))

