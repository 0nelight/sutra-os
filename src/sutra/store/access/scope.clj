(ns sutra.store.access.scope
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.catalog.grants :as grants]
            [sutra.kit.system :as sys]
            [sutra.store.ports :as ports]
            [sutra.store.storage.blob-store :as blob-store]
            [sutra.store.world :as world]
            [sutra.types.entities :as ents]
            [sutra.types.branch-kinds :as branch-kinds]
            [sutra.foundation.env :as env]))

(set! *warn-on-reflection* true)

(defn- provided [s k & args]
  (some-> (sys/provided s k) (apply args)))

(defn db
  [s]
  (or (provided s :store/db)
      (anomaly/raise!
       :fault
       "the installed system provides no :store/db — install one (sutra.kit.system/install!) with a store driver among its modules"
       {:capability :store/db})))

(defn datasets [s lane] (provided s :datasets/db lane))

(defn streams [s lane] (provided s :streams/db lane))

(defn coord
  []
  (or (provided (sys/process) :coord/db)
      (anomaly/raise! :fault "the installed system provides no :coord/db"
                      {:capability :coord/db})))

(defn blobs
  []
  (let [s     (sys/process)
        store (or (provided s :blobs/db)
                  (anomaly/raise! :fault "the installed system provides no :blobs/db"
                                  {:capability :blobs/db}))]
    (if-let [through (sys/provided s :blobs/read-through)]
      (through store)
      store)))

(defn code
  []
  (let [s (sys/process)]
    {:introspect-python  (sys/provided s :code/introspect-python)
     :introspect-clojure (sys/provided s :code/introspect-clojure)
     :sql-explain        (sys/provided s :sql/explain)}))

(defn repos
  ^java.io.File []
  (let [^String dir (or (provided (sys/process) :git/dir)
                        (anomaly/raise! :fault "the installed system provides no :git/dir"
                                        {:capability :git/dir}))]
    (java.io.File. dir)))

(defn foreign
  []
  (provided (sys/process) :foreign/connect))

(defn connect
  [auth]
  (partial (foreign) auth))

(defn drain-blobs!
  [store]
  (when (= :pg (blob-store/kind))
    (when-let [f (ports/blobs-drain store)]
      (f store))))

(defn- wall-clock [] (java.util.Date.))

(def at
  world/at)

(def context world/context)

(def in-context world/in-context)

(defn act-for
  [store auth]
  (assoc store :authority auth))

(defn as-system
  ([store] (assoc store :authority grants/system-authority))
  ([store user] (assoc store :authority (assoc grants/system-authority :user user))))

(defn store-of
  [s b auth]
  (cond-> (assoc (world/store (db s) b {:datasets      (datasets s nil)
                                        :streams       (streams s nil)
                                        :clock         wall-clock
                                        :instance-keys ents/instance-keys
                                        :instance-home (partial branch-kinds/instance-home
                                                                "main")})
                 ::sys/system s)
    auth (assoc :authority auth)))

(defn store
  ([] (store "main" nil))
  ([b] (store b nil))
  ([b auth] (store-of (sys/process) b auth)))

(defn schema-wipe-allowed?
  []
  (some? (env/getenv "SUTRA_ALLOW_SCHEMA_WIPE")))

(defn system-store
  ([] (system-store "main"))
  ([b] (store b grants/system-authority)))

(defn object-edited-attrs
  [store obj-type obj-id]
  (disj (or (get-in (world/edited store obj-type [obj-id]) [(str obj-id) :attrs])
            #{})
        :markings))
