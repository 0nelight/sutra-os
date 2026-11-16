(ns sutra.store.storage.node
  (:require [sutra.kit.anomaly :as anomaly]
            [clojure.string :as str]
            [sutra.foundation.env :as env]))

(set! *warn-on-reflection* true)

(defn- env [k]
  (some-> (env/getenv k) str/trim not-empty))

(defonce ^:private id*
  (delay
    (or (env "SUTRA_NODE_ID")
        (let [host (or (env "HOSTNAME")
                       (try (.getHostName (java.net.InetAddress/getLocalHost))
                            (catch java.net.UnknownHostException _ "host")))
              pid  (.pid (java.lang.ProcessHandle/current))]
          (str host "/" pid "/" (subs (str (java.util.UUID/randomUUID)) 0 8))))))

(defn id
  []
  @id*)

(def role
  (let [r (keyword (or (env "SUTRA_ROLE") "all"))]
    (when-not (#{:all :api :conductor :worker} r)
      (anomaly/raise! :fault (str "SUTRA_ROLE must be all, api, conductor or worker, not " (name r))
                             {:role r}))
    r))

(def lease-ms
  (or (some-> (env "SUTRA_LEASE_MS") parse-long) 30000))

