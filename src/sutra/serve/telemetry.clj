(ns sutra.serve.telemetry
  (:require [sutra.kit.attempt :as attempt]
            [sutra.foundation.periodic :as periodic]
            [sutra.foundation.settings :as settings]
            [clojure.string :as str]
            [sutra.catalog.branch-policy :as policy]
            [sutra.foundation.egress :as egress]
            [sutra.foundation.env :as env]
            [sutra.governance.api :as governance]
            [sutra.http.endpoint :as ep]
            [sutra.serve.openmetrics :as openmetrics]
            [sutra.serve.ports :as ports]
            [sutra.sources.datasources :as ds]
            [sutra.store.storage.node :as node]
            [sutra.store.storage.pg :as pg])
  (:import (com.sun.management OperatingSystemMXBean)
           (java.lang.management GarbageCollectorMXBean ManagementFactory)
           (java.nio.file FileStore Files Paths)))

(set! *warn-on-reflection* true)

(defn- read-file
  [path]
  (try (String. (Files/readAllBytes (Paths/get path (make-array String 0))) "UTF-8")
       (catch java.io.IOException _ nil)))

(defn- status-bytes
  [text k]
  (some->> text (re-find (re-pattern (str "(?m)^" k ":\\s+(\\d+) kB"))) second parse-long
           (* 1024)))

(defn- cgroup-long [file]
  (some-> (read-file (str "/sys/fs/cgroup/" file)) str/trim parse-long))

(defn- cgroup-cpus
  []
  (let [[quota period] (some-> (read-file "/sys/fs/cgroup/cpu.max") str/trim (str/split #"\s+"))]
    (when-let [q (some-> quota parse-long)]
      (/ (double q) (double (parse-long period))))))

(defn- cgroup []
  (let [usage (some->> (read-file "/sys/fs/cgroup/cpu.stat")
                       (re-find #"(?m)^usage_usec (\d+)") second parse-long)
        c     {:memory      (cgroup-long "memory.current")
               :memory-max  (cgroup-long "memory.max")
               :swap        (cgroup-long "memory.swap.current")
               :cpu-usec    usage
               :cpus        (cgroup-cpus)}]
    (when (some some? (vals c)) c)))

(defn- jvm []
  (let [rt  (Runtime/getRuntime)
        gcs (ManagementFactory/getGarbageCollectorMXBeans)]
    {:heap-used      (- (.totalMemory rt) (.freeMemory rt))
     :heap-committed (.totalMemory rt)
     :heap-max       (.maxMemory rt)
     :non-heap-used  (.getUsed (.getNonHeapMemoryUsage (ManagementFactory/getMemoryMXBean)))
     :threads        (.getThreadCount (ManagementFactory/getThreadMXBean))
     :gc-count       (reduce + (map #(max 0 (.getCollectionCount ^GarbageCollectorMXBean %)) gcs))
     :gc-ms          (reduce + (map #(max 0 (.getCollectionTime ^GarbageCollectorMXBean %)) gcs))
     :cpus           (.availableProcessors rt)}))

(defn- process []
  (let [status (read-file "/proc/self/status")
        os     (ManagementFactory/getOperatingSystemMXBean)]
    (cond-> {:rss  (status-bytes status "VmRSS")
             :swap (status-bytes status "VmSwap")}
      (instance? OperatingSystemMXBean os)
      (assoc :cpu-ms   (quot (.getProcessCpuTime ^OperatingSystemMXBean os) 1000000)
             :cpu-load (.getProcessCpuLoad ^OperatingSystemMXBean os)
             :load-avg (.getSystemLoadAverage os)))))

(defn- disk-dirs []
  (distinct (keep #(some-> % str/trim not-empty)
                  (concat (map env/getenv ["SUTRA_GIT_DIR" "SUTRA_BLOB_DIR" "SUTRA_XFER_DIR"])
                          [(System/getProperty "java.io.tmpdir")]))))

(defn- disks
  []
  (->> (disk-dirs)
       (keep (fn [d]
               (try (let [^FileStore fs (Files/getFileStore (Paths/get d (make-array String 0)))]
                      {:store (str fs) :dir d :total (.getTotalSpace fs)
                       :usable (.getUsableSpace fs)})
                    (catch java.io.IOException _ nil))))
       (group-by :store)
       (mapv (fn [[_ [f :as fs]]] (assoc (dissoc f :dir) :dirs (mapv :dir fs))))))

(defn- pools []
  (let [p (ports/pg-ds)]
    (when (instance? sutra.store.storage.pg.Pools p) (pg/lane-stats p))))

(defn sample
  []
  (let [rb (ManagementFactory/getRuntimeMXBean)]
    {:node      (node/id)
     :role      node/role
     :pid       (.pid (java.lang.ProcessHandle/current))
     :host      (or (env/getenv "HOSTNAME") "")
     :at        (System/currentTimeMillis)
     :uptime-ms (.getUptime rb)
     :java      (System/getProperty "java.version")
     :jvm       (jvm)
     :process   (process)
     :cgroup    (cgroup)
     :pools     (pools)
     :disks     (disks)}))

(defonce ^:private sampler (periodic/slot))

(defn- period-ms []
  (settings/value :telemetry-ms))

(defn- publish! []
  (when-let [{:keys [publish!]} (ports/telemetry-samples)]
    (try (publish! (sample))
         (catch Throwable t
           (attempt/rethrow-fatal! t)
           (println (str "[telemetry] sample not kept: " (.getMessage t)))))))

(defn start! [ms] (periodic/start! sampler "sutra-telemetry" 0 ms publish!))

(defn stop! [] (periodic/stop! sampler))

(defn- schema-state
  [{b :branch}]
  (cond (nil? b)                :unowned
        (= "main" (:name b))    :main
        (policy/purgeable? b)   :purge-due
        :else                   (policy/branch-state b)))

(defn branch-schemas
  [schemas]
  (let [rows (map #(assoc % :state (schema-state %)) schemas)
        sum  (fn [rs] {:schemas (count rs)
                       :tables  (reduce + (map :tables rs))
                       :empty   (reduce + (map :empty rs))
                       :bytes   (reduce + (map :bytes rs))})]
    {:by-state (into (sorted-map) (for [[s rs] (group-by :state rows)] [s (sum rs)]))
     :largest  (->> rows (sort-by :tables >) (take 10)
                    (mapv #(-> (select-keys % [:schema :state :tables :empty :bytes])
                               (assoc :branch (get-in % [:branch :name])))))}))

(defn- ratio [a b]
  (when (and a b (pos? b)) (/ (double a) (double b))))

(defn- node-findings
  [period {:keys [node age-ms jvm process cgroup pools]}]
  (let [at   {:node node}
        mem  (ratio (:memory cgroup) (:memory-max cgroup))
        heap (ratio (:heap-used jvm) (:heap-max jvm))
        wait (into {} (for [[l s] pools :when (pos? (:waiting s 0))] [l (:waiting s)]))]
    (cond-> []
      (> (or age-ms 0) (* 3 period))
      (conj (assoc at :kind :silent :age-ms age-ms
                   :says "no sample for three periods: the process is gone or stuck"))
      (> (or (:swap process) 0) (* 64 1024 1024))
      (conj (assoc at :kind :swapping :bytes (:swap process)
                   :says "part of the process lives in swap; the host is short of memory"))
      (and mem (>= mem 0.9))
      (conj (assoc at :kind :memory-limit :ratio mem
                   :says "the container is near its memory limit"))
      (and heap (>= heap 0.9))
      (conj (assoc at :kind :heap :ratio heap :says "the heap is near its maximum"))
      (seq wait)
      (conj (assoc at :kind :pool-waiting :lanes wait
                   :says "requests wait for a pg connection (a lane is exhausted)")))))

(defn findings
  [{:keys [nodes store schemas period-ms]}]
  (let [{:keys [purge-due unowned purged]} (:by-state schemas)
        conns   (reduce + (map :n (:connections store)))
        max-c   (get-in store [:settings :max-connections])]
    (cond-> (into [] (mapcat #(node-findings period-ms %)) nodes)
      purge-due
      (conj (assoc purge-due :kind :purge-due
                   :says "ended branches are due for purge and still hold their schemas"))
      unowned
      (conj (assoc unowned :kind :unowned-schemas
                   :says "schemas no branch owns: left behind by a drop"))
      purged
      (conj (assoc purged :kind :purged-schemas
                   :says "branches are purged and their schemas still exist"))
      (some-> (ratio conns max-c) (>= 0.8))
      (conj {:kind :connections :n conns :max max-c
             :says "pg connections are near max_connections"})
      :always
      (into (for [{:keys [disks node]} nodes
                  d disks
                  :let [free (ratio (:usable d) (:total d))]
                  :when (and free (< free 0.1))]
              {:kind :disk :node node :dirs (:dirs d) :usable (:usable d)
               :says "a disk this process writes is over 90 % full"})))))

(defn- agent-heaps []
  (->> (egress/agent-names)
       (mapv (fn [a] (future (when-let [s (ds/agent-status a 1500)]
                               {:agent a :heap (:heap s) :uptime-ms (:uptime-ms s)
                                :java (:java s)}))))
       (keep deref)
       vec))

(def ^:private role-order {:api 0 :all 0 :conductor 1 :worker 2})

(defn report
  [{:keys [here recent store agents period-ms]}]
  (let [nodes   (->> recent
                     (remove #(= (:node here) (:node %)))
                     (cons (assoc here :age-ms 0))
                     (sort-by (juxt #(role-order (keyword (:role %)) 3) :node))
                     vec)
        schemas (branch-schemas (:schemas store))]
    {:at        (:at here)
     :period-ms period-ms
     :nodes     nodes
     :agents    agents
     :store     (assoc (dissoc store :schemas) :branch-schemas schemas)
     :findings  (findings {:nodes nodes :store store :schemas schemas :period-ms period-ms})}))

(defn- current [{:keys [store]}]
  (let [samples (ports/telemetry-samples)
        fp      (ports/telemetry-store)
        agents  (future (agent-heaps))]
    (report {:here      (sample)
             :recent    (if samples ((:recent samples)) [])
             :store     (when fp
                          (fp {:branches (governance/branch-states store {:closed? true})}))
             :agents    @agents
             :period-ms (period-ms)})))

(def telemetry (ep/handler :telemetry {:call current}))

(def telemetry-metrics
  (ep/handler :telemetry-metrics
              {:respond (fn [a]
                          {:status  200
                           :headers {"Content-Type" openmetrics/content-type}
                           :body    (openmetrics/text (current a))})}))

(def module
  {:name        :telemetry
   :jobs        [{:name :telemetry :every :telemetry-ms
                  :start start! :stop stop!}]
   :contributes {:http/routes {:telemetry         {:get telemetry}
                               :telemetry-metrics {:get telemetry-metrics}}}})
