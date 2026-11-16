(ns sutra.serve.openmetrics
  (:require [clojure.string :as str]))

(def content-type "application/openmetrics-text; version=1.0.0; charset=utf-8")

(def ^:private node-metrics
  [["sutra_uptime_seconds" "gauge" "Process uptime." [:uptime-ms] 1/1000]
   ["sutra_sample_age_seconds" "gauge" "Age of the node's latest sample." [:age-ms] 1/1000]
   ["sutra_jvm_heap_used_bytes" "gauge" "JVM heap in use." [:jvm :heap-used] 1]
   ["sutra_jvm_heap_committed_bytes" "gauge" "JVM heap committed." [:jvm :heap-committed] 1]
   ["sutra_jvm_heap_max_bytes" "gauge" "JVM heap limit." [:jvm :heap-max] 1]
   ["sutra_jvm_non_heap_used_bytes" "gauge" "JVM non-heap in use." [:jvm :non-heap-used] 1]
   ["sutra_jvm_threads" "gauge" "Live JVM threads." [:jvm :threads] 1]
   ["sutra_jvm_gc_collections" "counter" "Garbage collections." [:jvm :gc-count] 1]
   ["sutra_jvm_gc_seconds" "counter" "Time spent in garbage collection." [:jvm :gc-ms] 1/1000]
   ["sutra_process_resident_memory_bytes" "gauge" "Resident set size." [:process :rss] 1]
   ["sutra_process_swap_bytes" "gauge" "Swap in use." [:process :swap] 1]
   ["sutra_process_cpu_seconds" "counter" "Process CPU time." [:process :cpu-ms] 1/1000]
   ["sutra_cgroup_memory_bytes" "gauge" "Container memory in use." [:cgroup :memory] 1]
   ["sutra_cgroup_memory_max_bytes" "gauge" "Container memory limit." [:cgroup :memory-max] 1]])

(defn- escape [v]
  (-> (str v) (str/replace "\\" "\\\\") (str/replace "\"" "\\\"") (str/replace "\n" "\\n")))

(defn- labels [m]
  (str "{" (str/join "," (for [[k v] (sort-by key m)] (str (name k) "=\"" (escape v) "\"")))
       "}"))

(defn- number [x]
  (let [d (double x)]
    (if (== d (Math/rint d)) (str (long d)) (str d))))

(defn- family
  [name type help samples]
  (when (seq samples)
    (concat [(str "# TYPE " name " " type) (str "# HELP " name " " help)]
            (for [[ls v] samples]
              (str name (when (= "counter" type) "_total") (labels ls) " " (number v))))))

(defn- node-labels [{:keys [node role]}] {:node node :role (some-> role name)})

(defn text
  [{:keys [nodes findings]}]
  (let [lines
        (concat
         (mapcat (fn [[nm type help path scale]]
                   (family nm type help
                           (for [n nodes :let [v (get-in n path)] :when (number? v)]
                             [(node-labels n) (* scale v)])))
                 node-metrics)
         (mapcat (fn [[k nm help]]
                   (family nm "gauge" help
                           (for [n nodes [lane st] (:pools n) :let [v (get st k)]
                                 :when (number? v)]
                             [(assoc (node-labels n) :lane (name lane)) v])))
                 [[:active "sutra_pg_pool_active" "Connections in use per pool lane."]
                  [:idle "sutra_pg_pool_idle" "Idle connections per pool lane."]
                  [:waiting "sutra_pg_pool_waiting" "Threads waiting for a connection."]
                  [:max "sutra_pg_pool_max" "Pool lane size."]])
         (mapcat (fn [[k nm help]]
                   (family nm "gauge" help
                           (for [n nodes d (:disks n) :let [v (get d k)] :when (number? v)]
                             [(assoc (node-labels n) :store (:store d)) v])))
                 [[:usable "sutra_disk_usable_bytes" "Usable space of a disk a process writes."]
                  [:total "sutra_disk_total_bytes" "Size of a disk a process writes."]])
         (family "sutra_findings" "gauge" "Telemetry findings by kind."
                 (for [[k fs] (sort-by (comp str key) (group-by :kind findings))]
                   [{:kind (some-> k name)} (count fs)])))]
    (str (str/join "\n" lines) (when (seq lines) "\n") "# EOF\n")))
