(ns ^:unit platform.openmetrics-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.serve.openmetrics :as om]))

(def ^:private report
  {:nodes    [{:node "api-1" :role :api :uptime-ms 61000 :age-ms 0
               :jvm {:heap-used 1048576 :gc-count 4 :gc-ms 1500 :threads 30}
               :process {:rss 2097152 :cpu-ms 2500}
               :pools {:control {:active 2 :idle 6 :waiting 0 :max 24}}
               :disks [{:store "/dev/vda1" :usable 1000 :total 4000 :dirs ["/tmp"]}]}
              {:node "worker \"7\"" :role :worker :jvm {:heap-used 10}}]
   :findings [{:kind :heap} {:kind :heap} {:kind :disk}]})

(def ^:private out (om/text report))

(defn- line? [s] (some #{s} (str/split-lines out)))

(deftest gauges-carry-node-and-role
  (is (line? "sutra_jvm_heap_used_bytes{node=\"api-1\",role=\"api\"} 1048576"))
  (is (line? "sutra_uptime_seconds{node=\"api-1\",role=\"api\"} 61")))

(deftest counters-end-in-total-and-are-scaled
  (is (line? "# TYPE sutra_jvm_gc_seconds counter"))
  (is (line? "sutra_jvm_gc_seconds_total{node=\"api-1\",role=\"api\"} 1.5"))
  (is (line? "sutra_process_cpu_seconds_total{node=\"api-1\",role=\"api\"} 2.5")))

(deftest lanes-disks-and-findings
  (is (line? "sutra_pg_pool_max{lane=\"control\",node=\"api-1\",role=\"api\"} 24"))
  (is (line? "sutra_disk_usable_bytes{node=\"api-1\",role=\"api\",store=\"/dev/vda1\"} 1000"))
  (is (line? "sutra_findings{kind=\"heap\"} 2")))

(deftest label-values-are-escaped
  (is (line? "sutra_jvm_heap_used_bytes{node=\"worker \\\"7\\\"\",role=\"worker\"} 10")))

(deftest the-exposition-is-well-formed
  (testing "ends in # EOF, a family without samples is left out"
    (is (str/ends-with? out "\n# EOF\n"))
    (is (not (str/includes? out "sutra_cgroup_memory_bytes"))))
  (is (= "# EOF\n" (om/text {:nodes [] :findings []}))))
