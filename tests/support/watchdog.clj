(ns support.watchdog
  (:require [clojure.string :as str]
            [kaocha.plugin :refer [defplugin]])
  (:import (java.util.concurrent Executors ScheduledExecutorService ScheduledFuture ThreadFactory
                                 TimeUnit)))

(defn- limit-s [env default]
  (or (some-> (System/getenv env) parse-long) default))

(def ^:private timer
  (delay (Executors/newSingleThreadScheduledExecutor
          (reify ThreadFactory
            (newThread [_ r] (doto (Thread. ^Runnable r "test-watchdog") (.setDaemon true)))))))

(def ^:private armed (atom {}))

(def ^:private library
  ["java." "jdk." "sun." "clojure." "kaocha." "com.zaxxer." "org.postgresql." "org.httpkit."
   "app//clojure."])

(defn- project-frame? [line]
  (let [f (str/trim line)]
    (and (not (str/blank? f))
         (not (str/starts-with? f "#"))
         (not-any? #(str/includes? f %) ["java.base@" "platform/java" "jdk.internal"])
         (not-any? #(str/starts-with? (str/replace f #"^app//" "") %) library))))

(defn- dump-threads
  []
  (let [f (java.io.File/createTempFile "sutra-threads" ".txt")]
    (.delete f)
    (.dumpThreads ^com.sun.management.HotSpotDiagnosticMXBean
                  (java.lang.management.ManagementFactory/getPlatformMXBean
                   com.sun.management.HotSpotDiagnosticMXBean)
                  (.getPath f)
                  com.sun.management.HotSpotDiagnosticMXBean$ThreadDumpFormat/TEXT_PLAIN)
    (doseq [block (str/split (slurp f) #"\n\s*\n")
            :let [lines (str/split-lines block)]
            :when (and (str/starts-with? (str/trim (first lines)) "#")
                       (not (str/includes? (first lines) "test-watchdog"))
                       (some project-frame? (rest lines)))]
      (println (str "\n--- " (str/trim (first lines))))
      (doseq [l (take 45 (rest lines))] (println "   " (str/trim l))))
    (println (str "\n[watchdog] full thread dump: " (.getPath f)))))

(defn- stalled! [id kind secs]
  (locking *out*
    (println (str "\n[watchdog] " (name kind) " " id " ran longer than " secs
                  "s — aborting the run"))
    (dump-threads)
    (flush))
  (.halt (Runtime/getRuntime) 3))

(defn- limit-for [testable]
  (case (:kaocha.testable/type testable)
    :kaocha.type/var [:test (max (limit-s "SUTRA_TEST_STALL_S" 60)
                                 (or (:stall-s (:kaocha.testable/meta testable)) 0))]
    :kaocha.type/ns  [:namespace (limit-s "SUTRA_TEST_NS_STALL_S" 300)]
    nil))

(defplugin support.watchdog/plugin
  (pre-test [testable _test-plan]
            (when-let [[kind secs] (limit-for testable)]
              (when (= :namespace kind)
                (doseq [[id [k ^ScheduledFuture f]] @armed :when (= :namespace k)]
                  (.cancel f false)
                  (swap! armed dissoc id)))
              (let [id (:kaocha.testable/id testable)
                    f  (.schedule ^ScheduledExecutorService @timer
                                  ^Runnable #(stalled! id kind secs)
                                  (long secs) TimeUnit/SECONDS)]
                (swap! armed assoc id [kind f])))
            testable)

  (post-test [testable _test-plan]
             (let [id (:kaocha.testable/id testable)]
               (some-> ^ScheduledFuture (second (get @armed id)) (.cancel false))
               (swap! armed dissoc id))
             testable))
