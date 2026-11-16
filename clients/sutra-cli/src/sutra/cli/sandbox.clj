(ns sutra.cli.sandbox
  (:require [clojure.java.io :as io]))

(def root-name
  ".sutra-export")

(def ^:private marker-name ".session.edn")

(def ^:private session-id
  (delay (let [pid (.pid (java.lang.ProcessHandle/current))]
           (str pid "-" (subs (str (java.util.UUID/randomUUID)) 0 8)))))

(defn- rm-r! [^java.io.File f]
  (when (.exists f)
    (doseq [c (reverse (file-seq f))] (.delete ^java.io.File c))))

(defn- dead-session?
  [^java.io.File dir]
  (let [m (io/file dir marker-name)]
    (if-not (.exists m)
      true
      (let [pid (try (:pid (read-string (slurp m))) (catch Exception _ nil))]
        (if-not (number? pid)
          true
          (not (-> (java.lang.ProcessHandle/of (long pid))
                   (.map (reify java.util.function.Function
                           (apply [_ h] (.isAlive ^java.lang.ProcessHandle h))))
                   (.orElse false))))))))

(defn- sweep-stale!
  [^java.io.File base]
  (when (.isDirectory base)
    (doseq [d (.listFiles base)
            :when (and (.isDirectory ^java.io.File d)
                       (not= (.getName ^java.io.File d) @session-id)
                       (dead-session? d))]
      (rm-r! d))))

(def ^:private root
  (delay
    (let [base (io/file root-name)
          dir  (io/file base @session-id)]
      (sweep-stale! base)
      (.mkdirs dir)
      (spit (io/file dir marker-name)
            (pr-str {:pid (.pid (java.lang.ProcessHandle/current))
                     :started (str (java.time.Instant/now))}))
      (.addShutdownHook (Runtime/getRuntime)
                        (Thread. #(do (rm-r! dir)
                                      (when (empty? (.list base)) (.delete base)))))
      dir)))

(defn scratch-file
  ^java.io.File [^String filename]
  (let [f (io/file (force root) filename)]
    (.mkdirs (.getParentFile f))
    f))

(defn session-root
  [] (.getPath ^java.io.File (force root)))
