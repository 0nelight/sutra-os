(ns ^:unit platform.cli-sandbox-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [sutra.cli.sandbox :as sandbox]))

(defn- tmp-dir ^java.io.File []
  (.toFile (java.nio.file.Files/createTempDirectory
            "cli-sandbox" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- rm-r! [^java.io.File f]
  (when (.exists f) (doseq [c (reverse (file-seq f))] (.delete ^java.io.File c))))

(defn- session-dir!
  [^java.io.File base nm pid]
  (let [d (io/file base nm)]
    (.mkdirs d)
    (when pid (spit (io/file d ".session.edn") (pr-str {:pid pid :started "t"})))
    (spit (io/file d "payload.bin") "x")
    d))

(deftest sweep-collects-dead-sessions-and-spares-live-ones
  (let [base (io/file (tmp-dir) ".sutra-export")
        dead (session-dir! base "111-dead" 99999999)
        live (session-dir! base "222-live" (.pid (java.lang.ProcessHandle/current)))
        bare (session-dir! base "333-bare" nil)]
    (try
      (#'sandbox/sweep-stale! base)
      (testing "a session whose owning process is gone is collected"
        (is (not (.exists dead)) "dead session root deleted"))
      (testing "a markerless leftover is collected — liveness cannot be proven"
        (is (not (.exists bare)) "markerless root deleted"))
      (testing "a CONCURRENT session's root survives — this is what makes the sweep safe"
        (is (.exists (io/file live "payload.bin"))
            "live sibling's in-flight file untouched"))
      (finally (rm-r! (.getParentFile base))))))

(defn- absolute-classpath
  []
  (let [here (io/file (System/getProperty "user.dir"))]
    (->> (str/split (System/getProperty "java.class.path")
                               (re-pattern java.io.File/pathSeparator))
         (map #(.getAbsolutePath (if (.isAbsolute (io/file %)) (io/file %) (io/file here %))))
         (str/join java.io.File/pathSeparator))))

(deftest session-root-is-swept-on-jvm-exit
  (testing "a child CLI JVM that writes scratch leaves its cwd exactly as it found it"
    (let [dir (tmp-dir)
          src "(require '[sutra.cli.sandbox :as sb])
               (spit (sb/scratch-file \"a/b.pdf\") \"junk\")
               (println \"WROTE\" (.exists (java.io.File. (sb/session-root))))"
          {:keys [out err exit]} (sh/with-sh-dir (.getPath dir)
                                   (sh/sh "java" "-cp" (absolute-classpath)
                                             "clojure.main" "-e" src))]
      (try
        (is (zero? exit) (str "child JVM exited cleanly; stderr: " err))
        (is (.contains (str out) "WROTE true") "child really did write scratch")
        (is (empty? (seq (.list dir)))
            (str "cwd is clean after exit — no " sandbox/root-name
                 "/ left behind, got " (vec (.list dir))))
        (finally (rm-r! dir))))))
