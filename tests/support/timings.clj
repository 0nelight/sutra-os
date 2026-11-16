(ns support.timings
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [kaocha.plugin :refer [defplugin]]))

(def ^:private started   (atom {}))
(def ^:private durations (atom {}))

(defn- ns-testable? [t]
  (= :kaocha.type/ns (:kaocha.testable/type t)))

(defplugin support.timings/plugin
  (pre-test [testable _test-plan]
            (when (ns-testable? testable)
              (swap! started assoc (:kaocha.testable/id testable) (System/nanoTime)))
            testable)

  (post-test [testable _test-plan]
             (when-let [t0 (and (ns-testable? testable)
                                (get @started (:kaocha.testable/id testable)))]
               (swap! durations assoc (:kaocha.testable/id testable)
                      (/ (double (- (System/nanoTime) t0)) 1e9)))
             testable)

  (post-run [result]
            (when-let [path (System/getenv "SUTRA_TEST_TIMINGS")]
              (io/make-parents path)
              (spit path
                    (str/join (for [[id secs] (sort-by key @durations)]
                                (String/format java.util.Locale/US "%s\t%.3f%n"
                                               (into-array Object [(name id) secs]))))))
            result))
