(ns support.load-gate
  (:require [kaocha.output :as output]
            [kaocha.plugin :refer [defplugin]]
            [kaocha.testable :as testable]
            [slingshot.slingshot :refer [throw+]]))

(defn- every-testable [t]
  (cons t (mapcat every-testable (or (:kaocha/tests t) (:kaocha.test-plan/tests t)))))

(defn- root-cause [^Throwable e]
  (if-let [c (.getCause e)] (recur c) e))

(defplugin support.load-gate/plugin
  (post-load [test-plan]
             (let [broken (->> (mapcat every-testable (:kaocha.test-plan/tests test-plan))
                               (filter ::testable/load-error))]
               (when (seq broken)
                 (doseq [t broken
                         :let [e (::testable/load-error t)]]
                   (output/error (str "bin/test: " (:kaocha.testable/id t)
                                      " failed to LOAD — no test in it ran:\n  "
                                      (.getMessage ^Throwable e)
                                      (let [c (root-cause e)]
                                        (when-not (identical? c e) (str "\n  caused by: " c))))))
                 (throw+ {:kaocha/early-exit 1})))
             test-plan))
