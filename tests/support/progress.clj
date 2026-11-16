(ns support.progress
  (:require [clojure.test :as t]
            [kaocha.hierarchy :as hierarchy]
            [kaocha.plugin :refer [defplugin]]
            [kaocha.result :as result]
            [kaocha.testable :as testable]))

(def ^:private state (atom nil))

(defn- enabled? [] (not= "0" (System/getenv "SUTRA_TEST_PROGRESS")))

(defn- every-ms []
  (or (some-> (System/getenv "SUTRA_TEST_PROGRESS_EVERY_MS") parse-long) 1000))

(defn- ns-testable? [t] (= :kaocha.type/ns (:kaocha.testable/type t)))

(defn- dur [ms]
  (let [s (quot (long ms) 1000)]
    (if (< s 60)
      (str s "s")
      (String/format java.util.Locale/US "%dm%02ds"
                     (into-array Object [(quot s 60) (rem s 60)])))))

(defn- line [{:keys [tests total ns-done ns-total assertions fails t0 label]}]
  (let [elapsed (- (System/currentTimeMillis) t0)
        pct     (if (pos? total) (/ (* 100.0 tests) total) 0.0)]
    (str "[test-progress] " tests "/" total " tests ("
         (String/format java.util.Locale/US "%.1f" (into-array Object [pct]))
         "%) · ns " ns-done "/" ns-total
         " · " assertions " assertions"
         (when (pos? fails) (str " · " fails " failing"))
         " · " (dur elapsed) " elapsed"
         (when label (str " · " label)))))

(defn- emit! [snapshot]
  (t/with-test-out
    (print (str \newline (line snapshot) \newline))
    (flush)))

(defn- tick! [label force?]
  (let [now (System/currentTimeMillis)
        s   (swap! state (fn [s]
                           (cond-> (assoc s :label label)
                             (or force? (>= (- now (:last s)) (every-ms)))
                             (assoc :last now :due? true))))]
    (when (:due? s)
      (swap! state dissoc :due?)
      (emit! s))))

(defplugin support.progress/plugin
  (pre-run [test-plan]
           (when (enabled?)
             (let [all (testable/test-seq test-plan)
                   now (System/currentTimeMillis)
                   run-ns? #(and (ns-testable? %)
                                 (boolean (some hierarchy/leaf? (testable/test-seq %))))]
               (reset! state {:tests 0 :assertions 0 :fails 0 :ns-done 0
                              :total    (count (filter hierarchy/leaf? all))
                              :ns-total (count (filter run-ns? all))
                              :t0 now :last 0})
               (tick! "start" true)))
           test-plan)

  (post-test [testable _test-plan]
             (when (and (enabled?) @state (ns-testable? testable))
               (let [{:kaocha.result/keys [count pass fail
                                           error]} (result/testable-totals testable)]
                 (swap! state (fn [s]
                                (-> s
                                    (update :tests + (or count 0))
                                    (update :assertions + (+ (or pass 0) (or fail 0) (or error 0)))
                                    (update :fails + (+ (or fail 0) (or error 0)))
                                    (update :ns-done inc)))))
               (tick! (some-> (:kaocha.testable/id testable) name) false))
             testable)

  (post-run [result]
            (when (and (enabled?) @state)
              (tick! "done" true))
            result))
