(ns sutra.foundation.fn-trace
  (:require [sutra.kit.refusal :as refusal]
            [clojure.string :as str]
            [sutra.foundation.armed :as armed]
            [sutra.foundation.profile :as prof])
  (:import (java.util.concurrent ConcurrentHashMap)
           (java.util.concurrent.atomic AtomicLong)))

(set! *warn-on-reflection* true)

(defonce ^:private wrapped
  (atom {}))

(defonce ^:private ^ConcurrentHashMap stats
  (ConcurrentHashMap.))

(def ^:private ^ThreadLocal child-ns
  (proxy [ThreadLocal] []
    (initialValue [] (java.util.ArrayDeque.))))

(def ^:const max-vars
  6000)

(def ^:const default-slow-ms 5)

(defonce ^:private slow-nanos (atom (* 1000000 default-slow-ms)))

(def ^:const off-limits
  ["sutra.access.sessions" "sutra.access.principals"
   "sutra.foundation.encoding" "common.crypto"
   "sutra.serve.server" "wire." "sutra.foundation.sql-trace"
   "sutra.foundation.fn-trace"
   "sutra.foundation.profile" "sutra.foundation.armed"
   "sutra.foundation.inflight" "sutra.observe"])

(defrecord Stat [^AtomicLong calls ^AtomicLong total ^AtomicLong self])

(defn- stat-of
  ^Stat [k]
  (or (.get stats k)
      (let [s (->Stat (AtomicLong. 0) (AtomicLong. 0) (AtomicLong. 0))]
        (or (.putIfAbsent stats k s) s))))

(defn- prim-hinted?
  [f]
  (some #(.startsWith (.getName ^Class %) "clojure.lang.IFn$")
        (.getInterfaces (class f))))

(defn- traceable?
  [v]
  (and (var? v) (.isBound ^clojure.lang.Var v) (fn? @v)
       (not (prim-hinted? @v))
       (let [m (meta v)]
         (and (not (:macro m)) (not (:protocol m))))))

(defn- wrap
  [k f]
  (fn [& args]
    (let [^java.util.ArrayDeque d (.get child-ns)
          stat    (stat-of k)
          frame (AtomicLong. 0)
          t0    (System/nanoTime)]
      (.push d frame)
      (try
        (apply f args)
        (finally
          (.pop d)
          (let [elapsed (- (System/nanoTime) t0)]
            (.incrementAndGet ^AtomicLong (:calls stat))
            (.addAndGet ^AtomicLong (:total stat) elapsed)
            (.addAndGet ^AtomicLong (:self stat) (- elapsed (.get frame)))
            (when-let [^AtomicLong parent (.peek d)]
              (.addAndGet parent elapsed))
            (when (> elapsed (long @slow-nanos))
              (prof/mark-ms! :Fn (/ (double elapsed) 1e6) {:fn k}))))))))

(declare restore-all! wrap-all!)

(defonce ^:private switch
  (armed/switch {:label "fn-trace" :on-arm #'wrap-all! :on-disarm #'restore-all!
                 :ttl-seconds 900}))

(defn armed?
  []
  (armed/armed? switch))

(defn- off-limits?
  [^String nm]
  (some #(or (= nm %) (.startsWith nm ^String %)) off-limits))

(defn- targets
  [prefixes]
  (for [n (all-ns)
        :let [nm (str (ns-name n))]
        :when (and (some #(.startsWith nm ^String %) prefixes)
                   (not (off-limits? nm)))
        [_ v] (ns-interns n)
        :when (traceable? v)]
    v))

(defn- restore-all!
  []
  (let [m @wrapped]
    (doseq [[v f] m] (alter-var-root v (constantly f)))
    (reset! wrapped {})
    {:restored (count m)}))

(defn- wrap-all!
  [{:keys [prefixes slow-ms]}]
  (.clear stats)
  (reset! slow-nanos (* 1000000 (long (or slow-ms default-slow-ms))))
  (let [prefixes (vec (remove str/blank? (map str prefixes)))
        vs       (vec (targets prefixes))]
    (when (> (count vs) max-vars)
      (refusal/refuse! :refusal.foundation/trace-too-wide
                       {:matched (count vs) :max max-vars :prefixes prefixes}))
    (doseq [v vs]
      (let [f @v]
        (swap! wrapped assoc v f)
        (alter-var-root v (constantly (wrap (str (.ns ^clojure.lang.Var v)
                                                 "/" (.sym ^clojure.lang.Var v))
                                            f)))))
    {:wrapped (count vs) :prefixes prefixes
     :slow-ms (quot (long @slow-nanos) 1000000)}))

(defn arm!
  [opts]
  (armed/arm! switch opts))

(defn disarm!
  []
  (armed/disarm! switch))

(defn- stat-rows
  []
  (let [ms (fn [^long ns] (/ (Math/round (/ (double ns) 1e5)) 10.0))]
    (vec (for [[k ^Stat s] stats
               :let [calls (.get ^AtomicLong (:calls s))]
               :when (pos? calls)]
           {:fn k :calls calls
            :total-ms (ms (.get ^AtomicLong (:total s)))
            :self-ms  (ms (.get ^AtomicLong (:self s)))}))))

(defn sorted-rows
  [rows sort-by-key limit]
  (let [key- (case (keyword sort-by-key) :total :total-ms :calls :calls :self-ms)]
    (vec (take (or limit 25) (sort-by (comp - key-) rows)))))

(defn- status
  [fns]
  {:armed (armed?)
   :lifecycle (armed/status switch)
   :wrapped (count @wrapped)
   :fns fns})

(defn report
  [{:keys [limit sort-by-key]}]
  (let [rows (stat-rows)]
    (assoc (status (count rows))
           :rows (sorted-rows rows sort-by-key limit))))

(def ^:private kept-per-sort 50)

(defn kept-rows
  [rows]
  (->> [:self :total :calls]
       (mapcat #(sorted-rows rows % kept-per-sort))
       distinct
       vec))

(defn node-report
  []
  (let [rows (stat-rows)]
    (assoc (status (count rows)) :rows (kept-rows rows))))
