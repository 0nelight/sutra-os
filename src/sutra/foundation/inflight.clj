(ns sutra.foundation.inflight
  (:require [clojure.string :as str])
  (:import (java.util.concurrent ConcurrentHashMap)
           (java.util.concurrent.atomic AtomicLong)))

(set! *warn-on-reflection* true)

(def ^:private plumbing
  ["sutra.foundation.sql_trace" "sutra.foundation.inflight"
   "sutra.store.pg." "sutra.store.core" "sutra.store.api" "sutra.store.machine"
   "sutra.store.memo" "sutra.store.records" "sutra.store.world$with_snapshot"
   "sutra.store.world$snapshot" "sutra.store.control$" "sutra.store.storage.pg"])

(defn- named?
  [^String c]
  (and (str/starts-with? c "sutra.")
       (not (some #(str/starts-with? c ^String %) plumbing))))

(defn- frame-name
  [^StackTraceElement f]
  (-> (clojure.lang.Compiler/demunge (.getClassName f))
      (str/replace #"--\d+" "")
      (str/replace #"/(fn|iter|eval)(?=/|$)" "")))

(defn stack
  ([] (stack (Thread/currentThread) 16))
  ([t] (stack t 16))
  ([^Thread t depth]
   (into [] (comp (keep (fn [^StackTraceElement f]
                          (let [c (.getClassName f)] (when (named? c) (frame-name f)))))
                  (dedupe)
                  (take depth))
         (.getStackTrace t))))

(defn frame-location
  [^StackTraceElement f]
  (let [c (.getClassName f)]
    (if (str/includes? c "$")
      (frame-name f)
      (str c "." (.getMethodName f)))))

(defn frame-label
  [^StackTraceElement f]
  (let [c (.getClassName f)]
    (if (str/starts-with? c "sutra.")
      (frame-name f)
      (str (peek (str/split c #"\.")) "." (.getMethodName f)))))

(defn blocked-in
  ([t] (blocked-in t 12))
  ([^Thread t depth]
   (into [] (comp (map frame-label) (dedupe) (take depth)) (.getStackTrace t))))

(defn registry
  []
  {:seq (AtomicLong. 0) :open (ConcurrentHashMap.)})

(defn open!
  [{:keys [^AtomicLong seq ^ConcurrentHashMap open]} entry]
  (let [id (.incrementAndGet seq)]
    (.put open id (assoc entry :seq id :t0 (System/nanoTime) :t (System/currentTimeMillis)))
    id))

(defn entry
  [{:keys [^ConcurrentHashMap open]} id]
  (.get open (long id)))

(defn close!
  [{:keys [^ConcurrentHashMap open]} id]
  (.remove open (long id)))

(defn running-ms
  [{:keys [t0]}]
  (when t0 (long (Math/round (/ (double (- (System/nanoTime) (long t0))) 1e6)))))

(defn running-exact-ms
  [{:keys [t0]}]
  (when t0 (/ (double (- (System/nanoTime) (long t0))) 1e6)))

(defn entries
  [{:keys [^ConcurrentHashMap open]}]
  (vec (.values open)))

(defn live
  ([reg] (live reg identity))
  ([reg prepare]
   (->> (entries reg)
        (map (fn [e] (prepare (assoc e :running-ms (running-ms e)))))
        (sort-by :seq)
        vec)))

(defn clear!
  [{:keys [^ConcurrentHashMap open]}]
  (.clear open)
  nil)
