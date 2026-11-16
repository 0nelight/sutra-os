(ns sutra.foundation.profile.scope
  (:require [sutra.kit.attempt :as attempt]
            [clojure.string :as str]
            [sutra.foundation.profile :as prof]
            [sutra.foundation.sql-trace :as sqltrace]
            [sutra.observe :as obs]))

(set! *warn-on-reflection* true)

(def ^:private interval-ms 50)
(def ^:private report-every-ms 5000)
(def ^:private depth 256)
(def ^:private rows-kept 40)
(def ^:private tracer-ttl-seconds (* 4 60 60))

(def ^:dynamic *scope* nil)

(def empty-tally
  {:samples 0 :ms 0.0 :cpu-ms 0.0 :layers {} :self {} :inclusive {}})

(defn- add [counts k ms cpu-ms]
  (update counts k (fn [[m c]] [(+ (double (or m 0)) ms) (+ (double (or c 0)) cpu-ms)])))

(defn- ours? [label]
  (str/starts-with? (str label) "sutra."))

(defn absorb
  [tally stack ms cpu-ms]
  (let [labels (map first stack)
        leaf   (first labels)
        tally  (-> tally
                   (update :samples inc)
                   (update :ms + ms)
                   (update :cpu-ms + cpu-ms))]
    (if-not leaf
      tally
      (-> tally
          (update :layers add (prof/layer-of leaf) ms cpu-ms)
          (update :self add leaf ms cpu-ms)
          (update :inclusive
                  (fn [counts]
                    (reduce #(add %1 %2 ms cpu-ms) counts (filter ours? (distinct labels)))))))))

(defn- rounded ^long [x] (Math/round (double x)))

(defn- pct [v total]
  (when (pos? (double total))
    (/ (Math/round (/ (* 1000.0 (double v)) (double total))) 10.0)))

(defn- rows [counts total k limit]
  (->> counts
       (sort-by (comp - first val))
       (take limit)
       (mapv (fn [[a [ms cpu]]]
               {k a :ms (rounded ms) :cpu-ms (rounded cpu) :pct (pct ms total)}))))

(defn report
  [{:keys [samples ms cpu-ms layers self inclusive]}]
  {:samples    samples
   :interval-ms interval-ms
   :thread-ms  (rounded ms)
   :cpu-ms     (rounded cpu-ms)
   :waiting-ms (rounded (max 0.0 (- (double ms) (double cpu-ms))))
   :by-layer   (mapv #(update % :layer obs/layer-name) (rows layers ms :layer rows-kept))
   :self       (rows self ms :frame rows-kept)
   :inclusive  (rows inclusive ms :frame rows-kept)})

(defonce ^:private borrowers (atom 0))

(defn- borrow-tracer! []
  (locking borrowers
    (cond
      (pos? (long @borrowers)) (do (swap! borrowers inc) true)
      (sqltrace/armed?)        false
      :else (do (sqltrace/arm! {:capture-params? false :shapes? true
                                :ttl-seconds tracer-ttl-seconds})
                (reset! borrowers 1)
                true))))

(defn- return-tracer! []
  (locking borrowers
    (when (zero? (long (swap! borrowers dec)))
      (sqltrace/disarm!))))

(defn joined
  [f]
  (fn []
    (if-let [{:keys [threads span]} *scope*]
      (let [t (Thread/currentThread)]
        (swap! threads conj t)
        (try (prof/with-span span (f))
             (finally (swap! threads disj t))))
      (f))))

(defn- sample!
  [{:keys [threads tally]} ms prev-cpu]
  (reduce (fn [cpus ^Thread t]
            (let [tid  (.threadId t)
                  cpu  (prof/cpu-of tid)
                  was  (get prev-cpu tid)
                  used (if (and was (not (neg? cpu)))
                         (/ (double (max 0 (- cpu (long was)))) 1e6)
                         0.0)]
              (swap! tally absorb (prof/trace->stack (.getStackTrace t) depth) ms used)
              (assoc cpus tid cpu)))
          {}
          @threads))

(defn- current-report
  [{:keys [tally span t0]}]
  (cond-> (assoc (report @tally)
                 :elapsed-ms (quot (- (System/nanoTime) (long t0)) 1000000)
                 :sql-shapes (sqltrace/span-shapes-report span {:limit 10}))
    (not (sqltrace/shapes?)) (assoc :sql-shapes-partial true)))

(defn- publish! [{:keys [on-report] :as scope}]
  (try (on-report (current-report scope))
       (catch Throwable t
         (attempt/rethrow-fatal! t)
         (println (str "[profile] report not kept: " (.getMessage t))))))

(defn- sampler-loop
  [{:keys [stop?] :as scope}]
  (fn []
    (loop [last-t (System/nanoTime) cpus {} reported (System/currentTimeMillis)]
      (when-not @stop?
        (when (try (Thread/sleep (long interval-ms)) true
                   (catch InterruptedException e
                     (.interrupt (Thread/currentThread))
                     (println (str "[profile] sampler stopped: " e))
                     false))
          (let [now   (System/nanoTime)
                cpus' (try (sample! scope (/ (double (- now last-t)) 1e6) cpus)
                           (catch Throwable t
                             (attempt/rethrow-fatal! t)
                             (println (str "[profile] sample lost: " (.getMessage t)))
                             cpus))
                due?  (>= (- (System/currentTimeMillis) reported) (long report-every-ms))]
            (when due? (publish! scope))
            (recur now cpus' (if due? (System/currentTimeMillis) reported))))))))

(defn run-scoped
  [{:keys [label on-report]} f]
  (let [scope   {:threads (atom #{}) :tally (atom empty-tally) :stop? (atom false)
                 :span label :on-report on-report :t0 (System/nanoTime)}
        tracer? (try (borrow-tracer!)
                     (catch Throwable t
                       (attempt/rethrow-fatal! t)
                       (println (str "[profile] no SQL shapes: " (.getMessage t)))
                       false))
        _       (sqltrace/watch-span! label)
        sampler (doto (Thread. ^Runnable (sampler-loop scope) "sutra-profiler-scope")
                  (.setDaemon true)
                  (.start))]
    (try
      (binding [*scope* scope] ((joined f)))
      (finally
        (reset! (:stop? scope) true)
        (.interrupt sampler)
        (.join sampler 1000)
        (publish! scope)
        (sqltrace/unwatch-span! label)
        (when tracer? (return-tracer!))))))
