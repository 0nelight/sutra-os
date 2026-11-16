(ns sutra.foundation.profile
  (:require [sutra.kit.attempt :as attempt]
            [sutra.kit.noted :as noted]
            [clojure.string :as str]
            [sutra.foundation.armed :as armed]
            [sutra.foundation.inflight :as inflight]
            [sutra.observe :as obs]))

(set! *warn-on-reflection* true)

(def ^:private frame-rules
  [[#"(?i)\.(park|parkNanos|await|awaitNanos|socketRead|epollWait|poll0|acquire)\b" :blocked]
   [#"^(org\.postgresql|com\.zaxxer|java\.sql|com\.mysql|com\.microsoft\.sqlserver|org\.sqlite)"
    :sql]
   [#"^sutra\.store" :store]
   [#"^sutra\.index" :index]
   [#"^(sutra\.serve|reitit|ring|muuntaja|org\.httpkit|HttpServer|Server\.)" :serve]
   [#"^(sutra\.conductor|sutra\.write|sutra\.data-plane)" :conductor]
   [#"(?i)(transit|GZIPOutputStream|Deflater|data\.json|jsonista|charset|CharsetEncoder)" :encode]
   [#"^(clojure\.|sutra\.)" :clojure]
   [#"^(java\.|javax\.|sun\.|jdk\.|com\.sun\.)" :jvm]])

(defn- layer-of* ^long [label]
  (let [s (str label)]
    (obs/layer-id (or (some (fn [[re k]] (when (re-find re s) k)) frame-rules) :other))))

(def layer-of (memoize layer-of*))

(def empty-recording
  {:threads {} :markers [] :marker-count 0 :dropped 0 :ticks 0
   :sample-count 0 :full? false})

(def ^:private empty-thread
  {:name nil :stack-ids {} :stacks [] :samples []})

(defn- intern-stack [t stack]
  (if-let [i (get-in t [:stack-ids stack])]
    [t i]
    (let [i (count (:stacks t))]
      [(-> t (assoc-in [:stack-ids stack] i) (update :stacks conj stack)) i])))

(defn absorb-sample [t {:keys [name stack cpu t-ms]}]
  (let [t     (assoc t :name (or name (:name t)))
        [t i] (if (seq stack) (intern-stack t stack) [t nil])]
    (update t :samples conj [i t-ms (or cpu 0)])))

(defn absorb [rec cap {:keys [t-ms threads]}]
  (if (or (:full? rec) (>= (long (:sample-count rec)) (long cap)))
    (assoc rec :full? true)
    (-> (reduce (fn [acc {:keys [tid] :as s}]
                  (update-in acc [:threads tid] (fnil absorb-sample empty-thread)
                             (assoc s :t-ms t-ms)))
                rec
                threads)
        (update :ticks inc)
        (update :sample-count + (count threads)))))

(defn record-marker [rec cap marker]
  (if (>= (long (:marker-count rec)) (long cap))
    (update rec :dropped inc)
    (-> rec (update :markers conj marker) (update :marker-count inc))))

(defn- track-tid ^long [track]
  (- (bit-and (hash (str track)) 0x7fffffff)))

(defn absorb-events [rec cap events]
  (reduce (fn [acc {:keys [track] :as e}]
            (let [tid (track-tid track)]
              (-> acc
                  (update-in [:threads tid]
                             (fnil (fn [t] (assoc t :name (or (:name t) (str track))))
                                   empty-thread))
                  (record-marker cap (-> e (dissoc :track) (assoc :tid tid))))))
          rec
          events))

(defn clock-shift
  [markers]
  (let [served (into {} (for [m markers :when (= "Request" (:name m))]
                          [(get-in m [:data :span]) m]))
        bounds (for [m markers
                     :when (= (obs/marker-name :api/transit) (:name m))
                     :let [r (get served (get-in m [:data :span]))]
                     :when r]
                 [(- (double (:end r)) (double (:end m)))
                  (- (double (:start r)) (double (:start m)))])]
    (when (seq bounds)
      (let [lo (reduce max (map first bounds))
            hi (reduce min (map second bounds))]
        (when (<= lo hi) (/ (+ lo hi) 2.0))))))

(defn align-browser
  [{:keys [markers] :as rec}]
  (let [tid   (track-tid "browser")
        shift (clock-shift markers)]
    (if-not shift
      rec
      (assoc rec :markers
             (mapv (fn [m]
                     (if (= tid (:tid m))
                       (-> m (update :start #(max 0.0 (+ (double %) shift)))
                           (update :end #(max 0.0 (+ (double %) shift))))
                       m))
                   markers)))))

(defn trace->stack
  [^"[Ljava.lang.StackTraceElement;" trace ^long depth]
  (let [n (min depth (alength trace))]
    (loop [i 0 out (transient [])]
      (if (>= i n)
        (persistent! out)
        (let [^StackTraceElement f (aget trace i)
              line (.getLineNumber f)]
          (recur (inc i) (conj! out [(inflight/frame-location f) (when (pos? line) line)])))))))

(def ^:const default-interval-ms 10)
(def ^:const default-depth 128)
(def ^:const max-markers 200000)
(def ^:const max-samples 300000)

(defonce ^:private recording (atom empty-recording))

(defonce ^:private clock
  (atom {:interval default-interval-ms :depth default-depth
         :t0-nanos nil :t0-epoch nil :sampler nil}))

(declare start-sampler! stop-sampler!)

(defonce ^:private switch
  (armed/switch {:label "profile" :on-arm #'start-sampler! :on-disarm #'stop-sampler!}))

(defn armed? []
  (armed/armed? switch))

(defn- elapsed-ms ^double []
  (if-let [t0 (:t0-nanos @clock)]
    (/ (double (- (System/nanoTime) (long t0))) 1e6)
    0.0))

(defonce ^:private ^ThreadLocal span (ThreadLocal.))

(defonce ^:private ^java.util.concurrent.ConcurrentHashMap in-span
  (java.util.concurrent.ConcurrentHashMap.))

(defn current-span [] (.get span))

(defn set-span! [v] (.set span v) v)

(defn enter-span! [label]
  (let [t (Thread/currentThread)]
    (.put in-span (.threadId t) {:thread t :label label})
    nil))

(defn leave-span! [prev]
  (let [t (Thread/currentThread)]
    (if prev
      (.put in-span (.threadId t) {:thread t :label prev})
      (.remove in-span (.threadId t)))
    nil))

(defmacro with-span [label & body]
  `(let [prev# (current-span)]
     (set-span! ~label)
     (enter-span! ~label)
     (try ~@body (finally (set-span! prev#) (leave-span! prev#)))))

(defmacro span-future [& body]
  `(let [span# (current-span)
         work# (fn [] ~@body)]
     (future (if span# (with-span span# (work#)) (work#)))))

(defn mark! [nm start end data]
  (when (armed?)
    (attempt/silently
      (swap! recording record-marker max-markers
             (obs/event {:kind nm :at start :dur (- (double end) (double start))
                         :tid (.threadId (Thread/currentThread))
                         :span (current-span) :data data})))
    nil))

(defn mark-ms! [nm ms data]
  (when (armed?)
    (let [end (elapsed-ms)]
      (mark! nm (max 0.0 (- end (double (or ms 0)))) end data))))

(defmacro timing [report & body]
  `(if-not (armed?)
     (do ~@body)
     (let [t0# (System/nanoTime)
           v#  (do ~@body)]
       (~report (/ (double (- (System/nanoTime) t0#)) 1e6) v#)
       v#)))

(defmacro with-mark [nm data & body]
  `(let [d# (when (armed?) ~data)]
     (timing (fn [ms# _#] (mark-ms! ~nm ms# d#)) ~@body)))

(defmacro with-mark-result [nm payload-fn & body]
  `(timing (fn [ms# v#]
             (mark-ms! ~nm ms# (attempt/silently (~payload-fn v#))))
           ~@body))

(defn ingest!
  [events]
  (when (and (armed?) (seq events))
    (let [t0 (:t0-epoch @clock)
          evs (obs/foreign-events t0 events)]
      (swap! recording absorb-events max-markers evs)
      {:ingested (count evs)})))

(defonce ^:private live-views (atom {}))

(defn add-live-view!
  [k f]
  (swap! live-views assoc k f)
  nil)

(defn- read-live-views []
  (into {} (for [[k f] @live-views]
             [k (noted/or-else (str "live view " k) nil (f))])))

(def ^:private ^java.lang.management.ThreadMXBean tmx
  (java.lang.management.ManagementFactory/getThreadMXBean))

(defonce ^:private cpu-time?
  (try (and (.isThreadCpuTimeSupported tmx)
            (do (when-not (.isThreadCpuTimeEnabled tmx) (.setThreadCpuTimeEnabled tmx true))
                (.isThreadCpuTimeEnabled tmx)))
       (catch UnsupportedOperationException _ false)))

(def ^:private own-threads
  #{"sutra-profiler" "sutra-deadman" "Reference Handler" "Finalizer" "Signal Dispatcher"
    "Common-Cleaner" "process reaper" "Attach Listener" "Notification Thread"})

(def ^:private own-prefixes
  ["GC Thread" "G1 " "VM " "C1 " "C2 " "JFR "])

(defn- own? [^String nm]
  (or (contains? own-threads nm)
      (boolean (some #(str/starts-with? nm %) own-prefixes))))

(defn cpu-of ^long [^long tid]
  (if-not cpu-time? 0 (try (.getThreadCpuTime tmx tid) (catch UnsupportedOperationException _ -1))))

(defn- carrier? [^Thread t]
  (= "jdk.internal.misc.CarrierThread" (.getName (class t))))

(defn- parked? [^Thread t]
  (not= "RUNNABLE" (str (.getState t))))

(defn- read-threads [^long depth]
  (let [platform (into {}
                       (keep (fn [^java.util.Map$Entry e]
                               (let [^Thread th (.getKey e)
                                     nm (.getName th)]
                                 (when-not (or (own? nm) (carrier? th))
                                   [(.threadId th)
                                    {:tid (.threadId th) :name nm
                                     :stack (trace->stack (.getValue e) depth)
                                     :cpu-total (cpu-of (.threadId th))}]))))
                       (Thread/getAllStackTraces))]
    (into (vec (vals platform))
          (keep (fn [{:keys [^Thread thread label]}]
                  (let [id (.threadId thread)]
                    (when-not (contains? platform id)
                      (if-not (.isAlive thread)
                        (do (.remove in-span id) nil)
                        {:tid id
                         :name (let [n (.getName thread)]
                                 (if (seq n) n (str label)))
                         :stack (trace->stack (.getStackTrace thread) depth)
                         :parked? (parked? thread)
                         :cpu-total (cpu-of id)})))))
          (vec (.values in-span)))))

(defn as-deltas [prev interval threads]
  (mapv (fn [{:keys [tid cpu-total stack parked?] :as s}]
          (let [now (long cpu-total)
                p   (get prev tid)
                measured? (and (not (neg? now)) p (not (neg? (long p))))]
            (-> s (dissoc :cpu-total :parked?)
                (assoc :cpu (cond
                              measured?  (max 0 (- now (long p)))
                              (neg? now) (if (and (seq stack) (not parked?))
                                           (* 1000000 (long interval))
                                           0)
                              :else      0)))))
        threads))

(defn- sampler-loop [^long interval ^long depth]
  (fn []
    (loop [prev {}]
      (when (armed?)
        (let [began (System/nanoTime)
              prev' (try
                      (let [threads (read-threads depth)]
                        (swap! recording absorb max-samples
                               {:t-ms (elapsed-ms)
                                :threads (as-deltas prev interval threads)})
                        (into {} (map (juxt :tid :cpu-total)) threads))
                      (catch Throwable t (attempt/rethrow-fatal! t) prev))
              spent (quot (- (System/nanoTime) began) 1000000)]
          (if (:full? @recording)
            (armed/off! switch)
            (do (try (Thread/sleep (max 1 (- interval spent)))
                     (catch InterruptedException _ nil))
                (recur prev'))))))))

(defn- stop-sampler! []
  (some-> ^Thread (:sampler @clock) .interrupt)
  (swap! clock assoc :sampler nil)
  nil)

(defn- start-sampler! [{:keys [interval-ms depth]}]
  (reset! recording empty-recording)
  (let [interval (max 1 (long (or interval-ms default-interval-ms)))
        depth    (max 8 (long (or depth default-depth)))]
    (swap! clock assoc :interval interval :depth depth
           :t0-nanos (System/nanoTime) :t0-epoch (System/currentTimeMillis)
           :sampler (doto (Thread. ^Runnable (sampler-loop interval depth) "sutra-profiler")
                      (.setDaemon true)
                      (.start)))
    {:interval-ms interval :depth depth :cpu-time cpu-time?}))

(defn arm! [opts] (armed/arm! switch opts))

(defn disarm! [] (armed/disarm! switch))

(defn- host []
  {:pid (str (.pid (java.lang.ProcessHandle/current)))
   :os (str (System/getProperty "os.name") " " (System/getProperty "os.arch"))
   :abi (System/getProperty "os.arch")
   :jvm (str "JVM " (System/getProperty "java.version"))
   :cpus (.availableProcessors (Runtime/getRuntime))})

(defn snapshot []
  (let [{:keys [interval depth t0-nanos t0-epoch]} @clock]
    (assoc (align-browser @recording)
           :live (read-live-views)
           :lifecycle (armed/status switch)
           :interval-ms interval
           :depth depth
           :cpu-time? cpu-time?
           :elapsed-ms (when t0-nanos (long (elapsed-ms)))
           :started-epoch t0-epoch
           :host (host))))

(defn status
  ([] (status (snapshot)))
  ([{:keys [lifecycle interval-ms depth cpu-time? ticks threads
            sample-count marker-count dropped elapsed-ms full?]}]
   (cond-> {:interval-ms interval-ms :depth depth
            :cpu-time cpu-time? :ticks ticks
            :threads (count threads)
            :samples sample-count
            :markers marker-count
            :dropped-markers dropped}
     full?      (assoc :full (str "sample cap " max-samples " reached — sampling stopped"))
     elapsed-ms (assoc :elapsed-ms elapsed-ms)
     :always    (merge lifecycle))))
