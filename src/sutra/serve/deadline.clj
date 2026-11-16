(ns sutra.serve.deadline
  (:require [clojure.string :as str]
            [sutra.foundation.encoding :as enc]
            [sutra.foundation.env :as env]
            [sutra.foundation.inflight :as inflight]
            [sutra.foundation.profile :as prof]
            [sutra.foundation.settings :as settings]
            [sutra.http.responses :refer [negotiated]]
            [sutra.kit.attempt :as attempt]
            [sutra.serve.span :as span]
            [sutra.types.paths :as paths]))

(set! *warn-on-reflection* true)

(def ^:private default-deadline-ms
  (or (some-> (env/getenv "SUTRA_REQUEST_DEADLINE_MS") parse-long) paths/default-deadline-ms))

(defn- deadline-ms
  [req]
  (paths/deadline-ms (:sutra/endpoint req) default-deadline-ms))

(def ^:private admission
  (java.util.concurrent.Semaphore. (int (settings/value :request-concurrency)) true))

(def ^:private admission-size
  (java.util.concurrent.atomic.AtomicLong. (settings/value :request-concurrency)))

(defn- request-concurrency
  []
  (let [want (long (settings/value :request-concurrency))
        d    (- want (.getAndSet ^java.util.concurrent.atomic.AtomicLong admission-size want))]
    (cond
      (pos? d) (.release ^java.util.concurrent.Semaphore admission (int d))
      (neg? d) (doto (Thread. ^Runnable #(.acquireUninterruptibly
                                          ^java.util.concurrent.Semaphore admission (int (- d)))
                              "sutra-admission-cut")
                 (.setDaemon true)
                 (.start)))
    want))

(def ^:private in-flight
  (java.util.concurrent.ConcurrentHashMap.))

(def ^:const expired-capacity
  64)

(defonce ^:private expired-ring
  (atom []))

(defn- record-expiry!
  [^Thread t {:keys [incident uri method t0]}]
  (let [frames (inflight/blocked-in t)
        row    {:incident incident :uri uri :method method
                :at (System/currentTimeMillis)
                :ran-ms (when t0
                          (long (Math/round (/ (double (- (System/nanoTime) (long t0))) 1e6))))
                :thread (.getName t)
                :stack frames}]
    (swap! expired-ring enc/conj-capped row expired-capacity)
    (prof/with-span incident
      (prof/mark-ms! :Stuck (:ran-ms row)
                     {:method method :uri uri :thread (.getName t)
                      :frame (or (first frames) "no sutra frame")}))
    (binding [*out* *err*]
      (println (str "[deadline] " incident " " method " " uri " stuck in: "
                    (if (seq frames) (str/join " <- " (take 6 frames)) "no sutra frame"))))
    row))

(defonce ^:private watchdog
  (delay
    (doto (Thread.
           ^Runnable
           (fn []
             (loop []
               (let [now (System/nanoTime)]
                 (doseq [e (vec (.entrySet ^java.util.concurrent.ConcurrentHashMap in-flight))]
                   (let [t (.getKey ^java.util.Map$Entry e)
                         {:keys [at state monitor] :as entry} (.getValue ^java.util.Map$Entry e)]
                     (locking monitor
                       (when (and (< (long at) now)
                                  (#{:running :expired} @state)
                                  (.containsKey ^java.util.concurrent.ConcurrentHashMap in-flight
                                                t))
                         (when (= :running @state)
                           (attempt/silently (record-expiry! t entry)))
                         (vreset! state :expired)
                         (.interrupt ^Thread t))))))
               (Thread/sleep 200)
               (recur)))
           "sutra-request-watchdog")
      (.setDaemon true)
      (.start))))

(defn requests-in-flight
  []
  {:deadline-ms default-deadline-ms
   :concurrency (request-concurrency)
   :permits-free (.availablePermits ^java.util.concurrent.Semaphore admission)
   :requests (->> (.entrySet ^java.util.concurrent.ConcurrentHashMap in-flight)
                  vec
                  (remove (fn [^java.util.Map$Entry e]
                            (identical? (.getKey e) (Thread/currentThread))))
                  (mapv (fn [^java.util.Map$Entry e]
                          (let [^Thread t (.getKey e)
                                {:keys [incident uri method t0 state]} (.getValue e)]
                            {:incident incident :uri uri :method method
                             :thread (.getName t)
                             :state (some-> state deref name)
                             :running-ms (when t0
                                           (long
                                            (Math/round
                                             (/ (double (- (System/nanoTime) (long t0))) 1e6))))
                             :stack (inflight/blocked-in t)})))
                  (sort-by (comp - #(or (:running-ms %) 0)))
                  vec)
   :expired (vec (reverse @expired-ring))})

(defn- late-response
  [req reason incident]
  (binding [*out* *err*]
    (println (str "[503] " incident " " (:request-method req) " " (:uri req) " — " reason)))
  (negotiated req 503
              {:error (str "the server did not answer "
                           (str/upper-case (name (or (:request-method req) :get)))
                           " " (:uri req) " — " reason
                           " (" incident ")")
               :data {:uri (:uri req)
                      :method (:request-method req)
                      :deadline-ms (deadline-ms req)
                      :incident incident
                      :reason reason}}))

(defn- expired-reason
  [req]
  (str "no answer within " (deadline-ms req)
       "ms — it was interrupted at the deadline and then failed. "
       (if (some-> (:sutra/endpoint req) (paths/writes? (:request-method req)) false?)
         "A read changes nothing, so asking again is safe."
         (str "The interrupt stops the work at its next store round trip; whether a step"
              " before that already committed is not known from here, so read the state"
              " back before repeating it."))))

(defn wrap-deadline
  [handler]
  (fn [req]
    (if-not (deadline-ms req)
      (handler req)
      (if-not (let [_ (request-concurrency)]
                (.tryAcquire ^java.util.concurrent.Semaphore admission
                             (long (settings/value :request-admission-wait-ms))
                             java.util.concurrent.TimeUnit/MILLISECONDS))
        (late-response req (str "the server is at capacity (" (request-concurrency)
                                " in flight, waited "
                                (settings/value :request-admission-wait-ms)
                                "ms) — nothing was started, so retrying is safe")
                       (span/incident!))
        (let [t        (Thread/currentThread)
              state    (volatile! :running)
              monitor  (Object.)
              incident (or (::span/span req) (span/incident!))
              done!    (fn []
                         (locking monitor
                           (when (= :running @state) (vreset! state :done))
                           (.remove ^java.util.concurrent.ConcurrentHashMap in-flight t)
                           (Thread/interrupted)
                           (= :expired @state)))]
          @watchdog
          (.put ^java.util.concurrent.ConcurrentHashMap in-flight t
                {:at (+ (System/nanoTime) (* 1000000 (long (deadline-ms req))))
                 :t0 (System/nanoTime)
                 :incident incident
                 :uri (str (:uri req))
                 :method (some-> (:request-method req) name str/upper-case)
                 :state state :monitor monitor})
          (try
            (let [res (handler req)]
              (if (and (done!) (<= 500 (long (or (:status res) 200))))
                (late-response req (expired-reason req) incident)
                res))
            (catch Throwable e
              (if (done!) (late-response req (expired-reason req) incident) (throw e)))
            (finally
              (done!)
              (.release ^java.util.concurrent.Semaphore admission))))))))
