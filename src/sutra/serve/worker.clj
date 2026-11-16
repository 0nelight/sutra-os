(ns sutra.serve.worker
  (:require [sutra.kit.attempt :as attempt]
            [sutra.foundation.settings :as settings]
            [sutra.builds.import :as importer]
            [sutra.builds.run.lock :as lock]
            [sutra.builds.run.monitor :as bm]
            [sutra.foundation.live-trace :as live]
            [sutra.builds.run.queue :as builds]
            [sutra.foundation.periodic :as periodic]
            [sutra.builds.run.schedule :as schedule]
            [sutra.kit.decisions :as decisions]
            [sutra.kit.system :as sys]
            [sutra.automation.rules :as rules]
            [sutra.serve.hot-patches :as hot]
            [sutra.store.access.scope :as ctx]
            [sutra.store.storage.node :as node]))

(set! *warn-on-reflection* true)

(def ^:private build-poll-ms 1000)
(def ^:private batch-poll-ms 5000)
(def ^:private mirror-ms 500)

(defn- build-cap [] (long (settings/value :build-concurrency)))

(def ^:private packet-bytes 7000)

(defonce ^:private state (atom nil))

(defonce ^:private bell (atom (promise)))

(defn- ring! [] (deliver @bell true))

(defn- nap!
  [ms]
  (deref (reset! bell (promise)) ms nil))

(defn- stopping? [] (boolean (some-> @state :sched deref :stopping?)))

(defn- run-request!
  [store {:keys [run-id branch context preset claim-id]}]
  (let [settlement (periodic/renewing
                    "sutra-claim-heartbeat" (quot builds/claim-ms 3)
                    #(builds/renew! store run-id claim-id)
                    #(try (builds/done (importer/run-build!
                                        store (or context {:branch branch})
                                        (assoc preset :run-id (or (parse-uuid run-id) run-id))))
                          (catch Throwable t (builds/failed t))))]
    (try (builds/finish! store run-id claim-id settlement)
         (catch Throwable t
           (attempt/rethrow-fatal! t)
           (println (str "[worker] build " run-id " ended (" (:status settlement)
                         ") but its settlement was not written: " (.getMessage t)))))
    (println (str "[worker] build " run-id " " (:status settlement)
                  (when-let [e (:error settlement)] (str ": " e))))))

(defn- seen
  [sched event]
  (assoc event
         :waiting (into #{} (filter lock/waiting?) (:running @sched))
         :cap (build-cap)
         :waiting-max (long (settings/value :build-waiting-max))
         :now (System/currentTimeMillis)))

(declare dispatch!)

(defn- effect!
  [sched store [kind x]]
  (case kind
    :tidy      (do (builds/tidy! store) nil)
    :take      (if-let [r (builds/take! store (node/id))]
                 {:kind :taken :request r}
                 {:kind :none})
    :start     (do (println (str "[worker] build " (:run-id x) " taken on " (node/id)))
                   (doto (Thread. ^Runnable
                          (fn [] (try (run-request! store x)
                                      (finally (dispatch! sched store
                                                          {:kind :finished
                                                           :run-id (:run-id x)}))))
                                  (str "sutra-worker-build-" (:run-id x)))
                     (.start))
                   nil)
    :hand-back (do (lock/hand-back! x) nil)
    :ring      (do (ring!) nil)))

(defn- dispatch!
  [sched store event]
  (loop [events [event]]
    (when-let [[event & more] (seq events)]
      (let [e (seen sched event)
            [old out] (loop []
                        (let [old @sched, out (schedule/step old e)]
                          (if (compare-and-set! sched old (first out)) [old out] (recur))))]
        (when (decisions/recording? :build/schedule)
          (decisions/record! :build/schedule [old e] out))
        (recur (into (vec more) (keep #(effect! sched store %)) (second out)))))))

(defn- build-lane!
  [sched]
  (while (not (stopping?))
    (try
      (dispatch! sched (ctx/system-store) {:kind :poll})
      (catch Throwable t
        (attempt/rethrow-fatal! t)
        (println (str "[worker] build pass failed: " (.getMessage t)))))
    (nap! build-poll-ms)))

(defn- batch-lane! []
  (while (not (stopping?))
    (try
      (doseq [{:keys [branch] :as x} (rules/executions-due (ctx/system-store))
              :while (not (stopping?))]
        (rules/run-batch! (ctx/at (ctx/system-store) branch) branch x))
      (catch Throwable t
        (attempt/rethrow-fatal! t)
        (println (str "[worker] batch pass failed: " (.getMessage t)))))
    (nap! batch-poll-ms)))

(defn- mirror!
  [buffer]
  (let [events (first (reset-vals! buffer []))]
    (when-let [broadcast (and (seq events) (sys/provided (sys/process) :cluster/broadcast))]
      (doseq [p (live/packets (builds/wire-safe events) packet-bytes)]
        (broadcast :live/events p)))))

(defn- mirror-lane!
  [buffer]
  (while (not (some-> @state :mirrored deref))
    (try (mirror! buffer)
         (catch Throwable t
           (attempt/rethrow-fatal! t)
           (println (str "[worker] live mirror failed: " (ex-message t)))))
    (Thread/sleep (long mirror-ms))))

(defn- thread [nm f] (doto (Thread. ^Runnable f ^String nm) (.start)))

(defn start! []
  (when (= :worker node/role)
    (let [sched  (atom schedule/initial)
          buffer (atom [])
          st     {:sched sched :mirrored (atom false)}]
      (reset! state st)
      (live/tap! #(swap! buffer conj (assoc % :at (System/currentTimeMillis))))
      (swap! state assoc
             :threads [(thread "sutra-worker-builds" #(build-lane! sched))
                       (thread "sutra-worker-batches" batch-lane!)]
             :mirror (thread "sutra-worker-mirror" #(mirror-lane! buffer))
             :buffer buffer)
      (println (str "[worker] ready — " (node/id) " runs builds (up to " (build-cap)
                    " at once) and the batches of manual executions")))))

(defn quiesce!
  []
  (when-let [{:keys [sched]} @state]
    (when-not (:stopping? @sched)
      (dispatch! sched nil {:kind :quiesce})
      (println "[worker] draining: nothing new is taken; what runs goes on to its end"))))

(defn busy?
  []
  (boolean (when-let [{:keys [sched threads]} @state]
             (or (seq (:running @sched)) (seq (bm/running-here))
                 (some #(.isAlive ^Thread %) threads)))))

(defn stop! []
  (when-let [{:keys [threads mirror mirrored buffer sched]} @state]
    (quiesce!)
    (doseq [^Thread t threads] (.join t))
    (while (seq (:running @sched)) (Thread/sleep 1000))
    (reset! mirrored true)
    (.join ^Thread mirror)
    (mirror! buffer)
    (live/tap! nil)
    (reset! state nil)
    (println "[worker] drained")))

(def module
  {:name        :worker
   :jobs        [{:name :worker :start start! :stop stop!}]
   :contributes {:cluster/listeners
                 {:build/requested (fn [_] (ring!))
                  :live/events     (fn [events] (when-not (= :worker node/role)
                                                  (live/ingest! events)))
                  :hot/patched     (fn [_] (when (= :worker node/role)
                                             (let [done (hot/follow!)]
                                               (println (str "[worker] followed hot patch: "
                                                             (count done) " namespace(s)")))))}}})
