(ns sutra.builds.run.monitor
  (:require [sutra.store.access.scope :as ctx]
            [sutra.builds.run.rows :as run-rows]
            [sutra.builds.run.lock :as lock]
            [sutra.builds.run.queue :as builds]
            [sutra.foundation.live-trace :as live]
            [sutra.foundation.scan-progress :as scan-progress]
            [sutra.store.storage.node :as node]
            [sutra.kit.effects :as effects]
            [sutra.builds.ports :as ports]
            [sutra.kit.noted :as noted]))

(set! *warn-on-reflection* true)

(defonce ^:private active (atom {}))

(defonce ^:private live-runs (atom #{}))

(defonce ^:private booted-ms (System/currentTimeMillis))

(defn- progress-of
  [m]
  (let [rt (Runtime/getRuntime) mb #(quot (long %) (* 1024 1024))]
    (cond-> {:at       (str (java.time.Instant/now))
             :phase    (:phase m)
             :current  (:current m)
             :phases   (:phases m)
             :steps    (:steps m)
             :done     (vec (take-last 40 (:done m)))
             :heap-mb  (mb (- (.totalMemory rt) (.freeMemory rt)))
             :heap-max-mb (mb (.maxMemory rt))}
      (seq (scan-progress/running)) (assoc :scans (scan-progress/running))
      (:profile m) (assoc :profile (:profile m)))))

(defn- renew-runs! []
  (let [store (ctx/system-store)]
    (when (seq @live-runs)
      (run-rows/renew-run-leases! store (node/id) (vec @live-runs) node/lease-ms))
    (doseq [[run-id a] @active]
      (run-rows/note-progress! store (node/id) run-id (progress-of @a)))
    (doseq [[run-id reason] (run-rows/cancel-requests store (node/id) (keys @active))]
      (some-> (get @active run-id) deref :work (effects/cancel! reason)))))

(defonce ^:private renewer
  (lock/lease-renewer "sutra-run-lease" "runs" #(renew-runs!)))

(defn run-began! [run-id]
  @renewer
  (swap! live-runs conj (str run-id)))

(defn run-ended! [run-id] (swap! live-runs disj (str run-id)))

(def ^:private stuck-after-ms
  (* 10 60 1000))

(defn- ticked
  [m]
  (assoc m :last-change (System/currentTimeMillis)))

(defn register!
  ([run-id total] (register! run-id total nil))
  ([run-id total {:keys [branch origin]}]
   @renewer
   (let [a (atom (ticked {:run-id  (str run-id)
                          :branch  (some-> branch str)
                          :origin  origin
                          :thread  (Thread/currentThread)
                          :started (System/currentTimeMillis)
                          :phase   :starting
                          :current nil
                          :done    []
                          :total   total
                          :work    (effects/work-token)}))]
     (swap! active assoc (str run-id) a)
     (live/emit! {:kind :build/start :run-id (str run-id) :total total})
     a)))

(defn note-profile!
  [progress report]
  (let [m (swap! progress assoc :profile report)]
    (run-rows/note-progress! (ctx/system-store) (node/id) (:run-id m) (progress-of m))))

(defn note-final!
  [run-id]
  (when-let [a (get @active (str run-id))]
    (run-rows/note-progress! (ctx/system-store) (node/id) (str run-id) (progress-of @a))))

(defn unregister! [run-id]
  (swap! active dissoc (str run-id))
  (live/emit! {:kind :build/end :run-id (str run-id)}))

(defn start-transform!
  [progress tname]
  (swap! progress #(ticked (assoc % :current (some-> tname name))))
  (live/emit! {:kind :build/transform-start :run-id (:run-id @progress)
               :transform (some-> tname name)}))

(defn finish-transform! [progress tname ms rows]
  (swap! progress #(-> % (update :done conj {:transform (some-> tname name)
                                             :ms ms :rows rows})
                       (assoc :current nil)
                       ticked))
  (live/emit! {:kind :build/transform-done :run-id (:run-id @progress)
               :transform (some-> tname name) :ms ms :rows rows}))

(defn work [progress] (some-> progress deref :work))

(defn- cancel-reason [progress] (some-> progress deref :work effects/reason))

(defn cancelled?
  [progress]
  (boolean (cancel-reason progress)))

(def ^:private cancel-text
  {:user   "build cancelled by user"
   :shutdown
   "build cancelled: its conductor stopped — the next one fires it again if it was scheduled"})

(defn check-cancelled!
  [progress where]
  (when-let [reason (cancel-reason progress)]
    (throw (ex-info (cancel-text reason (str "build cancelled (" (name reason) ")"))
                    (assoc where :cancelled true :reason reason)))))

(defn request-cancel!
  ([run-id] (request-cancel! run-id :user))
  ([run-id reason]
   (if-let [a (get @active (str run-id))]
     (do (effects/cancel! (:work @a) reason) true)
     false)))

(defn cancel-all!
  [reason]
  (into [] (filter #(request-cancel! % reason)) (keys @active)))

(defn- builds-elsewhere
  [store]
  (noted/or-else "builds elsewhere" [] (run-rows/running-runs-elsewhere store (node/id))))

(defn request-cancel-elsewhere!
  [store run-id]
  (let [id (str run-id)]
    (boolean
     (when (some #(= id (str (:id %))) (builds-elsewhere store))
       (let [marked? (pos? (run-rows/request-cancel! store id :user))]
         (when-let [broadcast (ports/cluster-broadcast store)]
           (broadcast :build/cancel {:run-id id}))
         marked?)))))

(defn any-build-active?
  [store]
  (or (seq (keys @active))
      (seq (builds-elsewhere store))
      (seq (builds/open store))))

(defn running-here
  []
  (vec (keys @active)))

(defn active-builds
  [store]
  (let [now (System/currentTimeMillis)]
    (->> (vals @active)
         (map (fn [a]
                (let [m     (deref a)
                      idle  (- now (or (:last-change m) (:started m) now))
                      alive (some-> ^Thread (:thread m) .isAlive)]
                  (-> (dissoc m :thread :work)
                      (assoc :cancel? (or (effects/reason (:work m)) false)
                             :idle-ms idle
                             :thread-alive? (boolean alive)
                             :stuck? (or (false? alive) (> idle stuck-after-ms)))))))
         (sort-by :started >)
         (into (for [{:keys [id branch owner]} (builds-elsewhere store)
                     :when (not (contains? @active (str id)))]
                 {:run-id (str id) :branch branch :owner owner :remote? true}))
         (into (for [{:keys [run-id branch origin owner queued-at]} (lock/queued store)]
                 {:run-id run-id :branch branch :origin origin :owner owner
                  :phase :queued :queued? true :queued-at (str queued-at)}))
         (into (for [{:keys [run-id branch origin status created-at]} (builds/open store)
                     :when (= "queued" status)]
                 {:run-id run-id :branch branch :origin origin :phase :requested
                  :queued? true :queued-at (str created-at)}))
         vec)))

(def ^:private zombie-reason
  "reaped: orphaned running run (process died mid-build)")

(def ^:private force-reason
  "force-reaped: hard-stopped (0-job zombie / stuck build)")

(defn- last-seen
  [{:keys [at phase current heap-mb heap-max-mb]}]
  (when at
    (str " — last seen " at " in " (some-> phase name)
         (when current (str " running " current))
         (when heap-mb (str ", heap " heap-mb "/" heap-max-mb " MB")))))

(defn reap-zombies!
  [store min-age-ms]
  (let [live     (into @live-runs (keys @active))
        uptime-s (quot (- (System/currentTimeMillis) booted-ms) 1000)
        stale    (run-rows/stale-running-runs store (quot (long min-age-ms) 1000)
                                              (+ uptime-s 5))]
    (into []
          (keep (fn [{:keys [id branch progress]}]
                  (let [reason (str zombie-reason (last-seen progress))]
                    (when (and (not (contains? live (str id)))
                               (pos? (run-rows/reap-expired-run! store branch id reason)))
                      {:id (str id) :branch branch :reason reason}))))
          stale)))

(defn force-fail!
  [store branch run-id]
  (request-cancel! run-id)
  (swap! active dissoc (str run-id))
  (lock/revoke! store run-id)
  (pos? (run-rows/reap-run! store branch run-id force-reason)))
