(ns sutra.schedules.dispatch
  (:require [sutra.kit.refusal :as refusal]
            [sutra.store.access.scope :as ctx]
            [sutra.store.seat :as seat]
            [sutra.builds.api :as builds]
            [sutra.kit.effects :as effects]
            [sutra.schedules.poller :as poller]
            [sutra.schedules.schedules :as schedules]
            [sutra.schedules.run-setups :as run-setups]
            [sutra.schedules.triggers :as triggers]))

(set! *warn-on-reflection* true)

(defn- owner [sched-id path] [:trigger sched-id path])

(defn- trigger-due
  [store branch]
  (when-not (builds/any-build-active? store)
    (let [schema (seat/branch-schema (ctx/at store branch))
          now    (schedules/now store)]
      (->> (schedules/list-all store branch)
           (remove :paused)
           (filter :trigger)
           (keep (fn [s]
                   (let [sigs  (or (triggers/signature store branch schema s) {})
                         truth (into {}
                                     (map (fn [[path leaf]]
                                            [path
                                             (if (= :cadence (some-> (:kind leaf) name keyword))
                                               (triggers/time-leaf-due? leaf (:last-fired-at s) now)
                                               (let [sig (get sigs path)]
                                                 (and (some some? sig)
                                                      (builds/moved?
                                                       (builds/observe!
                                                        store branch (owner (:id s) path) sig)
                                                       sig))))]))
                                     (triggers/leaves (:trigger s)))]
                     (when (triggers/satisfied? (:trigger s) truth)
                       (assoc s ::sigs sigs
                              ::seen (into {}
                                           (map (fn [p]
                                                  [p (builds/get store branch (owner (:id s) p))]))
                                           (keys sigs)))))))
           doall))))

(defn- due-schedules
  [store branch]
  (->> (concat (schedules/list-due store branch) (trigger-due store branch))
       (reduce (fn [m s] (assoc m (:id s) s)) {})
       vals))

(defn build-request
  [schema {:keys [full parameters] :as schedule} run-setup run-id]
  (let [{:keys [only strategy from]} (builds/schedule-targets schema schedule)
        values (or (:parameters run-setup) parameters)]
    (cond-> (merge (select-keys schedule [:retry-count :retry-backoff-ms :abort-on-failure])
                   {:origin (str "schedule " (:name schedule)
                                 (when run-setup (str " · " (:value run-setup))))})
      (seq only)   (assoc :only-types only)
      strategy     (assoc :build-strategy strategy :build-from from)
      full         (assoc :full-refresh true)
      (seq values) (assoc :parameters values)
      run-setup    (assoc :run-setup (select-keys run-setup [:key :value]))
      run-id       (assoc :run-id run-id))))

(defn fire!
  ([store branch schedule] (fire! store branch schedule nil))
  ([store branch schedule at] (fire! store branch schedule at nil))
  ([store branch schedule at run-id] (fire! store branch schedule at run-id nil))
  ([store branch {:keys [id] :as schedule} at run-id run-setup]
   (schedules/assert-fireable! schedule)
   (let [schema (seat/branch-schema (ctx/at store branch))]
     (when (builds/read-spec (ctx/at store branch))
       (let [{:keys [counts run-id snapshot-id]}
             (builds/build! store branch (build-request schema schedule run-setup run-id)
                              :fire)]
         (schedules/mark-fired! store branch id run-id at)
         {:counts counts :run-id run-id :snapshot-id snapshot-id})))))

(defn start!
  [store branch s values]
  (schedules/assert-fireable! s)
  (when-not (builds/read-spec (ctx/at store branch))
    (refusal/refuse! :refusal.schedules/no-import-plan {:branch branch}))
  (let [rs (when (schedules/parallelized? s) (schedules/run-setup s values))]
    (when-let [p (:problem rs)]
      (refusal/refuse! :refusal.schedules/schedule-problem {:schedule (:name s)} {:problem p}))
    (when (and (seq values) (not (schedules/parallelized? s)))
      (refusal/refuse! :refusal.schedules/not-parallelized {:schedule (:name s)}))
    (some->> rs (run-setups/upsert! store branch (:id s)))
    (let [run-id (builds/detach! (str "schedule " (:name s)) #(fire! store branch s nil % rs))]
      (when rs (run-setups/mark-started! store branch (:id s) rs run-id))
      (cond-> {:run-id (str run-id)} rs (assoc :run-setup (select-keys rs [:key :value]))))))

(defn- start-named!
  [store branch {:keys [schedule parameters]}]
  (let [s (or (schedules/named store branch schedule)
              (refusal/refuse! :refusal.schedules/unknown-schedule
                               {:schedule schedule} {:name (name schedule) :branch branch}))]
    (start! store branch s parameters)))

(defn settlement
  [outcome]
  (case outcome
    :fired       {:stamp? false :advance? true}
    :failed      {:stamp? true  :advance? true}
    :interrupted {:stamp? false :advance? false}))

(defn outcome
  [t]
  (if (= :shutdown (effects/cancellation t)) :interrupted :failed))

(defn- fire-and-log! [store branch s]
  (let [at  (schedules/now store)
        out (try
              (let [{:keys [run-id counts]} (fire! store branch s at)]
                (println (str "[scheduler] fired '" (:name s) "' (" (:id s) ") on branch "
                              branch " → run " run-id " counts=" counts))
                :fired)
              (catch Throwable t
                (let [o (outcome t)]
                  (println (str "[scheduler] "
                                (if (= :interrupted o) "INTERRUPTED — its conductor stopped"
                                    "FIRE FAILED")
                                " '" (:name s) "' (" (:id s) "): " (.getMessage t)))
                  o)))
        {:keys [stamp? advance?]} (settlement out)]
    (when stamp?
      (try (schedules/mark-fired! store branch (:id s) nil at)
           (catch InterruptedException e (throw e))
           (catch Exception t2
             (println (str "[scheduler] RECOVERY STAMP FAILED for '" (:name s) "' ("
                           (:id s) ") — schedule may hot-loop: " (.getMessage t2))))))
    (when advance?
      (doseq [[path sig] (::sigs s)]
        (builds/advance! store branch (owner (:id s) path) (get (::seen s) path) sig)))))

(def module
  {:name :scheduler
   :provides {:watch/wake     #(poller/poke-job! :scheduler)
              :stream/wake    #(poller/poke-job! :stream-index)
              :schedule/start start-named!}
   :jobs [{:name  :scheduler :every :scheduler-ms
           :tick  (poller/each-due "scheduler" due-schedules fire-and-log!
                                   #(str (:name %) " (" (:id %) ")"))}]})
