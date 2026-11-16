(ns sutra.builds.import
  (:require [sutra.kit.refusal :as refusal]
            [sutra.kit.decisions :as decisions]
            [sutra.kit.effects :as effects]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.transforms :as transforms]
            [sutra.projects.api :as projects]
            [sutra.foundation.profile.scope :as profile-scope]
            [sutra.store.access.scope :as ctx]
            [sutra.store.seat :as seat]
            [sutra.store.storage.command :as cmd]
            [sutra.store.storage.node :as node]
            [sutra.builds.import.env :as import-env]
            [sutra.builds.import.outcome :as outcome]
            [sutra.builds.import.phases :as phases]
            [sutra.builds.import.sources :as import-sources]
            [sutra.builds.jobspecs :as jobspecs]
            [sutra.builds.plan.jobspec :as jobspec]
            [sutra.builds.ledger :as ledger]
            [sutra.builds.ledger.facts :as facts]
            [sutra.builds.run :as runs]
            [sutra.builds.run.lane :as lane]
            [sutra.builds.run.lock :as lock]
            [sutra.builds.run.monitor :as bm]
            [sutra.builds.run.queue :as builds]
            [sutra.builds.run.rows :as run-rows]
            [sutra.parser.params :as params]
            [sutra.types.duration :as dur]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def max-retry-count
  4)

(defn build-options
  [{:keys [retry-count retry-backoff retry-backoff-ms abort-on-failure force]}]
  (let [n (cond
            (nil? retry-count) 0
            (and (integer? retry-count) (not (neg? retry-count))) (long retry-count)
            (and (string? retry-count) (re-matches #"\d+" (str/trim retry-count)))
            (Long/parseLong (str/trim retry-count))
            :else (refusal/refuse! :refusal.builds/retry-count-invalid {:retry-count retry-count}))]
    {:retry-count      (min n max-retry-count)
     :retry-backoff-ms (or (dur/->ms (if (some? retry-backoff-ms) retry-backoff-ms retry-backoff))
                           0)
     :abort-on-failure (boolean abort-on-failure)
     :force            (boolean force)}))

(defn- perform!
  [{:keys [store run-id]} effects]
  (let [effects (remove nil? effects)]
    (doseq [{:keys [io] :as fx} effects :when io]
      (case io
        :retract-outputs (ledger/retract-outputs! store (:v0 fx) (:datasets fx))
        :index-times     (projects/write-index-times! store (:types fx) run-id)))
    (when-let [records (seq (remove :io effects))]
      (cmd/execute! store records))))

(defn- run-build-phases!
  [{:keys [observe] :as env} state0]
  (reduce
   (fn [state {:keys [phase run]}]
     (observe {:event :phase/start :phase phase})
     (let [t0 (System/nanoTime)
           {state' :state :keys [effects observed]} (run env state)]
       (perform! env effects)
       (run! observe observed)
       (observe {:event :phase/end :phase phase :ms (import-env/ms-since t0)})
       state'))
   state0
   phases/phases))

(defn- run-phases!
  [{:keys [pa run-id] :as env} profile? state0]
  (if profile?
    (profile-scope/run-scoped {:label     (str "build " run-id)
                               :on-report #(bm/note-profile! pa %)}
                              #(run-build-phases! env state0))
    (run-build-phases! env state0)))

(def ^:private empty-build-state
  {:publish-failures [] :unpublished {:objects 0 :links 0}
   :stage-writes [] :backing-writes [] :rows {} :decls [] :index-report []})

(defn run-targets
  [{:keys [only-types only-transforms build-strategy origin full-refresh]}]
  (let [nm #(if (keyword? %) (name %) (str %))]
    (cond-> {:only (vec (sort (map nm (or (seq only-types) only-transforms))))}
      build-strategy (assoc :strategy build-strategy)
      origin         (assoc :origin origin)
      full-refresh   (assoc :full-refresh true))))

(defn- jobspec-ids
  [schema jobs]
  (into {} (for [t jobs] [t (jobspec/id schema (get-in schema [:transforms t]))])))

(defn- install-from-preset!*
  [store schema problems branch {:keys [only-types only-transforms build-strategy build-from
                                        trace? full-refresh origin run-id run-setup profile?]
                                 :as preset-block}]
  (let [options (cond-> (build-options preset-block) run-setup (assoc :run-setup run-setup))
        {:keys [datasources import node-scope scope jobs refusal] :as build-plan}
        (decisions/decide :build/plan
                          schema {:only only-types :transforms only-transforms
                                  :strategy build-strategy :from build-from})
        plan    {:datasources datasources :import import}
        run-id  (or run-id (java.util.UUID/randomUUID))
        targets (run-targets preset-block)
        asked   (System/nanoTime)]
    (when (and (empty? datasources) (empty? (:datasets schema)))
      (refusal/refuse! :refusal.builds/nothing-to-build {:keys (keys preset-block)}))
    (when-let [{:keys [code data params]} refusal]
      (refusal/refuse! code data params))
    (jobspecs/refuse-unrunnable! store problems jobs)
    (lock/with-claim store
      (merge (select-keys build-plan [:reads :writes])
             {:run-id run-id :branch branch :origin origin
              :targets targets})
      (fn [store]
        (runs/with-run!
          [[store _] (ctx/at store branch) {:id run-id :kind :import :inputs plan :options options
                                            :targets targets}]
          (do
            (cmd/execute! store (ledger/seed-jobs {:run-id         run-id
                                                   :branch         (:branch store)
                                                   :transform-rids jobs
                                                   :jobspec-ids    (jobspec-ids schema jobs)}))
            (let [env    (import-env/build-env store schema branch run-id
                                    {:plan plan :datasources datasources :node-scope node-scope
                                     :scope scope :origin origin
                                     :options (assoc options
                                                     :full-refresh (boolean full-refresh))})
                  queued (import-env/ms-since asked)
                  behind (lock/waited-behind run-id)]
              ((:observe env) {:event :phase/end :phase :queued :ms queued})
              (binding [effects/*work* (:work env)]
                (cond-> (outcome/build-result env
                                      (run-phases! env profile?
                                                   (assoc empty-build-state
                                                          :noted (facts/empty-facts trace?))))
                  behind (assoc-in [:summary :queued] {:ms queued :behind (vec behind)}))))))))))

(defn- note-failed-start!
  [store branch run-id ^Throwable t]
  (when run-id
    (try (run-rows/failed-before-start! store branch
                                        {:id run-id :kind :import
                                         :error-message (or (ex-message t) (str (class t)))})
         (catch InterruptedException e (throw e))
         (catch Exception e
           (println (str "[build] " run-id " failed and its row was not written: "
                         (ex-message e)))))))

(defn run-build!
  [store context preset-block]
  (let [branch (:branch context)
        store  (-> store
                   (ctx/in-context (dissoc context :authority))
                   (ctx/as-system)
                   (lane/on-lane :build))]
    (try
      (let [{:keys [schema problems]} (jobspecs/build-schema store (seat/branch-schema store))]
        (install-from-preset!* store
                               (update schema :transforms params/bind (:parameters preset-block))
                               problems branch preset-block))
      (catch Throwable t
        (when-not (:handed-back (ex-data t))
          (note-failed-start! store branch (:run-id preset-block) t))
        (throw t)))))

(defn- build-on-route
  [store branch preset-block mode]
  (case (builds/route node/role mode)
    :inline      (run-build! store (builds/request-context store branch) preset-block)
    :thread      {:run-id (builds/detach! (str "build " (:origin preset-block))
                                          #(run-build! store
                                                       (builds/request-context store branch)
                                                       (assoc preset-block :run-id %)))}
    :queue       {:run-id (builds/submit! store branch preset-block)}
    :queue-await (builds/await! store (builds/submit! store branch preset-block))))

(defn build!
  [store branch preset-block mode]
  (build-on-route store branch
                  (builds/crossed
                   (cond-> preset-block
                     (not (:origin preset-block))
                     (assoc :origin (str "user " (grants/current-user-id
                                                  (grants/actor store))))))
                  mode))

(defn install-from-preset!
  [store branch preset-block]
  (build! store branch preset-block :await))

(defn retry-set
  [schema jobs-rows]
  (let [ts        (:transforms schema)
        all       (set (keys ts))
        succeeded (into #{}
                        (comp (filter #(= (ledger/job-state :COMPLETED)
                                          (ledger/read-job-state (:status %))))
                              (keep :transform_rid))
                        jobs-rows)
        seed      (into #{} (remove #(succeeded (str %))) all)]
    (transforms/downstream-of (transforms/build-graph schema) seed)))

(defn retry-run!
  [store branch failed-run-id]
  (let [branch-store    (ctx/at store branch)
        failed (runs/get-run branch-store failed-run-id)]
    (when-not failed
      (refusal/refuse! :refusal.builds/retry-unknown-run {:run-id failed-run-id}))
    (when (and (:branch failed) (not= (str (:branch failed)) (str (or branch "main"))))
      (refusal/refuse! :refusal.builds/retry-other-branch
                       {:run-id failed-run-id :branch (:branch failed)}))
    (when-not (= "failed" (:status failed))
      (refusal/refuse! :refusal.builds/retry-run-not-failed
                       {:run-id failed-run-id :status (:status failed)}))
    (if (import-sources/read-spec branch-store)
      (let [rs     (retry-set (seat/branch-schema branch-store)
                              (ledger/jobs-of-run branch-store failed-run-id))
            result (install-from-preset!
                    store branch
                    (merge (select-keys failed [:retry-count :retry-backoff-ms :abort-on-failure])
                           {:only-transforms rs
                            :origin (str "retry of " failed-run-id)}))]
        (assoc result :retry-of failed-run-id
               :retried (mapv str rs)))
      (refusal/refuse! :refusal.builds/retry-no-import-plan {:run-id failed-run-id}))))
