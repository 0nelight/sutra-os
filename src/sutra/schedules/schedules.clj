(ns sutra.schedules.schedules
  (:require [sutra.kit.coll :as coll]
            [sutra.llm.api :as llm]
            [sutra.builds.api :as builds]
            [sutra.kit.refusal :as refusal]
            [clojure.set]
            [clojure.string :as str]
            [sutra.schedules.facts :as sf]
            [sutra.store.access.scope :as ctx]
            [sutra.store.seat :as seat]
            [sutra.parser.params :as params]
            [sutra.types.opid :as opid]
            [sutra.schedules.triggers :as triggers]
            [sutra.store.control.cadence :as cadence]
            [sutra.store.control :as ctl]
            [sutra.foundation.cron :as cron]))

(set! *warn-on-reflection* true)

(def ^:private table "build_schedules")

(defn- row->schedule [row]
  (clojure.set/rename-keys row {:build-from :from}))

(defn list-all [store branch]
  (mapv row->schedule (cadence/list-all store branch table)))

(defn watching
  [store branch dataset]
  (let [k (keyword (name dataset))]
    (->> (list-all store branch)
         (remove :paused)
         (filter #(some #{k} (triggers/watched-parents %)))
         (map :name)
         sort
         vec)))

(defn around-write
  [store schema branch dataset write!]
  (let [sig    #(triggers/parent-signature (ctx/at store branch) schema dataset)
        before (sig)
        result (write!)]
    {:result result
     :triggered-by (when (not= before (sig)) (watching store branch dataset))}))

(defn get-one
  [store branch id]
  (some-> (cadence/get-one store branch table id) row->schedule))

(defn named
  [store branch nm]
  (some #(when (= (name nm) (:name %)) %) (list-all store branch)))

(defn- blank-target? [t]
  (or (nil? t) (and (string? t) (str/blank? t))))

(defn assert-targeted!
  [{:keys [name target]}]
  (when (blank-target? target)
    (refusal/refuse! :refusal.schedules/no-target {:schedule name :target target}))
  nil)

(defn parallelized?
  [schedule]
  (some? (:run-setup-key schedule)))

(defn mode-problem
  [{:keys [run-setup-key parameters cron every-ms trigger]}]
  (cond
    (nil? run-setup-key)                     nil
    (not (contains? parameters run-setup-key))
    (str ":run-setup-key " (pr-str run-setup-key) " is none of the schedule's :parameters "
         (pr-str (vec (sort (keys parameters)))))
    (or cron every-ms trigger)
    "a parallelized schedule runs only by its run setups (an action's :run-schedule rule, or `schedule run` with parameter values) — drop :cron, :every-ms and :trigger"))

(defn run-setup
  [{:keys [run-setup-key parameters]} values]
  (let [unknown (vec (sort (remove (set (keys parameters)) (keys values))))
        merged  (merge parameters values)
        v       (get merged run-setup-key)]
    (cond
      (seq unknown) {:problem (str "the schedule declares no parameter "
                                   (str/join ", " (map pr-str unknown)))}
      (nil? v)      {:problem (str "a run setup needs a value for the run-setup key "
                                   (pr-str run-setup-key))}
      :else         {:key run-setup-key :value v :parameters merged})))

(defn assert-armable!
  [{:keys [paused trigger] :as spec}]
  (when-not paused (assert-targeted! spec))
  (triggers/assert-evaluatable! trigger)
  (when-let [p (mode-problem spec)]
    (refusal/refuse! :refusal.schedules/schedule-problem {:schedule (:name spec)} {:problem p}))
  nil)

(defn assert-parameters-declared!
  [schema {:keys [name parameters]}]
  (when-let [u (seq (params/undeclared (:transforms schema) parameters))]
    (refusal/refuse! :refusal.schedules/undeclared-parameters
                     {:schedule name :undeclared (vec u)}
                     {:params (str/join ", " (map pr-str u))})))

(defn assert-fireable!
  [spec]
  (assert-targeted! spec)
  nil)

(defn upsert!
  [store branch {:keys [id name target cron every-ms trigger scope from paused full
                     parameters run-setup-key] :as spec}]
  (assert-armable! spec)
  (when (or trigger (seq parameters))
    (let [schema (seat/branch-schema (ctx/at store branch))]
      (assert-parameters-declared! schema spec)
      (when trigger
        (triggers/assert-watchable! schema trigger)
        (triggers/assert-listenable! schema trigger))))
  (let [options  (builds/build-options spec)
        id (or id (opid/name-id :schedule name))
        cad?     (= :cadence (:kind trigger))
        cron     (or cron (when cad? (:cron trigger)))
        every-ms (or every-ms (when cad? (:every-ms trigger)))]
    (row->schedule
     (cadence/upsert! store branch table
                      {:id id :name name :target (some-> target clojure.core/name)
                       :cron cron :every-ms every-ms :trigger trigger :scope scope
                       :build-from (some-> from clojure.core/name) :paused (boolean paused)
                       :full (boolean full)
                       :parameters (not-empty parameters) :run-setup-key run-setup-key
                       :retry-count (:retry-count options)
                       :retry-backoff-ms (:retry-backoff-ms options)
                       :abort-on-failure (:abort-on-failure options)}))))

(defn delete! [store branch id]
  (cadence/delete! store branch table id))

(defn mark-fired!
  ([store branch id run-id] (mark-fired! store branch id run-id nil))
  ([store branch id run-id at]
   (cadence/mark-fired! store branch table id :last-run-id run-id at)))

(defn now
  ^java.time.LocalDateTime [store]
  (java.time.LocalDateTime/ofInstant (ctl/instant store) (java.time.ZoneId/systemDefault)))

(defn list-due
  [store branch]
  (let [now (now store)]
    (concat
     (mapv row->schedule (cadence/list-due store branch table))
     (->> (list-all store branch)
          (filter (fn [s]
                    (and (not (:paused s))
                         (:cron s)
                         (not (and (:every-ms s) (pos? (:every-ms s))))
                         (cron/due? (:cron s) (:last-fired-at s) now
                                    (java.time.ZoneId/systemDefault)))))))))

(defn schedule-build-closure
  [schema import-plan schedule]
  (let [{:keys [only] :as targets} (builds/schedule-targets schema schedule)]
    (when (seq only)
      (:build (:scope (builds/scoped-import-plan schema import-plan only targets))))))

(defn enriched-schedules
  [store schema branch]
  (let [imports (:plan (builds/derive-plan-leniently schema))
        now    (now store)
        llm    (fn [s] (if-let [clos (schedule-build-closure schema imports s)]
                         (mapv :transform (llm/llm-transforms schema clos))
                         []))
        zone   (java.time.ZoneId/systemDefault)
        runs   (coll/index-by (comp str :id) (builds/list-recent (ctx/at store branch) 500 nil))
        enrich (fn [s]
                 (merge s
                        (select-keys (sf/schedule-fact runs s) [:when :latest-run])
                        {:next-fire-at   (some-> ^java.time.LocalDateTime
                                                 (cron/next-fire-of s now zone)
                                                 (.atZone zone) .toInstant str)
                         :llm-transforms (llm s)}))]
    (mapv enrich (list-all store branch))))
