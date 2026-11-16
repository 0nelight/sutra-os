(ns sutra.scenarios.apply
  (:require [sutra.kit.attempt :as attempt]
            [sutra.kit.noted :as noted]
            [sutra.kit.refusal :as refusal]
            [sutra.kit.anomaly :as anomaly]
            [sutra.catalog.grants :as grants]
            [sutra.store.access.run-log :as run-log]
            [sutra.store.access.scope :as ctx]
            [sutra.store.control :as ctl]
            [sutra.catalog.actions :as actions]
            [sutra.governance.api :as governance]
            [sutra.actions.api :as actions-api]
            [sutra.scenarios.state :as scenario-state]
            [sutra.store.seat :as seat]
            [sutra.types.branch-kinds :as branch-kinds]
            [sutra.store.world :as world]))

(set! *warn-on-reflection* true)

(defn- drop-staging!
  [store staging]
  (noted/or-else (str "dropping the staging branch " staging) nil
    (governance/drop-branch! store staging)))

(defn create-scenario-branch!
  [store {:keys [parent name]}]
  (world/fork! store parent name {:kind :scenario})
  (scenario-state/opened! store name)
  {:name name :parent parent})

(def applied-action-name
  :sys/scenario-applied)

(def scenario-marker-actions
  #{(name actions-api/seal-action-name) (name applied-action-name)})

(def max-scenario-actions
  50)

(defn scenario-applied?
  [store]
  (= :applied (scenario-state/state store (:branch store))))

(defn seal-scenario!
  [store]
  (when-not (branch-kinds/scenario-branch? (:branch store))
    (refusal/refuse! :refusal.scenario/seal-non-scenario {:branch (:branch store)}))
  (scenario-state/seal! store (:branch store))
  (actions-api/log-action! store actions-api/seal-action-name
                           {:sealed-at (ctl/stamp store)})
  true)

(defn scenario-definition
  [store schema]
  (let [rows (run-log/action-log store)
        replayable? (fn [nm] (and nm (actions/resolve-action schema (keyword nm))))]
    (reduce
     (fn [acc {n :seq :keys [action-name payload provenance]}]
       (cond
         (contains? scenario-marker-actions action-name) acc
         (replayable? action-name)
         (update acc :replayable conj
                 (cond-> {:seq n :action (keyword action-name) :args payload}
                   (:oracle provenance) (assoc :oracle (:oracle provenance))))
         :else
         (update acc :opaque conj {:seq n :action action-name})))
     {:replayable [] :opaque []}
     rows)))

(defn replay-actions!
  ([store schema definition] (replay-actions! store schema definition {}))
  ([store schema {:keys [replayable opaque]} {:keys [force? again?]}]
   (when (and (seq opaque) (not force?))
     (refusal/refuse! :refusal.scenario/opaque-entries {:opaque opaque}
                      {:opaque-count (count opaque)}))
   (when (> (count replayable) max-scenario-actions)
     (refusal/refuse! :refusal.scenario/too-many-actions
                      {:actions (count replayable) :max max-scenario-actions}))
   (let [guarded (delay (governance/data-guard store))]
     (mapv (fn [{n :seq :keys [action args oracle]}]
             (try
               {:seq n :action action
                :result
                (anomaly/unwrap!
                 (actions-api/run-action!
                  (actions-api/request-on @guarded
                                      (cond-> {:action action :args args :replaying? true
                                               :branch (:branch store) :schema schema}
                                        (and oracle (not again?)) (assoc :oracle oracle)))))}
               (catch Throwable e
                 (attempt/rethrow-fatal! e)
                 (refusal/refuse! :refusal.scenario/replay-failed {:seq n :action action}
                                  {:reason (.getMessage e)} e))))
           replayable))))

(defn- on-branch
  [store b]
  (let [store (ctx/at store b)]
    [store (seat/branch-schema store)]))

(defn- build-aside!
  [store parent staging definition sealed? opts]
  (create-scenario-branch! store {:parent parent :name staging})
  (try (let [[store-tmp schema-tmp] (on-branch store staging)
             a (replay-actions! store-tmp schema-tmp definition opts)]
         (when sealed? (seal-scenario! store-tmp))
         a)
       (catch Throwable e
         (drop-staging! store staging)
         (throw e))))

(defn- aside-name [branch what] (str branch "." what "-" (subs (str (random-uuid)) 0 8)))

(def ^:private rebuild-attempts 3)

(defn rebuild-scenario!
  ([store branch parent] (rebuild-scenario! store branch parent {}))
  ([store branch parent opts]
   (when-not (branch-kinds/scenario-branch? branch)
     (refusal/refuse! :refusal.scenario/not-a-scenario {:branch branch}))
   (loop [attempt 1]
     (let [[store0 schema0] (on-branch store branch)
           rev0       (:rev (world/head store0))
           omit       (set (:omit opts))
           definition (cond-> (scenario-definition store0 schema0)
                        (seq omit) (update :replayable
                                           (fn [rs] (filterv #(not (omit (:seq %))) rs))))
           sealed?    (actions-api/scenario-sealed? store0)
           staging    (aside-name branch "rebuild")
           applied    (build-aside! store parent staging definition sealed? opts)
           switched   (try (world/adopt! store branch staging rev0 {:kind :scenario}) :ok
                           (catch clojure.lang.ExceptionInfo e
                             (if (= :conflict (:store/refusal (ex-data e))) :moved (throw e)))
                           (finally (drop-staging! store staging)))]
       (cond
         (= :ok switched)
         {:branch branch :replayed (count applied) :sealed? (boolean sealed?)}

         (< attempt rebuild-attempts) (recur (inc attempt))

         :else
         (refusal/refuse! :refusal.scenario/rebuilt-while-active
                          {:branch branch :reason :conflict}))))))

(defn duplicate-scenario!
  ([store source parent
    new-branch] (duplicate-scenario! store source parent new-branch identity {}))
  ([store source parent new-branch edit-fn opts]
   (when (world/exists? store new-branch)
     (refusal/refuse! :refusal.scenario/branch-exists {:branch new-branch}))
   (let [edited  (edit-fn (apply scenario-definition (on-branch store source)))
         staging (aside-name new-branch "dup")
         applied (build-aside! store parent staging edited false opts)]
     (try (scenario-state/opened! store new-branch)
          (world/adopt! store new-branch staging nil {:kind :scenario})
          (finally (drop-staging! store staging)))
     {:branch new-branch :replayed (count applied)})))

(defn- held-apply
  [store target {:keys [replayable opaque]} {:keys [force? again?]}]
  (when (and (seq opaque) (not force?))
    (refusal/refuse! :refusal.scenario/opaque-entries {:opaque opaque}
                     {:opaque-count (count opaque)}))
  (when (> (count replayable) max-scenario-actions)
    (refusal/refuse! :refusal.scenario/too-many-actions
                     {:actions (count replayable) :max max-scenario-actions}))
  (let [store-t   (ctx/at store target)
        schema (seat/branch-schema store-t)]
    (reduce
     (fn [acc [i {n :seq :keys [action args oracle]}]]
       (try
         (let [req (actions-api/request-on store-t
                                           (cond-> {:action action :args args :replaying? true
                                                :branch target :schema schema}
                                         (and oracle (not again?)) (assoc :oracle oracle)))
               r   (actions-api/run-held req action args {:prior (:ops acc)})]
           (-> acc
               (actions-api/add-unit (:commit-opts r))
               (update :log conj
                       (run-log/action-entry action args
                                             (actions-api/edited-objects schema (:tx-vector r))
                                             (when oracle {:oracle oracle :replayed-from n})
                                             nil))
               (update :applied inc)))
         (catch Throwable e
           (attempt/rethrow-fatal! e)
           (refusal/refuse! :refusal.scenario/replay-failed-at-action
                            {:seq n :action action :index i} {:reason (.getMessage e)} e))))
     {:ops [] :log [] :records [] :applied 0}
     (map-indexed vector replayable))))

(defn apply-scenario!
  ([store branch target] (apply-scenario! store branch target {}))
  ([store branch target {:keys [again?] :as opts}]
   (when-not (branch-kinds/scenario-branch? branch)
     (refusal/refuse! :refusal.scenario/not-a-scenario {:branch branch}))
   (let [[store-sc schema-sc] (on-branch store branch)
         definition (scenario-definition store-sc schema-sc)]
     (when (and (scenario-applied? store-sc) (not again?))
       (refusal/refuse! :refusal.scenario/applied-already
                        {:branch branch
                        :remedy "pass --again to run them a second time on purpose"}))
     (let [{:keys [ops log records applied]} (try (held-apply store target definition opts)
                                                  (catch clojure.lang.ExceptionInfo e
                                                    (refusal/refuse!
                                                     :refusal.scenarios/does-not-apply
                                                     (assoc (ex-data e)
                                                            :branch branch :target target)
                                                     {:why (.getMessage e)}
                                                     e)))]
       (anomaly/unwrap!
        (actions-api/commit-run! (ctx/at store target)
                            {:ops     ops
                             :log     log
                             :records (cond-> records
                                        (not again?) (into (scenario-state/applying store branch)))
                             :actor    (grants/principal store)
                             :at-head? true}))
       (actions-api/log-action! store-sc applied-action-name
                           {:target target :actions applied
                            :applied-at (ctl/stamp store-sc)})
       {:branch branch :target target :applied applied :sealed? true}))))
