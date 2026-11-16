(ns sutra.authoring.install
  (:require [sutra.kit.refusal :as refusal]
            [sutra.store.access.substrate :as sub]
            [sutra.store.access.scope :as scope]
            [sutra.catalog.kernel :as kernel]
            [sutra.store.read-only :as read-only]
            [sutra.store.world :as world]
            [sutra.authoring.install.findings.sql :as sql-findings]
            [sutra.authoring.install.plan :as plan]
            [sutra.authoring.install.diff :as diff]
            [sutra.authoring.install.live :as live]
            [sutra.store.kept :as kept]
            [sutra.actions.api :as actions]
            [sutra.types.finding :as finding]
            [sutra.types.plan :as cp]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]))

(set! *warn-on-reflection* true)

(defn- output-datasets
  [{:keys [output outputs]}]
  (for [o (cons output outputs)
        :when (= :dataset (some-> (:kind o) keyword))
        :let [d (:dataset o)]
        :when d]
    (let [nm (rid/->name d)]
      (when-not (re-matches #"[a-z][a-z0-9_]*" nm)
        (refusal/refuse! :refusal.authoring/output-not-a-dataset {:dataset nm}))
      (keyword nm))))

(defn with-decorated-datasets
  [schema declared-of known?]
  (let [new-ds (for [[k body] (:transforms schema)
                     :when (and (= :python (some-> (:language body) keyword)) (:code body))
                     d (output-datasets (declared-of k body))
                     :when (not (or (contains? (:datasets schema) d) (known? d)))]
                 [d (cond-> {} (:project body) (assoc :project (:project body)))])]
    (cond-> schema
      (seq new-ds) (update :datasets (fnil into {}) new-ds))))

(defn named-dataset-refs
  [current t]
  (let [idx (sch/index current)
        ref (fn [r]
              (let [d (:dataset r)
                    n (when (and (map? r) d) (sch/name-of idx (rid/->name d)))]
                (cond-> r n (assoc :dataset n))))]
    (cond-> t
      (:output t)         (update :output ref)
      (seq (:outputs t))  (update :outputs #(mapv ref %))
      (map? (:inputs t))  (update :inputs update-vals ref))))

(defn plan-schema
  [schema current {:keys [code only] :as opts}]
  (let [introspect  (fn [code body]
                      (update (kernel/introspect-code-transform code body)
                              0 #(named-dataset-refs current %)))
        declared-of (fn [k body]
                      (first (introspect code (assoc body :name k
                                                     :sutra.code/section :transforms))))
        known?      #(some? (rid/resolve-name->rid (sch/name-index current) #{:datasets} %))]
    (plan/plan-schema (cond-> schema
                        (or (nil? only) (only :datasets))
                        (with-decorated-datasets declared-of known?))
                      current
                      (assoc opts :introspect introspect))))

(defn planned-ontology
  [schema current opts]
  (let [plan (plan-schema schema current (assoc opts :only plan/ontology-phases))
        idx  (sch/index current)]
    (reduce (fn [acc {:keys [kind entity outcome args]}]
              (if-not (#{:created :updated} outcome)
                acc
                (let [{[section kw] :path
                       value :value} (kernel/step nil :create kind (assoc args :name entity))
                      at (or (sch/rid-of idx section kw) kw)]
                  (-> acc
                      (update-in [:schema section at] merge value)
                      (update-in [:changed section] (fnil conj #{}) (rid/->name kw))))))
            {:schema current :changed {}}
            plan)))

(defn- read-edit-surface
  [store current]
  (let [surface (delay (world/edit-surface store current))]
    {:types      (delay (:types @surface))
     :properties (delay (:properties @surface))
     :rows       (delay (sub/counts-by-type store current))}))

(defn on-index
  [store b]
  (fn [_what f] (f (live/index-of (kept/db store b)))))

(defn planned
  [opts what f]
  (if-let [p (:planned opts)]
    (p what f)
    (throw (ex-info "an install on a store plans through its branch's index (:planned)"
                    {:what what}))))

(defn plan-install
  [store schema current opts]
  (let [code (scope/code)
        plan (planned opts :plan #(plan-schema schema current (merge opts % {:code code})))
        edits (read-edit-surface store current)
        {:keys [found sql]}
        (planned opts :findings
                 (fn [ix] (let [d (diff/diff schema current plan edits ix)]
                            {:found (vec (plan/diff-findings d))
                             :sql   (vec (sql-findings/staged-sql-specs d))})))
        touched (plan/touched-names plan)
        judged  (cp/check!
                 (cp/of-findings
                  (for [f (plan/with-severity
                            (concat found
                                    (sql-findings/introspection-problems
                                     (keep :introspection-failure plan))
                                    (sql-findings/sql-compile-findings
                                     (sql-findings/sql-verdicts store (scope/code) current sql))
                                    (plan/already-staged-problems (actions/staged-creates store)
                                                                  plan)))]
                    (cond-> f
                      (and (finding/blocking? f) (plan/standing? touched f))
                      (assoc :severity :warn))))
                 {:code   :refusal.authoring/plan-problems
                  :data   {:sutra/gate :plan-problems :tally (plan/plan-tally plan)
                           :planned (plan/planned-rows plan)}
                  :params (fn [bs] {:headline (plan/refusal-headline bs)})})
        warns   (remove finding/blocking? (:findings judged))]
    {:calls    (vec (for [{:keys [action args]} plan :when action]
                      {:action action :args (actions/with-fresh-id action args)}))
     :tally    (cond-> (plan/plan-tally plan)
                 (seq warns) (assoc :warnings (mapv :message warns)))}))

(defn- asked
  [opts]
  (select-keys opts [:provenance :project]))

(defn preview-install
  [store schema current opts]
  (let [store                 (read-only/of store)
        {:keys [calls tally]} (plan-install store schema current opts)]
    {:tally tally
     :stage (when (seq calls) (actions/plan-actions store calls (asked opts)))}))

(defn check-install
  [store schema current opts]
  (let [{:keys [tally stage]} (preview-install store schema current opts)]
    (some-> stage actions/judge-stage!)
    tally))

(defn stage-schema!
  [store schema current opts]
  (let [{:keys [calls tally]} (plan-install store schema current opts)]
    (when (seq calls)
      (actions/stage-actions! store calls (asked opts)))
    tally))

(defn publish-schema!
  [store schema current opts callbacks]
  (let [{:keys [calls tally]} (plan-install store schema current opts)]
    {:tally  tally
     :commit (when (seq calls)
               (actions/publish-requests!
                (actions/request-on store (merge {:branch (:branch store) :schema nil}
                                                 (asked opts)))
                calls callbacks))}))
