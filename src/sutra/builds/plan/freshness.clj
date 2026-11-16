(ns sutra.builds.plan.freshness
  (:require [sutra.kit.coll :as coll]
            [sutra.store.access.datasets :as dsets]
            [sutra.kit.attempt :as attempt]
            [sutra.kit.noted :as noted]
            [sutra.media.api :as media]
            [sutra.catalog.grants :as grants]
            [sutra.store.access.scope :as ctx]
            [sutra.store.access.substrate :as sub]
            [sutra.catalog.backing :as backing]
            [sutra.catalog.outputs :as catalog-outputs]
            [sutra.catalog.transforms :as transforms]
            [sutra.catalog.lookup :as lookup]
            [sutra.catalog.llm-models :as llm-models]
            [sutra.types.naming :as naming]
            [sutra.store.control :as ctl]
            [sutra.builds.plan :as plan]
            [sutra.builds.plan.virtual :as virtual]
            [sutra.types.virtual :as vtypes]
            [sutra.kit.digest :as digest]
            [sutra.kit.tree :as tree]
            [sutra.builds.ledger :as ledger]
            [sutra.parser.params :as params]
            [sutra.store.storage.command :as cmd]
            [sutra.types.kinds :as kinds]
            [sutra.types.transform-io :as tio]
            [sutra.builds.plan.outputs :as outputs]))

(set! *warn-on-reflection* true)

(defn- canonical
  [v]
  (tree/canonical {:order pr-str :leaf #(if (set? %) (vec (sort-by pr-str %)) %)} v))

(defn- input-table
  [schema in]
  (case (kinds/physical (:kind in))
    :dataset-table   (some-> (lookup/rid-of schema :datasets (:dataset in)) naming/dataset-table)
    :transform-table (naming/trans-table (catalog-outputs/transform-input-key schema in))
    nil))

(defn- virtual-input
  [schema in]
  (when (= :dataset-table (kinds/physical (:kind in))) (virtual/spec-of schema (:dataset in))))

(defn- input-question
  [schema in]
  (case (kinds/freshness (:kind in))
    :clock [:clock (:object-type in)]
    :items [:items (:media-set in)]
    (if-let [v (virtual-input schema in)]
      [:probe v]
      (some->> (input-table schema in) (vector :content)))))

(defn- answer
  [store schema [q x]]
  (case q
    :clock   [:clock (sub/type-clock store x)]
    :probe   (virtual/signature (ctx/connect (grants/source-authority store)) schema x true)
    :content (dsets/content-signature store x)
    :items   (media/items-signature store schema x)
    nil      nil))

(defn- asker
  [store schema]
  (fn [q] (answer store schema q)))

(declare input-sigs output-sigs)

(defn- output-question [o] (some->> (:dataset o) naming/dataset-table (vector :content)))

(defn fingerprint-questions
  [schema t-kws]
  (into #{}
        (comp (keep #(get-in schema [:transforms %]))
              (mapcat (fn [t] (concat (vals (input-sigs identity schema t))
                                      (vals (output-sigs identity t)))))
              (remove #{nil ::unknown}))
        t-kws))

(defn read-world
  [ask questions]
  (into {} (map (fn [q] [q (noted/or-else "freshness" ::unknown (ask q))])) questions))

(defn world-asker
  [world]
  (fn [q]
    (cond (nil? q)            nil
          (contains? world q) (get world q)
          :else               (throw (ex-info "question not read before deciding"
                                              {:question q})))))

(defn- world-of
  [store schema t-kws]
  (world-asker (read-world (asker store schema) (fingerprint-questions schema t-kws))))

(defn inputs-of
  [t]
  (cond-> (or (:inputs t) {})
    (:input t) (assoc :input (:input t))))

(defn- volatile-job?
  [schema t]
  (boolean
   (or (empty? (inputs-of t))
       (seq (:external-systems t))
       (some (fn [in]
               (or (= :datasource (:kind in))
                   (some-> (virtual-input schema in) vtypes/detected? not)))
             (vals (inputs-of t))))))

(defn- output-decls
  [schema t]
  (mapv (fn [o]
          (let [[section k] (outputs/output-ref o)]
            (if section (get-in schema [section (or (lookup/rid-of schema section k) k)]) o)))
        (catalog-outputs/outputs-of t)))

(defn- signed-inputs
  [schema t]
  (into {} (for [[alias in] (inputs-of t)
                 :when (or (= :object-type (:kind in)) (input-table schema in))
                 :when (not (some-> (virtual-input schema in) vtypes/detected? not))]
             [alias in])))

(defn- input-sigs
  [ask schema t]
  (update-vals (signed-inputs schema t)
               #(attempt/or-answer ::unknown (ask (input-question schema %)))))

(defn- output-sigs
  [ask t]
  (into {} (for [o (tio/outputs-of t)
                 :let [d (:dataset o)]
                 :when d]
             [(str d) (attempt/or-answer ::unknown (ask (output-question o)))])))

(defn- fingerprint-of
  [schema t sigs outs]
  (when (and t (not (volatile-job? schema t))
             (not-any? #{::unknown} (concat (vals sigs) (vals outs))))
    (let [logic (params/substitute-transform (llm-models/resolved schema t))]
      (digest/sha256-hex (pr-str (canonical [logic (output-decls schema t) sigs outs]))))))

(defn- job-fingerprint
  [ask schema t-kw]
  (let [t (get-in schema [:transforms t-kw])]
    (when t (fingerprint-of schema t (input-sigs ask schema t) (output-sigs ask t)))))

(defn input-sigs-note
  [store schema noted t-kw]
  (when-not (get-in noted [:input-sigs t-kw])
    (when-let [t (get-in schema [:transforms t-kw])]
      [:input-sigs t-kw (input-sigs (asker store schema) schema t)])))

(defn stampable-jobs
  [schema jobs withheld {:keys [incomplete pending-observed]}]
  (let [by-str (coll/index-by str (keys (:transforms schema)))
        skip   (-> #{}
                   (into (keep #(some-> (:producer (backing/backing-of schema :object-type %)) str))
                         withheld)
                   (into (map str) incomplete)
                   (into (keep (fn [[c n]] (when (pos? n) (str c)))) pending-observed))]
    (for [j jobs
          :when (= (ledger/job-state :COMPLETED) (:status j))
          :let [t-kw (by-str (str (:transform_rid j)))]
          :when (and t-kw (not (skip (str t-kw))))]
      [(:job_rid j) t-kw])))

(defn- written-keys
  [schema jobs]
  (into #{}
        (mapcat (fn [[_ k]] (plan/output-keys schema k (get-in schema [:transforms k]))))
        jobs))

(defn signatures-as-read
  [schema t written seen at-end]
  (into {}
        (map (fn [[alias sig]]
               [alias (if (written (plan/input-key schema (get (inputs-of t) alias)))
                        sig
                        (get seen alias sig))]))
        at-end))

(defn stamp-fingerprints!
  [store run-id schema withheld {started :input-sigs :as job-result}]
  (when run-id
    (let [done    (stampable-jobs schema (ledger/jobs-of-run store run-id) withheld job-result)
          ask     (world-of store schema (map second done))
          written (written-keys schema done)]
      (cmd/execute!
       store
       (ledger/stamp-fingerprints
        run-id
        (keep (fn [[jr t-kw]]
                (let [t      (get-in schema [:transforms t-kw])
                      at-end (input-sigs ask schema t)
                      sigs   (signatures-as-read schema t written (get started t-kw at-end) at-end)]
                  (some->> (fingerprint-of schema t sigs (output-sigs ask t)) (vector jr))))
              done))))))

(defn- link-ends
  [{:keys [spec]}]
  (into #{} (remove nil?) (concat [(:from-type spec) (:to-type spec)]
                                  (mapcat (juxt :from-type :to-type) (:sources spec)))))

(defn- stale-closure
  [graph nodes stale quiet]
  (let [on-type    (group-by :key (filter #(#{:object :supplement} (:kind %)) nodes))
        runs?      (fn [s n] (or (nil? (:t-kw n)) (contains? s (:t-kw n))))
        type-runs? (fn [s ot] (some #(runs? s %) (get on-type ot)))]
    (loop [s (set stale)]
      (let [s' (-> s
                   (into (transforms/downstream-of graph (remove (some-fn nil? quiet) s)))
                   (into (for [[ot ns] on-type :when (type-runs? s ot), n ns] (:t-kw n)))
                   (into (for [n nodes
                               :when (and (= :link (:kind n))
                                          (some #(type-runs? s %) (link-ends n)))]
                           (:t-kw n)))
                   (disj nil))]
        (if (= s s') s (recur s'))))))

(defn- fresh-endpoints
  [store nodes runs?]
  (let [running-types (into #{} (comp (filter #(#{:object :supplement} (:kind %)))
                                      (filter runs?) (map :key))
                            nodes)
        fresh-objs    (into {} (comp (filter #(= :object (:kind %)))
                                     (remove runs?)
                                     (map (juxt :key :spec)))
                            nodes)
        needed        (into #{} (comp (filter #(and (= :link (:kind %)) (runs? %)))
                                      (mapcat link-ends)
                                      (filter fresh-objs)
                                      (remove running-types))
                            nodes)]
    (reduce (fn [acc ot]
              (let [rows (vec (sub/read-type store ot))
                    spec (get fresh-objs ot)
                    pk   (:primary-key (first (plan/entries spec)))]
                (-> acc
                    (assoc-in [:fresh-rows ot] rows)
                    (assoc-in [:fresh-pk-index ot]
                              (into {} (keep (fn [{:keys [id values]}]
                                               (when-let [v (get values pk)] [v id])))
                                    rows)))))
            {:fresh-rows {} :fresh-pk-index {}}
            needed)))

(defn job-status
  [{:keys [volatile? fingerprint]} prev]
  (cond volatile?                                             :volatile
        (and fingerprint (= fingerprint (:fingerprint prev))) :fresh
        :else                                                 :stale))

(defn- deferrable
  [nodes]
  (->> (filter :t-kw nodes)
       (group-by :t-kw)
       (keep (fn [[t-kw ns]] (when (every? #(= :producer (:kind %)) ns) t-kw)))
       set))

(defn- since
  [last t-kws]
  (into {} (map (fn [t-kw] [t-kw (:run_id (get last (str t-kw)))])) t-kws))

(defn fresh-plan
  [graph nodes standing last]
  (let [t-kws    (into #{} (keep :t-kw) nodes)
        status   (into {}
                       (map (fn [t-kw]
                              [t-kw (job-status (get standing t-kw) (get last (str t-kw)))])) t-kws)
        with     (fn [s] (into #{} (keep (fn [[t-kw status]] (when (= s status) t-kw))) status))
        volatile (with :volatile)
        pulled   (into #{} (filter t-kws) (stale-closure graph nodes volatile #{}))
        held     (into #{} (filter (deferrable nodes)) (remove volatile pulled))
        runs     (stale-closure graph nodes (into (with :stale) (remove held pulled)) volatile)
        deferred (into #{} (remove runs) held)]
    {:runs     runs
     :deferred deferred
     :skipped  (since last (remove (some-fn runs deferred) (with :fresh)))}))

(defn settle
  [deferred standing last]
  (let [fresh? #(= :fresh (job-status (get standing %) (get last (str %))))]
    {:runs    (into #{} (remove fresh?) deferred)
     :skipped (since last (filter fresh? deferred))}))

(defn- standing
  [store schema t-kws]
  (let [ask (world-of store schema t-kws)]
    (into {}
          (map (fn [t-kw]
                 [t-kw
                  {:volatile?
                   (boolean (some->> (get-in schema [:transforms t-kw]) (volatile-job? schema)))
                   :fingerprint (job-fingerprint ask schema t-kw)}]))
          t-kws)))

(defn- skip!
  [store run-id skipped]
  (when (seq skipped)
    (cmd/execute!
     store
     (for [[t-kw run] skipped]
       (ledger/skip-unstarted-jobs run-id (ctl/stamp store) [(ledger/job-rid run-id t-kw)]
                                   (str "fresh — inputs and logic unchanged since run " run))))
    (println (format "  ✓ %d job(s) fresh, not recomputed" (count skipped)))))

(defn freshness
  [store build-branch run-id schema graph nodes {:keys [full-refresh force]}]
  (if (or full-refresh force (nil? run-id))
    {:runs? (constantly true)}
    (let [last  (apply dissoc (ledger/last-fingerprints store build-branch)
                       (ledger/snapshot-requests store build-branch))
          now   (standing store schema (distinct (keep :t-kw nodes)))
          plan  (fresh-plan graph nodes now last)
          may   (into (:runs plan) (:deferred plan))
          runs? (fn [n] (or (nil? (:t-kw n)) (contains? may (:t-kw n))))]
      (skip! store run-id (:skipped plan))
      (merge {:runs? runs? :deferred (:deferred plan) :last last}
             (fresh-endpoints store nodes runs?)))))

(defn settle-level!
  [store schema run-id {:keys [deferred last]} level]
  (let [mine (into #{} (comp (keep :t-kw) (filter (or deferred #{}))) level)]
    (if (empty? mine)
      level
      (let [{:keys [skipped]} (settle mine (standing store schema mine) last)]
        (skip! store run-id skipped)
        (into [] (remove #(contains? skipped (:t-kw %))) level)))))
