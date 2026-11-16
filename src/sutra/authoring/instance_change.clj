(ns sutra.authoring.instance-change
  (:require [clojure.string :as str]
            [sutra.governance.api :as governance]
            [sutra.catalog.branch-policy :as policy]
            [sutra.types.entities :as ents]
            [sutra.types.rid :as rid]
            [sutra.catalog.grants :as grants]
            [sutra.store.world :as world]
            [sutra.actions.api :as actions]
            [sutra.authoring.stage :as stage]
            [sutra.store.seat :as seat]))

(set! *warn-on-reflection* true)

(defn instance-pred
  [schema]
  (let [held (into {} (for [[s k] (ents/instance-keys schema) :when (set? k)]
                        [s
                         (into #{} (keep #(rid/->name (get-in schema [s % (rid/name-field-of s)])))
                               k)]))]
    (fn [author nm e]
      (let [s (ents/author->section author)]
        (or (ents/authored-instance? s (or e {}))
            (contains? (get held s) (rid/->name nm)))))))

(defn split
  [inst? {:keys [schema opts]}]
  (let [whole? (comp ents/instance-section? ents/author->section)
        cut    (fn [want s m]
                 (let [mine? (fn [nm e] (= want (boolean (inst? s nm e))))]
                   (if (map? m)
                     (into (empty m) (filter (fn [[k e]] (mine? k e))) m)
                     (into [] (filter #(mine? (first %) nil)) m))))
        part   (fn [want]
                 {:schema (into {} (for [[s m] schema
                                         :when (coll? m)
                                         :let [v (cut want s m)]
                                         :when (seq v)]
                                     [s v]))
                  :opts   (cond-> opts
                            (:strict-sections opts)
                            (update :strict-sections
                                    #(into #{} (remove (fn [s] (and (whole? s) (not want)))) %))
                            (:strict-projected opts)
                            (update :strict-projected
                                    (fn [d]
                                      (delay (into {}
                                                   (for [[s ns] (force d)
                                                         :let
                                                           [ns (into #{}
                                                                     (filter #(= want
                                                                                 (boolean
                                                                                  (inst? s % nil))))
                                                                     ns)]
                                                         :when (seq ns)]
                                                     [s ns]))))))})]
    [(part true) (part false)]))

(defn change?
  [{:keys [schema opts]}]
  (boolean (or (seq schema) (seq (force (:strict-projected opts))))))

(defn home
  [store branch]
  (or (world/instance-home (world/at store branch)) (str branch)))

(defn- protected?
  [seen h]
  (boolean (:protected (policy/policy-of (:governance seen) h))))

(defn- direct?
  [store seen h]
  (or (grants/all-projects? (grants/read-rights store))
      (not (protected? seen h))))

(defn in-the-way
  [store seen h]
  (when (protected? seen h)
    (seq (remove (comp ents/instance-section? :section)
                 (actions/list-staged-enriched (world/at store h))))))

(defn- in-the-way-refusal
  [h rows]
  (str h " already carries " (count rows) " staged change(s) no instance change can land"
       " beside: " (str/join ", " (for [r rows] (str (:verb r) " " (:type r) " #" (:seq r))))
       ". Landing commits " h "'s whole batch, and a protected " h " takes one only when"
       " every row of it is the instance's — these are a branch's to carry. Take them off"
       " " h " first (`changeset drop <seq>`), or commit them from a branch."))

(defn- take-back!
  [store h before]
  (doseq [{s :seq} (actions/list-staged-enriched (world/at store h))
          :when (not (contains? before s))]
    (let [store (world/at store h)]
      (actions/discard-staged! store (actions/staging-view store) s))))

(defn- land-or-take-back!
  [store seen h before]
  (let [r (try (actions/land store seen h)
               (catch clojure.lang.ExceptionInfo e (take-back! store h before) (throw e)))]
    (when (:land-refused r) (take-back! store h before))
    r))

(defn- staged-seqs [store h]
  (into #{} (map :seq) (actions/list-staged-enriched (world/at store h))))

(defn land!
  [store seen h part preview?]
  (let [blocked (in-the-way store seen h)]
    (if (and blocked (not preview?))
      {:branch h :land-refused (in-the-way-refusal h blocked) :land-status 409}
      (let [head-store    (world/at store h)
            before (staged-seqs store h)
            staged (stage/stage! {:store store :branch h :schema (seat/branch-schema head-store)}
                               {:schema (:schema part) :preview? (boolean preview?)}
                               (:opts part))
            land?  (and (not preview?) (seq (remove before (staged-seqs store h))))]
        (cond-> (assoc staged :branch h)
          blocked (assoc :land-blocked (in-the-way-refusal h blocked))
          land?   (merge (land-or-take-back! store seen h before)))))))

(defn request-fields
  [nm h part]
  {:kind :instance-change :projection (some-> nm name) :home h
   :schema (:schema part)
   :opts (-> (:opts part) (update :strict-projected force))})

(defn arrive!
  [store seen nm branch part preview? justification]
  (when (change? part)
    (let [h (home store branch)]
      {:instance
       (cond
         preview?                (land! store seen h part true)
         (direct? store seen h)  (land! store seen h part false)
         :else
           {:requested (governance/create-request! store (request-fields nm h part) justification)
            :branch h})})))

(defn apply-request!
  [store seen {:keys [home schema opts]}]
  (land! store seen home {:schema schema :opts opts} false))
