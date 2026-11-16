(ns sutra.actions.landing
  (:require [sutra.kit.refusal :as refusal]
            [sutra.kit.anomaly :as anomaly]
            [sutra.parser.project-export :as pe]
            [sutra.catalog.branch-policy :as policy]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.project-scope :as project-scope]
            [sutra.store.edge :as edge]
            [sutra.store.access.scope :as ctx]
            [sutra.actions.staged :as staged-run]
            [sutra.governance.api :as governance]
            [sutra.actions.ports :as ports]))

(set! *warn-on-reflection* true)

(defn- builds [k]
  (fn [store & args] (apply (ports/builds-landing store k) store args)))

(def after-commit-callbacks
  {:reset-consumer!  (builds :reset-consumer!)
   :guard-reset!     (builds :guard-reset!)
   :reregister-link! (builds :reregister-link!)})

(def commit-callbacks
  (assoc after-commit-callbacks :run-fn (builds :run-fn)))

(defn commit-branch!
  [store seen branch]
  (staged-run/commit-staged! (ctx/at store branch) seen
                             (assoc commit-callbacks
                                    :guard (fn [store touched rows]
                                             (project-scope/require-schema-change!
                                              (grants/actor store) touched)
                                             (project-scope/require-unlocked!
                                              (:schema seen)
                                              (ports/locked-projects store branch (:schema seen))
                                              touched)
                                             (governance/write-guard store seen branch rows
                                                                     {:touched touched})))))

(defn branch-of [{branch :sutra/branch}] (or branch pe/trunk-branch))

(defn land-asked?
  [seen branch asked]
  (if (some? asked)
    (boolean asked)
    (boolean (:land-pushes (policy/policy-of (:governance seen) branch)))))

(defn land
  [store seen branch]
  (let [r (anomaly/catching #(commit-branch! store (edge/guarding seen) branch))]
    (cond
      (anomaly/refusal? r)
      {:land-refused (anomaly/message r) :land-status (anomaly/status r)}

      (policy/unclean? (:status r))
      (refusal/refuse! :refusal.actions/commit-raced
                       {:status (:status r) :conflicts (:conflicts r)})

      :else
      {:landed (select-keys r [:status :committed :snapshot-id])})))

(defn land?
  [{e :sutra/edge :as req} asked]
  (land-asked? e (branch-of req) asked))

(defn land!
  [{store :sutra/store e :sutra/edge :as req} resp]
  (update resp :body merge (land store e (branch-of req))))
