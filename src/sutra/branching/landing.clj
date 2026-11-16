(ns sutra.branching.landing
  (:require [sutra.types.entities :as ents]
            [clojure.string :as str]
            [sutra.kit.lifecycle :as lc]
            [sutra.governance.api :as governance]
            [sutra.catalog.branch-policy :as policy]
            [sutra.catalog.project-scope :as project-scope]
            [sutra.store.world :as world]
            [sutra.types.advisory :as adv]
            [sutra.types.project-graph :as pgraph]
            [sutra.types.rid :as rid]
            [sutra.authoring.api :as authoring]))

(set! *warn-on-reflection* true)

(defn- blocked
  [{:keys [id proposal admin? actor source-exists? source-rev target-policy owners tasks
           owner? facts-error source-tip check-rows changed-paths approval-policy preview
           unit-tests source-policy locked check]}]
  (let [p   proposal
        pre (policy/merge-preview-of preview)]
    (or (when (nil? p) {:error (str "no such proposal: " id)})
        (when-not source-exists? (policy/merge-check-block :gone))
        (when-not (lc/allowed? policy/proposal-lifecycle :land (policy/proposal-state p))
          {:error (str "proposal is " (:status p) ", not open")})
        (policy/owner-refusal owners actor (:source-branch p) "land its proposals" admin?)
        (when-let [hit (project-scope/locked-hit (:theirs-schema check) locked (:diff pre))]
          {:error (:error hit) :locked true :project (:project hit)})
        (policy/landing-block
         p {:policy          target-policy
            :tasks           tasks
            :facts-error     facts-error
            :owner?          owner?
            :source-rev      source-rev
            :fresh           (policy/fresh-approvals p source-rev)
            :path-reviewers  (policy/approval-policy-reviewers (or approval-policy [])
                                                               (or changed-paths []))
            :source-tip      source-tip
            :check-status-fn (when check-rows (constantly check-rows))})
        (policy/ontology-block (:space source-policy) tasks)
        (when-let [failed (seq (filter #(pos? (:failed % 0)) unit-tests))]
          {:error      (str "unit tests fail — the proposal does not land while one does: "
                            (str/join "; " (for [{:keys [transform tests]} failed
                                                 t tests :when (not (:passed? t))]
                                             (str transform " · " (:name t) ": " (:detail t)))))
           :unit-tests (vec failed)})
        (when (= :conflicts (:status pre))
          {:error     "merge conflicts must be resolved before this proposal can land"
           :conflicts (:conflicts pre)})
        (policy/merge-check-block (:status pre)))))

(defn- merges-source?
  [source source-policy]
  (boolean (and source
                (not= source policy/default-branch-name)
                (not (:protected source-policy))
                (not (policy/ended? source-policy)))))

(defn decide
  [{:keys [proposal check preview source-rev source-policy check-rows] :as reads}]
  (if-let [why (blocked reads)]
    {:refused why}
    (let [{source :source-branch target :target-branch} proposal
          {:keys [heads]} check
          merged          (governance/heads-merge heads {})
          {:keys [error]} (governance/judged merged)]
      (if error
        {:refused {:error "the merged ontology is not valid" :reasons error}}
        {:land {:world          (governance/rebase-schema merged)
                :source         source
                :target         target
                :parent         {:branch source :rev source-rev}
                :fences         [[:branch-head source source-rev]]
                :merged-source? (merges-source? source source-policy)
                :diff           (:diff (policy/merge-preview-of preview))
                :advisories     (some-> check-rows adv/of-checks)}}))))

(defn commit!
  [store id proposal {:keys [world source target parent fences merged-source?]}]
  (let [{:keys [basis]} (authoring/install-world!
                         store target world
                         {:message (str "merge " source)
                          :parents [parent]
                          :records (governance/landing-records store id proposal merged-source?)
                          :fences  fences})]
    (world/reparent! store source basis)
    (governance/merge-committed! store id (:rev basis))
    {:rev (:rev basis)}))

(def ^:private buildable (ents/sections-in :buildable))

(defn build-scope
  [world diff]
  (let [names (fn [ids]
                (vec (sort (distinct
                            (for [section buildable
                                  [r spec] (get world section)
                                  :when (contains? ids (str r))
                                  :let [n (get spec (rid/name-field-of section))]
                                  :when n]
                              (name n))))))
        ids   (into #{} (map (comp str :rid)) diff)]
    {:modified (names ids)
     :affected (names (pgraph/downstream world ids))}))

(defn landed
  [proposal-now {:keys [rev]} {:keys [diff merged-source? advisories world]}]
  (cond-> (assoc proposal-now
                 :merge-commit      (str rev)
                 :diff              diff
                 :build-scope       (build-scope world diff)
                 :file-plane-status (policy/file-plane-status nil true)
                 :source-merged     merged-source?)
    (seq advisories) (assoc :advisories advisories)))
