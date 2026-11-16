(ns sutra.builds.jobspecs
  (:require [sutra.types.plan :as cp]
            [clojure.string :as str]
            [sutra.builds.ledger :as ledger]
            [sutra.builds.plan.jobspec :as jobspec]
            [sutra.kit.refusal :as refusal]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.access.scope :as ctx]
            [sutra.store.memo :as memo]
            [sutra.store.seat :as seat]
            [sutra.store.world :as world]
            [sutra.types.schema :as sch]))

(set! *warn-on-reflection* true)

(def ^:private max-depth 32)
(defonce ^:private base-memo (atom {}))
(def ^:private bases-spec {::base {:cap 64}})

(def ^:private jobspec-keys [:transforms :llm-models])

(defn- base-world
  [store parent rev]
  (memo/through base-memo bases-spec ::base [(:db store) (str parent) rev]
                #(select-keys (world/world (ctx/at store parent) rev) jobspec-keys)))

(defn chain
  [store]
  (loop [b (str (:branch store)) acc []]
    (let [{:keys [base]} (when-not (= "main" b) (world/branch store b))
          parent    (:branch base)
          main?     (nil? parent)
          own       (select-keys (seat/branch-schema (ctx/at store b))
                                 (conj jobspec-keys :datasets))
          base-w    (when-not main? (base-world store parent (:rev base)))
          acc       (conj acc (assoc own :branch b
                                     :published (jobspec/published own base-w main?)))]
      (if (or main? (<= max-depth (count acc)))
        acc
        (recur (str parent) acc)))))

(defn resolved
  [store]
  (:jobspecs (jobspec/build-transforms (chain store) (seat/branch-schema store))))

(defn- say [{:keys [problem transform-rid branch ref branches by]}]
  (case problem
    :split   (str transform-rid " resolves to JobSpecs of " (str/join " and " branches))
    :missing (str transform-rid " falls back to " branch "'s JobSpec, which names "
                  (str/join " " ref) " — not on this branch")
    :claimed (str transform-rid " would also write " (str/join " " ref) ", which "
                  by " builds here")))

(defn build-schema
  [store schema]
  (if (= "main" (str (:branch store)))
    {:schema schema :problems []}
    (let [{:keys [transforms datasets problems]} (jobspec/build-transforms (chain store) schema)]
      {:schema   (sch/indexed (-> schema
                                  (assoc :transforms transforms)
                                  (update :datasets merge datasets)))
       :problems problems})))

(defn refuse-unrunnable!
  [store problems jobs]
  (let [job?     (set (map str jobs))
        in-build (filter (comp job? :transform-rid) problems)]
    (cp/check! (cp/of-findings (cp/problems :builds/jobspec-incompatible
                                            (map #(assoc % :message (say %)) in-build)))
               {:code   :refusal.builds/unrunnable-jobs
                :params (fn [bs] {:branch (str (:branch store))
                                  :detail (str/join "; " (map :message bs))})})
    nil))

(defn rollback-check
  [store ds ds-rid ds-name token]
  (let [{:keys [refuse warning]}
        (jobspec/rollback-verdict ds (get (resolved store) [:datasets (str ds-rid)])
                                  (ledger/jobspec-of-version store ds-rid (:branch store) token))
        horizon (dsets/horizon store ds-rid)]
    (when refuse (refusal/refuse! refuse {:dataset ds-name}))
    (when (some-> (dsets/snapshot-id token) (< horizon))
      (refusal/refuse! :refusal.builds/version-retained-away
                       {:dataset ds-name :version token :horizon horizon}))
    warning))
