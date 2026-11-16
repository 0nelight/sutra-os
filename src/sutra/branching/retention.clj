(ns sutra.branching.retention
  (:require [sutra.store.control :as ctl]
            [sutra.notifications.api :as notifications]
            [sutra.access.api :as access]
            [sutra.store.access.scope :as ctx]
            [sutra.repos.api :as repos]
            [sutra.builds.api :as builds]
            [sutra.governance.api :as governance]
            [sutra.catalog.branch-policy :as policy]
            [sutra.store.seat :as seat]))

(set! *warn-on-reflection* true)

(defn- texts
  [kind branch]
  (case kind
    :branch-inactive
    {:heading (str "Branch " branch " is inactive")
     :message (str "The branch " branch " has seen no change for a while and is now marked "
                   "inactive. Without further changes it will be closed, and closing is "
                   "final. Change something on it or remove the label to keep it active.")}
    :branch-closed
    {:heading (str "Branch " branch " was closed")
     :message (str "The branch " branch " saw no change for too long and was closed. It "
                   "cannot be reopened; its data is deleted in seven days.")}))

(defn- notify-retention!
  [store {:keys [kind branch owners]}]
  (notifications/send! store policy/default-branch-name
                       (map #(access/username-of store %) owners)
                       (assoc (texts kind branch) :source :branch)))

(defn purge-branch-data!
  [store branch]
  (governance/drop-branch! store branch)
  (doseq [repo-rid (distinct (map :repo-rid (repos/repo-anchors store)))]
    (repos/forget-branch! store (ctx/repos) repo-rid branch)))

(defn sweep-deps
  [store]
  {:drop-branch-fn #(purge-branch-data! store %)
   :notify-fn      #(notify-retention! store %)})

(def ^:private dataset-retention-every-ms (* 60 60 1000))

(defonce ^:private dataset-retention-at (atom 0))

(defn- dataset-retention!
  [store]
  (let [now (ctl/millis store)]
    (when (and (= policy/default-branch-name (str (:branch store)))
               (> (- now (long @dataset-retention-at)) dataset-retention-every-ms))
      (reset! dataset-retention-at now)
      (let [{:keys [marked swept]} (builds/retain! store (seat/branch-schema store) {:apply? true})]
        (when (or (seq marked) (seq swept))
          (println (str "[retention] datasets: " (count marked) " marked, "
                        (count (remove :error swept)) " swept ("
                        (reduce + 0 (keep :rows swept)) " past rows deleted), "
                        (count (filter :error swept)) " failed")))))))

(def retention-tick!
  (seat/on-branch
   "retention"
   (fn [store _bh]
     (try (dataset-retention! store)
          (catch InterruptedException e (throw e))
          (catch Exception t (println (str "[retention] datasets: " (.getMessage t)))))
     (let [{:keys [closed marked-inactive reactivated purged purge-failed]}
           (governance/retention-sweep! store (sweep-deps store))
           runs (builds/prune-run-payloads! store {:apply? true})]
       (doseq [{:keys [branch error]} purge-failed]
         (println (str "[retention] purge failed: " branch " — " error)))
       (when (or (seq closed) (seq marked-inactive) (seq reactivated) (seq purged))
         (println (str "[retention] " (count marked-inactive) " → inactive, "
                       (count reactivated) " → active, "
                       (count closed) " closed, "
                       (count purged) " purged")))
       (when (pos? (:runs runs 0))
         (println (str "[retention] " (:runs runs) " run payloads cleared ("
                       (quot (:bytes runs) 1024) " kB)")))))))
