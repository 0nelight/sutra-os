(ns sutra.automation.executions
  (:require [sutra.builds.api :as builds]
            [sutra.kit.lifecycle :as lc]
            [sutra.store.control :as ctl]
            [sutra.store.control.lifecycle :as lcs]))

(set! *warn-on-reflection* true)

(def claim-ms
  (* 3 60 1000))

(def execution-table
  (lc/table
   {:name   :automation-execution
    :states #{:running :done :cancelled}
    :final  #{:done :cancelled}
    :edges  {:claim   {:from  #{:running} :to :running
                       :guard (fn [r {:keys [now]}]
                                (when-not (builds/free? r now) "another worker's claim holds"))}
             :advance {:from #{:running}
                       :to   (fn [_ {:keys [left]}] (if (seq left) :running :done))}
             :cancel  {:from #{:running} :to :cancelled}}}))

(defn create!
  [store branch {:keys [id automation-id pending batch-size parallelism]}]
  (ctl/insert! store :automation-executions
               {:id id :branch branch :automation-id automation-id :pending (vec pending)
                :total (count pending) :batches 0 :batch-size batch-size
                :parallelism parallelism :status (if (seq pending) "running" "done")}))

(defn running
  [store branch]
  (sort-by :created-at (ctl/list store :automation-executions
                                 {:branch branch :status "running"})))

(defn of-automation
  [store branch automation-id limit]
  (ctl/list store :automation-executions {:branch branch :automation-id automation-id}
            {:limit (or limit 20)}))

(defn claim!
  [store branch id]
  (let [now (ctl/millis store)]
    (ctl/swap! store :automation-executions {:id id :branch branch}
               (lcs/stepping execution-table :claim {:now now}
                             (fn [old _] (builds/claimed old (str (random-uuid)) now claim-ms))))))

(defn renew!
  [store branch id claim-id]
  (ctl/swap! store :automation-executions {:id id :branch branch}
             #(builds/renewed % claim-id (ctl/millis store) claim-ms)))

(defn next-batch
  [{:keys [pending batch-size]}]
  (vec (take (max 1 (or batch-size 1)) pending)))

(defn advance!
  [store branch id claim-id n]
  (ctl/swap! store :automation-executions {:id id :branch branch}
             (fn [old]
               (when (builds/mine? old claim-id)
                 (let [left (vec (drop n (:pending old)))
                       to   (lc/next-state execution-table :advance (lcs/status-state old) old
                                           {:left left})]
                   (cond-> (assoc (builds/released old) :pending left
                                  :batches (inc (or (:batches old) 0)))
                     to (assoc :status (name to))))))))

(defn cancel!
  [store branch id]
  (ctl/swap! store :automation-executions {:id id :branch branch}
             (lcs/stepping execution-table :cancel (fn [_ _] nil))))
