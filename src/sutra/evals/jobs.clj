(ns sutra.evals.jobs
  (:require [sutra.builds.api :as builds]
            [sutra.foundation.live-trace :as live]
            [sutra.kit.anomaly :as anomaly]
            [sutra.kit.lifecycle :as lc]
            [sutra.store.access.scope :as ctx]
            [sutra.store.control :as ctl]
            [sutra.store.control.lifecycle :as lcs]
            [sutra.store.storage.node :as node])
  (:import (java.util UUID)
           (java.util.concurrent ExecutorService Executors)))

(set! *warn-on-reflection* true)

(def ^:private kept 16)

(def job-table
  (lc/table {:name   :eval-job
             :states #{:running :done :failed}
             :final  #{:done :failed}
             :edges  {:end {:from #{:running} :to (fn [_ {:keys [status]}] (keyword status))}}}))

(defonce ^:private results (atom {}))

(defonce ^:private here (atom {}))

(defonce ^:private pool
  (delay (Executors/newFixedThreadPool 2)))

(defn- lease-until [store]
  (java.util.Date. (long (+ (inst-ms (ctl/clock store)) (long node/lease-ms)))))

(defn- renew! []
  (when-let [held (seq @here)]
    (ctl/commit! (ctx/system-store)
                 (vec (for [[id {:keys [branch progress]}] held]
                        (fn [store at]
                          (ctl/updating store :eval-jobs
                                        {:branch branch :id id :owner (node/id) :status "running"}
                                        {:lease-until (lease-until store) :progress progress}
                                        at)))))))

(defonce ^:private renewer
  (builds/lease-renewer "sutra-eval-lease" "eval-jobs" #(renew!)))

(defn- report! [id {:keys [total tick phase]}]
  (swap! here update id
         (fn [j] (when j
                   (update j :progress
                           (fn [p] (cond-> (or p {:done 0})
                                     total (assoc :total total)
                                     tick  (update :done + tick)
                                     phase (assoc :phase phase))))))))

(defn- keep-result! [id r]
  (swap! results (fn [m] (let [m (assoc m id r)]
                           (if (> (count m) kept) (dissoc m (first (sort (keys m)))) m)))))

(defn- run-id-of [r]
  (or (:run-id r) (some-> (:runs r) first :run-id)))

(defn- ended! [store branch id m]
  (try (ctl/commit! store [[:swap :eval-jobs {:branch branch :id id}
                            (lcs/stepping job-table :end {:status (:status m)}
                                          (fn [_ _] (assoc m :ended-at ctl/now
                                                           :lease-until nil)))]])
       (catch InterruptedException e (throw e))
       (catch Exception t
         (binding [*out* *err*]
           (println (str "eval job " id ": its end could not be stored — " (ex-message t)))))))

(defn start!
  ([store branch suite f] (start! store branch suite nil f))
  ([store branch suite detail f]
   @renewer
   (let [id  (str (UUID/randomUUID))
         row (cond-> {:id id :branch branch :suite (some-> suite str) :status "running"
                      :owner (node/id) :lease-until (lease-until store)}
               detail (assoc :detail detail))
         run (bound-fn []
               (try (let [r (f #(report! id %))]
                      (keep-result! id r)
                      (ended! store branch id
                              (cond-> {:status "done" :run-id (run-id-of r)}
                                (:detail r) (assoc :detail (merge detail (:detail r))))))
                    (catch Throwable t
                      (let [e (str (some-> (anomaly/category (ex-data t)) name (str ": "))
                                   (or (ex-message t) (str (class t))))]
                        (live/emit! {:kind :eval/failed :level :error :job id
                                     :suite (some-> suite str) :error (live/clip e)})
                        (ended! store branch id {:status "failed" :error e})))
                    (finally (swap! here dissoc id))))]
     (ctl/commit! store [#(ctl/swapping %1 :eval-jobs {:branch branch :id id} (constantly row) %2)])
     (swap! here assoc id {:branch branch})
     (.submit ^ExecutorService @pool ^Runnable run)
     (dissoc row :owner :lease-until))))

(defn- stored-result
  [store branch run-id suite]
  (when-let [run (ctl/get store :eval-runs {:branch branch :id run-id})]
    (let [metrics (ctl/list store :eval-results {:branch branch :run-id run-id})
          cases   (ctl/list store :eval-case-results {:branch branch :run-id run-id})]
      {:suite        suite
       :run-id       run-id
       :ref          (:ref run)
       :iterations   (:iterations run)
       :cases        (count cases)
       :summary      (:summary run)
       :metrics      (mapv (fn [m] (merge (:detail m)
                                          {:name (keyword (:metric m)) :value (:value m)
                                           :variance (:variance m) :status (:status m)}))
                           metrics)
       :case-results (mapv (fn [c] {:id (:case-id c) :passed (:passed c) :input (:input c)
                                    :expected (:expected c) :actual (:actual c)
                                    :metrics (:metrics c) :error (:error c)
                                    :trace (:trace c) :intermediates (:intermediates c)})
                           cases)})))

(defn- stored-outcome
  [store branch {:keys [suite run-id detail]}]
  (if-let [ids (seq (:run-ids detail))]
    {:experiment (:experiment detail)
     :runs       (vec (keep #(stored-result store branch % suite) ids))}
    (stored-result store branch run-id suite)))

(defn- held? [store {:keys [id lease-until]}]
  (or (contains? @here id)
      (not (builds/lapsed? lease-until (ctl/millis store)))))

(defn- as-job [store branch row]
  (let [id       (:id row)
        progress (or (get-in @here [id :progress]) (:progress row))]
    (-> (case (:status row)
          "done"    (assoc row :result (or (get @results id)
                                           (stored-outcome store branch row)))
          "running" (if (held? store row)
                      (cond-> row progress (assoc :progress progress))
                      (assoc row :status "lost"
                             :error (str "the process that ran this job ended before it did —"
                                         " no run was stored; start the suite again")))
          row)
        (dissoc :owner :lease-until))))

(defn job [store branch id]
  (some->> (ctl/get store :eval-jobs {:branch branch :id (str id)}) (as-job store branch)))

(defn latest [store branch suite]
  (some->> (ctl/list store :eval-jobs {:branch branch :suite (str suite)} {:limit 1})
           first
           (as-job store branch)))
