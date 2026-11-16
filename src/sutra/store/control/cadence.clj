(ns sutra.store.control.cadence
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.store.control :as ctl]))

(set! *warn-on-reflection* true)

(def cadence-table
  {"build_schedules" :build-schedules
   "automations"     :automations
   "health_checks"   :health-checks
   "eval_suites"     :eval-suites})

(defn cadence-list-due [store branch t]
  (let [now-ms (ctl/millis store)]
    (filterv (fn [{:keys [every-ms last-fired-at created-at]}]
               (and every-ms (pos? every-ms)
                    (let [since (or last-fired-at created-at)]
                      (or (nil? since) (>= (- now-ms (.getTime ^java.util.Date since)) every-ms)))))
             (ctl/list store t {:branch branch :paused false} {:order [[:created-at :asc]]}))))

(defn cadence-mark-fired!
  ([store branch t id run-col run-val] (cadence-mark-fired! store branch t id run-col run-val nil))
  ([store branch t id run-col run-val at]
   (ctl/update! store t {:branch branch :id id} {:last-fired-at (or at ctl/now) run-col run-val})
   nil))

(defn- t [table]
  (or (get cadence-table table)
      (anomaly/raise! :fault (str "no control-row table for cadence table " (pr-str table))
                             {:table table :known (keys cadence-table)})))

(defn list-all [store branch table]
  (ctl/list store (t table) {:branch branch}))

(defn get-one [store branch table id]
  (ctl/get store (t table) {:branch branch :id id}))

(defn delete! [store branch table id]
  (ctl/delete! store (t table) {:branch branch :id id})
  {:deleted id})

(defn list-due [store branch table]
  (cadence-list-due store branch (t table)))

(defn mark-fired!
  ([store branch table id run-col run-val] (mark-fired! store branch table id run-col run-val nil))
  ([store branch table id run-col run-val at]
   (cadence-mark-fired! store branch (t table) id run-col run-val at)))

(defn upsert! [store branch table row]
  (ctl/upsert! store (t table) (assoc row :branch branch)))

(defn list-due-on [store branches table]
  (if (empty? branches)
    []
    (cadence-list-due store (set branches) (t table))))
