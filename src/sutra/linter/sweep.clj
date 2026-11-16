(ns sutra.linter.sweep
  (:require [sutra.kit.attempt :as attempt]
            [sutra.builds.api :as builds]
            [sutra.lineage.api :as lineage]
            [sutra.linter.core :as core]
            [sutra.linter.rules :as rules]
            [sutra.schedules.api :as schedules]
            [sutra.store.access.scope :as ctx]
            [sutra.store.control :as ctl]
            [sutra.store.world :as world]))

(set! *warn-on-reflection* true)

(def ^:private month-seconds (* 30 24 60 60))

(def ^:private table :linter-recommendations)

(defn- closure-of
  [schema plan s]
  (try (keep #(some-> (get-in schema [:transforms % :transform-name]) name)
             (schedules/build-closure schema plan s))
       (catch Exception e
         (attempt/rethrow-fatal! e)
         (println (str "[linter] schedule " (:name s) ": " (ex-message e)))
         nil)))

(defn- schedules-off-main
  [store]
  (vec (for [{b :name} (world/branches store)
             :when (not= "main" (str b))
             s (schedules/list-all store b)
             :when (not (:paused s))]
         {:name (name (:name s)) :branch (str b)})))

(defn- read-facts
  [store schema branch answers]
  (let [plan (:plan (builds/derive-plan-leniently schema))]
    (core/sweep-facts
     {:now              (ctl/millis store)
      :lineage          (lineage/facts schema answers {})
      :schedules        (schedules/list-all store branch)
      :runs             (builds/list-recent (ctx/at store branch) 2000 nil month-seconds)
      :branch-schedules (schedules-off-main store)
      :closure-of       #(closure-of schema plan %)
      :targets-of       schedules/targets
      :watched-of       schedules/watched-parents
      :at-ms            #(.toEpochMilli (java.time.Instant/parse (str %)))})))

(defn sweep!
  [store schema branch answers]
  (let [{:keys [found failed]} (rules/run (read-facts store schema branch answers))
        rows (core/merged (ctl/list store table {:branch branch}) found (ctl/clock store))]
    (doseq [{:keys [rule error]} failed]
      (println (str "[linter] " (name rule) " failed: " error)))
    (ctl/commit! store (mapv (fn [r]
                               (let [row (assoc r :branch branch)]
                                 [:swap table (select-keys row [:rule :resource :branch])
                                  (constantly row)]))
                             rows))
    {:found    (count found)
     :resolved (count (filter :resolved-at rows))
     :failed   (mapv :rule failed)}))

(defn listed
  [store branch query]
  (core/view (ctl/list store table {:branch branch}) query (ctl/clock store)))

(defn- change!
  [store branch {:keys [rule resource]} sets]
  (ctl/commit! store [[:update table {:branch branch :rule rule :resource resource} sets]])
  {:rule rule :resource resource})

(defn snooze! [store branch body]
  (change! store branch body (core/snooze-sets body (ctl/clock store))))

(defn unsnooze! [store branch body]
  (change! store branch body core/unsnooze-sets))

(defn assign! [store branch {:keys [assignee] :as body}]
  (change! store branch body {:assignee assignee}))
