(ns sutra.linter.core
  (:require [sutra.kit.coll :as coll]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def ^:private day-ms (* 24 60 60 1000))

(defn sweep-facts
  [{:keys [now lineage schedules runs branch-schedules closure-of targets-of watched-of at-ms]}]
  (let [timed (fn [fact] (cond-> fact (:at fact) (assoc :at-ms (at-ms (:at fact)))))]
    {:now        now
     :known      (set (map #(second (str/split (str %) #"/" 2)) (keys (:nodes lineage))))
     :transforms (mapv #(-> % (update :last-success timed) (update :last-failure timed))
                       (:transforms lineage))
     :schedules  (mapv (fn [s]
                         (assoc s
                                :name    (name (:name s))
                                :builds  (vec (closure-of s))
                                :targets (mapv name (targets-of s))
                                :watched (mapv name (watched-of s))))
                       schedules)
     :runs       (vec runs)
     :branch-schedules (vec branch-schedules)}))

(defn- key-of [{:keys [rule resource]}] [(name rule) resource])

(defn merged
  [existing found now]
  (let [old  (coll/index-by key-of existing)
        hits (coll/index-by key-of found)]
    (concat
     (for [[k {:keys [mode project detail]}] hits
           :let [before (get old k)
                 open?  (and before (nil? (:resolved-at before)))]]
       (merge (when open? (select-keys before [:snoozed-until :snooze-reason :assignee]))
              {:rule (first k) :resource (second k) :mode (name mode) :project project
               :detail detail :last-seen now :resolved-at nil
               :first-seen (if open? (:first-seen before) now)}))
     (for [[k before] old
           :when (and (not (hits k)) (nil? (:resolved-at before)))]
       (assoc before :resolved-at now)))))

(defn state-of
  [{:keys [snoozed-until]} now]
  (cond (nil? snoozed-until)                     "default"
        (> (inst-ms snoozed-until) (inst-ms now)) "snoozed"
        :else                                    "snooze-expired"))

(defn- matches? [{:keys [mode rule project assignee]} row]
  (and (or (nil? mode) (= (name mode) (:mode row)))
       (or (nil? rule) (= (name rule) (:rule row)))
       (or (nil? project) (= project (:project row)))
       (or (nil? assignee) (= assignee (:assignee row)))))

(defn view
  [rows {:keys [snoozed] :as query} now]
  (let [open (->> rows
                  (remove :resolved-at)
                  (map #(assoc % :state (state-of % now)))
                  (filter #(or snoozed (not= "snoozed" (:state %))))
                  (filter #(matches? query %))
                  (sort-by (juxt :mode :rule :resource))
                  vec)]
    {:recommendations open
     :by-rule         (frequencies (map :rule open))
     :resolved        (count (filter :resolved-at rows))}))

(defn snooze-sets [{:keys [days reason]} now]
  {:snooze-reason reason
   :snoozed-until (java.util.Date. (long (+ (inst-ms now) (* (long days) day-ms))))})

(def unsnooze-sets {:snoozed-until nil :snooze-reason nil})
