(ns sutra.schedules.facts
  (:require [clojure.string :as str]
            [sutra.types.rid :as rid]))

(set! *warn-on-reflection* true)

(def ^:private nm rid/->name)

(defn targets [s]
  (into #{} (comp (map str/trim) (remove str/blank?)
                  (map #(let [i (str/index-of % "/")] (if i (subs % (inc i)) %))))
        (str/split (str (:target s)) #",")))

(def ^:private cadence-words
  [[:cron #(str "cron " %)] [:every-ms #(str "every " % "ms")]])

(defn- cadence-word [m]
  (some (fn [[k f]] (some-> (get m k) f)) cadence-words))

(def ^:private trigger-words
  {"parent-updated" (fn [_ t] (str "when " (str/join ", " (map nm (flatten [(:of t)])))
                                   " updates"))
   "listener"       (fn [_ t] (str "listener " (nm (:of t))))
   "cadence"        (fn [_ t] (or (cadence-word t) "cadence"))
   "all-of"         (fn [w t] (str/join " and " (map w (:triggers t))))
   "any-of"         (fn [w t] (str/join " or " (map w (:triggers t))))})

(defn- trigger-tree-word [t]
  (let [k (some-> (:kind t) nm)]
    ((get trigger-words k (constantly k)) trigger-tree-word t)))

(defn trigger-word [s]
  (or (not-empty (str/join " · " (remove nil? [(cadence-word s)
                                               (when (map? (:trigger s))
                                                 (trigger-tree-word (:trigger s)))])))
      "manual"))

(defn- scope-word [sc] (if (keyword? sc) (name sc) (str sc)))

(defn schedule-fact
  [runs s]
  (let [r (get runs (some-> (:last-run-id s) str))]
    (cond-> {:schedule (nm (:name s))
             :paused   (boolean (:paused s))
             :when     (trigger-word s)
             :targets  (vec (sort (targets s)))}
      (:scope s)         (assoc :scope (scope-word (:scope s)))
      (:from s)          (assoc :from (str (:from s)))
      (:full s)          (assoc :full true)
      (:last-fired-at s) (assoc :last-fired (str (:last-fired-at s)))
      r                  (assoc :latest-run
                                (cond-> {:status (str (:status r)) :run-id (str (:id r))}
                                  (:finished-at r) (assoc :at (str (:finished-at r))))))))
