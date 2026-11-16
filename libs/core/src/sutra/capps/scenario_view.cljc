(ns sutra.capps.scenario-view
  (:require [clojure.string :as str]))

(defn- seg-of
  [x]
  (let [s (str (if (map? x) (:scenario x) x))]
    (when-not (str/blank? s) s)))

(defn- as-list
  [x]
  (cond
    (nil? x)        []
    (sequential? x) (vec x)
    :else           [x]))

(defn- named
  [known x]
  (let [seg (seg-of x)]
    {:scenario seg
     :name     (or (when (map? x) (:name x))
                   (some #(when (= seg (seg-of %)) (:name %)) known)
                   seg)}))

(defn manager-list
  [{:keys [static dynamic dynamic?]} known]
  (if-not (or dynamic? (seq (as-list static)))
    (mapv #(assoc (named known %) :removable? true) known)
    (let [fixed (into [] (comp (filter seg-of) (map #(named known %))) (as-list static))
          fixed? (into #{} (map :scenario) fixed)]
      (into (mapv #(assoc % :removable? false) fixed)
            (comp (filter seg-of)
                  (map #(named known %))
                  (remove (comp fixed? :scenario))
                  (map #(assoc % :removable? true)))
            (as-list dynamic)))))

(defn candidates
  [configured known]
  (if (some? configured)
    (into [] (comp (filter seg-of) (map #(named known %))) (as-list configured))
    (mapv #(named known %) known)))

(defn toggled
  [picked seg]
  (let [picked (into [] (keep seg-of) (as-list picked))]
    (if (some #{seg} picked)
      (vec (remove #{seg} picked))
      (conj picked seg))))

(defn- segs [x] (into [] (keep seg-of) (as-list x)))

(defn dynamic-after-create [dynamic seg] (conj (segs dynamic) seg))

(defn dynamic-after-drop [dynamic seg] (vec (remove #{seg} (segs dynamic))))

(defn dynamic-after-rename
  [dynamic old-seg new-seg]
  (mapv #(if (= old-seg %) new-seg %) (segs dynamic)))

(defn active-after-drop
  [active seg]
  (when-not (= seg (seg-of active)) active))

(defn summary-rows
  [{:keys [replayable opaque]}]
  (vec (sort-by :seq
                (concat (map #(assoc % :removable? true) replayable)
                        (map #(assoc % :removable? false) opaque)))))

(defn removed
  [undo row]
  (conj (vec undo) (select-keys row [:action :args])))

(defn undone
  [undo]
  [(peek (vec undo)) (if (seq undo) (pop (vec undo)) [])])
