(ns sutra-ui.state.machine-testing
  (:require [sutra-ui.state.machine :as machine]))

(defn step
  [db event]
  (let [e (machine/effects db event)]
    [(if (contains? e :db) (:db e) db) (vec (:fx e))]))

(defn run
  [db event]
  (let [[db fx] (step db event)]
    (reduce (fn [db [k e]] (if (= :dispatch k) (run db e) db)) db fx)))

(defn requests
  [[_ fx]]
  (keep (fn [[k a]]
          (cond
            (= :call k) a
            (and (= :dispatch k) (= :rf/request (first a)))
            (let [[_ req reply] a]
              {:call (:ask req) :ok [:rf/answered req reply] :err [:rf/failed req reply]})))
        fx))

(defn calls
  [r]
  (map :call (requests r)))
