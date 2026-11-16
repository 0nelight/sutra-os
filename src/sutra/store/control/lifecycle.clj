(ns sutra.store.control.lifecycle
  (:require [sutra.kit.lifecycle :as lc]
            [sutra.store.control :as ctl]
            [sutra.types.plan :as plan]))

(set! *warn-on-reflection* true)

(defn status-state
  [row]
  (some-> (:status row) keyword))

(defn stepping
  ([table event sets] (stepping table event nil sets))
  ([table event input sets]
   (fn [old]
     (when-let [to (lc/next-state table event (status-state old) old input)]
       (assoc (merge old (sets old to)) :status (name to))))))

(defn transitioning
  [t where table event {:keys [id state-of input sets]}]
  (fn [store at]
    (ctl/swapping store t where
                  (fn [old]
                    (let [to (lc/decide table (or id where) event (state-of old) old input)]
                      (merge old (sets old to))))
                  at)))

(defn transition-plan
  [store t where table event {:keys [id state-of input] :as opts}]
  (let [old (ctl/get store t where)]
    (plan/transition table (or id where) event (state-of old) old input
                     (fn [_] [:plan t (transitioning t where table event opts)]))))
