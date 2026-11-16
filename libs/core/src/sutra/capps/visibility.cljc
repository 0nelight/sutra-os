(ns sutra.capps.visibility
  (:require [sutra.capps.variables :refer [deref* eval-pred]]))

(defn truthy?
  [x default]
  (cond
    (nil? x)   default
    #?@(:cljs [(satisfies? IDeref x) (boolean @x)]
        :clj  [(instance? clojure.lang.IDeref x) (boolean @x)])
    (fn? x)    (boolean (x))
    (and (vector? x) (or (keyword? (first x)) (string? (first x)) (symbol? (first x))))
    (boolean (eval-pred x))
    (coll? x)  (boolean (seq x))
    :else      (boolean x)))

(defn visible?
  [node]
  (and (not (:hidden? node))
       (truthy? (:when node) (not (contains? node :when)))))

(defn pane-visible?
  [pane viewer]
  (or (nil? viewer) (nil? (:visibility pane))
      (let [{:keys [roles profiles] when* :when} (:visibility pane)]
        (and (or (empty? roles)    (contains? (set roles) (:role viewer)))
             (or (empty? profiles) (contains? (set profiles) (deref* (:profile viewer))))
             (truthy? when* true)))))
