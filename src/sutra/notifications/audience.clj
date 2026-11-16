(ns sutra.notifications.audience
  (:require [clojure.string :as str]
            [sutra.access.api :as access]))

(set! *warn-on-reflection* true)

(def max-recipients
  {:template 500 :function 50})

(defn names
  [xs]
  (into [] (comp (mapcat #(if (sequential? %) % [%]))
                 (keep #(some-> % (cond-> (keyword? %) name) str str/trim not-empty))
                 (distinct))
        xs))

(defn expand
  [groups ns]
  (letfn [(members [g seen]
            (if (seen g)
              #{}
              (let [{u :users sub :groups} (get groups g)
                    seen (conj seen g)]
                (into (set u) (mapcat #(members % seen)) sub))))]
    (into [] (comp (mapcat #(if (contains? groups %) (sort (members % #{})) [%])) (distinct))
          ns)))

(defn usernames
  [store ns]
  (let [ns (names ns)]
    (if (empty? ns) [] (expand (access/group-directory store) ns))))
