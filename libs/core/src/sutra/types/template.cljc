(ns sutra.types.template
  (:require [sutra.kit.refusal :as refusal]
            [sutra.types.link-side :as ls]))

(defn search-around-sides
  [search-around]
  (cond (nil? search-around) [] (sequential? search-around) (vec search-around)
    :else [search-around]))

(defn traversal-anchor-problem
  [parameters pname]
  (let [{:keys [search-around of]} (get parameters pname)]
    (when (and search-around (not= :object (:type (get parameters of))))
      (refusal/message :refusal.compute/traversal-anchor-not-object
                       {:parameter pname :search-around search-around :of of}))))

(defn search-around-problem
  [parameters pname links-near]
  (let [{:keys [search-around of object-type]} (get parameters pname)
        start (:object-type (get parameters of))
        nm    #(some-> % name)]
    (when (and search-around start)
      (loop [cur (nm start), [l & more] (search-around-sides search-around), walked []]
        (if (nil? l)
          (when (not= cur (nm object-type))
            (str "template parameter " pname ": :search-around " (pr-str search-around)
                 " ends at " cur ", not at its :object-type " (nm object-type)))
          (if-let [side (ls/table-side (links-near cur) cur l)]
            (recur (nm (:far side)) more (conj walked l))
            (str "template parameter " pname ": " cur " has no link side named " (nm l)
                 " (the way " (pr-str (into [(keyword (nm of))] walked)) " stands at " cur
                 "; sides: "
                 (pr-str (sort (map :api-name (ls/table-sides (links-near cur) cur))))
                 ")")))))))

(defn parameter-problems
  [parameters]
  (vec (keep #(traversal-anchor-problem parameters %) (sort (keys parameters)))))
