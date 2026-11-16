(ns sutra.builds.run.setup
  (:require [sutra.parser.params :as params]))

(set! *warn-on-reflection* true)

(defn scopes?
  [{:keys [key]} t]
  (and (some? key) (contains? (params/declared t) key)))

(defn column
  [{:keys [key]}]
  key)

(defn stamp
  [{:keys [value] :as run-setup} rows]
  (let [col (column run-setup)]
    (mapv #(assoc % col value) rows)))

(defn share-key
  [{:keys [value]} pk-value]
  [value pk-value])

(defn in-share?
  [run-setup row]
  (= (:value run-setup) (get row (column run-setup))))

(defn stale
  [standing-ids written-ids]
  (vec (remove (set written-ids) standing-ids)))
