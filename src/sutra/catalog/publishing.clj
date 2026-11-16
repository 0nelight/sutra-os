(ns sutra.catalog.publishing
  (:require [sutra.kit.coerce :refer [drop-empty]]
            [sutra.types.entities :as ents]
            [sutra.types.rid :as rid]
            [sutra.types.shape :as tshape]
            [sutra.types.versions :as vers]))

(set! *warn-on-reflection* true)

(defn ref-names
  [W kk]
  (let [secs (vec rid/name-field-of)
        nm   (fn [v]
               (reduce (fn [acc [sk nk]]
                         (let [es (get W sk)]
                           (if-let [n (when (map? es) (get (get es v) nk))] n acc)))
                       v secs))]
    (fn [e] (rid/rewrite-refs kk e (fn [_targets v] (nm v))))))

(defn versioned
  [sec k args live]
  (let [want (vers/carry args live)
        was  (some-> live (#(drop-empty (tshape/->author sec % {:name k}))) vers/snapshot)]
    (vers/publish-head want live (not= (vers/snapshot want) was))))

(defn versioned-body
  [live tag body]
  (let [sec (ents/tag->section tag)]
    (if-not (and (vers/publishes? sec) (some? (:name body)))
      body
      (let [k (keyword (name (:name body)))
            e (live sec k)]
        (if (:auto-publish body) (versioned sec k body e) (vers/carry body e))))))
