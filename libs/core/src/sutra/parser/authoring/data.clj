(ns sutra.parser.authoring.data
  (:require [clojure.walk :as walk]
            [sutra.parser.ent :as ent]))

(set! *warn-on-reflection* true)

(def ^:private prose-keys
  #{:description :title :display-name :plural-display-name :name :message :label :text :help
    :placeholder :query :code :source :reason :subtitle :title-template})

(defn- keywordize-ids [x]
  (cond
    (map? x)    (into {} (map (fn [[k v]] [k (if (prose-keys k) v (keywordize-ids v))])) x)
    (vector? x) (mapv keywordize-ids x)
    (and (string? x) (re-matches #"[a-z][a-z0-9_-]*" x)) (keyword x)
    :else x))

(defn- plain [x]
  (walk/postwalk #(cond (map? %) (into {} %) (seq? %) (vec %) :else %) x))

(defn- ->body [schema]
  (keywordize-ids (ent/decode-schema (plain schema))))

(defn ->document
  [doc]
  (if (contains? doc :schema) (update (plain doc) :schema ->body) (->body doc)))

(defn ->data
  [doc]
  (cond
    (map? doc)        (into (sorted-map) (map (fn [[k v]] [(->data k) (->data v)])) doc)
    (keyword? doc)    (if (namespace doc) (str (namespace doc) "/" (name doc)) (name doc))
    (set? doc)        (vec (sort-by str (map ->data doc)))
    (sequential? doc) (mapv ->data doc)
    :else doc))
