(ns sutra.capps.intent
  (:require [sutra.kit.coerce :refer [->kw]]
            [sutra.types.refs :as refs]))

(def intents [:none :primary :success :warning :danger])

(def ^:private destructive-ops
  #{:delete-object :delete-link :delete-of-interface :delete-link-of-interface})

(def ^:private edit-ops
  (into #{:create :update :create-link :create-of-interface :update-of-interface
          :create-link-of-interface}
        destructive-ops))

(defn of-rule-ops
  [ops]
  (let [edits (filter edit-ops (map ->kw ops))]
    (when (and (seq edits) (every? destructive-ops edits)) :danger)))

(defn action-intent
  [action]
  (of-rule-ops (keep :op
                     (refs/steps (:rules action)))))

(defn resolve-intent
  [authored inferred fallback]
  (or (->kw authored) (->kw inferred) fallback))

(defn btn-props
  [intent]
  (let [i (or (->kw intent) :primary)]
    (if (= :none i) {} {:intent i})))

(defn one-primary
  [implicit-primary? items]
  (first
   (reduce (fn [[out seen?] it]
             (let [authored  (->kw (:intent it))
                   implicit? (and (nil? authored) (implicit-primary? it))]
               (cond
                 implicit?             [(conj out (cond-> it seen? (assoc :intent :none))) true]
                 (= :primary authored) [(conj out it) true]
                 :else                 [(conj out it) seen?])))
           [[] false]
           items)))
