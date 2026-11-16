(ns sutra.actions.request
  (:require [sutra.kit.anomaly :as anomaly]
            [clojure.string :as str]
            [sutra.catalog.grants :as grants]
            [sutra.store.access.scope :as ctx]
            [sutra.catalog.branch-policy :as policy]
            [sutra.store.seat :as seat]))

(set! *warn-on-reflection* true)

(defn action-request
  [m]
  (let [branch    (str (or (:branch m) (:branch (:store m)) "main"))
        authority (if (contains? m :authority) (:authority m) (:authority (:store m)))
        store        (or (:store m)
                         (anomaly/raise! :fault
                                         (str "an action request names the store value it runs"
                                              " on (:store)")
                                         {:branch branch}))]
    (merge {:dry-run? false :replaying? false :trace nil :effect-gate (policy/request-gate branch)}
           m
           {:branch    branch
            :authority authority
            :schema    (if (contains? m :schema)
                         (:schema m)
                         (seat/branch-schema (ctx/at store branch)))
            :store        store})))

(defn request-on
  [store m]
  (let [b (str (or (:branch m) (:branch store) "main"))]
    (action-request (assoc m :branch b :authority (grants/request-authority store)
                           :store (ctx/at store b)))))
(defn create-action?
  [action]
  (boolean (and action (str/starts-with? (name action) "create-"))))

(defn with-fresh-id
  [action args]
  (if (and (create-action? action) (not (:id args))) (assoc args :id (random-uuid)) args))
