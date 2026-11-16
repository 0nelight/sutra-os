(ns sutra.governance.protection
  (:require [sutra.kit.coll :as coll]
            [clojure.string :as str]
            [sutra.catalog.branch-policy :refer [actor-name effective policy-change policy-of]]
            [sutra.foundation.ops :as op]
            [sutra.store.control :as ctl]))

(set! *warn-on-reflection* true)

(defn governance
  [store]
  (coll/index-by :branch (ctl/list store :branch-governance {} {:all-branches? true})))

(defn policy
  [store branch]
  (policy-of (governance store) branch))

(defn policy-in
  [store branch]
  (effective (governance store) branch))

(defn protected?
  [store branch]
  (boolean (:protected (policy store branch))))

(defn changeset-offen?
  [store branch]
  (boolean
   (when-not (str/blank? (str branch))
     (some? (ctl/get store :proposals {:status "open" :source-branch (str branch)})))))

(defn auto-land?
  [store branch]
  (boolean (:auto-land (policy store branch))))

(defn set-policy!
  [store branch opts]
  (let [{:keys [sets] :as change} (policy-change opts)]
    (cond
      (:error change) change
      :else (do (when (seq sets)
                  (ctl/upsert! store :branch-governance
                               (assoc sets :branch branch :updated-by (actor-name store))))
                (policy store branch)))))

(op/defop :governance/policies [{:keys [store]} _]
  (governance store))

(op/defop :governance/policy-in [{:keys [store]} {:keys [branch]}]
  (policy-in store branch))
