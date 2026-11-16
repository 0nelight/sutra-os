(ns sutra.governance.branches
  (:require [sutra.governance.branch-state :as branch-state]
            [sutra.store.control :as ctl]
            [sutra.store.world :as world]))

(set! *warn-on-reflection* true)

(defn current-commit
  [store]
  (:rev (world/head store)))

(defn list-branches
  [store]
  (mapv (fn [r] {:name (:name r) :parent (get-in r [:base :branch])}) (world/branches store)))

(defn branch-exists?
  [store name]
  (world/exists? store name))

(defn create-branch!
  [store {:keys [parent name space]}]
  (world/fork! store parent name {:kind :feature :gen (str (random-uuid))})
  (branch-state/forked! store name)
  (when space
    (ctl/commit! store [[:upsert :branch-governance [{:branch name :space (str space)}]]]))
  (cond-> {:name name :parent parent} space (assoc :space (str space))))

(defn drop-branch!
  [store name]
  (world/retire! store name))
