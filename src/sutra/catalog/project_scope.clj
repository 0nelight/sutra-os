(ns sutra.catalog.project-scope
  (:require [sutra.kit.coll :as coll]
            [sutra.kit.anomaly :as anomaly]
            [sutra.kit.refusal :as refusal]
            [sutra.kit.tree :as tree]
            [clojure.string :as str]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]))

(set! *warn-on-reflection* true)

(defn may?
  [auth op project]
  (grants/project-allows? auth project op))

(defn project-name
  [schema project]
  (some-> (get-in schema [:projects project :project-name]) name))

(defn holds-nothing?
  [schema project]
  (not-any? (fn [[section m]]
              (and (not= :projects section) (map? m)
                   (some #(and (map? %) (= project (:project %))) (vals m))))
            schema))

(defn- denial
  [op project-name what]
  (let [role (if (#{:create :update} op) "Editor" "Owner")]
    (cond
      (= :read op)  [:refusal.catalog/project-read-denied {:what what}]
      project-name  [:refusal.catalog/project-change-denied-named
                     {:project-name project-name :role role}]
      :else         [:refusal.catalog/project-change-denied {:role role}])))

(defn require!
  [auth op project what]
  (when-not (may? auth op project)
    (let [[code params] (denial op nil what)]
      (refusal/refuse! code
                       {:sutra/acl-denied true :project project :operation op
                        ::denial {:what what :name? (may? auth :read project)}}
                       params))))

(def ^:private project-sections #{"projects" "spaces" "organizations"})

(def ^:private governing-sections
  {"roles" :manage-permissions "markings" :manage-markings})

(defn- row-needs
  [{:keys [status section project moved-from]}]
  (cond
    (governing-sections section)
    [[(governing-sections section) nil]]

    (project-sections section)
    (case status
      "added"    [[:create nil]]
      "modified" [[:manage-permissions project]]
      "removed"  [[:delete project]])

    :else
    (case status
      "added"    [[:create project]]
      "modified" (cond-> [[:update project]] moved-from (conj [:update moved-from]))
      "removed"  [[:delete project]])))

(defn require-schema-change!
  [auth rows]
  (doseq [{:keys [name section] :as row} rows
          [op project] (row-needs row)]
    (require! auth op project (str section " " name))))

(def ^:dynamic *unlocking*
  false)

(defn locked-hit
  [schema locked rows]
  (when (seq locked)
    (first (for [{:keys [name section project]} rows
                 :let [pn (project-name schema project)]
                 :when (contains? locked pn)]
             (let [params {:what (str section " " name) :project-name pn}]
               {:project pn :what (:what params)
                :code    :refusal.catalog/project-locked
                :params  params
                :error   (refusal/message :refusal.catalog/project-locked params)})))))

(defn require-unlocked!
  [schema locked rows]
  (when-not *unlocking*
    (when-let [hit (locked-hit schema locked rows)]
      (refusal/refuse! (:code hit) {:sutra/acl-denied true :project (:project hit) :locked true}
                       (:params hit)))))

(defn named
  [schema r]
  (let [{:keys [project operation] d ::denial :as data} (anomaly/data r)
        nm (when (:name? d) (project-name schema project))]
    (if nm
      (let [[code params] (denial operation nm (:what d))]
        (refusal/refused code (dissoc data :refusal/code :refusal/params) params))
      r)))

(defn may-unplaced?
  [auth op project]
  (or (nil? project) (may? auth op project)))

(defn require-unplaced!
  [auth op project what]
  (when project (require! auth op project what)))

(defn resolve-project
  [schema p]
  (let [s (some-> p name str/trim not-empty)]
    (when s
      (or (when (contains? (:projects schema) s) s)
          (lookup/rid-of schema :projects (keyword s))
          (refusal/refuse! :refusal.catalog/project-not-on-branch {:project s})))))

(defn require-home!
  [project]
  (when (nil? project)
    (refusal/refuse! :refusal.catalog/move-target-no-project {})))

(defn require-move!
  [auth from to what]
  (require-home! to)
  (require-unplaced! auth :update from what)
  (require! auth :update to what))

(defn by-rid [nodes] (coll/index-by :rid nodes))

(defn node-home
  [idx n]
  (if (= :folder (keyword (:node-kind n)))
    (:project n)
    (some-> (get idx (:parent-rid n)) :project)))

(defn visible-nodes
  [auth nodes]
  (let [idx (by-rid nodes)
        ok? (fn ok? [n depth]
              (and (may-unplaced? auth :read (node-home idx n))
                   (let [p (get idx (:parent-rid n))]
                     (or (nil? p) (> depth 32) (ok? p (inc depth))))))]
    (filterv #(ok? % 0) nodes)))

(defn subtree-folders
  [nodes rid]
  (let [kids (group-by :parent-rid nodes)
        sub  (comp (filter #(= :folder (keyword (:node-kind %)))) (map :rid))]
    (set (tree/reach #(sequence sub (get kids %)) [rid]))))
