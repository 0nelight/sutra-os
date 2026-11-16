(ns sutra.checkouts.rename
  (:require [clojure.string :as str]
            [sutra.authoring.api :as authoring]
            [sutra.checkouts.slice :as slice]
            [sutra.health.api :as health]
            [sutra.parser.project-export :as pe]
            [sutra.repos.api :as repos]
            [sutra.store.access.scope :as scope]))

(set! *warn-on-reflection* true)

(defn rewrites
  [texts field old new]
  (vec (for [[path text] texts
             :let [after (authoring/rename-in-python text field old new)]
             :when (not= text after)]
         {:path path :text after})))

(def ^:private node-kind {:datasets "dataset" :object-types "object-type"})

(defn check-after
  [c kind old new]
  (let [old  (name old)
        new  (name new)
        bare (when (= "object-type" kind) (str "|\\{\\{\\s*" old "\\s*\\}\\}"))
        re   (re-pattern (str "\\{\\{\\s*" kind "\\s*:\\s*" old "\\s*\\}\\}" bare))
        hit? (and (= kind (some-> (:node-kind c) name)) (= old (some-> (:node-key c) name)))
        q    (when (string? (:query c))
               (str/replace (:query c) re (str "{{" kind ":" new "}}")))]
    (when (or hit? (not= q (:query c)))
      (cond-> (assoc c :query q)
        hit? (assoc :node-key new)))))

(defn- follow-checks!
  [{:keys [store branch]} kinds-key old new]
  (when-let [kind (node-kind kinds-key)]
    (let [after (keep #(check-after % kind old new) (health/list-all store branch))]
      (doseq [c after] (health/upsert! store branch c))
      (when (seq after) {:checks-rewritten (count after)}))))

(defn- python-texts
  [r branch]
  (when-let [sha (some #(some->> (repos/branch->ref %) (repos/ref-sha (scope/repos) r))
                       (distinct [(str branch) pe/trunk-branch]))]
    (into {}
          (for [[path {:keys [blob]}] (repos/read-tree (scope/repos) r sha)
                :when (str/ends-with? path ".py")]
            [path (String. ^bytes (repos/blob-bytes (scope/repos) r blob) "UTF-8")]))))

(defn- code-changes
  [{:keys [schema branch]} field old new]
  (vec (for [[r spec] (:repositories schema)
             change   (rewrites (python-texts r branch) field old new)]
         (assoc change :rid r :repo (some-> (:repository-name spec) name)))))

(defn- commit-rewrites!
  [{:keys [branch] :as ctx} changes message]
  (try
    (when (not= (str branch) pe/trunk-branch) (slice/ensure-proposal! ctx message))
    {:code-rewritten (slice/commit-code! ctx changes message)}
    (catch clojure.lang.ExceptionInfo e
      {:code-not-rewritten {:why   (ex-message e)
                            :files (mapv #(select-keys % [:repo :path]) changes)}})))

(defn rename!
  [{:keys [store schema branch edge] :as ctx} kinds-key from to]
  (let [field   (authoring/python-field kinds-key)
        changes (when field (code-changes ctx field from to))
        r       (authoring/rename-entity (scope/at store branch) schema kinds-key from to
                                         {:edge edge})]
    (if (:conflict r)
      r
      (merge r
             (follow-checks! ctx kinds-key from to)
             (when (seq changes)
               (commit-rewrites! (assoc ctx :branch (or branch pe/trunk-branch))
                                 changes
                                 (str "rename " (name from) " to " (name to))))))))
