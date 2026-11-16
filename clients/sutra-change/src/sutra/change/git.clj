(ns sutra.change.git
  (:require [babashka.process :as p]
            [clojure.string :as str]
            [sutra.tooling.git-hooks :as hooks]))

(defn- run
  [dir & args]
  (let [{:keys [exit out]} (apply p/sh {:dir (or dir ".") :err :string} "git" args)]
    (when (zero? exit) (str/trim out))))

(defn- lines [s] (->> (str/split-lines (or s "")) (remove str/blank?) vec))

(defn root
  [dir] (run dir "rev-parse" "--show-toplevel"))

(defn hooks-dir
  [dir]
  (hooks/absolutize dir (run dir "rev-parse" "--git-path" "hooks")))

(def ^:private vorgangs-spuren
  [["rebase-merge" :rebase] ["rebase-apply" :rebase] ["MERGE_HEAD" :merge]
   ["CHERRY_PICK_HEAD" :cherry-pick] ["REVERT_HEAD" :revert]])

(defn laufender-vorgang
  [dir]
  (some (fn [[spur art]]
          (when-let [p (run dir "rev-parse" "--git-path" spur)]
            (when (.exists (java.io.File. (hooks/absolutize dir p))) art)))
        vorgangs-spuren))

(defn remote-url [dir] (run dir "config" "--get" "remote.origin.url"))

(defn head-sha [dir] (run dir "rev-parse" "HEAD"))

(defn branch [dir] (run dir "rev-parse" "--abbrev-ref" "HEAD"))

(defn rev [dir r] (run dir "rev-parse" "--verify" "--quiet" (str r "^{commit}")))

(defn merge-base [dir a b] (run dir "merge-base" a b))

(defn show
  [dir rev path]
  (run dir "show" (str rev ":" path)))

(defn ls-files
  ([dir] (lines (run dir "ls-files")))
  ([dir rev] (lines (run dir "ls-tree" "-r" "--name-only" rev))))

(defn changed-paths
  [dir base]
  (->> (concat (lines (run dir "diff" "--name-only" base "--"))
               (lines (run dir "ls-files" "--others" "--exclude-standard")))
       distinct sort vec))

(defn committed-paths
  [dir base]
  (when base
    (->> (lines (run dir "diff" "--name-only" base "HEAD" "--")) sort vec)))

(defn upstream
  [dir] (run dir "rev-parse" "--abbrev-ref" "--symbolic-full-name" "@{upstream}"))

(defn dirty-paths
  [dir] (changed-paths dir "HEAD"))

(defn ahead-count
  [dir base]
  (some-> (run dir "rev-list" "--count" (str base "..HEAD")) parse-long))

(defn behind-count
  [dir base trunk]
  (some-> (run dir "rev-list" "--count" (str base ".." trunk)) parse-long))

(defn ancestor?
  [dir a b]
  (let [{:keys [exit]} (p/sh {:dir dir :err :string} "git" "merge-base" "--is-ancestor" a b)]
    (zero? exit)))

(defn commit-date
  [dir rev] (run dir "log" "-1" "--format=%aI" rev))

(defn pushed?
  [dir sha]
  (boolean (seq (lines (run dir "branch" "-r" "--contains" sha)))))
