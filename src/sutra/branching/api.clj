(ns sutra.branching.api
  (:require [sutra.branching.proposals :as proposals]
            [sutra.branching.push :as push]))

(set! *warn-on-reflection* true)

(defn commit-files!
  [store schema repo-rid branch commit]
  (push/commit-files! store schema repo-rid branch commit))

(defn check-files
  [store live repo-rid branch commit]
  (push/check-files store live repo-rid branch commit))

(defn propose! [store schema body admin?] (proposals/propose! store schema body admin?))
