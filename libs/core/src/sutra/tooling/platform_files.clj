(ns sutra.tooling.platform-files
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn- git [dir & args]
  (let [{:keys [exit out]} (apply sh/sh "git" (concat args [:dir (str dir)]))]
    (when (zero? exit) (str/trim (str out)))))

(defn- exclude!
  [dir paths]
  (when-let [info (git dir "rev-parse" "--absolute-git-dir")]
    (let [f    (io/file info "info" "exclude")
          cur  (if (.exists f) (slurp f) "")
          want (remove (set (str/split-lines cur)) (map #(str "/" %) paths))]
      (when (seq want)
        (io/make-parents f)
        (spit f (str cur
                     (when (and (seq cur) (not (str/ends-with? cur "\n"))) "\n")
                     (str/join "\n" want) "\n"))))))

(defn- path-str [k] (if (keyword? k) (subs (str k) 1) (str k)))

(defn lay-down!
  [dir files]
  (let [tracked (set (some-> (git dir "ls-files" "--" ".") str/split-lines))
        written (into []
                      (keep (fn [[path text]]
                              (let [path (path-str path)]
                                (when-not (tracked path)
                                  (let [f (io/file dir path)]
                                    (io/make-parents f)
                                    (spit f text)
                                    path)))))
                      files)]
    (exclude! dir (map path-str (keys files)))
    written))
