(ns sutra.types.branch-kinds
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def machine-namespaces
  ["staging/" "run/" "scenario/" "proposal-preview/"])

(defn machine-branch?
  [b]
  (boolean (some #(str/starts-with? (str b) %) machine-namespaces)))

(def staging-namespace "staging/")

(defn staging-branch?
  [b]
  (str/starts-with? (str b) staging-namespace))

(defn staging-branch
  [parent user-id]
  (when (and parent (not (staging-branch? parent)))
    (str staging-namespace user-id "/" parent)))

(defn staging-parent
  [b]
  (when (staging-branch? b) (nth (str/split (str b) #"/" 3) 2 nil)))

(defn instance-home
  [trunk branch]
  (let [b (str branch)]
    (cond (= (str trunk) b)    nil
          (staging-branch? b) (when-let [p (staging-parent b)] (instance-home trunk p))
          :else               (str trunk))))

(def sketch-namespace
  "sketch/")

(defn sketch-branch?
  [branch]
  (str/starts-with? (str branch) sketch-namespace))

(def scenario-namespace
  "scenario/")

(defn scenario-branch?
  [branch]
  (str/starts-with? (str branch) scenario-namespace))

(defn projectable-branch?
  [branch]
  (let [b (str branch)]
    (and (not (str/blank? b))
         (not-any? #(str/starts-with? b %) machine-namespaces))))
