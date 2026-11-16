(ns sutra.foundation.confine
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.kit.refusal :as refusal])
  (:import (java.io File)))

(set! *warn-on-reflection* true)

(defn- refuse! [path root code]
  (refusal/refuse! code {:path (str path) :root (str root) :reason (keyword (name code))
                         :confine/refused true}))

(defn within?
  [^File root ^File f]
  (.startsWith (.toPath (.getCanonicalFile f)) (.toPath (.getCanonicalFile root))))

(defn confine-path
  ^File [root path]
  (let [p    (str path)
        root (.getCanonicalFile (io/file (str root)))]
    (when (or (str/blank? p) (str/includes? p "\u0000"))
      (refuse! p root :refusal.path/empty-or-nul))
    (when (or (.isAbsolute (io/file p)) (str/starts-with? p "/") (str/starts-with? p "\\"))
      (refuse! p root :refusal.path/absolute))
    (when (some #{".."} (str/split p #"[/\\]"))
      (refuse! p root :refusal.path/dot-dot))
    (when (str/starts-with? p "-")
      (refuse! p root :refusal.path/leading-dash))
    (let [f (.getCanonicalFile (io/file root p))]
      (when-not (within? root f) (refuse! p root :refusal.path/outside))
      f)))

(defn confine-absolute
  ^File [roots path]
  (let [f (.getCanonicalFile (io/file (str path)))]
    (when-not (and (.isAbsolute (io/file (str path)))
                   (some #(within? (io/file (str %)) f) roots))
      (refuse! path (str/join ":" roots) :refusal.path/outside))
    f))
