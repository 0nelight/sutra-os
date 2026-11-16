(ns sutra.kit.sql
  (:require [clojure.string :as str]))

(defn quote-with
  [open ^String close s]
  (let [s (str s)]
    (str open (if (str/includes? s close) (str/replace s close (str close close)) s) close)))

(defn dq [s] (quote-with "\"" "\"" s))

(defn sq [s] (quote-with "'" "'" s))

(defn qualified
  [quote & parts]
  (str/join "." (map quote parts)))
