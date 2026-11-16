(ns sutra.types.api-name
  (:require [clojure.string :as str]))

(def ^:private letters
  [["ä" "ae"] ["ö" "oe"] ["ü" "ue"] ["Ä" "Ae"] ["Ö" "Oe"] ["Ü" "Ue"]
   ["ß" "ss"] ["ø" "oe"] ["Ø" "Oe"]])

(defn transliterate
  [s]
  (reduce (fn [acc [from to]] (str/replace acc from to)) s letters))

(defn- non-ascii [s]
  (re-seq #"[^\x00-\x7F]" s))

(defn problem
  [x]
  (let [s (if (keyword? x) (name x) (str x))]
    (when-let [bad (seq (distinct (non-ascii s)))]
      (let [fixed (transliterate s)]
        (str (pr-str (keyword s)) " is an API name, and an API name is spelled in ASCII. "
             (if (non-ascii fixed)
               (str "Replace " (str/join ", " bad) ".")
               (str "Write " (pr-str (keyword fixed)) "."))
             " The spelling people read goes in the display name (:name \"…\"),"
             " which may carry any character.")))))

(defn valid? [x] (nil? (problem x)))
