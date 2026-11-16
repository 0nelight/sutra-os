(ns sutra.types.advisory
  (:require [clojure.string :as str]))

(def marker
  "! ")

(defn attach
  [detail advisories]
  (str/join "\n" (into [detail] (map #(str marker %)) advisories)))

(defn lines
  [detail]
  (into []
        (comp (filter #(str/starts-with? % marker))
              (map #(subs % (count marker))))
        (str/split-lines (str detail))))

(defn of-checks
  [checks]
  (into []
        (mapcat (fn [{:keys [status detail]}]
                  (when (= :passed (keyword status)) (lines detail))))
        checks))
