(ns sutra.parser.ent-errors
  (:require [clojure.string :as str]))

(defn humanize->lines
  ([errs] (->> (humanize->lines errs []) distinct sort vec))
  ([errs path]
   (let [seg (fn [p] (str/join " > " (map #(if (keyword? %) (name %) (str %)) p)))]
     (cond
       (nil? errs) []
       (map? errs) (mapcat (fn [[k v]]
                             (humanize->lines v (cond-> path (not= :malli/error k) (conj k))))
                           errs)
       (and (sequential? errs) (seq errs) (every? string? errs))
       [(str (if (seq path) (seg path) "<root>") " — " (str/join "; " errs))]
       (sequential? errs)
       (apply concat (map-indexed (fn [i v] (humanize->lines v (conj path i))) errs))
       (string? errs) [(str (seg path) " — " errs)]
       :else [(str (seg path) " — invalid value")]))))

(def key-reading-hint
  (str "(a `disallowed key` = an unknown/misspelled key at that path; "
       "a `missing required key` = a key the grammar requires there.)"))

(defn ent-errors-params
  [what errs]
  (let [ls (humanize->lines errs)]
    {:what what :n (count ls)
     :lines (str/join "\n" (map #(str "  • " %) ls))
     :hint key-reading-hint}))

(defn format-ent-errors
  [prefix source errs]
  (let [{:keys [n lines hint]} (ent-errors-params prefix errs)]
    (str prefix " (" source "): " n " problem(s) — each is `path — reason`:\n"
         lines "\n" hint)))
