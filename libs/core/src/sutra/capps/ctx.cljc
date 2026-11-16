(ns sutra.capps.ctx
  (:require [clojure.string :as str]))

(def locales
  [:de :en])

(def fallback-locale
  :de)

(def utc
  "UTC")

(defn ->locale
  [tag]
  (when (some? tag)
    (let [s (if (keyword? tag) (name tag) (str tag))]
      (some #{(-> s str/lower-case (str/split #"[-_.]") first keyword)} locales))))

(defn of
  ([] (of nil))
  ([{:keys [locale zone now]}]
   {:locale (or (->locale locale) fallback-locale)
    :zone   (or (not-empty (str/trim (str zone))) utc)
    :now    now}))
