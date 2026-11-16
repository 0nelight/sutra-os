(ns sutra.types.column
  (:require [clojure.string :as str]
            [sutra.types.api-name :as api-name]
            [sutra.types.rid :as rid]))

(def ^:private foldable
  #"[äöüÄÖÜßøØ?!-]")

(defn- fold-col
  [s]
  (-> s
      api-name/transliterate
      (str/replace "?" "") (str/replace "!" "")
      (str/replace "-" "_")))

(def ^:private col-cache-max 20000)

(def ^:private col-cache (atom {}))

(defn attr->col
  [attr]
  (if-let [hit (get @col-cache attr)]
    hit
    (let [s   (name attr)
          col (keyword (if (re-find foldable s) (fold-col s) s))]
      (swap! col-cache (fn [c] (assoc (if (< col-cache-max (count c)) {} c) attr col)))
      col)))

(defn sql-name
  [k]
  (if-let [loc (and (string? k) (rid/rid-locator k))]
    (str/replace loc "-" "_")
    (name (attr->col k))))
