(ns sutra.types.sql-ident
  (:require [clojure.string :as str]
            [sutra.kit.sql :as kit-sql]))

(def mark \`)

(def mark-str (str mark))

(defn quote-ident
  [s]
  (kit-sql/quote-with mark-str mark-str s))

(defn render-marks
  [sql quote-name]
  (str/replace sql #"`((?:[^`]|``)*)`"
               (fn [[_ marked]] (quote-name (str/replace marked "``" "`")))))

(def default-dialect :postgres)
