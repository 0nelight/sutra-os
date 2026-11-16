(ns sutra.kit.html
  (:require [clojure.string :as str]))

(defn esc
  [v]
  (-> (str v) (str/replace "&" "&amp;") (str/replace "<" "&lt;") (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")))
