(ns sutra.kit.slug
  (:require [clojure.string :as str]
            [sutra.types.api-name :as api-name]))

(defn slug
  [s]
  (-> (str s) str/trim api-name/transliterate str/lower-case
      (str/replace #"[^a-z0-9]+" "-")
      (str/replace #"^-+|-+$" "")))
