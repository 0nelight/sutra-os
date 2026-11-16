(ns sutra.kit.placeholder
  (:require [clojure.string :as str]))

(defn fill
  [s params]
  (str/replace s #"\{([a-zA-Z0-9-]+)(:pr)?\}"
               (fn [[whole n pr]]
                 (let [v (get params (keyword n) ::missing)]
                   (cond (= ::missing v) whole
                         pr              (pr-str v)
                         :else           (str v))))))
