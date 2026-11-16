(ns blueprint-cljs.core
  "Shared conventions, ported from Blueprint (Apache-2.0).
   Blueprint class `bp6-x` is `bp-x` here, modifier `bp6-intent-primary` is
   `bp-intent-primary`; props keep Blueprint's names in kebab-case."
  (:require [clojure.string :as str]))

(def ns-prefix "bp-")

(defn cls
  "`(cls \"button\")` → \"bp-button\"."
  [s]
  (str ns-prefix s))

(defn cx
  "Joins class names, skipping nil/false/blank; a map adds each key whose value is truthy."
  [& xs]
  (->> xs
       (mapcat (fn [x]
                 (cond
                   (map? x)        (keep (fn [[k v]] (when v k)) x)
                   (sequential? x) x
                   :else           [x])))
       (keep #(when % (if (keyword? %) (name %) (str %))))
       (remove str/blank?)
       (str/join " ")))

(def intents #{:none :primary :success :warning :danger})

(defn intent-class
  [intent]
  (when (and intent (not= intent :none))
    (cls (str "intent-" (name intent)))))

(def dark-class (cls "dark"))

(defn dom-props
  "`props` without the kit's own keys, for passing the rest on to the DOM element."
  [props own-keys]
  (apply dissoc props own-keys))

(defn merge-class
  "Adds `:class` from the caller's props to the kit's classes."
  [kit-class props]
  (cx kit-class (:class props)))
