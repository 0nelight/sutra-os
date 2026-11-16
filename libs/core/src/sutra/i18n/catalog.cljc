(ns sutra.i18n.catalog
  (:require [clojure.string :as str]
            [sutra.kit.placeholder :as placeholder]
            #?(:clj [sutra.i18n.inline :as inline]))
  #?(:cljs (:require-macros [sutra.i18n.inline :as inline])))

(def locales
  [:de :en])

(def fallback
  :de)

(def ^:private bundles
  #?(:clj  (inline/load-all)
     :cljs (inline/bundles)))

(defn text
  ([l k] (text l k nil))
  ([l k params]
   (if-let [s (some #(get-in bundles [% k]) (cons l (cons fallback locales)))]
     (if (str/includes? s "{")
       (placeholder/fill s params)
       s)
     (str "⟦" k "⟧"))))
