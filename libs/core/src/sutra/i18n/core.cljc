(ns sutra.i18n.core
  (:require [clojure.string :as str]
            [sutra.i18n.catalog :as catalog]))

(def default-platform "Sutra-OS")

(defonce ^:private chosen (atom catalog/fallback))

(defonce ^:private platform (atom default-platform))

(defn set-platform-title! [t]
  (reset! platform (or (not-empty (str/trim (str t))) default-platform)))

(defn locale [] @chosen)

(defn from-tag [tag]
  (when tag
    (let [l (-> (str tag) str/lower-case (str/split #"[-_.]") first keyword)]
      (when (some #{l} catalog/locales) l))))

(defn set-locale! [l]
  (reset! chosen (or (some #{l} catalog/locales) (from-tag l) catalog/fallback)))

#?(:clj
   (defn from-env []
     (from-tag (or (System/getenv "SUTRA_LANG") (System/getenv "LANG")))))

(defn t-in
  ([l k] (t-in l k nil))
  ([l k params] (catalog/text l k (assoc params :platform @platform))))

(defn t
  ([k] (t k nil))
  ([k params] (t-in @chosen k params)))
