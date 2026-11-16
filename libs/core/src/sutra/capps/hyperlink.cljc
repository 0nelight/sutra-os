(ns sutra.capps.hyperlink
  (:require [clojure.string :as str]
            [sutra.capps.sidebar :as sidebar]
            [sutra.kit.url :as url]))

(def landing-page "/object-workspace")

(defn- ->key [x] (some-> x name keyword))

(defn- blank-value? [x] (or (nil? x) (str/blank? (str x))))

(defn- allowed-url
  [u]
  (url/safe-url u))

(defn- encoder
  [template]
  (let [t (str/lower-case template)]
    (cond
      (str/starts-with? t "tel:")    #(str/replace (str %) #"[^+0-9]" "")
      (str/starts-with? t "mailto:") #(str/replace (sidebar/encode %) "%40" "@")
      :else                          sidebar/encode)))

(defn- filled
  [template values]
  (let [template (str template)
        names    (map second (re-seq #"\{\s*([^{}\s]+)\s*\}" template))]
    (when (not-any? #(blank-value? (get values (keyword %))) names)
      (let [enc (encoder template)]
        (str/replace template #"\{\s*([^{}\s]+)\s*\}"
                     (fn [[_ nm]] (enc (get values (keyword nm)))))))))

(defn href
  [{:keys [url-type url template property hide-if-null?]} values]
  (let [prop-value (when property (get values (->key property)))]
    (if (and (= :property (->key url-type)) hide-if-null? (blank-value? prop-value))
      nil
      (or (allowed-url (case (->key url-type)
                         :property  prop-value
                         :hardcoded url
                         :templated (filled template values)
                         nil))
          landing-page))))
