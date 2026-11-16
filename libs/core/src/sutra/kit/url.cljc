(ns sutra.kit.url
  (:require [clojure.string :as str]))

(def link-schemes #{"http" "https" "mailto" "tel"})
(def web-schemes #{"http" "https"})

(def ^:private invisible
  (re-pattern (str "[\\x00-\\x20\\x7F\\xA0" (char 0x2000) "-" (char 0x200B)
                   (apply str (map char [0x1680 0x180E 0x2028 0x2029 0x202F 0x205F 0x3000
                                         0xFEFF]))
                   "]")))

(def ^:private controls #"[\x00-\x1F\x7F]")

(def ^:private image-data
  #"(?i)^data:image/(png|jpe?g|gif|webp|avif|bmp|svg\+xml);base64,[A-Za-z0-9+/=]*$")

(defn- scheme
  [s]
  (some-> (re-find #"^([A-Za-z][A-Za-z0-9+.\-]*):" s) second str/lower-case))

(defn- relative?
  [s]
  (and (not (re-find #"^[/\\][/\\]" s))
       (or (re-find #"^[/?#]" s)
           (re-find #"^\.\.?/" s)
           (not (str/includes? s ":")))))

(defn safe-url
  ([u] (safe-url u link-schemes))
  ([u schemes]
   (when (and (some? u) (not (coll? u)))
     (let [shown  (str/trim (str/replace (str u) controls ""))
           judged (str/replace shown invisible "")]
       (when (seq judged)
         (if-let [sch (scheme judged)]
           (when (contains? schemes sch) shown)
           (when (relative? judged) shown)))))))

(defn href
  [u]
  (or (safe-url u) "#"))

(defn image-src
  [u]
  (let [s (some-> u str (str/replace controls "") str/trim)]
    (if (and s (re-find image-data (str/replace s invisible "")))
      s
      (safe-url u web-schemes))))
