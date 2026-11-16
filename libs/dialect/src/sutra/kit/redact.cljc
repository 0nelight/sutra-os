(ns sutra.kit.redact
  (:require [clojure.string :as str]))

(def marker "[redacted]")

(def ^:private secret-words
  #{"password" "passwd" "pwd" "passphrase" "secret" "secrets" "token" "cipher" "credential"
    "credentials" "authorization" "cookie" "apikey" "privatekey"})

(def ^:private secret-compounds
  #{"api-key" "private-key" "client-secret" "set-cookie" "secret-cipher"})

(defn- words
  [k]
  (let [n (if (keyword? k) (name k) (str k))]
    (->> (str/split n #"(?<=[a-z0-9])(?=[A-Z])|[-_.\s]+")
         (remove str/blank?)
         (map str/lower-case))))

(defn secret-key?
  [k]
  (when (or (keyword? k) (string? k) (symbol? k))
    (let [ws (words k)]
      (boolean (or (some secret-words ws)
                   (some secret-compounds (map #(str %1 "-" %2) ws (rest ws))))))))

(def ^:private string-rules
  [[#"(?i)([a-z][a-z0-9+.-]*://)[^/@\s:]+:[^/@\s]*@" (str "$1" marker "@")]
   [#"(?i)\b(password|passwd|pwd|secret|token|api[-_]?key)=[^;&\s]*" (str "$1=" marker)]
   [#"(?i)\b(bearer|basic)\s+[A-Za-z0-9._~+/=-]{8,}" (str "$1 " marker)]])

(defn scrub
  [s]
  (if (string? s) (reduce (fn [s [re rep]] (str/replace s re rep)) s string-rules) s))

(defn redact
  [x]
  (cond
    (map? x)        (reduce-kv (fn [m k v]
                                 (assoc m k (if (and (secret-key? k) (some? v)) marker (redact v))))
                               x x)
    (string? x)     (scrub x)
    (vector? x)     (mapv redact x)
    (set? x)        (into (empty x) (map redact) x)
    (seq? x)        (map redact x)
    :else           x))
