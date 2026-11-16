(ns sutra.foundation.encoding
  (:require [sutra.kit.refusal :as refusal]
            [sutra.kit.uuid :as uuid]
            [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(set! *warn-on-reflection* true)

(defn ->payload [v] (pr-str v))

(defn <-payload [s]
  (when s (edn/read-string (str s))))

(defn edn-or-nil
  [s]
  (when (some? s)
    (try (edn/read-string (str s)) (catch RuntimeException _ nil))))

(defn json-or-nil
  [s & opts]
  (when (some? s)
    (try (apply json/read-str (str s) opts) (catch Exception _ nil))))

(defn kebab-key
  [k]
  (keyword (str/replace (name k) "_" "-")))

(defn ->uuid
  [v]
  (cond
    (uuid? v) v
    (and (string? v) (seq v))
    (try (java.util.UUID/fromString v)
         (catch IllegalArgumentException _ nil))
    :else nil))

(defn coerce-uuids
  [m]
  (walk/postwalk
   (fn [x] (or (when (string? x) (->uuid x)) x))
   m))

(defn safe-long
  [v]
  (when (some? v)
    (try (Long/parseLong (str v)) (catch NumberFormatException _ nil))))

(defn clip
  [n s]
  (when (some? s) (let [s (str s)] (subs s 0 (min n (count s))))))

(defn elide
  [n s]
  (let [s (str s)] (if (> (count s) n) (str (subs s 0 n) "…") s)))

(defn conj-capped
  [v x n]
  (let [v (conj v x)]
    (if (> (count v) n) (subvec v (- (count v) n)) v)))

(defn pk-uuid
  [object-type pk-value]
  (str (uuid/v5 uuid/dns-namespace (str (name object-type) "/" (pr-str pk-value)))))

(defn object-id
  [type-rid pk-value]
  (when-not (and (string? type-rid) (str/starts-with? type-rid "ri."))
    (refusal/refuse! :refusal.foundation/object-id-needs-type {:type-ref type-rid}))
  (pk-uuid type-rid pk-value))

(defn transform-row-id
  [t-kw key-vals]
  (str (uuid/v5 uuid/dns-namespace
           (str (name t-kw) "/"
                (->> key-vals (map pr-str) (str/join ""))))))

