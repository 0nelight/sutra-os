(ns conformance.schema
  (:require [clojure.string :as str]))

(defn- unescape [s] (-> s (str/replace "~1" "/") (str/replace "~0" "~")))

(defn resolve-ref
  [root ref]
  (when (str/starts-with? ref "#")
    (let [segs (remove str/blank? (str/split (subs ref 1) #"/"))]
      (get-in root (mapv unescape segs)))))

(defn- integral? [v]
  (or (integer? v) (and (number? v) (== v (Math/floor (double v))))))

(defn- type-ok? [t v]
  (case t
    "object"  (map? v)
    "array"   (sequential? v)
    "string"  (string? v)
    "integer" (and (number? v) (integral? v))
    "number"  (number? v)
    "boolean" (boolean? v)
    "null"    (nil? v)
    true))

(defn- format-ok? [fmt v]
  (case fmt
    "date-time"     (try (java.time.OffsetDateTime/parse v) true (catch Exception _ false))
    "uri-reference" (try (java.net.URI. v) true (catch Exception _ false))
    true))

(defn- code-points [^String s] (.codePointCount s 0 (count s)))

(declare errors)

(defn- object-errors [root s v path]
  (let [props (get s "properties" {})
        pats  (get s "patternProperties" {})
        addl  (get s "additionalProperties" true)]
    (concat
     (for [k (get s "required") :when (not (contains? v k))]
       (str path ": missing required property " (pr-str k)))
     (mapcat (fn [[k x]]
               (let [p        (str path "/" k)
                     own      (when (contains? props k) [(get props k)])
                     by-pat   (for [[re ps] pats :when (re-find (re-pattern re) k)] ps)
                     matched  (concat own by-pat)]
                 (cond
                   (seq matched) (mapcat #(errors root % x p) matched)
                   (false? addl) [(str path ": property " (pr-str k) " is not allowed")]
                   (map? addl)   (errors root addl x p)
                   :else         [])))
             v))))

(defn- array-errors [root s v path]
  (let [prefix (get s "prefixItems")
        items  (get s "items")]
    (concat
     (when-let [n (get s "minItems")]
       (when (< (count v) n) [(str path ": fewer than " n " items")]))
     (when-let [n (get s "maxItems")]
       (when (> (count v) n) [(str path ": more than " n " items")]))
     (mapcat (fn [i x] (errors root (nth prefix i) x (str path "/" i)))
             (range (min (count prefix) (count v))) v)
     (when (and items (not (false? items)) (not (true? items)))
       (mapcat (fn [i x] (errors root items x (str path "/" i)))
               (range (count prefix) (count v)) (drop (count prefix) v)))
     (when (and (false? items) (> (count v) (count prefix)))
       [(str path ": more items than prefixItems allows")]))))

(defn- string-errors [s v path]
  (concat
   (when-let [n (get s "minLength")]
     (when (< (code-points v) n) [(str path ": shorter than " n)]))
   (when-let [n (get s "maxLength")]
     (when (> (code-points v) n) [(str path ": longer than " n)]))
   (when-let [re (get s "pattern")]
     (when-not (re-find (re-pattern re) v) [(str path ": does not match " re)]))
   (when-let [f (get s "format")]
     (when-not (format-ok? f v) [(str path ": not a valid " f)]))))

(defn- number-errors [s v path]
  (concat
   (when-let [n (get s "minimum")] (when (< v n) [(str path ": below " n)]))
   (when-let [n (get s "maximum")] (when (> v n) [(str path ": above " n)]))))

(defn errors
  ([root v] (errors root root v ""))
  ([root s v path]
   (cond
     (true? s)  []
     (false? s) [(str path ": no value is allowed here")]
     (nil? s)   []
     :else
     (let [t     (get s "type")
           types (if (sequential? t) t (when t [t]))]
       (vec
        (concat
         (when-let [r (get s "$ref")]
           (if-let [target (resolve-ref root r)]
             (errors root target v path)
             [(str path ": unresolvable $ref " r)]))
         (when (and types (not-any? #(type-ok? % v) types))
           [(str path ": expected " (str/join " or " types) ", got " (pr-str v))])
         (when (and (contains? s "enum") (not (some #(= % v) (get s "enum"))))
           [(str path ": " (pr-str v) " is not one of " (pr-str (get s "enum")))])
         (when (and (contains? s "const") (not= v (get s "const")))
           [(str path ": must be " (pr-str (get s "const")))])
         (when (map? v) (object-errors root s v path))
         (when (sequential? v) (array-errors root s v path))
         (when (string? v) (string-errors s v path))
         (when (number? v) (number-errors s v path))
         (when-let [xs (get s "allOf")] (mapcat #(errors root % v path) xs))
         (when-let [xs (get s "anyOf")]
           (when (every? seq (map #(errors root % v path) xs))
             [(str path ": matches none of anyOf")]))
         (when-let [xs (get s "oneOf")]
           (let [n (count (filter empty? (map #(errors root % v path) xs)))]
             (when (not= 1 n) [(str path ": matches " n " of oneOf, not exactly 1")])))
         (when-let [x (get s "not")]
           (when (empty? (errors root x v path)) [(str path ": matches a schema under not")]))))))))

(defn errors-at
  [root ref v]
  (errors root {"$ref" ref} v ""))
