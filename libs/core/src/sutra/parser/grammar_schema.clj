(ns sutra.parser.grammar-schema
  (:require [clojure.data.json :as json]
            [clojure.walk :as walk]
            [malli.core :as m]
            [malli.json-schema :as mjs]
            [sutra.parser.ent :as ent]))

(def dir "boundaries/authoring-language")

(def languages
  (sorted-map "ent" nil "enta" :app "entp" :pipeline "entt" :doc-template "entv" :view))

(defn- check-note
  [s]
  (let [msg (:error/message (m/properties s))]
    (m/schema [:any {:json-schema/x-sutra-check
                     (if (string? msg) msg "checked by the Sutra parser beyond this schema")}])))

(defn- noted
  [s]
  (m/walk s (m/schema-walker #(if (= :fn (m/type %)) (check-note %) %))))

(defn- edn-name [k] (if (keyword? k) (subs (str k) 1) k))

(defn- sorted
  [x]
  (cond (map? x)        (into (sorted-map)
                              (map (fn [[k v]] [(str (edn-name k)) (sorted v)]))
                              x)
        (sequential? x) (mapv sorted x)
        (set? x)        (mapv sorted (sort-by str x))
        (keyword? x)    (edn-name x)
        (or (symbol? x) (instance? java.util.regex.Pattern x)) (str x)
        :else           x))

(defn- draft-2020
  [x]
  (walk/postwalk
   (fn [n]
     (cond
       (and (map? n) (sequential? (:items n)))
       (-> n
           (assoc :prefixItems (:items n) :items (get n :additionalItems false))
           (dissoc :additionalItems))
       (and (map? n) (:oneOf n)) (-> n (assoc :anyOf (:oneOf n)) (dissoc :oneOf))
       :else n))
   x))

(def ^:private hoist-min-chars
  200)

(defn- hash-name
  [^String s]
  (let [md (java.security.MessageDigest/getInstance "SHA-256")]
    (str "h-" (subs (apply str (map #(format "%02x" (bit-and % 0xff))
                                    (.digest md (.getBytes s "UTF-8"))))
                    0 12))))

(defn- text [x] (json/write-str x :escape-slash false))

(def ^:private schema-maps #{"properties" "patternProperties" "$defs" "dependentSchemas"})

(def ^:private schema-one
  #{"items" "additionalProperties" "additionalItems" "not" "if" "then" "else" "contains"
    "propertyNames" "unevaluatedItems" "unevaluatedProperties"})

(def ^:private schema-many #{"anyOf" "oneOf" "allOf" "prefixItems" "items"})

(defn- children
  [f n]
  (if-not (map? n)
    n
    (into (sorted-map)
          (map (fn [[k v]]
                 [k (cond (and (schema-maps k) (map? v))
                          (into (sorted-map) (map (fn [[a b]] [a (f b)])) v)
                          (and (schema-one k) (map? v))         (f v)
                          (and (schema-many k) (sequential? v)) (mapv f v)
                          :else                                 v)]))
          n)))

(defn- tally
  [x]
  (let [acc (volatile! {})]
    (letfn [(visit [n]
              (when (map? n) (vswap! acc update (text n) (fnil inc 0)))
              (children visit n)
              n)]
      (children visit x))
    @acc))

(defn- replace-top-down
  [x names]
  (children (fn step [n]
              (if-let [nm (and (map? n) (names (text n)))]
                {"$ref" (str "#/$defs/" nm)}
                (children step n)))
            x))

(defn- hoisted
  [doc]
  (let [defs  (get doc "definitions")
        doc   (-> (dissoc doc "definitions")
                  (cond-> (seq defs) (assoc "$defs" (into (sorted-map) defs))))
        doc   (walk/postwalk #(if (and (string? %) (.startsWith ^String % "#/definitions/"))
                                (str "#/$defs/" (subs % (count "#/definitions/")))
                                %)
                             doc)]
    (loop [doc doc]
      (let [body  (dissoc doc "$defs")
            defs  (get doc "$defs" (sorted-map))
            names (into {}
                        (for [[t n] (tally (assoc body "$defs" defs))
                              :when (and (< 1 n) (<= hoist-min-chars (count t)))]
                          [t (hash-name t)]))]
        (if (empty? names)
          doc
          (let [new-defs (into (sorted-map)
                               (for [[t nm] names :when (not (contains? defs nm))]
                                 [nm (json/read-str t)]))
                defs'    (into (sorted-map)
                               (map (fn [[k v]] [k (replace-top-down (sorted v) names)]))
                               (merge defs new-defs))]
            (recur (assoc (replace-top-down body names) "$defs" defs'))))))))

(defn json-schema
  [ext]
  (hoisted
   (sorted
    (merge {"$schema" "https://json-schema.org/draft/2020-12/schema"
            "$id"     (str "urn:sutra:interface:authoring-language:" ext ":"
                           ent/grammar-version)
            "title"   (str "Sutra ." ext " grammar " ent/grammar-version)
            "$comment"
            (str "Generated from the Malli grammar of sutra.parser.ent; do not edit. The texts"
                 " are EDN; this schema describes them under the mapping: map → object,"
                 " vector/list/set → array, keyword → string of its name with namespace"
                 " (no colon). x-sutra-check marks a rule the parser checks in code."
                 " A $defs entry named h-<hash> is a subschema the grammar repeats.")
            "x-sutra-grammar-version" ent/grammar-version}
           (draft-2020 (mjs/transform (noted (ent/grammar-of (get languages ext)))))))))

(defn file-of [ext] (str dir "/" ext ".schema.json"))

(defn render [ext] (str (json/write-str (json-schema ext) :indent true :escape-slash false) "\n"))
