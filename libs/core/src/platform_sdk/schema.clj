(ns platform-sdk.schema
  (:require [sutra.kit.digest :as digest]
            [clojure.string :as str]
            [sutra.types.link-side :as ls]
            [sutra.types.rid :as rid]))

(set! *warn-on-reflection* true)

(defn token [v]
  (cond
    (nil? v)     ""
    (boolean? v) (if v "true" "false")
    (keyword? v) (name v)
    (vector? v)  (token (first v))
    (seq? v)     (token (first v))
    :else        (str v)))

(defn object-types [schema] (or (:object-types schema) {}))
(defn link-types [schema] (or (:link-types schema) {}))
(defn action-types
  [schema]
  (let [idx (rid/rid->name-index schema)]
    (into {} (for [[k a] (:actions schema)]
               [(or (:action-name a) k)
                (rid/rewrite-action-param-refs (fn [_ v] (or (rid/resolve-rid->name idx v) v))
                                               a)]))))

(defn function-name
  [spec]
  (or (:function-name spec) (:name spec)))

(defn functions
  [schema]
  (concat (remove :library (vals (or (:functions schema) {})))
          (filter function-name (vals (or (:transforms schema) {})))))

(defn attributes [spec]
  (for [a (:attribute-list spec)]
    [(:attribute-name a) (or (:attribute-type a) :string)]))

(defn primary-key
  ([spec] (primary-key spec nil))
  ([spec backing]
   (some-> (or (get-in backing [:object-types (token (:object-type-name spec)) :primary-key])
               (first (:match-keys spec)))
           token
           keyword)))

(defn name->rid
  [schema]
  (into {} (for [[rid spec] (object-types schema)] [(:object-type-name spec) rid])))

(defn rid->name
  [schema]
  (into {} (for [[rid spec] (object-types schema)] [rid (:object-type-name spec)])))

(defn link-index
  [schema]
  (let [->name (rid->name schema)
        nm     (fn [endpoint] (get ->name endpoint endpoint))]
    (into {} (for [[rid spec] (link-types schema)]
               [(:link-type-name spec)
                {:rid rid
                 :from (nm (:from-object-type spec))
                 :to (nm (:to-object-type spec))
                 :cardinality (:cardinality spec)
                 :forward (ls/api-name rid spec :to)
                 :reverse (ls/api-name rid spec :from)}]))))

(defn- far-cardinality
  [cardinality dir]
  (if (contains? (if (= :to dir) #{:many-to-one :one-to-one} #{:one-to-many :one-to-one})
                 (keyword (token cardinality)))
    :one
    :many))

(defn- side-entries
  [schema type-rid]
  (let [->name (rid->name schema)
        found  (concat (ls/outgoing schema type-rid)
                       (some->> (get ->name type-rid) (ls/outgoing schema)))]
    (for [[_ [{:keys [api-name link dir link-type far-type]}]]
          (group-by (juxt :link :dir) found)]
      [(keyword (token api-name))
       {:link-rid       link
        :link-type-name (:link-type-name link-type)
        :far-type       (keyword (token (get ->name far-type far-type)))
        :cardinality    (far-cardinality (:cardinality link-type) dir)}])))

(defn ambiguous-sides
  [schema type-rid]
  (vec (sort (for [[nm entries] (group-by first (side-entries schema type-rid))
                   :when (< 1 (count entries))]
               nm))))

(defn sides
  [schema type-rid]
  (let [bad (set (ambiguous-sides schema type-rid))]
    (into (sorted-map) (remove (comp bad first)) (side-entries schema type-rid))))

(defn action-params
  [spec]
  (sort-by (fn [[k v]] [(if (:required v) 0 1) (token k)]) (:params spec)))

(defn function-params
  [spec]
  (sort-by (comp token key)
           (into {} (for [[k v] (or (:param-types spec) (:inputs spec))]
                      [k (if (map? v) (:kind v) v)]))))

(defn fingerprint
  [schema]
  (let [obj (for [spec (sort-by (comp token :object-type-name)
                                (vals (object-types schema)))]
              (str "O|" (token (:object-type-name spec)) "|"
                   (str/join "," (for [[n t] (attributes spec)]
                                   (str (token n) ":" (token t))))
                   "|" (str/join "," (map token (:match-keys spec)))))
        lnk (for [[nm info] (sort-by (comp token key) (link-index schema))]
              (str "L|" (token nm) "|" (token (:from info)) "|" (token (:to info))
                   "|" (token (:cardinality info))
                   "|" (token (:forward info)) "|" (token (:reverse info))))
        act (for [[nm spec] (sort-by (comp token key) (action-types schema))]
              (str "A|" (token nm) "|"
                   (str/join "," (for [[p s] (action-params spec)]
                                   (str (token p) ":" (token (:type s))
                                        ":" (token (boolean (:required s))))))))
        fns (for [spec (sort-by (comp token function-name) (functions schema))]
              (str "F|" (token (function-name spec)) "|"
                   (str/join "," (for [[p t] (function-params spec)]
                                   (str (token p) ":" (token t))))))]
    (subs (digest/sha256-hex (str/join "\n" (concat obj lnk act fns))) 0 16)))

(defn object-type-index
  [schema backing]
  (into (sorted-map)
        (for [[rid spec] (object-types schema)]
          [(keyword (token (:object-type-name spec)))
           {:primary-key (primary-key spec backing)
            :attributes (into (sorted-map) (attributes spec))
            :links (sides schema rid)}])))

(defn rid-index
  [schema]
  {:types (into (sorted-map) (for [[rid spec] (object-types schema)]
                               [(keyword (token (:object-type-name spec))) rid]))})
