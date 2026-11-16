(ns sutra.catalog.lookup
  (:require [sutra.kit.coerce :as coerce]
            [sutra.types.attribute :as attribute]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]))

(set! *warn-on-reflection* true)

(def TYPES {:object :object-types :link :link-types})

(defn get-type [sc k n] (get-in sc [(TYPES k) n]))

(defn attrs
  [schema type-kw]
  (or (:attribute-list (get-in schema [:object-types type-kw]))
      (:property-list (get-in schema [:interfaces type-kw]))))

(defn conn-type [schema ct-name]
  (get-in schema [:link-types ct-name]))

(defn from-type [ct-def] (:from-object-type ct-def))
(defn to-type [ct-def] (:to-object-type ct-def))

(defn iface? [schema kw]
  (contains? (:interfaces schema) kw))

(defn rid-of
  [schema kinds-key name-kw]
  (sch/rid-of (sch/index schema) kinds-key name-kw))

(defn action
  [schema a]
  (let [as (:actions schema)]
    (or (get as a) (some->> (rid-of schema :actions a) (get as)))))

(defn actions-by-name
  [schema]
  (into {} (for [[k a] (:actions schema)] [(or (:action-name a) k) a])))

(defn name-of
  [schema _kinds-key rid]
  (when (rid/rid? rid) (sch/name-of (sch/index schema) rid)))

(defn names-and-rids [schema kinds-key]
  (let [m (get schema kinds-key), nf (rid/name-field-of kinds-key)]
    (into (set (keys m)) (map (comp nf val)) m)))

(defn attrs-index-of
  [schema]
  (persistent!
   (reduce (fn [m k] (assoc! m k (attribute/index (map attribute/normalize (attrs schema k)))))
           (transient {})
           (concat (keys (:interfaces schema)) (keys (:object-types schema))))))

(defn attrs-index
  [schema type-kw]
  (or (get (sch/column schema ::attrs attrs-index-of) type-kw)
      (attribute/index (map attribute/normalize (attrs schema type-kw)))))

(defn attr-type-map [schema tn]
  (when (attrs schema tn) (:types (attrs-index schema tn))))

(defn ->type-rid
  [schema kinds-key token]
  (when token
    (if (rid/rid? token) token
        (rid-of schema kinds-key (coerce/->kw token)))))
