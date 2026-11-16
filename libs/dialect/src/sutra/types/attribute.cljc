(ns sutra.types.attribute
  (:require [sutra.kit.coll :as coll]))

(def list-keys
  [:attribute-list :attributes :property-list :properties :columns])

(defn normalize
  [item]
  (cond
    (map? item)        item
    (keyword? item)    {:attribute-name item}
    (sequential? item) (let [[n t opts] item]
                         (cond-> {:attribute-name n :attribute-type t}
                           (map? opts) (assoc :opts opts)))))

(defn ->tuple
  [item]
  (if (map? item)
    (let [{:keys [attribute-name attribute-type opts]} item]
      (cond-> [attribute-name attribute-type] (seq opts) (conj opts)))
    item))

(defn attr-name
  [item]
  (:attribute-name (normalize item)))

(defn rows
  ([spec] (rows spec list-keys))
  ([spec ks]
   (into [] (keep normalize)
         (some (fn [k] (let [v (get spec k)] (when (seq v) v))) ks))))

(defn derived-expression
  [{:keys [opts]}]
  (let [e (:expression (:derived opts))]
    (when (vector? e) e)))

(defn index
  [attr-rows]
  (let [rs (vec attr-rows)]
    {:rows    rs
     :names   (mapv :attribute-name rs)
     :by-name (coll/index-by :attribute-name rs)
     :types   (into {} (map (juxt :attribute-name :attribute-type)) rs)
     :derived (into [] (keep (fn [a]
                               (when-let [e (derived-expression a)]
                                 [(:attribute-name a) e])))
                    rs)}))

(defn of
  [spec]
  (index (rows spec)))

(def shared-metadata-keys
  [:description :visibility :constraints :format :render-hints])

(defn- blank? [v]
  (or (nil? v) (and (string? v) (= "" v)) (and (coll? v) (empty? v))))

(defn shared-opts
  [sp]
  (let [dn (or (:display-name sp) (when (string? (:name sp)) (:name sp)))
        m  (cond-> (select-keys sp shared-metadata-keys)
             (some? dn)          (assoc :name dn)
             (:value-type sp)    (assoc :value-type (keyword (:value-type sp))))]
    (not-empty (into {} (remove (comp blank? val)) m))))

(defn shared-attr
  [sp-kw sp]
  (let [t    (:type sp)
        pair [(keyword sp-kw) (if (sequential? t) (vec t) (some-> t keyword))]
        opts (shared-opts sp)]
    (cond-> pair opts (conj opts))))
