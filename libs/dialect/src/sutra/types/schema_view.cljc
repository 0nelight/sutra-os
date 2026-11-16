(ns sutra.types.schema-view
  (:require [sutra.types.entities :as ents]
            [sutra.types.link-side :as ls]
            [sutra.types.value-types :as vt]))

(defn- view-fields
  [attr-list vts]
  (mapv (fn [{a :attribute-name t :attribute-type o :opts}]
          (cond-> {:name a :type t}
            (seq o) (assoc :opts o)))
        (vt/resolve-value-types (vec attr-list) vts)))

(defn- view-link-constraint [lc]
  (cond-> {:link-name      (:link-name lc)
           :to-object-type (:to-object-type lc)
           :to-interface   (:to-interface lc)
           :cardinality    (ls/constraint-cardinality (:cardinality lc))}
    (:required lc) (assoc :required true)))

(def ^:private link-side-fields
  [:forward-api-name :reverse-api-name :forward-name :reverse-name
   :forward-plural :reverse-plural])

(defn app-object-types
  [schema]
  (into {}
        (for [[k a] (:applications schema)
              :let [ts (->> (cons (:object-type a) (get-in a [:layout :object-types]))
                            (keep #(some-> % name keyword))
                            distinct
                            vec)]
              :when (seq ts)]
          [k ts])))

(def ^:private viewed-axes
  (let [fs (ents/fields :object-types)]
    (for [k (concat (ents/axis :object-types :presentation) (ents/axis :object-types :identity))]
      [k (:present (fs k))])))

(defn- view-axes
  [v]
  (for [[k present] viewed-axes
        :let [x (get v k)]
        :when (case present :seq (seq x) :truthy x (some? x))]
    [k (cond (= :truthy present) true (sequential? x) (vec x) :else x)]))

(defn view-shape
  [schema]
  (let [vts     (:value-types schema)
        obj-kvs (for [[k v] (:object-types schema)]
                  [k (into {:fields     (view-fields (:attribute-list v) vts)
                            :implements (vec (or (:implements v) []))
                            :symbol     (:symbol v)
                            :plural     (:plural v)
                            :title-key  (:title-key v)}
                           (view-axes v))])
        conns   (vec (for [[k v] (:link-types schema)]
                       (merge {:name            k
                               :from            (:from-object-type v)
                               :to              (:to-object-type v)
                               :cardinality     (:cardinality v)
                               :id              (:id v)
                               :external-source (:external-source v)}
                              (select-keys v link-side-fields))))
        ifaces  (for [[k v] (:interfaces schema)]
                  [k {:name             k
                      :extends          (vec (or (:extends v) []))
                      :display-name     (:display-name v)
                      :description      (:description v)
                      :status           (:status v)
                      :searchable       (:searchable v)
                      :property-list    (view-fields (or (:property-list v) []) vts)
                      :link-constraints (mapv view-link-constraint
                                              (or (:link-constraints v) []))}])]
    {:objects      (into {} (for [[k v] obj-kvs] [k (:fields v)]))
     :object-types (into {} obj-kvs)
     :links        conns
     :interfaces   (into {} ifaces)
     :transforms   (or (:transforms schema) {})}))
