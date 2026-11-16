(ns sutra.catalog.shape
  (:require [sutra.kit.anomaly :as anomaly]
            [clojure.string :as str]
            [sutra.types.column :as column]
            [sutra.types.world-rules :as rules]
            [sutra.kit.uuid :as uuid]
            [sutra.types.backing :as backing]
            [sutra.types.value-types :as vt]
            [sutra.types.rid :as rid]))

(set! *warn-on-reflection* true)

(def empty-schema-map
  {:object-types {} :link-types {} :actions {} :object-sets {}})

(defn- sql-name [k]
  (if-let [loc (and (string? k) (rid/rid-locator k))]
    (str/replace loc "-" "_")
    (name (column/attr->col k))))

(defn property-renames
  [old-types new-types]
  (into {}
        (for [[a t] old-types
              :when (not (contains? new-types a))
              [a' t'] new-types
              :when (and (not (contains? old-types a'))
                         (= (sql-name a) (sql-name a'))
                         (vt/same-storage? t t'))]
          [a a'])))

(def ^:private cardinality-rules
  {:one-to-one   {:uniques [["from_id"] ["to_id"]]
                  :identity-cols [:from-id :to-id] :sorted? true}
   :many-to-one  {:uniques [["from_id"]]
                  :identity-cols [:from-id] :sorted? false}
   :one-to-many  {:uniques [["to_id"]]
                  :identity-cols [:to-id] :sorted? false}
   :many-to-many {:uniques []
                  :identity-cols [:from-id :to-id] :sorted? true}})

(defn link-id-fn
  [schema-map conn-type-kw]
  (let [card (or (get-in schema-map [:link-types conn-type-kw :cardinality])
                 (anomaly/raise! :fault (str "link-id-for: unknown link-type "
                                             (pr-str conn-type-kw))
                                 {:link-type conn-type-kw
                                  :known (set (keys (:link-types schema-map)))}))
        rule (or (cardinality-rules card)
                 (anomaly/raise! :fault (str "link-id-for: unknown cardinality "
                                             (pr-str card))
                                 {:link-type conn-type-kw :cardinality card}))
        cols  (:identity-cols rule)
        sort? (:sorted? rule)
        seed  (str (name conn-type-kw) "/")]
    (fn [from-id to-id]
      (let [row   {:from-id (str from-id) :to-id (str to-id)}
            parts (if sort? (sort (map row cols)) (mapv row cols))]
        (uuid/v5 uuid/dns-namespace (str seed (str/join "|" parts)))))))

(defn link-id-for
  [schema-map conn-type-kw {:keys [from-id to-id]}]
  ((link-id-fn schema-map conn-type-kw) from-id to-id))

(defn owns-edge-rows?
  [schema conn-kw]
  (nil? (rules/fk-link-spec schema conn-kw)))

(defn migrate-external-source-to-transforms
  [schema]
  (let [obj-with-ext (->> (:object-types schema)
                          (filter (fn [[_ spec]] (some? (:external-source spec)))))
        conn-with-ext (->> (:link-types schema)
                           (filter (fn [[_ spec]] (some? (:external-source spec)))))
        ds-with-ext  (->> (:datasets schema)
                          (filter (fn [[_ spec]] (some? (:external-source spec)))))
        existing-trans (or (:transforms schema) {})
        name->key (into {} (map (fn [[k spec]] [(:transform-name spec) k]))
                        existing-trans)
        lift      (fn [t]
                    (let [nm (:transform-name t)]
                      (if-let [ek (name->key nm)]
                        [ek (assoc t :id ek)]
                        [nm t])))
        obj-ts  (into {}
                      (for [[k spec] obj-with-ext
                            :let [nm (or (:object-type-name spec) k)]]
                        (lift
                         (backing/in-project-of spec
                                                (backing/object-transform
                                                 nm (:external-source spec))))))
        conn-ts (into {}
                      (for [[k spec] conn-with-ext
                            :let [nm (or (:link-type-name spec) k)]]
                        (lift (backing/in-project-of spec
                                                     (backing/link-transform
                                                      nm (:from-object-type spec)
                                                      (:to-object-type spec)
                                                      (:external-source spec))))))
        ds-ts   (into {}
                      (for [[k spec] ds-with-ext
                            :let [nm (or (:dataset-name spec) k)]]
                        (lift
                         (backing/in-project-of spec
                                                (backing/dataset-transform
                                                 nm (:external-source spec))))))
        strip   (fn [m]
                  (reduce-kv (fn [m k spec]
                               (assoc m k (dissoc spec :external-source)))
                             {} (or m {})))]
    (-> schema
        (update :object-types     strip)
        (update :link-types strip)
        (update :datasets   strip)
        (assoc :transforms (merge existing-trans obj-ts conn-ts ds-ts)))))

(defn- unpack-attribute [[attr-name attr-type]]
  {:attribute-name attr-name :attribute-type attr-type})

(def ^:private carried
  [:project :description :display-name])

(defn- unpack-object-type
  [[type-kw v]]
  (cond
    (map? v)
    [type-kw (cond-> (merge (select-keys v (into carried [:primary-key :match-keys :title]))
                            {:object-type-name type-kw
                             :attribute-list   (mapv unpack-attribute (:attributes v))})
               (:external-source v) (assoc :external-source (:external-source v))
               (seq (:implements v)) (assoc :implements (vec (:implements v)))
               (seq (:symbol v))     (assoc :symbol (:symbol v))
               (seq (:plural v))     (assoc :plural (:plural v)))]
    :else
    [type-kw {:object-type-name type-kw
              :attribute-list   (mapv unpack-attribute v)}]))

(defn- unpack-link-type
  [v]
  (let [[name from to cardinality opts] v]
    [name (cond-> (merge (select-keys opts carried)
                         {:link-type-name name
                          :from-object-type     from
                          :to-object-type       to
                          :cardinality          cardinality})
            (:external-source opts) (assoc :external-source (:external-source opts)))]))

(defn unpack-schema
  [m]
  (-> m
      (dissoc :objects :links)
      (assoc :object-types (into {} (map unpack-object-type) (:objects m))
             :link-types   (into {} (map unpack-link-type) (:links m)))
      (update :actions     #(or % {}))
      (update :object-sets #(or % {}))
      (update :functions   #(or % {}))
      (update :agents      #(or % {}))
      (update :transforms  #(or % {}))))
