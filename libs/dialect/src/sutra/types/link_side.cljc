(ns sutra.types.link-side
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.kit.tree :as tree]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]
            [sutra.ref.sides :as sides]))

(def ^:private name-field (rid/name-field-of :link-types))

(defn api-name
  [link-key ct dir]
  (or (get ct (if (= :to dir) :forward-api-name :reverse-api-name))
      (get ct name-field)
      link-key))

(defn ends
  [lt]
  (into [] (comp (remove nil?) (distinct)) [(:from-object-type lt) (:to-object-type lt)]))

(defn flip [dir] (case dir :to :from, :from :to))

(defn- far-type [ct dir]
  (if (= :to dir) (:to-object-type ct) (:from-object-type ct)))

(defn- near-type [ct dir]
  (if (= :to dir) (:from-object-type ct) (:to-object-type ct)))

(defn constraint-cardinality
  [c]
  (case (some-> c name keyword)
    (:one :many-to-one :one-to-one)   :one
    (:many :one-to-many :many-to-many) :many
    nil))

(defn normal-constraint [lc]
  (cond-> lc (:cardinality lc) (update :cardinality constraint-cardinality)))

(defn far-cardinality
  [card dir]
  (case [dir card]
    ([:to :many-to-one] [:to :one-to-one] [:from :one-to-many] [:from :one-to-one]) :one
    ([:to :one-to-many] [:to :many-to-many] [:from :many-to-one] [:from :many-to-many]) :many
    nil))

(defn- side [link-key ct dir]
  {:api-name (api-name link-key ct dir) :link link-key :dir dir
   :link-type ct :far-type (far-type ct dir)})

(defn satisfies-type?
  [schema type-key want]
  (or (= type-key want)
      (boolean (some #{want} (tree/reach #(get-in schema [:interfaces % :extends])
                                         (get-in schema [:object-types type-key :implements]))))))

(defn- on-side? [schema type-key side]
  (satisfies-type? schema type-key side))

(defn outgoing
  [schema type-key]
  (for [[k ct] (:link-types schema)
        dir    [:to :from]
        :when  (on-side? schema type-key (near-type ct dir))]
    (side k ct dir)))

(defn- link-key [schema handle]
  (if (contains? (:link-types schema) handle)
    handle
    (sch/rid-of (sch/index schema) :link-types handle)))

(defn resolve-side
  [schema type-key handle]
  (let [{:keys [link dir]}
        (try (sides/resolve-side schema type-key handle)
             (catch #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) e
               (let [{k :ref/kind :keys [candidates]} (ex-data e)]
                 (throw (case k
                          :ambiguous-side
                          (anomaly/ex
                           (anomaly/coded :refusal.dialect/link-side-ambiguous :incorrect
                             "Link side {link:pr} is ambiguous on {type} — every side of a link type needs its own API name (:forward-api-name / :reverse-api-name)"
                             {:type type-key :link handle :candidates candidates}))
                          :unknown-side
                          (anomaly/ex
                           (anomaly/coded :refusal.dialect/no-link-side :incorrect
                             "Object type {type} has no link side named {link:pr} (sides: {sides:pr})"
                             {:type type-key :link handle}
                             {:sides (sort (map :api-name (outgoing schema type-key)))}))
                          e)))))]
    (side link (get-in schema [:link-types link]) dir)))

(defn resolve-hop
  [schema type-key handle dir]
  (if dir
    (let [k  (link-key schema handle)
          ct (get-in schema [:link-types k])]
      (when-not ct
        (anomaly/coded! :refusal.dialect/unknown-link :incorrect "Unknown link: {link}"
          {:link handle}))
      (side k ct dir))
    (resolve-side schema type-key handle)))

(defn name-conflicts
  [schema]
  (let [sat (fn [t] (into #{t} (tree/reach #(get-in schema [:interfaces % :extends])
                                           (get-in schema [:object-types t :implements]))))]
    (for [type-key (keys (:object-types schema))
          :let [s (sat type-key)]
          [n sides] (group-by :api-name (for [[k ct] (sort-by (comp str key)
                                                             (:link-types schema))
                                              dir    [:to :from]
                                              :when  (contains? s (near-type ct dir))]
                                          (side k ct dir)))
          :when (< 1 (count sides))]
      {:type type-key :api-name n :sides (mapv #(select-keys % [:link :dir]) sides)})))

(defn table-sides
  [links type]
  (let [t (->kw type)]
    (for [[l {:keys [from to forward-api-name reverse-api-name]}] links
          [dir near far nm] [[:to from to (or forward-api-name l)]
                             [:from to from (or reverse-api-name l)]]
          :when (= (->kw near) t)]
      {:api-name (->kw nm) :link (->kw l) :dir dir :far (->kw far)})))

(defn table-side
  [links type handle]
  (let [h     (->kw handle)
        cands (filterv #(= h (:api-name %)) (table-sides links type))]
    (when (= 1 (count cands)) (first cands))))

(defn table-entry
  [ct name-of]
  (cond-> {:from (name-of (:from-object-type ct)) :to (name-of (:to-object-type ct))}
    (:forward-api-name ct) (assoc :forward-api-name (:forward-api-name ct))
    (:reverse-api-name ct) (assoc :reverse-api-name (:reverse-api-name ct))))
