(ns sutra.store.access.row-shape
  (:require [sutra.types.naming :as naming]
            [sutra.types.column :as column]
            [clojure.data.json :as json]
            [sutra.kit.system :as sys]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn ->row
  [pk {:keys [id values]}]
  (into {pk (str id)}
        (map (fn [[k v]] [(column/sql-name k) v]))
        values))

(defn array-keys-for
  [store schema table-name key-fn]
  (into #{} (map key-fn) (some-> (sys/provided (sys/of store) :schema/array-cols)
                                 (apply [schema table-name]))))

(defn decode-array-vals
  [array-keys values]
  (reduce (fn [m k]
            (let [v (get m k)]
              (if (string? v) (assoc m k (json/read-str v :key-fn keyword)) m)))
          values array-keys))

(defn snake-key [col] (keyword col))

(defn array-keys
  [store schema table-name]
  (array-keys-for store schema table-name snake-key))

(defn project-row-values
  [array-keys rows]
  (mapv (fn [r] (->> (dissoc r
                             (keyword naming/pk-col)
                             (keyword naming/markings-col))
                     (decode-array-vals array-keys)))
        rows))

(def deletion-key :is_deleted)

(defn tombstone?
  [row]
  (let [v (get row deletion-key)]
    (cond
      (nil? v)     false
      (boolean? v) v
      (number? v)  (not (zero? v))
      (string? v)  (not (contains? #{"" "0" "false" "f" "no"} (str/lower-case v)))
      :else        true)))

(defn strip-deletion-key
  [row]
  (dissoc row deletion-key))
