(ns sutra.types.naming
  (:require [clojure.string :as str]
            [sutra.types.column :as column]))

(set! *warn-on-reflection* true)

(def ^:const pk-col "internal_uuid")

(def ^:const markings-col "_markings")

(def table-families
  [{:family :datasets :prefix "d_"}
   {:family :links    :prefix "c_"}
   {:family :streams  :prefix "h_"}])

(def ^:private prefix-of
  (into {} (map (juxt :family :prefix)) table-families))

(def dataset-prefix (prefix-of :datasets))
(def stream-prefix (prefix-of :streams))

(defn dataset-table
  [ds-kw] (str dataset-prefix (column/sql-name ds-kw)))

(defn trans-table [t-kw]     (dataset-table t-kw))

(defn stream-table
  [ds-kw] (str stream-prefix (column/sql-name ds-kw)))

(defn serving-table
  [obj-kw] (dataset-table obj-kw))

(defn obj-table
  [obj-kw] (serving-table obj-kw))

(defn read-key
  [attr]
  (keyword (str/lower-case (column/sql-name attr))))

(defn column-aliases
  [attrs]
  (into {}
        (keep (fn [a]
                (let [declared (keyword (name a))
                      k        (read-key a)]
                  (when (not= declared k) [declared k]))))
        attrs))

(defn declared-aliases
  [attrs]
  (into {} (map (fn [[declared k]] [k declared])) (column-aliases attrs)))

(defn column-key
  [attr]
  (keyword (str/replace (column/sql-name attr) "_" "-")))
