(ns support.store-catalog
  (:require [support.system :as tsys]
            [sutra.store.pg.dataset-store :as dstore]))

(defn- with-table [branch table f]
  (let [t (str table)]
    (dstore/with-store* (tsys/ds)
      (fn [store] (f store (dstore/read-scope store (or branch "main") t) t)))))

(defn table?
  [branch table]
  (with-table branch table (fn [s sc t] (dstore/table-exists? s sc t))))

(defn columns
  [branch table]
  (with-table branch table (fn [s sc t] (into #{} (map keyword) (dstore/columns s sc t)))))

(defn column-type
  [branch table col]
  (with-table branch table (fn [s sc t] (get (dstore/column-types s sc t) (str col)))))
