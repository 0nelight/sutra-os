(ns sutra.types.settings
  (:require [clojure.edn :as edn]
            [clojure.string :as str])
  (:import (java.util Base64)))

(set! *warn-on-reflection* true)

(defn- blank? [v] (str/blank? (str (or v ""))))

(defn- read-edn [v] (try (edn/read-string v) (catch RuntimeException _ ::invalid)))

(defn decode
  [fields rows]
  (into {}
        (for [[f {:keys [key type default]}] fields
              :let [raw (get rows key)]]
          [f (case type
               :boolean (= "true" raw)
               :edn     (let [v (when-not (blank? raw) (read-edn raw))]
                          (if (or (nil? v) (= ::invalid v)) default v))
               (if (blank? raw) default raw))])))

(defn view
  [fields cfg ready?]
  (into {:ready (boolean ready?)}
        (for [[f {:keys [secret?]}] fields]
          (if secret?
            [(keyword (str (name f) "?")) (not (blank? (get cfg f)))]
            [f (get cfg f)]))))

(defn- input [f {in :input}] (or in f))

(defn problem
  [fields opts]
  (some (fn [[f {:keys [type] :as spec}]]
          (let [v (get opts (input f spec))]
            (when-not (blank? v)
              (case type
                :edn    (when (= ::invalid (read-edn v))
                          {:error (str (name (input f spec)) " is not valid EDN")
                           :field (input f spec)})
                :base64 (when (try (.decode (Base64/getDecoder) ^String v) false
                                (catch IllegalArgumentException _ true))
                          {:error (str (name (input f spec)) " is not valid base64")
                           :field (input f spec)})
                nil))))
        fields))

(defn rows
  [fields opts]
  (into {}
        (keep (fn [[f {:keys [key type] :as spec}]]
                (let [v (get opts (input f spec))]
                  (cond
                    (= :boolean type) (when (some? v) [key (str (boolean v))])
                    (blank? v)        nil
                    :else             [key (str v)]))))
        fields))
