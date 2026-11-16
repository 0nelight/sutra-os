(ns sutra.builds.http.files
  (:require [clojure.data.csv :as csv]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [sutra.builds.interchange :as ix]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [sutra.compute.api :as compute]
            [sutra.http.endpoint :as ep]
            [sutra.http.responses :refer [ok fault]]
            [sutra.kit.refusal :as refusal]
            [sutra.store.access.dataset-rows :as dsrows]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.access.scope :as ctx]
            [sutra.types.granular :as granular]
            [sutra.types.virtual :as vtypes])
  (:import (java.io ByteArrayOutputStream InputStream)
           (java.util Base64)))

(set! *warn-on-reflection* true)

(def ^:private driver "import sutra_interchange\nsutra_interchange.main()\n")

(defn- format-of [s]
  (let [f (keyword (str/lower-case (or (not-empty (str s)) "csv")))]
    (when (contains? ix/formats f) f)))

(defn- convert!
  [job]
  (let [out (compute/run-python! {:label :dataset-interchange :code driver
                                  :stdin (json/write-str job)})
        res (try (json/read-str out)
                 (catch Exception e
                   {"ok"    false
                    "error" (str "the converter produced no result: " (ex-message e))}))]
    (if (get res "ok")
      res
      (refusal/refuse! :refusal.data/file-conversion {:error (get res "error")}
                       {:format (str (get job "format"))}))))

(defn- dataset-of [schema id]
  (let [rid (if (contains? (:datasets schema) id) id
                (lookup/rid-of schema :datasets (keyword (str id))))]
    [rid (get-in schema [:datasets rid])]))

(defn- readable-rows
  [store schema rid ds as-of]
  (if (= :restricted-view (:kind ds))
    (let [{x :dataset policy :policy} (:restricted-view ds)
          backing (if (contains? (:datasets schema) x) x (lookup/rid-of schema :datasets x))]
      [(get-in schema [:datasets backing :columns])
       (granular/restricted-rows policy (grants/read-rights store)
                                 (dsets/preview-rows store backing as-of
                                                     (get-in schema [:datasets backing
                                                                     :columns])))])
    [(:columns ds) (dsets/preview-rows store rid as-of (:columns ds))]))

(defn- file-bytes
  ^bytes [fmt columns rows]
  (let [names (mapv (comp name :attribute-name) columns)
        maps  (mapv (fn [{:keys [values]}]
                      (into {} (map (fn [n] [n (ix/plain (get values (keyword n)))])) names))
                    rows)]
    (if (= :csv fmt)
      (.getBytes ^String (ix/csv names (mapv (fn [m] (mapv #(get m %) names)) maps)) "UTF-8")
      (.decode (Base64/getDecoder)
               ^String (get (convert! {"op" "encode" "format" (name fmt)
                                       "columns" (mapv ix/column-json columns)
                                       "rows" maps})
                            "bytes")))))

(def dataset-export
  (ep/handler
   :dataset-export
   {:respond
    (fn [{:keys [store branch schema id as-of] :as a}]
      (let [[rid ds] (dataset-of schema id)
            fmt      (format-of (:format a))]
        (cond
          (nil? ds)  (refusal/refuse! :refusal.http/unknown-dataset {:id id})
          (nil? fmt) (refusal/refuse! :refusal.builds/unknown-format
                                      {:format (:format a) :formats (vec (keys ix/formats))})
          (vtypes/virtual? ds)
          (refusal/refuse! :refusal.builds/virtual-no-rows {:id id})
          (seq (:markings ds))
          (refusal/refuse! :refusal.data/file-export-markings
                           {:markings (vec (:markings ds))} {:id (str id)})
          :else
          (try
            (let [[columns rows] (readable-rows (ctx/at store branch) schema rid ds as-of)
                  nm (name (or (:dataset-name ds) rid))
                  {:keys [media-type extension]} (ix/formats fmt)]
              {:status  200
               :headers {"Content-Type" media-type
                         "Content-Disposition" (str "attachment; filename=\"" nm "."
                                                    extension "\"")}
               :body    (file-bytes fmt columns rows)})
            (catch InterruptedException e (throw e))
            (catch clojure.lang.ExceptionInfo e (throw e))
            (catch Exception e (fault e {:id id :format fmt}))))))}))

(defn- slurp-bounded
  ^bytes [^InputStream in]
  (let [out (ByteArrayOutputStream.)
        buf (byte-array 65536)]
    (loop [n 0]
      (let [r (.read in buf)]
        (cond (neg? r)                          (.toByteArray out)
              (> (+ n r) ix/max-file-bytes)     nil
              :else (do (.write out buf 0 r) (recur (+ n r))))))))

(defn- csv-maps [^bytes data]
  (let [[header & rows] (csv/read-csv (String. data "UTF-8"))
        header (mapv #(str/replace (str/trim %) "﻿" "") header)]
    (mapv #(zipmap header %) rows)))

(defn- decoded [fmt ^bytes data]
  (if (= :csv fmt)
    (csv-maps data)
    (get (convert! {"op" "decode" "format" (name fmt)
                    "bytes" (.encodeToString (Base64/getEncoder) data)})
         "rows")))

(def dataset-import
  (ep/handler
   :dataset-import
   {:respond
    (fn [{:keys [store branch schema mode key req] nm :name :as a}]
      (let [[rid ds] (dataset-of schema nm)
            fmt      (format-of (:format a))
            mode     (keyword (or mode "append"))
            key-col  (some-> key keyword)]
        (cond
          (nil? ds)  (refusal/refuse! :refusal.http/unknown-dataset {:name nm})
          (nil? fmt) (refusal/refuse! :refusal.builds/unknown-format {:format (:format a)})
          (not (#{:append :snapshot} mode))
          (refusal/refuse! :refusal.builds/upload-mode {:mode mode})
          (nil? (:body req)) (refusal/refuse! :refusal.builds/empty-upload {})
          :else
          (if-let [data (slurp-bounded (:body req))]
            (let [rows (ix/file-rows (:columns ds) (decoded fmt data))]
              (if-let [{:keys [message data]} (dsrows/write-problem rid ds nm rows mode
                                                                    key-col)]
                (refusal/refuse! :refusal.builds/rows-refused (or data {}) {:message message})
                (let [store (ctx/at store branch)]
                  (dsets/ensure-datasets! store {rid (:columns ds)})
                  (dsets/write-rows! store rid mode (dsrows/records nm key-col rows))
                  (ok {:dataset nm :format fmt :written (count rows) :mode mode
                       :key key-col :branch branch}))))
            (refusal/refuse! :refusal.builds/file-too-large {:limit ix/max-file-bytes})))))}))
