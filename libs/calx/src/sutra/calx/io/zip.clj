(ns sutra.calx.io.zip
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io FilterInputStream InputStream)
           (java.util.zip ZipEntry ZipInputStream)))

(set! *warn-on-reflection* true)

(def default-limits
  {:max-entries     10000
   :max-entry-bytes (* 64 1024 1024)
   :max-total-bytes (* 128 1024 1024)
   :max-ratio       100
   :ratio-floor     (* 1024 1024)})

(defn- refuse! [reason data]
  (throw (ex-info (str "zip refused: " reason) (assoc data :reason reason :zip/refused true))))

(defn safe-entry-name?
  [^String name]
  (and (string? name)
       (not (str/blank? name))
       (not (str/includes? name "\\"))
       (not (str/includes? name "\u0000"))
       (not (str/starts-with? name "/"))
       (not (re-find #"^[A-Za-z]:" name))
       (not-any? #{".."} (str/split name #"/"))))

(defn- counting ^FilterInputStream [^InputStream in counter]
  (proxy [FilterInputStream] [in]
    (read
      ([] (let [b (.read ^InputStream in)] (when (>= b 0) (vswap! counter inc)) b))
      ([bs] (let [n (.read ^InputStream in ^bytes bs)] (when (pos? n) (vswap! counter + n)) n))
      ([bs off len]
       (let [n (.read ^InputStream in ^bytes bs (int off) (int len))]
         (when (pos? n) (vswap! counter + n))
         n)))
    (skip [n] (let [k (.skip ^InputStream in (long n))] (vswap! counter + k) k))))

(defn- read-entry
  ^bytes [^ZipInputStream zin ^ZipEntry e room raw limits]
  (let [{:keys [max-entry-bytes max-ratio ratio-floor]} limits
        cap    (min room max-entry-bytes)
        raw0   @raw
        out    (java.io.ByteArrayOutputStream.)
        buf    (byte-array 65536)]
    (loop [n 0]
      (let [k (.read zin buf 0 (alength buf))]
        (if (neg? k)
          (.toByteArray out)
          (let [n (+ n k)]
            (when (> n cap)
              (refuse! (if (> n max-entry-bytes) :entry-too-large :total-too-large)
                       {:entry (.getName e) :limit cap}))
            (when (and (> n ratio-floor) (> n (* max-ratio (max 1 (- @raw raw0)))))
              (refuse! :ratio {:entry (.getName e) :max-ratio max-ratio}))
            (.write out buf 0 k)
            (recur n)))))))

(defn read-entries
  ([in] (read-entries in {}))
  ([in limits]
   (let [limits (merge default-limits limits)
         raw    (volatile! 0)]
     (with-open [zin (ZipInputStream. (counting (io/input-stream in) raw))]
       (loop [m {} entries 0 room (:max-total-bytes limits)]
         (if-let [e (.getNextEntry zin)]
           (let [nm (.getName e)]
             (when (>= entries (:max-entries limits))
               (refuse! :too-many-entries {:limit (:max-entries limits)}))
             (when-not (safe-entry-name? nm) (refuse! :unsafe-name {:entry nm}))
             (if (.isDirectory e)
               (recur m (inc entries) room)
               (let [bs (read-entry zin e room raw limits)]
                 (recur (assoc m nm bs) (inc entries) (- room (alength bs))))))
           m))))))

(defn bounded
  ^InputStream [^InputStream in max-bytes]
  (let [n (volatile! 0)
        check! (fn [k]
                 (when (pos? k)
                   (when (> (vswap! n + k) max-bytes)
                     (throw (java.io.IOException.
                             (str "stream exceeds " max-bytes " bytes")))))
                 k)]
    (proxy [FilterInputStream] [in]
      (read
        ([] (let [b (.read in)] (when (>= b 0) (check! 1)) b))
        ([bs] (check! (.read in ^bytes bs)))
        ([bs off len] (check! (.read in ^bytes bs (int off) (int len)))))
      (skip [k] (check! (.skip in (long k)))))))
