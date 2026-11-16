(ns sutra.sources.proxy.frames
  (:require [clojure.edn :as edn])
  (:import (java.nio ByteBuffer)
           (java.nio.charset StandardCharsets)))

(set! *warn-on-reflection* true)

(def window-bytes (* 1024 1024))

(def chunk-bytes (* 64 1024))

(def ^:private type->code {:open 1 :opened 2 :data 3 :close 4 :window 5 :config 6})

(def ^:private code->type (into {} (map (fn [[k v]] [v k])) type->code))

(defn encode
  ^ByteBuffer [type id payload]
  (let [^bytes payload (or payload (byte-array 0))
        ^ByteBuffer buf (ByteBuffer/allocate (+ 5 (alength payload)))]
    (.put buf (byte (type->code type)))
    (.putInt buf (int id))
    (.put buf payload)
    (.flip buf)
    buf))

(defn decode
  [^ByteBuffer buf]
  (let [buf     (.duplicate buf)
        code    (.get buf)
        id      (.getInt buf)
        payload (byte-array (.remaining buf))]
    (.get buf payload)
    {:type (or (code->type code) (throw (ex-info "unknown proxy frame" {:code code})))
     :id id :payload payload}))

(defn edn-bytes ^bytes [v] (.getBytes (pr-str v) StandardCharsets/UTF_8))

(defn read-edn [^bytes b]
  (when (pos? (alength b)) (edn/read-string (String. b StandardCharsets/UTF_8))))

(defn credit-bytes ^bytes [n] (.array (.putInt (ByteBuffer/allocate 4) (int n))))

(defn read-credit [^bytes b] (.getInt (ByteBuffer/wrap b)))

(defn collector
  []
  (let [parts (java.io.ByteArrayOutputStream.)]
    (fn [^ByteBuffer buf last?]
      (let [b (byte-array (.remaining buf))]
        (.get (.duplicate buf) b)
        (.write parts b)
        (when last?
          (let [whole (ByteBuffer/wrap (.toByteArray parts))]
            (.reset parts)
            whole))))))
