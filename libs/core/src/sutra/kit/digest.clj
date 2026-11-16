(ns sutra.kit.digest
  (:require [clojure.string :as str])
  (:import (java.security MessageDigest)))

(set! *warn-on-reflection* true)

(defn hex
  [^bytes bs]
  (let [sb (StringBuilder. (* 2 (alength bs)))]
    (doseq [b bs]
      (.append sb (Character/forDigit (bit-and (bit-shift-right b 4) 0xf) 16))
      (.append sb (Character/forDigit (bit-and b 0xf) 16)))
    (str sb)))

(defn- ->bytes ^bytes [x]
  (if (bytes? x) x (.getBytes (str x) "UTF-8")))

(defn digest
  ^bytes [^String algo x]
  (.digest (MessageDigest/getInstance algo) (->bytes x)))

(defn sha256 ^bytes [x] (digest "SHA-256" x))

(defn sha256-hex
  [x]
  (hex (sha256 x)))

(defn salted
  [salt secret]
  (str salt "$" (sha256-hex (str salt secret))))

(defn same?
  [a b]
  (boolean (and (seq (str a)) (seq (str b))
                (MessageDigest/isEqual (->bytes (str a)) (->bytes (str b))))))

(defn salted-ok?
  [secret stored]
  (let [[salt h] (str/split (str stored) #"\$" 2)]
    (boolean (and salt h (same? h (sha256-hex (str salt secret)))))))
