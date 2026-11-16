(ns sutra.kit.uuid
  (:import (java.nio ByteBuffer)
           (java.security MessageDigest)
           (java.util UUID)))

(set! *warn-on-reflection* true)

(def dns-namespace
  (UUID/fromString "6ba7b810-9dad-11d1-80b4-00c04fd430c8"))

(defn- ns-bytes [^UUID u]
  (let [buf (ByteBuffer/allocate 16)]
    (.putLong buf (.getMostSignificantBits u))
    (.putLong buf (.getLeastSignificantBits u))
    (.array buf)))

(defn v5
  [^UUID nsuuid ^String s]
  (let [md (MessageDigest/getInstance "SHA-1")
        _  (.update md ^bytes (ns-bytes nsuuid))
        _  (.update md (.getBytes s "UTF-8"))
        digest (.digest md)
        bytes  (byte-array 16)]
    (System/arraycopy digest 0 bytes 0 16)
    (aset-byte bytes 6 (unchecked-byte (bit-or 0x50 (bit-and (aget bytes 6) 0x0f))))
    (aset-byte bytes 8 (unchecked-byte (bit-or 0x80 (bit-and (aget bytes 8) 0x3f))))
    (let [buf (ByteBuffer/wrap bytes)]
      (UUID. (.getLong buf) (.getLong buf)))))
