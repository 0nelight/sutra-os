(ns sutra.cli.seal
  (:import [java.security MessageDigest SecureRandom KeyFactory]
           [java.security.spec X509EncodedKeySpec]
           [java.util Base64]
           [javax.crypto Cipher]))

(def ^:private h-len
  32)

(defn- sha256 ^bytes [^bytes b]
  (.digest (MessageDigest/getInstance "SHA-256") b))

(defn- i2osp-4
  ^bytes [^long i]
  (byte-array [(unchecked-byte (bit-shift-right i 24))
               (unchecked-byte (bit-shift-right i 16))
               (unchecked-byte (bit-shift-right i 8))
               (unchecked-byte i)]))

(defn- mgf1
  ^bytes [^bytes seed ^long len]
  (let [out (byte-array len)]
    (loop [counter 0 written 0]
      (if (>= written len)
        out
        (let [block (sha256 (byte-array (concat seed (i2osp-4 counter))))
              n     (min h-len (- len written))]
          (System/arraycopy block 0 out written n)
          (recur (inc counter) (+ written n)))))))

(defn- xor-bytes ^bytes [^bytes a ^bytes b]
  (let [out (byte-array (alength a))]
    (dotimes [i (alength a)]
      (aset-byte out i (unchecked-byte (bit-xor (aget a i) (aget b i)))))
    out))

(defn- raw-rsa-cipher
  ^Cipher [pub]
  (doto (Cipher/getInstance "RSA/ECB/NoPadding")
    (.init Cipher/ENCRYPT_MODE pub)))

(defn- modulus-bytes
  ^long [pub]
  (alength ^bytes (.doFinal (raw-rsa-cipher pub) (byte-array 1))))

(defn- public-key
  [^String spki-b64]
  (.generatePublic (KeyFactory/getInstance "RSA")
                   (X509EncodedKeySpec. (.decode (Base64/getDecoder) spki-b64))))

(defn- eme-oaep-encode
  ^bytes [^long k ^bytes m]
  (let [ps-len (- k (alength m) (* 2 h-len) 2)
        _      (when (neg? ps-len)
                 (throw (ex-info (str "secret too large to seal: " (alength m)
                                      " bytes, max " (- k (* 2 h-len) 2)
                                      " for this key")
                                 {:bytes (alength m) :modulus-bytes k})))
        db     (byte-array (concat (sha256 (byte-array 0))
                                   (byte-array ps-len)
                                   [(byte 1)]
                                   m))
        seed   (let [s (byte-array h-len)] (.nextBytes (SecureRandom.) s) s)
        masked-db   (xor-bytes db (mgf1 seed (alength db)))
        masked-seed (xor-bytes seed (mgf1 masked-db h-len))]
    (byte-array (concat [(byte 0)] masked-seed masked-db))))

(defn seal
  ^String [^String pubkey-b64 ^String plaintext]
  (let [pub (public-key pubkey-b64)
        em  (eme-oaep-encode (modulus-bytes pub) (.getBytes plaintext "UTF-8"))]
    (.encodeToString (Base64/getEncoder) (.doFinal (raw-rsa-cipher pub) em))))
