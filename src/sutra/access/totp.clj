(ns sutra.access.totp
  (:require [clojure.string :as str])
  (:import (java.net URLEncoder)
           (java.nio ByteBuffer)
           (java.security MessageDigest SecureRandom)
           (java.util Base64)
           (javax.crypto Cipher Mac)
           (javax.crypto.spec GCMParameterSpec SecretKeySpec)))

(set! *warn-on-reflection* true)

(def step-seconds 30)

(def digits 6)

(def ^:private seed-bytes 20)

(defonce ^:private rng (SecureRandom.))

(def ^:private alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567")

(defn base32
  [^bytes bs]
  (let [bits (apply str (map #(let [s (Integer/toBinaryString (bit-and % 0xff))]
                                (str (subs "00000000" (count s)) s))
                             bs))
        bits (str bits (subs "0000" 0 (mod (- 5 (mod (count bits) 5)) 5)))]
    (apply str (map #(.charAt ^String alphabet (Integer/parseInt (apply str %) 2))
                    (partition 5 bits)))))

(defn unbase32
  ^bytes [s]
  (let [s (str/upper-case (str/replace (str s) #"[\s=]" ""))]
    (when (and (seq s) (every? #(str/index-of alphabet %) s))
      (let [bits (apply str (map #(let [b (Integer/toBinaryString (str/index-of alphabet %))]
                                    (str (subs "00000" (count b)) b))
                                 s))]
        (byte-array (map #(unchecked-byte (Integer/parseInt (apply str %) 2))
                         (partition 8 bits)))))))

(defn new-seed
  []
  (let [b (byte-array seed-bytes)] (.nextBytes ^SecureRandom rng b) (base32 b)))

(defn step-at
  [ms]
  (quot (long ms) (* 1000 step-seconds)))

(defn code
  [seed step]
  (let [mac (doto (Mac/getInstance "HmacSHA1") (.init (SecretKeySpec. (unbase32 seed) "RAW")))
        h   (.doFinal mac (.array (.putLong (ByteBuffer/allocate 8) (long step))))
        o   (bit-and (aget h (dec (alength h))) 0x0f)
        n   (bit-or (bit-shift-left (bit-and (aget h o) 0x7f) 24)
                    (bit-shift-left (bit-and (aget h (+ o 1)) 0xff) 16)
                    (bit-shift-left (bit-and (aget h (+ o 2)) 0xff) 8)
                    (bit-and (aget h (+ o 3)) 0xff))
        s   (str (mod n 1000000))]
    (str (subs "000000" (count s)) s)))

(defn- same? [^String a ^String b]
  (MessageDigest/isEqual (.getBytes a "UTF-8") (.getBytes b "UTF-8")))

(defn verify
  [seed submitted ms last-step]
  (let [c (str/replace (str submitted) #"\s" "")
        s (step-at ms)]
    (when (and (re-matches #"\d{6}" c) (unbase32 seed))
      (some (fn [st] (when (and (same? c (code seed st)) (> st (long (or last-step -1)))) st))
            [(dec s) s (inc s)]))))

(defn uri
  [seed account issuer]
  (let [enc #(str/replace (URLEncoder/encode (str %) "UTF-8") "+" "%20")]
    (str "otpauth://totp/" (enc issuer) ":" (enc account)
         "?secret=" seed "&issuer=" (enc issuer)
         "&algorithm=SHA1&digits=" digits "&period=" step-seconds)))

(defn- aes-key ^SecretKeySpec [^bytes k]
  (SecretKeySpec. (.digest (MessageDigest/getInstance "SHA-256") k) "AES"))

(defn seal
  [k seed]
  (let [nonce (byte-array 12)
        _     (.nextBytes ^SecureRandom rng nonce)
        c     (doto (Cipher/getInstance "AES/GCM/NoPadding")
                (.init Cipher/ENCRYPT_MODE (aes-key k) (GCMParameterSpec. 128 nonce)))
        ct    (.doFinal c (.getBytes (str seed) "UTF-8"))
        out   (byte-array (+ 12 (alength ct)))]
    (System/arraycopy nonce 0 out 0 12)
    (System/arraycopy ct 0 out 12 (alength ct))
    (.encodeToString (Base64/getEncoder) out)))

(defn unseal
  [k s]
  (try
    (let [b (.decode (Base64/getDecoder) (str s))
          c (doto (Cipher/getInstance "AES/GCM/NoPadding")
              (.init Cipher/DECRYPT_MODE (aes-key k)
                     (GCMParameterSpec. 128 (java.util.Arrays/copyOfRange b 0 12))))]
      (String. (.doFinal c (java.util.Arrays/copyOfRange b 12 (alength b))) "UTF-8"))
    (catch javax.crypto.AEADBadTagException _ nil)
    (catch IllegalArgumentException _ nil)))
