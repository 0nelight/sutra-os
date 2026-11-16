(ns sutra.access.password
  (:require [clojure.string :as str]
            [sutra.kit.digest :as digest])
  (:import (java.security MessageDigest SecureRandom)
           (java.util Base64)
           (org.bouncycastle.crypto.generators Argon2BytesGenerator)
           (org.bouncycastle.crypto.params Argon2Parameters Argon2Parameters$Builder)))

(set! *warn-on-reflection* true)

(def params {:m 19456 :t 2 :p 1})

(def ^:private salt-bytes 16)

(def ^:private hash-bytes 32)

(defonce ^:private rng (SecureRandom.))

(defn random-bytes ^bytes [n]
  (let [b (byte-array n)] (.nextBytes ^SecureRandom rng b) b))

(defn random-token
  ([] (random-token 32))
  ([n] (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) (random-bytes n))))

(defn- b64 ^String [^bytes b] (.encodeToString (.withoutPadding (Base64/getEncoder)) b))

(defn- unb64 ^bytes [^String s] (.decode (Base64/getDecoder) s))

(defn- argon2id
  ^bytes [^String password ^bytes salt {:keys [m t p]} n]
  (let [ps  (-> (Argon2Parameters$Builder. Argon2Parameters/ARGON2_id)
                (.withVersion Argon2Parameters/ARGON2_VERSION_13)
                (.withMemoryAsKB (int m))
                (.withIterations (int t))
                (.withParallelism (int p))
                (.withSalt salt)
                (.build))
        out (byte-array n)]
    (doto (Argon2BytesGenerator.) (.init ps) (.generateBytes (.getBytes password "UTF-8") out))
    out))

(defn encode
  ([password] (encode password params))
  ([password {:keys [m t p] :as ps}]
   (let [salt (random-bytes salt-bytes)]
     (str "$argon2id$v=19$m=" m ",t=" t ",p=" p "$" (b64 salt) "$"
          (b64 (argon2id (str password) salt ps hash-bytes))))))

(def ^:private phc
  (re-pattern (str "\\$argon2id\\$v=19\\$m=(\\d{1,7}),t=(\\d{1,3}),p=(\\d{1,2})"
                   "\\$([A-Za-z0-9+/]+)\\$([A-Za-z0-9+/]+)")))

(defn parse
  [stored]
  (let [s (str stored)]
    (if-let [[_ m t p salt h] (re-matches phc s)]
      {:algo   :argon2id
       :params {:m (parse-long m) :t (parse-long t) :p (parse-long p)}
       :salt   (unb64 salt) :hash (unb64 h)}
      (when-let [[_ salt h] (re-matches #"([^$]+)\$([0-9a-f]{64})" s)]
        {:algo :legacy :salt salt :hash h}))))

(defn- same-bytes? [^bytes a ^bytes b] (MessageDigest/isEqual a b))

(defn verify?
  [password stored]
  (let [{:keys [algo params salt hash]} (parse stored)
        pw (str password)]
    (boolean
     (case algo
       :argon2id (same-bytes? hash (argon2id pw salt params (alength ^bytes hash)))
       :legacy   (same-bytes? (.getBytes ^String hash "UTF-8")
                              (.getBytes ^String (digest/sha256-hex (str salt pw)) "UTF-8"))
       false))))

(defn needs-rehash?
  [stored]
  (let [{:keys [algo] ps :params} (parse stored)]
    (not (and (= :argon2id algo) (= ps params)))))

(def ^:private dummy (delay (encode (random-token))))

(defn burn!
  [password]
  (verify? password @dummy)
  false)

(defn initial-password
  []
  (random-token 16))

(defn hash-or-nil
  [password]
  (when-not (str/blank? (str (or password ""))) (encode password)))
