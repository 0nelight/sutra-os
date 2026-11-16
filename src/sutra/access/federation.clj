(ns sutra.access.federation
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [sutra.kit.refusal :as refusal]
            [sutra.kit.digest :as digest]
            [sutra.store.storage.instance-config :as ic])
  (:import (java.security KeyFactory KeyPairGenerator PrivateKey PublicKey Signature)
           (java.security.spec PKCS8EncodedKeySpec X509EncodedKeySpec)
           (java.util Base64)))

(set! *warn-on-reflection* true)

(def ^:private key-cfg "federation_signing_key")
(def ^:private trusts-cfg "federation_trusts")
(def ^:private seen-cfg "federation_seen_assertions")

(def assertion-ttl-ms 60000)

(defn- b64 [^bytes b] (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) b))

(defn- unb64 ^bytes [s]
  (try (.decode (Base64/getUrlDecoder) (str s))
       (catch IllegalArgumentException _ nil)))

(defn- public-of ^PublicKey [s]
  (when-let [b (unb64 s)]
    (try (.generatePublic (KeyFactory/getInstance "Ed25519") (X509EncodedKeySpec. b))
         (catch java.security.GeneralSecurityException _ nil))))

(defn- private-of ^PrivateKey [s]
  (.generatePrivate (KeyFactory/getInstance "Ed25519") (PKCS8EncodedKeySpec. (unb64 s))))

(defn- fresh-keys []
  (let [kp (.generateKeyPair (KeyPairGenerator/getInstance "Ed25519"))]
    {:private (b64 (.getEncoded (.getPrivate kp)))
     :public  (b64 (.getEncoded (.getPublic kp)))}))

(defn fingerprint
  [public-key]
  (->> (subs (digest/sha256-hex (str public-key)) 0 32)
       (partition 4) (map str/join) (str/join ":")))

(defn- own-keys
  [store]
  (let [v (ic/get-cfg store key-cfg "")]
    (edn/read-string
     (if (str/blank? (str v))
       (ic/swap-cfg! store key-cfg (fn [v] (if (str/blank? (str v)) (pr-str (fresh-keys)) v)))
       v))))

(defn identity-of
  [store]
  (let [{:keys [public]} (own-keys store)]
    {:public-key public :fingerprint (fingerprint public)}))

(defn trusts
  [store]
  (let [v (ic/get-cfg store trusts-cfg "")]
    (if (str/blank? (str v)) {} (edn/read-string (str v)))))

(defn- view [[nm t]]
  (-> (select-keys t [:url :inbound? :outbound? :subjects :declared-at :declared-by])
      (assoc :name nm :fingerprint (fingerprint (:public-key t)))))

(defn trust-rows [store] (mapv view (sort-by key (trusts store))))

(defn instances
  [store]
  (vec (for [[nm t] (sort-by key (trusts store)) :when (:outbound? t)]
         {:name nm :url (:url t)})))

(def ^:private name-shape #"[a-z0-9][a-z0-9-]{0,62}")

(defn- url-problem [url]
  (let [^java.net.URI u (try (java.net.URI. (str url))
                            (catch java.net.URISyntaxException _ nil))]
    (when-not (and u (#{"http" "https"} (.getScheme u)) (.getHost u)
                   (str/blank? (.getRawQuery u)) (str/blank? (.getRawFragment u)))
      [:refusal.access/trust-url {:url url}])))

(defn trust-problem
  [own-public {nm :name :keys [url public-key subjects]}]
  (cond
    (not (and (string? nm) (re-matches name-shape nm)))
    [:refusal.access/trust-name {:given nm}]
    (url-problem url) (url-problem url)
    (nil? (public-of public-key))
    [:refusal.access/trust-public-key {}]
    (= public-key own-public) [:refusal.access/trust-self {}]
    (not (and (map? (or subjects {}))
              (every? #(and (string? (key %)) (string? (val %))) subjects)))
    [:refusal.access/trust-subjects {}]))

(defn- subjects-of
  [s]
  (cond
    (nil? s)    {}
    (string? s) (into {} (for [pair (str/split s #"\s*,\s*") :when (not (str/blank? pair))]
                           (let [[r l] (str/split pair #"=" 2)] [r l])))
    (map? s)    (update-keys s #(if (keyword? %) (subs (str %) 1) %))
    :else       s))

(defn- truthy? [v] (or (true? v) (= "true" v)))

(defn declare-trust!
  [store by m]
  (let [m   (-> m
                (update :subjects subjects-of)
                (update :inbound? truthy?)
                (update :outbound? truthy?))
        own (:public-key (identity-of store))]
    (when-let [[code params] (trust-problem own m)]
      (refusal/refuse! code {:name (:name m)} params))
    (let [row {:url        (str/replace (str (:url m)) #"/+$" "")
               :public-key (:public-key m)
               :inbound?   (:inbound? m)
               :outbound?  (:outbound? m)
               :subjects   (into (sorted-map) (:subjects m))
               :declared-at (System/currentTimeMillis)
               :declared-by by}]
      (ic/swap-cfg! store trusts-cfg
                    (fn [v] (pr-str (assoc (if (str/blank? (str v)) {} (edn/read-string v))
                                           (:name m) row))))
      (view [(:name m) row]))))

(defn revoke-trust!
  [store nm]
  (let [had? (contains? (trusts store) nm)]
    (when had?
      (ic/swap-cfg! store trusts-cfg (fn [v] (pr-str (dissoc (edn/read-string v) nm)))))
    had?))

(defn- sign ^String [^PrivateKey k ^String payload]
  (let [s (doto (Signature/getInstance "Ed25519")
            (.initSign k) (.update (.getBytes payload "UTF-8")))]
    (b64 (.sign s))))

(defn- signed? [^PublicKey k ^String payload sig]
  (boolean
   (when-let [b (unb64 sig)]
     (try (let [s (doto (Signature/getInstance "Ed25519")
                    (.initVerify k) (.update (.getBytes payload "UTF-8")))]
            (.verify s b))
          (catch java.security.GeneralSecurityException _ false)))))

(defn assertion
  [store nm username now-ms]
  (when-let [t (get (trusts store) nm)]
    (when (:outbound? t)
      (let [{:keys [private public]} (own-keys store)
            payload (pr-str {:iss (fingerprint public)
                             :aud (fingerprint (:public-key t))
                             :sub (str username)
                             :exp (+ (long now-ms) assertion-ttl-ms)
                             :jti (str (random-uuid))})
            body    (b64 (.getBytes payload "UTF-8"))]
        {:assertion (str body "." (sign (private-of private) payload))
         :url       (:url t)}))))

(defn- read-claims [body]
  (try (let [m (edn/read-string (String. (unb64 body) "UTF-8"))]
         (when (map? m) m))
       (catch Exception _ nil)))

(defn verify
  [store assertion now-ms]
  (let [[body sig] (str/split (str assertion) #"\." 2)
        payload    (some-> body unb64 (String. "UTF-8"))
        claims     (when body (read-claims body))
        own        (fingerprint (:public-key (identity-of store)))
        [nm t]     (first (filter #(= (:iss claims) (fingerprint (:public-key (val %))))
                                  (trusts store)))]
    (cond
      (nil? claims)                         {:refused "malformed assertion"}
      (nil? t)                              {:refused "no trust for this issuer"}
      (not (:inbound? t))                   {:refused "the trust does not accept logins"}
      (not (signed? (public-of (:public-key t)) payload sig)) {:refused "bad signature"}
      (not= own (:aud claims))              {:refused "addressed to another instance"}
      (not (and (int? (:exp claims)) (< (long now-ms) (long (:exp claims)))))
      {:refused "expired"}
      (not (string? (:jti claims)))         {:refused "malformed assertion"}
      :else {:trust nm :subject (:sub claims) :claims claims
             :local (get-in t [:subjects (:sub claims)])})))

(defn sign-payload
  [store ^String payload]
  (let [{:keys [private public]} (own-keys store)]
    {:iss (fingerprint public) :public-key public :sig (sign (private-of private) payload)}))

(defn signature-valid?
  [public-key ^String payload sig]
  (if-let [k (public-of public-key)] (signed? k payload sig) false))

(defn trust-by-fingerprint
  [store fp]
  (first (filter #(= fp (fingerprint (:public-key (val %)))) (trusts store))))

(defn signed-by
  [store ^String payload {:keys [iss sig]}]
  (let [own (:public-key (identity-of store))]
    (if (= iss (fingerprint own))
      (when (signature-valid? own payload sig) :self)
      (when-let [[nm t] (trust-by-fingerprint store iss)]
        (when (signature-valid? (:public-key t) payload sig) nm)))))

(defn spend!
  [store {:keys [jti exp]} now-ms]
  (let [fresh? (volatile! false)]
    (ic/swap-cfg! store seen-cfg
                  (fn [v]
                    (let [seen (into {} (filter #(< (long now-ms) (long (val %))))
                                     (if (str/blank? (str v)) {} (edn/read-string v)))]
                      (vreset! fresh? (not (contains? seen jti)))
                      (pr-str (assoc seen jti exp)))))
    @fresh?))
