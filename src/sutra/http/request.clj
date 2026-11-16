(ns sutra.http.request
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn bearer-token [req]
  (some->> (get-in req [:headers "authorization"])
           (re-find #"(?i)^Bearer\s+(.+)$")
           second))

(defn loopback?
  [addr]
  (let [a (str addr)]
    (or (str/starts-with? a "127.")
        (contains? #{"::1" "0:0:0:0:0:0:0:1" "[::1]"} a))))

(def ^:private configured-proxies
  (delay (into #{} (comp (map str/trim) (remove str/blank?))
               (str/split (str (System/getenv "SUTRA_TRUSTED_PROXIES")) #","))))

(defn trusted-proxy?
  [addr]
  (or (loopback? addr) (contains? @configured-proxies (str addr))))

(defn- header [req k] (some-> (get-in req [:headers k]) str str/trim not-empty))

(defn client-ip
  ([req] (client-ip req trusted-proxy?))
  ([{:keys [remote-addr] :as req} trusted?]
   (or (when (trusted? remote-addr)
         (or (header req "x-real-ip")
             (some-> (header req "x-forwarded-for") (str/split #",") last str/trim not-empty)))
       remote-addr)))

(defn secure?
  ([req] (secure? req trusted-proxy?))
  ([{:keys [scheme remote-addr] :as req} trusted?]
   (or (= :https scheme)
       (and (trusted? remote-addr)
            (= "https" (some-> (header req "x-forwarded-proto") str/lower-case))))))
