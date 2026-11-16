(ns sutra.peering.wire
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [sutra.access.api :as access]
            [sutra.foundation.outbound :as out]
            [sutra.kit.refusal :as refusal]
            [sutra.sources.api :as sources]
            [cognitect.transit :as transit])
  (:import (java.io ByteArrayInputStream ByteArrayOutputStream)
           (java.net InetAddress URI)))

(set! *warn-on-reflection* true)

(def timeout-ms 15000)

(defn envelope
  [store payload]
  (let [text (pr-str payload)]
    (assoc (select-keys (access/sign-payload store text) [:iss :sig]) :payload text)))

(defn opened
  [store {:keys [payload iss sig]}]
  (let [signer (when (and (string? payload) (string? sig))
                 (access/signed-by store payload {:iss iss :sig sig}))
        data   (when signer (try (edn/read-string payload) (catch Exception _ nil)))]
    (cond
      (nil? signer)       {:refused "no trust of this instance signed the payload"}
      (= :self signer)    {:refused "an instance does not peer with itself"}
      (not (map? data))   {:refused "the payload does not read"}
      :else               [signer data])))

(defn- host-port [url]
  (let [u (URI. (str url))
        port (.getPort u)]
    (str (.getHost u) ":" (if (pos? port) port (if (= "https" (.getScheme u)) 443 80)))))

(defn- ->transit
  ^String [x]
  (let [out (ByteArrayOutputStream.)]
    (transit/write (transit/writer out :json) x)
    (.toString out "UTF-8")))

(defn- <-transit
  [body]
  (try (let [in (if (instance? java.io.InputStream body)
                  body
                  (ByteArrayInputStream. (.getBytes (str body) "UTF-8")))
             x  (transit/read (transit/reader in :json))]
         (when (map? x) x))
       (catch Exception _ nil)))

(defn trust-url
  [store trust]
  (or (get-in (access/instance-trusts store) [trust :url])
      (refusal/refuse! :refusal.peering/no-trust {:trust trust})))

(defn send!
  [store trust path payload]
  (let [url (str (trust-url store trust) path)]
    (sources/assert-egress-permitted! store {:egress [(host-port url)]
                                             :source-name (str "peer:" trust)})
    (let [{:keys [status body error]}
          (out/post! url {:headers {"Content-Type" "application/transit+json"
                                    "Accept"       "application/transit+json"}
                          :body    (->transit (envelope store payload))
                          :timeout timeout-ms})
          answer (<-transit body)]
      (cond
        error (refusal/refuse! :refusal.peering/peer-unreachable {:trust trust :url url}
                               {:why error})
        (not (<= 200 (long status) 299))
        (refusal/refuse! :refusal.peering/peer-refused {:trust trust :status status :answer answer}
                         {:why (or (:error answer)
                                   (subs (str body) 0 (min 200 (count (str body)))))})
        :else (or (some-> (:payload answer) edn/read-string) answer)))))

(def ^:dynamic *transport*
  nil)

(defn deliver!
  [store trust path payload]
  ((or *transport* send!) store trust path payload))

(defn- ip-bytes ^bytes [s]
  (try (.getAddress (InetAddress/getByName (str s))) (catch Exception _ nil)))

(defn- in-cidr? [ip cidr]
  (let [[net bits] (str/split (str cidr) #"/" 2)
        a (ip-bytes ip) n (ip-bytes net)]
    (when (and a n (= (alength a) (alength n)))
      (let [bits (if bits (parse-long bits) (* 8 (alength a)))]
        (every? (fn [i]
                  (let [take (min 8 (max 0 (- (long bits) (* 8 i))))
                        mask (bit-and 0xff (bit-shift-left 0xff (- 8 take)))]
                    (= (bit-and (aget a i) mask) (bit-and (aget n i) mask))))
                (range (alength a)))))))

(defn ingress-allows?
  [ingress ip]
  (or (empty? ingress) (boolean (some #(in-cidr? ip %) ingress))))
