(ns sutra.foundation.media-grant
  (:require [sutra.foundation.egress :as egress-agents]
            [clojure.string :as str]
            [sutra.foundation.rpc :as rpc]
            [sutra.types.paths :as paths])
  (:import (java.security MessageDigest)))

(set! *warn-on-reflection* true)

(def ttl-ms
  (* 12 60 60 1000))

(defn- mac [master exp path] (rpc/token-for master (str "build-grant:" exp ":" path)))

(defn item-path [address] (paths/url :xfer-media {:hash address}))

(defn issue
  ([refs now-ms] (issue (egress-agents/egress-master) refs now-ms))
  ([master refs now-ms]
   (let [exp (+ (long now-ms) (long ttl-ms))]
     (into {} (map (fn [r] [r (str exp "." (mac master exp (item-path r)))])) refs))))

(defn admits?
  ([tok path now-ms] (admits? (egress-agents/egress-master) tok path now-ms))
  ([master tok path now-ms]
   (let [[exp sig] (str/split (str tok) #"\." 2)
         exp       (some-> exp parse-long)]
     (boolean
      (and exp sig (< (long now-ms) (long exp))
           (MessageDigest/isEqual (.getBytes ^String (mac master exp path) "UTF-8")
                                  (.getBytes ^String sig "UTF-8")))))))
