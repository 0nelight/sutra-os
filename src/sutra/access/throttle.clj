(ns sutra.access.throttle
  (:require [clojure.string :as str]
            [sutra.http.responses :as responses]
            [sutra.http.security-log :as seclog]
            [sutra.kit.rate-limit :as rate]
            [sutra.access.ports :as ports]
            [sutra.store.control :as ctl]))

(set! *warn-on-reflection* true)

(def ^:private window-ms (* 15 60 1000))

(def policy
  {:account {:capacity 5 :per-ms window-ms}
   :ip      {:capacity 20 :per-ms window-ms}
   :client  {:capacity 10 :per-ms window-ms}})

(defn failed
  [state [kind :as key] now]
  (update state kind #(:state (rate/spend % (policy kind) key now 1))))

(defn succeeded
  [state ks]
  (reduce (fn [s [kind :as k]]
            (cond-> s (and (= :account kind) (contains? s kind)) (update kind rate/forget k)))
          state ks))

(defn retry-after-ms
  [state ks now]
  (reduce max 0 (for [[kind :as k] ks] (rate/wait-ms (get state kind) (policy kind) k now 1))))

(defn keys-for
  [{:keys [account ip client]}]
  (cond-> []
    (not (str/blank? (str account))) (conj [:account (str/lower-case (str/trim (str account)))])
    (not (str/blank? (str ip)))      (conj [:ip (str ip)])
    (not (str/blank? (str client)))  (conj [:client (str client)])))

(defn- state
  [req]
  (ports/throttle-buckets (:sutra/store req)))

(defn- now
  [req]
  (ctl/millis (:sutra/store req)))

(defn retry-after-seconds
  [req m]
  (let [ms (retry-after-ms @(state req) (keys-for m) (now req))]
    (when (pos? ms) (long (Math/ceil (/ ms 1000.0))))))

(defn- failed!
  [req m]
  (let [t (now req)]
    (swap! (state req) #(reduce (fn [s k] (failed s k t)) % (keys-for m))))
  nil)

(defn- succeeded!
  [req m]
  (swap! (state req) succeeded (keys-for m))
  nil)

(defn too-many
  [seconds]
  (responses/too-many "too many failed sign-in attempts — try again later"
                      (* 1000 seconds)))

(defn throttled
  [req attempt fields f]
  (if-let [wait (retry-after-seconds req attempt)]
    (do (seclog/security-event! :login/throttled req :denied (assoc fields :retry-after wait))
        (too-many wait))
    (f)))

(defn attempted!
  ([req attempt ok? fields]
   (attempted! req attempt ok? fields (if ok? :login/success :login/failure)))
  ([req attempt ok? fields event]
   (if ok? (succeeded! req attempt) (failed! req attempt))
   (seclog/security-event! event req (if ok? :success :failure) fields)))

(def module
  {:name  :throttle
   :opens {:access/throttle (fn [_ _] (atom {}))}})
