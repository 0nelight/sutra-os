(ns sutra.serve.errors
  (:require [clojure.string :as str]
            [sutra.access.http.auth :as auth]
            [sutra.builds.run :as runs]
            [sutra.catalog.grants :as grants]
            [sutra.http.provenance :as provenance]
            [sutra.http.endpoint :as ep]
            [sutra.http.refusals :as refusals
             :refer [store-unavailable transit-safe unavailable]]
            [sutra.http.responses :refer [negotiated]]
            [sutra.http.security-log :as seclog]
            [sutra.kit.anomaly :as anomaly]
            [sutra.kit.attempt :as attempt]
            [sutra.kit.err :as err]
            [sutra.kit.redact :as redact]
            [sutra.serve.span :as span]))

(set! *warn-on-reflection* true)

(defn- incident-id
  []
  (str "inc-" (subs (str/replace (str (random-uuid)) "-" "") 0 16)))

(defn- print-trace!
  [^Throwable e]
  (doseq [^Throwable t (err/causes e)]
    (println (str "  " (when-not (identical? t e) "caused by ") (.getName (class t)) ": "
                  (seclog/encode (redact/scrub (str (.getMessage t))))))
    (doseq [f (take 60 (.getStackTrace t))]
      (println (str "    at " f)))))

(defn- fault-logged!
  [req ^Throwable e incident]
  (let [user (get-in (seclog/seen req) [:principal :username])]
    (locking *err*
      (binding [*out* *err*]
        (println (str "[500] " incident " " (some-> (:request-method req) name str/upper-case)
                      " " (seclog/encode (:uri req))
                      (when user (str " user=" (seclog/encode user)))
                      (when-let [s (::span/span req)] (str " span=" (seclog/encode s)))
                      " at=" (java.time.Instant/now)))
        (when-let [d (ex-data e)]
          (println (str "  data " (seclog/encode (pr-str (redact/redact d))))))
        (print-trace! e)
        (flush)))))

(defn- error-schema
  [req]
  (attempt/silently (ep/request-schema req)))

(defn- operator?
  [req]
  (let [{:keys [principal agent?]} (seclog/seen req)]
    (or (and (= grants/admin-username (:username principal)) (not agent?))
        (and (nil? principal) (boolean auth/*allow-anon*)))))

(defn- fault-response
  [req ^Throwable e]
  (let [incident (incident-id)]
    (fault-logged! req e incident)
    (provenance/with-origin
      req
      (negotiated req 500
                  (if (operator? req)
                    (cond-> {:error    (redact/scrub (runs/name-rids (error-schema req)
                                                                     (.getMessage e)))
                             :incident incident}
                      (ex-data e) (assoc :data (transit-safe (redact/redact (ex-data e)))))
                    {:error    (str "internal error (incident " incident ")")
                     :incident incident}))
      e)))

(defn- answered
  [handler req]
  (try (let [res (handler req)]
         (if (anomaly/refusal? res)
           (refusals/response req res)
           res))
       (catch clojure.lang.ExceptionInfo e
         (if-let [r (anomaly/of e)]
           (refusals/response req r)
           (if-let [u (store-unavailable e)]
             (unavailable u req)
             (fault-response req e))))
       (catch Exception e
         (if-let [u (store-unavailable e)]
           (unavailable u req)
           (fault-response req e)))))

(defn wrap-exceptions
  [handler]
  (fn [req]
    (seclog/observing req #(answered handler %))))
