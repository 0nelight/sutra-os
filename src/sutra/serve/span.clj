(ns sutra.serve.span
  (:require [clojure.string :as str]
            [sutra.foundation.profile :as prof]
            [sutra.observe :as obs]
            [sutra.serve.gzip :as gzip]))

(set! *warn-on-reflection* true)

(defonce ^:private incident-counter
  (java.util.concurrent.atomic.AtomicLong. 0))

(defn incident!
  []
  (str "rq-" (.incrementAndGet ^java.util.concurrent.atomic.AtomicLong incident-counter)))

(defn- request-span
  [request]
  (or (obs/span-of (:headers request)) (incident!)))

(defn wrap-profile
  [handler]
  (fn [request]
    (let [t0     (System/nanoTime)
          span   (request-span request)
          request (assoc request ::span span)
          armed? (prof/armed?)
          echo!  (fn [resp]
                   (cond-> resp
                     (map? resp)
                     (update :headers assoc obs/span-header span
                             obs/timing-header
                             (format "app;dur=%.1f"
                                     (/ (double (- (System/nanoTime) t0)) 1e6)))
                     (and (map? resp) armed?)
                     (update :headers assoc obs/profile-header "on")))]
      (if-not armed?
        (echo! (handler request))
        (let [method (str/upper-case (name (or (:request-method request) :get)))
              uri    (str (:uri request))]
          (prof/with-span span
            (echo!
             (prof/with-mark-result
               :Request
               (fn [resp] (let [n (gzip/body-bytes resp)]
                            (cond-> {:method method :uri uri :status (:status resp)}
                              n (assoc :bytes n))))
               (handler request)))))))))
