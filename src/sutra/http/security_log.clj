(ns sutra.http.security-log
  (:require [sutra.kit.noted :as noted]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [sutra.http.request :as http-request]
            [sutra.kit.redact :as redact]
            [sutra.types.entities :as ents]
            [sutra.types.paths :as paths]))

(set! *warn-on-reflection* true)

(defn seen
  [req]
  (some-> (::seen req) deref))

(defn encode
  [s]
  (str/replace (str s) #"[\p{Cntrl}  ]"
               (fn [^String c]
                 (case c "\n" "\\n" "\r" "\\r" "\t" "\\t"
                       (format "\\u%04x" (int (.charAt c 0)))))))

(defn client-ip
  [req]
  (or (:sutra/client-ip req) (when req (http-request/client-ip req))))

(defn- endpoint [req] (:sutra/endpoint req))

(defn- route [req]
  (or (some-> (endpoint req) paths/paths) (:uri req)))

(defn- principal [req]
  (or (get-in req [:principal :username])
      (get-in (seen req) [:principal :username])))

(defn- named [x]
  (if (keyword? x) (subs (str x) 1) x))

(defn- json-value [v]
  (cond (keyword? v) (named v)
        (or (nil? v) (string? v) (number? v) (boolean? v) (coll? v)) v
        :else (str v)))

(defn- write-line! [m]
  (let [line (str "[security] "
                  (json/write-str (redact/redact m) :escape-unicode false :escape-slash false
                                  :key-fn (comp str named)
                                  :value-fn (fn [_ v] (json-value v))))]
    (locking *err*
      (binding [*out* *err*] (println line) (flush)))))

(defn security-event!
  ([event req outcome] (security-event! event req outcome nil))
  ([event req outcome m]
   (noted/or-else (str "security event " event) nil
     (write-line!
      (merge (cond-> {:at      (str (java.time.Instant/now))
                      :event   event
                      :outcome outcome
                      :principal (principal req)
                      :ip      (client-ip req)}
               req (assoc :method (some-> (:request-method req) name str/upper-case)
                          :route (route req))
               (endpoint req) (assoc :endpoint (endpoint req)))
             m)))
   nil))

(defn note!
  ([req] (note! req nil))
  ([req m]
   (when-let [v (::seen req)]
     (vswap! v merge m)
     (when-let [p (:principal req)]
       (vswap! v assoc :principal (select-keys p [:username :role])
               :agent? (boolean (:sutra/agent? req)))))))

(defn- writes?
  [req]
  (if-let [id (endpoint req)]
    (paths/writes? id (:request-method req))
    (contains? #{:post :put :patch :delete} (:request-method req))))

(defn- audit [id] (get-in paths/endpoints [id :audit]))

(defn- admin-endpoint? [id]
  (or (= :access (audit id)) (= :admin (get-in paths/endpoints [id :auth]))))

(defn- touched-sections
  [req]
  (let [schema (get-in req [:body-params :schema])]
    (when (map? schema)
      (not-empty
       (into (sorted-map)
             (keep (fn [[k v]]
                     (when (contains? ents/access-sections (keyword k))
                       [k (if (map? v) (vec (sort (map (comp str named) (keys v)))) [])])))
             schema)))))

(defn- outcome [status]
  (cond (nil? status) :unknown (< status 400) :success (#{401 403} status) :denied
        :else :failure))

(defn- refusal-reason
  [res]
  (let [b (:body res)]
    (when (map? b)
      (cond-> {:error (some-> (:error b) str (subs 0 (min 200 (count (str (:error b))))))}
        (:category b) (assoc :category (:category b))
        (get-in b [:data :refusal/code]) (assoc :code (get-in b [:data :refusal/code]))))))

(defn observed
  [req res]
  (let [status (:status res)
        id     (endpoint req)]
    (cond
      (= 403 status)
      (security-event! (if (writes? req) :authz/write-denied :authz/denied) req :denied
                       (refusal-reason res))

      (and (writes? req) (admin-endpoint? id))
      (security-event! :admin/change req (outcome status))

      (and (writes? req) (= :schema (audit id)))
      (when-let [ts (touched-sections req)]
        (security-event! :admin/schema-change req (outcome status) {:sections ts}))))
  res)

(defn observing
  [req f]
  (let [req (assoc req ::seen (volatile! {}))]
    (observed req (f req))))
