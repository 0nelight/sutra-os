(ns platform-sdk.errors)

(def kinds #{:auth :not-found :conflict :query :server :usage :drift})

(defn error
  ([kind message] (error kind message {}))
  ([kind message data] (ex-info message (assoc data :sdk/kind kind))))

(defn raise!
  ([kind message] (raise! kind message {}))
  ([kind message data] (throw (error kind message data))))

(defn kind [e] (:sdk/kind (ex-data e)))

(defn sdk-error? [e] (contains? kinds (kind e)))

(defn auth-error?      [e] (= :auth      (kind e)))
(defn not-found-error? [e] (= :not-found (kind e)))
(defn conflict-error?  [e] (= :conflict  (kind e)))
(defn query-error?     [e] (= :query     (kind e)))
(defn server-error?    [e] (= :server    (kind e)))
(defn usage-error?     [e] (= :usage     (kind e)))
(defn drift-error?     [e] (= :drift     (kind e)))

(defn status->kind
  [status]
  (cond
    (contains? #{401 403} status) :auth
    (= 404 status)                :not-found
    (= 409 status)                :conflict
    (<= 400 (or status 0) 499)    :query
    :else                         :server))

(defn- server-message
  [body]
  (cond
    (map? body)    (or (:error body) (:message body) (:problem body))
    (string? body) body))

(defn check!
  [{:keys [status body]} ctx]
  (if (<= 200 (or status 0) 299)
    body
    (raise! (status->kind status)
            (str (name (or (:endpoint ctx) :request)) ": HTTP " status
                 (when-let [m (server-message body)] (str " — " m)))
            (assoc ctx :status status :body body))))
