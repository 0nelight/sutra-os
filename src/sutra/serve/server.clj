(ns sutra.serve.server
  (:require [sutra.kit.refusal :as refusal]
            [sutra.kit.anomaly :as anomaly]
            [sutra.access.http.host :as host]
            [sutra.store.access.scope :as ctx]
            [sutra.store.world :as world]
            [sutra.builds.run.monitor :as bm]
            [sutra.store.seat :as seat]
            [sutra.serve.deadline :as deadline]
            [sutra.serve.errors :as errors]
            [sutra.serve.gzip :as gzip]
            [sutra.serve.lifecycle :as lc]
            [sutra.serve.hot-patches :as hot]
            [sutra.serve.nrepl :as nrepl]
            [sutra.serve.span :as span]
            [sutra.serve.worker :as worker]
            [sutra.foundation.settings :as settings]
            [sutra.wire.codec :as codec]
            [sutra.wire.protocol :as protocol]
            [clojure.string :as str]
            [sutra.foundation.sql-trace :as sqltrace]
            [sutra.foundation.profile :as prof]
            [sutra.store.storage.instance-config :as ic]
            [sutra.access.principals :as pr]
            [sutra.kit.system :as sys]
            [sutra.schedules.dispatch]
            [sutra.foundation.contracts :as ct]
            [sutra.foundation.boundaries :as boundaries]
            [sutra.types.ir :as tir]
            [sutra.types.paths :as paths]
            [sutra.types.ctx :as tctx]
            [sutra.types.schema :as sch]
            [sutra.types.versions :as vers]
            [org.httpkit.server :as http-kit]
            [org.httpkit.client :as http-client]
            [sutra.schedules.poller :as poller]
            [reitit.core :as r]
            [sutra.store.storage.node :as node]
            [reitit.ring :as ring]
            [reitit.ring.middleware.parameters :as parameters]
            [ring.util.response :as resp]
            [sutra.http.endpoint :as ep]
            [sutra.http.origin :as origin]
            [sutra.feedback.places :as places]
            [sutra.http.security-log :as seclog]
            [sutra.kit.attempt :as attempt]
            [sutra.http.responses :refer
             [json-response transit-write-handlers]]
            [muuntaja.core :as m]
            [sutra.access.http.auth :as auth]
            [sutra.llm.http]
            [sutra.foundation.env :as env]))

(set! *warn-on-reflection* true)

(declare app)

(defn- wrap-endpoint
  [handler]
  (fn [req]
    (handler (assoc req :sutra/endpoint (some-> (ring/get-match req) :data :name)))))

(defonce ^:private server (atom nil))

(declare module built)

(defn modules
  []
  (conj (sys/discover) module))

(defonce ^:private configured
  (delay (sys/configure (:system @built) (into {} (System/getenv)))))

(defn system
  []
  @configured)

(defn- refuse-unless-utc!
  []
  (let [z (java.time.ZoneId/systemDefault)]
    (when-not (= java.time.ZoneOffset/UTC (.normalized z))
      (anomaly/raise! :fault
                      (str "the JVM runs in zone " z
                           ", not UTC — Sutra does not boot: the store would"
                           " read and write every timestamp shifted by its offset. Start it with"
                           " -Duser.timezone=UTC (the :dev alias does) or TZ=UTC.")
                      {:zone (str z)}))))

(def ^:private warm-history-wait-ms 60000)

(defn- warm-history!
  []
  (let [warm (future
               (try (world/warm-history! (world/at (ctx/system-store) "main"))
                    (catch Exception e
                      (binding [*out* *err*]
                        (println "[warm-history] failed:" (ex-message e))))))]
    (deref warm warm-history-wait-ms nil)))

(defn- arm-sql-trace! []
  (sqltrace/set-role! "backend")
  (when (seq (env/env "SUTRA_SQL_TRACE" nil))
    (sqltrace/arm! {:file (env/env "SUTRA_SQL_TRACE_FILE" nil)})))

(defn- lm-service-url
  [sys-v port]
  (or (not-empty (env/env "SUTRA_LM_SERVICE_URL" ""))
      (when (and (not (env/env "SUTRA_TRANSFORM_URL" nil))
                 (contains? (sys/contributions sys-v :http/routes) :llm-chat))
        (str "http://127.0.0.1:" port "/api/llm/chat"))))

(defn- seed-store!
  [sys-v port]
  (when-not auth/*allow-anon*
    (lc/timed "default admin"
              #(pr/ensure-default-admin! (ctx/system-store) {:anon? auth/*allow-anon*})))
  (when-let [u (lm-service-url sys-v port)]
    (ic/set-lm-service-url! (ctx/system-store) u)))

(defn- listen!
  [port]
  (let [s (http-kit/run-server #'app {:ip (env/env "SUTRA_HTTP_BIND" "0.0.0.0")
                                      :port port :legacy-return-value? false
                                      :max-line 65536
                                      :max-body (* 128 1024 1024)})]
    (reset! server s)
    (println (str "Sutra-OS HTTP listening on http://" (env/env "SUTRA_HTTP_BIND" "localhost")
                  ":" port "/"))
    s))

(defn start!
  ([] (start! nil))
  ([{:keys [port nrepl?] :or {nrepl? true} :as o}]
   (refuse-unless-utc!)
   (let [sys-v (lc/timed "system" #(sys/install! (or (:system o) (system))))
         port  (or port (Long/parseLong (env/env "SUTRA_HTTP_PORT" "8030")))]
     (lc/timed "routes" #(deref built))
     (prof/add-live-view! :requests #'deadline/requests-in-flight)
     (println (str "Starting Sutra-OS on branch " (seat/seated-branch)))
     (arm-sql-trace!)
     (lc/timed "lifecycle" lc/start!)
     (seed-store! sys-v port)
     (when nrepl? (nrepl/start-nrepl!))
     (lc/timed "history" warm-history!)
     (let [s (listen! port)]
       (sys/start-jobs! sys-v settings/lookup)
       s))))

(defn- stop-grace-ms
  []
  (or (some-> (env/env "SUTRA_STOP_GRACE_MS" "") not-empty parse-long) 30000))

(def ^:private drain-beat-ms 10000)

(defn drain-step
  [{:keys [ticks? requests? work? left-ms]}]
  (cond (not (or ticks? requests? work?)) :done
        (pos? left-ms)              :wait
        :else                       :out-of-time))

(defn drain-lines
  [builds]
  (for [{:keys [run-id current done total idle-ms cancel? remote? queued?]} builds
        :when (not (or remote? queued?))]
    (str "run " (subs run-id 0 (min 8 (count run-id))) " · " (or current "between steps")
         " · " (count done) "/" (or total "?") " transforms done"
         " · last moved " (quot (or idle-ms 0) 1000) "s ago"
         (when cancel? (str " · cancelled (" (name cancel?) ")")))))

(defn stop!
  []
  (when-let [s @server]
    (let [grace    (stop-grace-ms)
          deadline (+ (System/currentTimeMillis) grace)
          left     #(max 0 (- % (System/currentTimeMillis)))
          drained  (future (some-> (http-kit/server-stop! s {:timeout grace})
                                   (deref (+ grace 5000) nil)))
          _        (poller/quiesce! 0)
          _        (worker/quiesce!)
          cancelled (when-not (= :worker node/role) (bm/cancel-all! :shutdown))]
      (when (seq cancelled)
        (println (str "[stop] cancelled " (count cancelled)
                      " build(s) — the next conductor fires the scheduled ones again")))
      (loop []
        (let [slice     (min deadline (+ (System/currentTimeMillis) drain-beat-ms))
              ticks?    (not (poller/quiesce! (left slice)))
              requests? (= ::pending (deref drained (left slice) ::pending))
              work?     (worker/busy?)]
          (case (drain-step {:ticks? ticks? :requests? requests? :work? work?
                             :left-ms (left deadline)})
            :done        nil
            :out-of-time (println "[stop] the grace ran out with work still running")
            :wait        (do (println (str "[drain] " (quot (left deadline) 1000)
                                           "s of grace left · "
                                           (if ticks? "a job tick runs" "no job tick") " · "
                                           (if requests? "requests in flight"
                                               "no request in flight")
                                           (when work? " · the worker still runs work")))
                             (when (and work? (not ticks?) (not requests?))
                               (Thread/sleep (long (min drain-beat-ms (left deadline)))))
                             (doseq [l (drain-lines
                                        (attempt/or-answer []
                                                           (bm/active-builds (ctx/system-store))))]
                               (println (str "[drain]   " l)))
                             (recur))))))
    (sys/stop! (sys/process))
    (reset! server nil)
    (println "Sutra-OS HTTP stopped.")))

(defn -main [& _]
  (println (str "[boot] jvm + code loaded "
                (.getUptime (java.lang.management.ManagementFactory/getRuntimeMXBean)) "ms"))
  (lc/timed "hot replay" hot/replay!)
  (start!)
  (.addShutdownHook (Runtime/getRuntime) (Thread. ^Runnable stop!))
  @(promise))

(def transit-only
  (m/create
   (-> m/default-options
       (update :formats select-keys ["application/transit+json"])
       (assoc-in [:formats "application/transit+json" :encoder-opts :handlers]
                 transit-write-handlers)
       (assoc :default-format "application/transit+json"))))

(def wire-formats
  (m/create
   (-> m/default-options
       (update :formats select-keys ["application/transit+json" "application/json"])
       (assoc-in [:formats "application/transit+json" :encoder-opts :handlers]
                 transit-write-handlers)
       (assoc :default-format "application/transit+json"))))

(def api-version-header "sutra-api-version")

(defn- wire-of
  [request]
  (if (get-in request [:headers api-version-header]) wire-formats transit-only))

(defn- published-of [schema]
  (sch/column schema ::published (comp sch/indexed vers/published-view)))

(defn- branch-view-of [schema trunk]
  (let [held (sch/column schema ::branch-view (fn [_] (atom nil)))
        [seen view] @held]
    (if (identical? seen trunk)
      view
      (second (reset! held [trunk (sch/indexed (vers/branch-view schema trunk))])))))

(defn- consumer-schema
  [store branch schema]
  (if (or (nil? schema) (nil? branch) (= tctx/default-branch branch))
    (published-of schema)
    (branch-view-of schema (seat/branch-schema (ctx/at store tctx/default-branch)))))

(defn- published-view
  [handler]
  (fn [{store :sutra/store branch :sutra/branch schema :sutra/schema :as req}]
    (handler (assoc req
                    :sutra/head-schema schema
                    :sutra/schema (consumer-schema store branch schema)))))

(defn- branch-gated
  [handler]
  (fn [req]
    (if (:sutra/branch req)
      (handler req)
      (refusal/refused :refusal.http/no-branch {}))))

(declare contract-of)

(defn- json-sent?
  [request]
  (some-> (get-in request [:headers "content-type"]) (str/starts-with? "application/json")))

(defn- wrap-format
  [handler]
  (fn [request]
    (let [wire    (wire-of request)
          raw?    (and (json-sent? request) (nil? (contract-of request :request)))
          request (if raw?
                    (m/negotiate-request-response wire request)
                    (m/negotiate-and-format-request wire request))
          json?   (= "application/json" (get-in request [:muuntaja/response :format]))]
      (some->> (handler request)
               (#(if (and json? (coll? (:body %)))
                   (-> %
                       (assoc :body (:body (json-response (:status %) (:body %))))
                       (assoc-in [:headers "Content-Type"] "application/json; charset=utf-8"))
                   (prof/with-mark :Encode {:format "transit"}
                     (m/format-response wire request %))))))))

(def ^:private api-version
  (get-in (boundaries/register) [:boundaries :boundary/http-api :version]))

(defn- no-store
  [headers]
  (if (some #(.equalsIgnoreCase "cache-control" (str %)) (keys headers))
    headers
    (assoc headers "Cache-Control" "no-store")))

(defn- wrap-api-version
  [handler]
  (fn [request]
    (if (str/starts-with? (str (:uri request)) "/api/")
      (some-> (handler request)
              (assoc-in [:headers "Sutra-API-Version"] api-version)
              (cond-> (env/auth-hardening?) (update :headers no-store)))
      (handler request))))

(defn- wrap-auth-marked
  [handler]
  (let [authed (auth/wrap-auth
                (fn [{::keys [auth-t0] :as request}]
                  (prof/mark-ms! :Auth (/ (double (- (System/nanoTime) (long auth-t0))) 1e6) nil)
                  (seclog/note! request {:herkunft? (places/admitted? request)})
                  (handler request)))]
    (fn [request]
      (authed (assoc request ::auth-t0 (System/nanoTime))))))

(defn- endpoint-facts-of
  [loaded handler-bindings]
  (let [absent?     #(when-let [p (paths/plugin-of %)] (not (loaded p)))
        unbound     (remove #(or (handler-bindings %) (absent? %)) (keys paths/paths))
        misdeclared (for [[id binds] handler-bindings
                          [_ h] binds
                          :let [own (ep/endpoint-of h)]
                          :when (and own (not= (paths/query id) (paths/query own)))]
                      [id own])]
    (assert (empty? unbound)
            (str "declared in `sutra.types.paths` but bound to no handler: "
                 (vec (sort unbound))))
    (assert (empty? misdeclared)
            (str "an alias id declares other query params than the handler's own endpoint: "
                 (vec misdeclared)))
    (into (sorted-map)
          (map (fn [id]
                 (let [r (paths/endpoints id)]
                   [id (cond-> {:methods (vec (sort (keys (handler-bindings id))))
                                :auth    (:auth r :user)
                                :service (:service r)}
                         (:branch? r)  (assoc :branch? true)
                         (:request r)  (assoc :request (:request r))
                         (:response r) (assoc :response (:response r)))])))
          (keys handler-bindings))))

(defn- contract-of
  [req k]
  (let [data (:data (ring/get-match req))
        m    (if (= :head (:request-method req)) :get (:request-method req))]
    (or (get-in paths/endpoints [(:name data) :methods m k])
        (get data (keyword "sutra" (name k))))))

(defn- json-body?
  [req]
  (and (some? (:body-params req))
       (= "application/json" (get-in req [:muuntaja/request :format]))))

(def ^:dynamic *enforce-all-contracts?*
  (= "all" (some-> (System/getenv "SUTRA_CONTRACTS_ENFORCE") str/trim)))

(defn- enforced?
  [schema]
  (or *enforce-all-contracts?*
      (not (qualified-keyword? schema))
      (= "sutra.foundation.contracts" (namespace schema))))

(defn wrap-endpoint-contract
  [handler]
  (fn [req]
    (let [schema (contract-of req :request)
          req    (cond-> req
                   (and schema (json-body? req)) (update :body-params #(ct/decode-json schema %)))
          problem (some-> schema (ct/http-check (:body-params req)))]
      (cond
        (nil? problem)       (handler req)
        (enforced? schema)   (refusal/refused :refusal.serve/request-does-not-satisfy-its {:problem problem})
        :else                (do (binding [*out* *err*]
                                   (println "request contract (log only):" (:sutra/endpoint req)
                                            schema (pr-str problem))
                                   (flush))
                                 (handler req))))))

(defn wrap-endpoint-response-contract
  [handler]
  (fn [req]
    (let [resp (handler req)]
      (when (ct/checking?)
        (when-let [schema (contract-of req :response)]
          (when (and (<= 200 (:status resp 200) 299) (not (string? (:body resp))))
            (ct/check! schema (:body resp)))))
      resp)))

(defn- routes-of
  [handler-bindings]
  (let [orphans (remove paths/paths (keys handler-bindings))]
    (assert (empty? orphans)
            (str "server.http: handlers bound to ids absent from sutra.types.paths/paths: "
                 (vec orphans)))
    (mapv (fn [[id binds]]
            [(paths/paths id)
             (assoc (cond-> binds
                      (paths/branch-scoped id)
                      (update-vals branch-gated)
                      (paths/consumer-surface id)
                      (update-vals published-view)
                      (get-in paths/endpoints [id :request])
                      (assoc :sutra/request (get-in paths/endpoints [id :request]))
                      (get-in paths/endpoints [id :response])
                      (assoc :sutra/response (get-in paths/endpoints [id :response])))
                    :name id)])
          handler-bindings)))

(def ^:private public-dir
  (not-empty (env/getenv "SUTRA_PUBLIC_DIR")))

(defn- spa-fallback
  [{:keys [request-method uri]}]
  (if (and (= request-method :get)
           (not (str/starts-with? uri "/api/")))
    (or (some-> (if public-dir
                  (resp/file-response "index.html" {:root public-dir})
                  (resp/resource-response "public/index.html"))
                (resp/content-type "text/html"))
        {:status  404
         :headers {"Content-Type" "text/plain; charset=utf-8"}
         :body
         "No web UI is served here. Use bin/sutra, the Python SDK or the API under /api/.\n"})
    {:status 404 :body "not found"}))

(defn- plugin-handler-of
  [system]
  (let [opts {:host              host/capabilities
              :principal-context auth/principal-context
              :with-principal    auth/with-principal-context*
              :session-for       #(auth/session-for (ctx/store) %)
              :store-for            #(ctx/store (seat/seated-branch))}]
    (apply ring/routes
           (for [[_ make] (sort-by key (sys/contributions system :http/raw-handlers))]
             (make opts)))))

(defn- wrap-protocol
  [handler]
  (fn [request]
    (if-let [m (protocol/mismatch (get-in request [:headers (str/lower-case protocol/header)]))]
      {:status  426
       :headers {"Content-Type" "application/transit+json"
                 protocol/header (str protocol/version)}
       :body    (String. ^bytes (codec/encode {:error :protocol-mismatch
                                               :message
                                               (str "protocol mismatch: client v" (:server m)
                                                    ", server v" (:client m))
                                               :client (:server m) :server (:client m)})
                         "UTF-8")}
      (some-> (handler request)
              (assoc-in [:headers protocol/header] (str protocol/version))))))

(defn- conductor-url
  []
  (when (= :api node/role)
    (or (not-empty (env/getenv "SUTRA_CONDUCTOR_URL"))
        (anomaly/raise! :fault
                        "SUTRA_ROLE=api needs SUTRA_CONDUCTOR_URL, where the conductor answers"
                        {}))))

(def ^:private conductor-router
  (r/router (into (mapv (fn [id] [(paths/paths id) {:name id}]) (sort paths/in-conductor))
                  (map (fn [id] [(paths/paths id) {:name id :every? true}]))
                  (sort paths/in-every-process))))

(def ^:private conductor-timeout-ms
  (or (some-> (env/getenv "SUTRA_CONDUCTOR_TIMEOUT_MS") parse-long) (* 6 3600 1000)))

(def ^:private not-forwarded
  #{"host" "content-length" "connection" "transfer-encoding" "accept-encoding" "keep-alive"
    "upgrade"})

(defn- conductor-says
  [{:keys [status headers body error]}]
  (if error
    {:status  (if (instance? org.httpkit.client.TimeoutException error) 504 503)
     :headers {"Content-Type" "text/plain; charset=utf-8"}
     :body    (str "the conductor did not answer: " (ex-message error) "\n")}
    {:status  status
     :headers (into {} (keep (fn [[k v]]
                               (let [k (name k)]
                                 (when-not (#{"content-length" "transfer-encoding" "connection"} k)
                                   [k (if (sequential? v) (vec v) (str v))]))))
                    headers)
     :body    body}))

(defn- wrap-conductor
  [handler]
  (if-let [conductor-url (conductor-url)]
    (fn [{:keys [request-method uri query-string headers body remote-addr] :as req}]
      (let [m (r/match-by-path conductor-router uri)]
        (if-not m
          (handler req)
          (let [both?  (get-in m [:data :every?])
                body   (if (and both? body) (.readAllBytes ^java.io.InputStream body) body)
                answer (conductor-says
                        @(http-client/request
                          {:method           request-method
                           :url
                           (str conductor-url uri (when query-string (str "?" query-string)))
                           :headers          (-> (into {} (remove (comp not-forwarded key)) headers)
                                                 (assoc "accept-encoding" "identity")
                                                 (update "x-forwarded-for" #(or % remote-addr)))
                           :body             body
                           :as               :stream
                           :timeout          conductor-timeout-ms
                           :follow-redirects false}))]
            (if (and both? (<= 200 (:status answer) 299))
              (handler (assoc req :body (some-> ^bytes body java.io.ByteArrayInputStream.)))
              answer)))))
    handler))

(defn- build
  [mods]
  (let [s        (sys/assemble mods)
        bindings (auth/gate-bindings
                  (update-vals (get-in s [:tables :http/routes])
                               #(update-vals % wrap-endpoint-contract)))
        facts    (endpoint-facts-of (set (map :name mods)) bindings)]
    {:system           s
     :handler-bindings bindings
     :endpoint-facts   facts
     :ir               (tir/ir facts)
     :prefix-authority (sys/contributions s :http/prefix-authority)
     :plugin-handler   (plugin-handler-of s)
     :routes           (routes-of bindings)}))

(defn- ring-app
  [{:keys [routes plugin-handler]}]
  (origin/wrap-ws-origin
   (wrap-protocol
    (wrap-conductor
     (span/wrap-profile
      (gzip/wrap-gzip
       (ring/ring-handler
        (ring/router
         routes
         {:data {:muuntaja transit-only
                 :middleware [wrap-endpoint
                              parameters/parameters-middleware
                              wrap-format
                              wrap-endpoint-response-contract
                              deadline/wrap-deadline
                              errors/wrap-exceptions
                              host/wrap-host
                              wrap-auth-marked]}})
        (apply
         ring/routes
         (remove nil?
                 [plugin-handler
                  (when public-dir
                    (ring/create-file-handler
                     {:path "/"
                      :root public-dir
                      :index-files ["index.html"]}))
                  (ring/create-resource-handler
                   {:path "/"
                    :root "public"
                    :index-files ["index.html"]})
                  spa-fallback])))))))))

(def ^:private built
  (delay (let [b (build (modules))] (assoc b :app (wrap-api-version (ring-app b))))))

(defn assembled
  []
  (:system @built))

(defn handler-bindings [] (:handler-bindings @built))

(defn endpoint-facts [] (:endpoint-facts @built))

(defn ir [] (:ir @built))

(defn prefix-authority [] (:prefix-authority @built))

(defn plugin-handler [] (:plugin-handler @built))

(defn app [request] ((:app @built) request))

(defn exchange
  [{:keys [method path headers body]}]
  (let [[uri query] (str/split path #"\?" 2)
        resp (app {:request-method method :uri uri :query-string query :headers headers
                   :body (some-> ^bytes body java.io.ByteArrayInputStream.)
                   :scheme :http :server-name "localhost" :server-port 0
                   :remote-addr "127.0.0.1" :protocol "HTTP/1.1"})
        b    (:body resp)]
    {:status  (:status resp)
     :headers (:headers resp)
     :body    (cond (bytes? b)                         b
                    (instance? java.io.InputStream b) (.readAllBytes ^java.io.InputStream b)
                    (nil? b)                          (byte-array 0)
                    :else                             (.getBytes (str b) "UTF-8"))}))

(def ^:private trace-stuck
  (ep/handler :trace-stuck {:call (fn [_] (deadline/requests-in-flight))}))

(def module
  {:name        :server
   :provides    {:http/exchange (fn [m] (exchange m))}
   :contributes {:http/routes {:trace-stuck {:get trace-stuck}}}})
