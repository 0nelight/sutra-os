(ns sutra.wire.transport
  (:require #?(:clj [clojure.string :as str])
            [sutra.wire.codec :as codec]
            [sutra.wire.protocol :as protocol]
            #?(:cljs [sutra.observe :as obs])
            #?(:cljs [sutra.wire.abort :as abort]))
  #?(:clj (:import [java.net URI]
                   [java.net.http HttpClient HttpRequest HttpRequest$Builder
                    HttpResponse$BodyHandlers HttpRequest$BodyPublishers])))

(def CT "application/transit+json")

(defonce log!-hook (atom nil))

(defonce on-http-error (atom nil))

(defonce on-protocol-mismatch (atom nil))
(defonce ^:private req-counter (atom 0))

(defonce mint-span!-hook (atom nil))

#?(:cljs
   (defn- mint-span! []
     (when-let [f @mint-span!-hook] (f))))

(defonce profile-armed? (atom false))

(defn- emit! [event] (when-let [f @log!-hook] (f event)))

(defn on-branch
  [path branch]
  (if (and branch (re-find #"^/api/" path) (not (re-find #"[?&]branch=" path)))
    (str path (if (re-find #"\?" path) "&" "?") "branch="
         #?(:clj  (.replace (java.net.URLEncoder/encode (str branch) "UTF-8") "+" "%20")
            :cljs (js/encodeURIComponent branch)))
    path))

#?(:clj
   (do
     (defonce ^:private jvm-client (delay (HttpClient/newHttpClient)))
     (defonce ^:private base-url (atom nil))

     (defn set-base!
       [url]
       (reset! base-url url))

     (defn current-base
       []
       @base-url)

     (def ^:dynamic *auth-token* nil)

     (defmacro with-token
       [token & body]
       `(binding [*auth-token* ~token] ~@body))

     (def ^:dynamic *branch* nil)

     (def ^:dynamic *project*
       nil)

     (defmacro with-project
       [project & body]
       `(binding [*project* ~project] ~@body))

     (defmacro with-branch
       [branch & body]
       `(binding [*branch* ~branch] ~@body))

     (def ^:dynamic *exchange*
       nil)

     (defn- headers-of [{:keys [token project headers]} accept]
       (cond-> (merge {"accept" accept (str/lower-case protocol/header) (str protocol/version)}
                      (update-keys headers (comp str/lower-case name)))
         (seq token)          (assoc "authorization" (str "Bearer " token))
         (seq (str project))  (assoc "x-sutra-project" (str project))))

     (def ^:dynamic *headers*
       nil)

     (defmacro with-headers
       [headers & body]
       `(binding [*headers* ~headers] ~@body))

     (defn- builder
       ^HttpRequest$Builder [who {:keys [base-url token branch project headers]} path accept]
       (let [base (or base-url
                      (throw (ex-info (str "sutra.wire.transport/" who
                                           ": base URL not set; call set-base! first")
                                      {:path path})))
             b (-> (HttpRequest/newBuilder (URI/create (str base (on-branch path branch))))
                   (.header "Accept" accept)
                   (.header protocol/header (str protocol/version)))]
         (when (seq token)
           (.header b "Authorization" (str "Bearer " token)))
         (when (seq (str project))
           (.header b "X-Sutra-Project" (str project)))
         (doseq [[k v] headers] (.header b (name k) (str v)))
         b))

     (defn ambient [] {:base-url @base-url :token *auth-token* :branch *branch* :project *project*
                       :headers *headers*})

     (defn- exchange!
       [^HttpRequest$Builder b handler]
       (let [req  (.build b)
             resp (.send @jvm-client req handler)]
         (when-let [m (protocol/mismatch
                       (-> resp .headers (.firstValue protocol/header) (.orElse nil)))]
           (let [e (ex-info
                    (str "Protokoll-Abweichung: dieser Client spricht v" (:client m)
                         ", " (.getHost (.uri ^HttpRequest req)) " spricht v" (:server m)
                         (if (> (:server m) (:client m))
                           " — den Checkout aktualisieren (git pull)."
                           " — der Server ist älter als dieser Checkout; deployen oder auf dessen Stand zurück."))
                    (assoc m ::protocol-mismatch true :base (str (.uri ^HttpRequest req))))]
             (when-let [f @on-protocol-mismatch] (f e))
             (throw e)))
         resp))

     (defn- authed
       ^HttpRequest$Builder [who path accept]
       (builder who (ambient) path accept))

     (defn- keywordize-top
       [decoded]
       (if (and (map? decoded) (some string? (keys decoded)))
         (into {} (map (fn [[k v]] [(if (string? k) (keyword k) k) v])) decoded)
         decoded))

     (defn- retryable-503?
       [method {:keys [status body]}]
       (when (= 503 status)
         (let [text (str (when (map? body) (:error body)) " "
                         (when (map? body) (:status body)))]
           (or (re-find #"(?i)restarting|nothing was started" text)
               (and (= :get method)
                    (re-find #"(?i)temporarily unavailable|connection is not available" text))))))

     (def ^:private retry-delays-ms [250 500 1000 2000 4000])

     (declare send-once!)

     (defn send-to!
       ([target method path] (send-to! target method path nil))
       ([target method path body]
        (loop [[d & more] retry-delays-ms]
          (let [r (send-once! target method path body)]
            (if (and d (retryable-503? method r))
              (do (Thread/sleep (long d)) (recur more))
              r)))))

     (defn- send-once!
       [target method path body]
       (let [pub (if body
                   (HttpRequest$BodyPublishers/ofByteArray (codec/encode body))
                   (HttpRequest$BodyPublishers/noBody))
             t0 (System/nanoTime)
             id (swap! req-counter inc)
             _ (emit! {:phase :request :id id :method method :uri path :body body})
             [status raw branch]
             (try
               (if-let [x *exchange*]
                 (let [r (x {:method  method
                             :path    (on-branch path (:branch target))
                             :headers (cond-> (headers-of target CT)
                                        body (assoc "content-type" CT))
                             :body    (when body (codec/encode body))})]
                   [(:status r) (:body r) (some-> (get (:headers r) "X-Sutra-Branch")
                                                  (java.net.URLDecoder/decode "UTF-8"))])
                 (let [b    (.method (builder "send-to!" target path CT)
                                     (.toUpperCase (name method)) pub)
                       _    (when body (.header b "Content-Type" CT))
                       resp (exchange! b (HttpResponse$BodyHandlers/ofByteArray))]
                   [(.statusCode resp) (.body resp)
                    (some-> resp .headers (.firstValue "X-Sutra-Branch") (.orElse nil)
                            (java.net.URLDecoder/decode "UTF-8"))]))
               (catch Throwable e
                 (emit! {:phase :error :id id :method method :uri path :body body :error e})
                 (throw e)))
             decoded (if (<= 200 status 299)
                       (codec/decode raw)
                       (keywordize-top
                        (try (codec/decode raw)
                             (catch RuntimeException _ (String. ^bytes raw "UTF-8")))))
             elapsed (/ (- (System/nanoTime) t0) 1e6)]
         (emit! {:phase :response :id id :method method :uri path :body body
                 :status status :decoded decoded :ok? (<= 200 status 299)
                 :elapsed-ms elapsed})
         (cond-> {:status status :body decoded}
           branch (assoc :branch branch))))

     (defn send!
       ([method path] (send! method path nil))
       ([method path body] (send-to! (ambient) method path body)))

     (defn get-bytes-from!
       [target path]
       (let [b (builder "get-bytes-from!" target path "*/*")]
         (.GET b)
         (let [resp (exchange! b
                               (HttpResponse$BodyHandlers/ofByteArray))]
           {:status       (.statusCode resp)
            :bytes        (.body resp)
            :content-type (-> resp .headers (.firstValue "content-type") (.orElse nil))})))

     (defn post-bytes-to!
       ([target path bytes] (post-bytes-to! target path bytes "application/octet-stream"))
       ([target path bytes content-type]
        (let [b (-> (builder "post-bytes-to!" target path CT)
                    (.header "Content-Type" content-type)
                    (.POST (HttpRequest$BodyPublishers/ofByteArray bytes)))
              resp (exchange! b
                              (HttpResponse$BodyHandlers/ofByteArray))]
          {:status (.statusCode resp) :body (codec/decode (.body resp))})))

     (defn get-bytes!
       [path]
       (let [b (authed "get-bytes!" path "*/*")]
         (.GET b)
         (let [resp (exchange! b
                               (HttpResponse$BodyHandlers/ofByteArray))]
           {:status       (.statusCode resp)
            :bytes        (.body resp)
            :content-type (-> resp .headers (.firstValue "content-type") (.orElse nil))})))

     (defn post-bytes!
       ([path bytes] (post-bytes! path bytes "application/octet-stream"))
       ([path bytes content-type]
        (let [b (-> (authed "post-bytes!" path CT)
                    (.header "Content-Type" content-type)
                    (.POST (HttpRequest$BodyPublishers/ofByteArray bytes)))
              resp (exchange! b
                              (HttpResponse$BodyHandlers/ofByteArray))]
          {:status (.statusCode resp) :body (codec/decode (.body resp))})))

     (defn post-json!
       [path ^String json-body]
       (let [b (-> (authed "post-json!" path "application/json")
                   (.header "Content-Type" "application/json")
                   (.POST (HttpRequest$BodyPublishers/ofString json-body)))
             resp (exchange! b
                             (HttpResponse$BodyHandlers/ofString))
             raw  (.body resp)]
         {:status (.statusCode resp)
          :body   (try ((requiring-resolve 'clojure.data.json/read-str) raw :key-fn keyword)
                       (catch RuntimeException _ raw))}))))

#?(:cljs
   (defonce ^:private page-branch
     (atom (some-> js/globalThis .-location .-search (js/URLSearchParams.) (.get "branch")
                   not-empty))))

#?(:cljs
   (defn set-page-branch!
     [branch]
     (reset! page-branch (when (not= "main" branch) (not-empty branch)))))

#?(:cljs
   (defonce ^:private branch-scope (atom (constantly true))))

#?(:cljs
   (defn set-branch-scope!
     [f]
     (reset! branch-scope f)))

#?(:cljs
   (defn selected-page-branch [] @page-branch))

#?(:cljs
   (def ontology-storage-key "sutra.ontology"))

#?(:cljs
   (defonce ^:private page-ontology
     (atom (try (not-empty (.getItem js/localStorage ontology-storage-key))
                (catch :default _ nil)))))

#?(:cljs
   (defn selected-ontology [] @page-ontology))

#?(:cljs
   (defn set-page-ontology!
     [o]
     (let [o (not-empty (str o))]
       (reset! page-ontology o)
       (try (if o
              (.setItem js/localStorage ontology-storage-key o)
              (.removeItem js/localStorage ontology-storage-key))
            (catch :default _ nil)))))

#?(:cljs
   (defn effective-page-branch []
     (when (@branch-scope) @page-branch)))

#?(:cljs
   (defn on-page-branch
     [path]
     (on-branch path (effective-page-branch))))

#?(:cljs
   (defn- received
     [method path body t0 id span response]
     (-> response
         (.then
          (fn [r]
            (when-let [m (protocol/mismatch (.get (.-headers r) protocol/header))]
              (let [e (ex-info (str "Protokoll-Abweichung: dieser Client spricht v"
                                    (:client m)
                                    ", der Server spricht v" (:server m))
                               (assoc m ::protocol-mismatch true :status (.-status r)))]
                (when-let [f @on-protocol-mismatch] (f e))
                (throw e)))
            (-> (.text r)
                (.then (fn [t-body]
                         (let [d0 (js/performance.now)
                               decoded (when (seq t-body)
                                         (try (codec/decode t-body)
                                              (catch :default _ t-body)))
                               decode-ms (- (js/performance.now) d0)
                               status (.-status r)
                               ok? (.-ok r)
                               elapsed (- (js/performance.now) t0)]
                           (emit! {:phase :response :id id :method method
                                   :span
                                   (or (not-empty (str (.get (.-headers r) obs/span-header)))
                                       span)
                                   :profiling? (reset! profile-armed?
                                                       (= "on"
                                                          (.get (.-headers r)
                                                                obs/profile-header)))
                                   :uri path :body body :status status
                                   :decoded decoded :ok? ok?
                                   :elapsed-ms elapsed
                                   :decode-ms decode-ms
                                   :chars (.-length t-body)})
                           (if ok?
                             decoded
                             (let [server-msg (cond
                                                (map? decoded)
                                                (or (:error decoded)
                                                    (:message decoded))
                                                (string? decoded) decoded)
                                   msg (if (and server-msg (seq (str server-msg)))
                                         (str "HTTP " status ": " server-msg)
                                         (str "HTTP " status))]
                               (when-let [f @on-http-error]
                                 (f {:status status :body decoded}))
                               (throw (ex-info msg {:status status
                                                    :body decoded}))))))))))
         (.catch (fn [e]
                   (emit! {:phase :error :id id :method method :uri path
                           :body body :error e})
                   (throw e))))))

#?(:cljs
   (defonce ^:private gets-under-way (atom {})))

#?(:cljs
   (defn- send-now!
     [method path body]
     (let [t0   (js/performance.now)
            id   (swap! req-counter inc)
           span (mint-span!)]
       (emit! {:phase :request :id id :method method :uri path :body body :span span})
       (received method path body t0 id span
                 (abort/fetch!
                  #(js/fetch
                    (on-page-branch path)
                    (clj->js
                     (cond-> {:method      (.toUpperCase (name method))
                              :credentials "include"
                              :headers     (cond-> {:accept CT
                                                    protocol/header (str protocol/version)}
                                             span (assoc obs/span-header span)
                                             @page-ontology
                                             (assoc "x-sutra-ontology" @page-ontology))}
                       body (-> (assoc-in [:headers :content-type] CT)
                                (assoc :body (codec/encode body))))))
                  method path)))))

#?(:cljs
   (defn send!
     ([method path] (send! method path nil))
     ([method path body]
      (if (= :get method)
        (let [k (on-page-branch path)]
          (or (get @gets-under-way k)
              (let [p (send-now! method path body)
                    done #(when (identical? p (get @gets-under-way k))
                            (swap! gets-under-way dissoc k))]
                (swap! gets-under-way assoc k p)
                (.then p done done)
                p)))
        (do (reset! gets-under-way {})
            (let [p (send-now! method path body)
                  done #(reset! gets-under-way {})]
              (.then p done done)
              p))))))

#?(:cljs
   (defn adopt!
     ([path response] (adopt! :get path response))
     ([method path response]
      (let [id (swap! req-counter inc)]
        (emit! {:phase :request :id id :method method :uri path})
        (received method path nil (js/performance.now) id nil response)))))
