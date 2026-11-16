(ns sutra.serve.stream
  (:require [sutra.kit.refusal :as refusal]
            [sutra.catalog.lookup :as lookup]
            [sutra.foundation.encoding :as enc]
            [sutra.serve.object-set :as object-set]
            [sutra.store.access.scope :as ctx]
            [sutra.store.access.streams :as streams]
            [sutra.store.access.watch :as watch]
            [sutra.catalog.grants :as grants]
            [sutra.kit.attempt :as attempt]
            [sutra.kit.noted :as noted]
            [clojure.data.json :as json]
            [sutra.builds.run.stream-index :as stream-index]
            [org.httpkit.server :as http-kit]
            [sutra.http.endpoint :as ep]
            [sutra.http.responses :refer [ok]]))

(set! *warn-on-reflection* true)

(defn- with-stream
  [f]
  (fn [{:keys [store schema branch name] :as a}]
    (ok (f (ctx/at store branch) schema (keyword name) a))))

(def list-streams
  (ep/handler :streams
              {:respond (fn [{:keys [store schema branch]}]
                          (let [store (ctx/at store branch)]
                            (ok {:streams (mapv #(streams/status store schema (:name %))
                                                (streams/streams schema))})))}))

(def get-stream
  (ep/handler :stream-by
              {:respond (with-stream
                          (fn [store schema nm _]
                            (assoc (streams/status store schema nm)
                                   :types (stream-index/liveness store schema nm))))}))

(def get-offsets
  (ep/handler :stream-offsets
              {:respond (with-stream
                          (fn [store schema nm _]
                            {:stream (clojure.core/name nm)
                             :end-offsets (streams/end-offsets store schema nm)}))}))

(def get-records
  (ep/handler :stream-records
              {:respond (with-stream
                          (fn [store schema nm {:keys [partition start-offset limit]}]
                            {:stream (clojure.core/name nm)
                             :partition (or partition 0)
                             :records (streams/records store schema nm
                                                       {:partition (or partition 0)
                                                        :start-offset start-offset
                                                        :limit limit})}))}))

(def get-view
  (ep/handler :stream-view
              {:respond (with-stream (fn [store schema nm {:keys [limit]}]
                                       (streams/view store schema nm limit)))}))

(def publish
  (ep/handler :stream-records
              {:respond (fn [{:keys [store schema branch name body]}]
                          (let [records (:records body)]
                            (if-not (sequential? records)
                              (refusal/refused :refusal.serve/publish-takes-records {:got (type records)})
                              (let [published (streams/publish! (ctx/at store branch) schema
                                                                (keyword name) (vec records))]
                                (stream-index/wake! store)
                                (ok published)))))}))

(def archive
  (ep/handler :stream-archive
              {:respond (with-stream
                          (fn [store schema nm _]
                            (let [stream  (streams/require-stream! schema nm)
                                  indexed
                                  (stream-index/index!
                                   store schema
                                   (assoc stream :stream-name (clojure.core/name nm)))]
                              (assoc (streams/archive! store schema nm) :indexed indexed))))}))

(def reset-stream
  (ep/handler :stream-reset
              {:respond (with-stream (fn [store schema nm _] (streams/reset! store schema nm)))}))

(defn- sse
  [event data]
  (str "event: " event "\ndata: " (json/write-str data) "\n\n"))

(defonce ^:private hub
  (atom {:state :running :channels {}}))

(defn- register!
  [ch unsub]
  (let [[old] (swap-vals! hub #(cond-> % (= :running (:state %)) (assoc-in [:channels ch] unsub)))]
    (when-not (= :running (:state old))
      (attempt/silently (unsub))
      (attempt/silently (http-kit/close ch)))))

(defn- unregister! [ch]
  (let [[old] (swap-vals! hub update :channels dissoc ch)]
    (get-in old [:channels ch])))

(defn- open-hub! [] (swap! hub assoc :state :running) nil)

(defonce ^:private heartbeat
  (delay
    (doto (Thread.
           ^Runnable
           (fn []
             (loop []
               (Thread/sleep 25000)
               (doseq [[ch _] (:channels @hub)]
                 (attempt/silently
                  (http-kit/send! ch (if (http-kit/websocket? ch)
                                       (json/write-str {:type "ping"})
                                       ": ping\n\n")
                                  false)))
               (recur)))
           "sutra-stream-heartbeat")
      (.setDaemon true)
      (.start))))

(defn close-all!
  []
  (let [[old] (swap-vals! hub (constantly {:state :stopped :channels {}}))]
    (doseq [[ch unsub] (:channels old)]
      (attempt/silently (unsub))
      (attempt/silently (http-kit/close ch))))
  (streams/drop-subscribers!)
  (watch/drop-subscribers!))

(defn- ws-send!
  [ch m]
  (http-kit/send! ch (json/write-str m) false))

(defn- unsubscribe-all!
  [subs]
  (doseq [[_ {:keys [unsub]}] @subs] (attempt/silently (unsub)))
  (reset! subs {}))

(def ^:private inline-cap
  50)

(defn- changed-message
  [store schema {:keys [id nm rid where] :as sub} {:keys [ids] :as payload}]
  (let [refresh {:type "refreshObjectSet" :id id :objectType nm :runId (:run-id payload)}
        known   (:members sub)]
    (if-not (and (seq ids) (<= (count ids) inline-cap) (object-set/addressable? schema rid))
      (do (when where
            (reset! known (object-set/members store schema rid where)))
          refresh)
      (noted/or-else (str "object set " id " update, the set is read again") refresh
        (let [matching (object-set/matching store schema rid where ids)
              {:keys [updates members]}
              (object-set/updates sub nm (some-> known deref) ids matching)]
          (when known (reset! known members))
          (when (seq updates)
            {:type "objectSetChanged" :id id :objectType nm :updates updates
             :runId (:run-id payload)}))))))

(defn- readable-type?
  [store schema rid]
  (or (grants/auth-off? store)
      (try (grants/authorize-read! (grants/read-rights store) (get-in schema [:object-types rid])
                                   rid)
           true
           (catch clojure.lang.ExceptionInfo e
             (if (:sutra/acl-denied (ex-data e)) false (throw e))))))

(defn- subscription-for
  [store schema ch branch {:keys [object-type where] :as wanted}]
  (when-let [rid (when-let [r (and (string? object-type)
                                   (lookup/->type-rid schema :object-types object-type))]
                   (when (readable-type? store schema r) r))]
    (let [store (ctx/at store branch)
          sub   (assoc wanted :id (str (random-uuid)) :nm object-type :rid rid
                       :members (when where (atom #{})))
          unsub (watch/subscribe!
                 branch rid
                 (fn [payload]
                   (when-let [m (changed-message store schema sub payload)]
                     (ws-send! ch m))))]
      (try
        (when where (reset! (:members sub) (object-set/members store schema rid where)))
        (assoc sub :unsub unsub)
        (catch clojure.lang.ExceptionInfo e
          (unsub)
          (throw e))))))

(defn- refusal
  [error nm]
  {:type "error" :errors [{:error error :args [{:name "objectSet" :value nm}]}]})

(defn- subscribe-one!
  [store schema ch branch subs raw]
  (let [k (select-keys raw ["objectSet" "propertySet"])]
    (try
      (let [wanted (object-set/parse raw)
            nm     (:object-type wanted)]
        (if-let [s (or (get @subs k)
                       (when-let [s (subscription-for store schema ch branch wanted)]
                         (swap! subs assoc k s)
                         s))]
          {:type "success" :id (:id s) :objectType nm}
          (refusal "INVALID_OBJECT_TYPE" nm)))
      (catch clojure.lang.ExceptionInfo e
        (refusal (or (object-set/error-of e) "INVALID_FILTER") (.getMessage e))))))

(defn- resubscribe!
  [store schema ch branch subs wanted]
  (let [keep? (into #{} (map #(select-keys % ["objectSet" "propertySet"])) wanted)]
    (doseq [[k {:keys [unsub id]}] @subs
            :when (not (keep? k))]
      (attempt/silently (unsub))
      (swap! subs dissoc k)
      (ws-send! ch {:type "subscriptionClosed" :id id
                    :cause {:type "reason" :reason "USER_CLOSED"}}))
    (mapv #(subscribe-one! store schema ch branch subs %) wanted)))

(defn watch-ontology
  [req]
  (if-not (:websocket? req)
    (refusal/refused :refusal.serve/api-ontology-watch-is-a {:upgrade "websocket"})
    (let [store     (:sutra/store req)
          schema (:sutra/schema req)
          branch (:sutra/branch req)
          subs   (atom {})]
      @heartbeat
      (http-kit/as-channel
       req
       {:on-open    (fn [ch]
                      (register! ch #(unsubscribe-all! subs))
                      (ws-send! ch {:type "open" :branch branch}))
        :on-receive (fn [ch raw]
                      (let [msg (enc/json-or-nil raw)
                            reqs (get msg "requests")]
                        (if-not (sequential? reqs)
                          (ws-send! ch {:type "error" :id (get msg "id")
                                        :errors [{:error "INVALID_REQUEST"}]})
                          (ws-send! ch {:type      "subscribeResponses"
                                        :id        (get msg "id")
                                        :responses (resubscribe!
                                                    store schema ch branch subs
                                                    reqs)}))))
        :on-close   (fn [ch _status]
                      (unregister! ch)
                      (unsubscribe-all! subs))}))))

(defn subscribe
  [req]
  (let [nm     (get-in req [:path-params :name])
        branch (:sutra/branch req)
        known? (boolean (streams/stream-of (:sutra/schema req) (keyword nm)))]
    (if-not known?
      (refusal/refused :refusal.serve/unknown-stream {:name nm})
      (do
        @heartbeat
        (http-kit/as-channel
         req
         {:on-open
          (fn [ch]
            (http-kit/send! ch {:status 200
                                :headers {"Content-Type" "text/event-stream"
                                          "Cache-Control" "no-cache"
                                          "X-Accel-Buffering" "no"}
                                :body (sse "open" {:stream nm :branch branch})}
                            false)
            (let [unsub (streams/subscribe!
                         branch nm
                         (fn [payload] (http-kit/send! ch (sse "records" payload) false)))]
              (register! ch unsub)))
          :on-close
          (fn [ch _status]
            (when-let [unsub (unregister! ch)] (unsub)))})))))

(def module
  {:name        :stream
   :jobs        [{:name :stream :start open-hub! :stop close-all!}]
   :contributes {:http/routes
                 {:ontology-watch   {:get watch-ontology}
                  :stream-archive   {:post archive}
                  :stream-by        {:get get-stream}
                  :stream-offsets   {:get get-offsets}
                  :stream-records   {:get get-records :post publish}
                  :stream-reset     {:post reset-stream}
                  :stream-subscribe {:get subscribe}
                  :stream-view      {:get get-view}
                  :streams          {:get list-streams}}}})
