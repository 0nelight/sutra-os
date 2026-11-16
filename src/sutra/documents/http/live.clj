(ns sutra.documents.http.live
  (:require [sutra.http.endpoint :as ep]
            [sutra.kit.refusal :as refusal]
            [clojure.data.json :as json]
            [org.httpkit.server :as http-kit]
            [sutra.documents.records :as docs]
            [sutra.catalog.project-scope :as scope]
            [sutra.types.live-doc :as live]))

(set! *warn-on-reflection* true)

(def ^:private flush-ms 1000)
(def ^:private ping-every 25)

(defonce ^:private hub (atom {:rooms {} :stores {} :channels {} :pending {}}))
(defonce ^:private locks (atom {}))

(defn- lock-of [k] (or (get @locks k) (get (swap! locks update k #(or % (Object.))) k)))

(defn- with-lock
  [k f]
  (locking (lock-of k) (f)))

(defn- room-key [store id] [(:branch store) (str id)])

(defn- send! [ch m] (http-kit/send! ch (json/write-str m) false))

(defn- effect!
  [k [kind a b]]
  (let [ch #(get-in @hub [:channels %])]
    (case kind
      :send    (some-> (ch a) (send! b))
      :close   (some-> (ch a) http-kit/close)
      :persist (swap! hub assoc-in [:pending k] a)
      :retire  (swap! hub update :rooms dissoc k)
      :discard (swap! hub #(-> % (update :rooms dissoc k) (update :pending dissoc k))))))

(defn- step!
  [k store event]
  (with-lock
    k #(when-let [room (or (get-in @hub [:rooms k])
                           (when (= :join (first event))
                             (live/room (second k) (docs/live-log store (second k)))))]
         (let [[room' fx] (live/step room event)]
           (swap! hub (fn [h] (-> h (assoc-in [:rooms k] room') (assoc-in [:stores k] store))))
           (run! (fn [e] (effect! k e)) fx)))))

(defn- flush!
  [k]
  (with-lock
    k #(let [[old] (swap-vals! hub update :pending dissoc k)]
         (when-let [log (get-in old [:pending k])]
           (try (docs/save-live-log! (get-in old [:stores k]) (second k) log)
                (catch InterruptedException e (throw e))
                (catch Exception e
                  (swap! hub update-in [:pending k] (fn [p] (or p log)))
                  (println "documents-live: Protokoll nicht gespeichert" k
                           (ex-message e))))))))

(defn- flush-all! [] (run! flush! (keys (:pending @hub))))

(defn resetting
  [store id f]
  (let [k (room-key store id)]
    (with-lock k #(let [v (f)]
                    (step! k store [:reset])
                    v))))

(defn as-peer
  [store id peer f]
  (let [k (room-key store id)]
    (with-lock k #(if (some-> (get-in @hub [:rooms k]) (live/present? peer))
                    (f)
                    (refusal/refuse! :refusal.documents/diese-live-sitzung-ist-beendet {:error :not-live :id (str id)})))))

(defn- inbound
  [raw]
  (try (if-let [m (live/accept (json/read-str (str raw) :key-fn keyword))]
         {:msg m}
         {:reason "unreadable"})
       (catch Exception e {:reason (str "unreadable: " (ex-message e))})))

(defn- peer-of
  [req doc client]
  {:peer   (str (random-uuid))
   :user   (or (ep/signed-in-user req) "")
   :client client
   :edit?  (scope/may-unplaced? (:sutra/auth req) :update (:project doc))})

(defn open
  [req]
  (let [store  (:sutra/store req)
        id     (get-in req [:path-params :id])
        client (some-> (get-in req [:query-params "client"]) parse-long)
        doc    (when (:websocket? req) (docs/document store (:sutra/auth req) id :read))]
    (cond
      (not (:websocket? req))
      (refusal/refused :refusal.documents/api-documents-id-live-ist {:upgrade "websocket"})

      (nil? doc)
      (refusal/refused :refusal.documents/kein-solches-dokument {:id id})

      (not (nat-int? client))
      (refusal/refused :refusal.documents/client-nennt-die-yjs-client {:client client})

      :else
      (let [k (room-key store id)
            p (peer-of req doc client)]
        (http-kit/as-channel
         req
         {:on-open    (fn [ch]
                        (swap! hub assoc-in [:channels (:peer p)] ch)
                        (step! k store [:join p]))
          :on-receive (fn [ch raw]
                        (let [{:keys [msg reason]} (inbound raw)]
                          (if msg
                            (step! k store [:receive (:peer p) msg])
                            (send! ch {:type "refused" :reason reason}))))
          :on-close   (fn [_ _]
                        (step! k store [:leave (:peer p)])
                        (swap! hub update :channels dissoc (:peer p)))})))))

(defonce ^:private ticker (atom nil))

(defn- tick! [n]
  (flush-all!)
  (when (zero? (mod n ping-every))
    (doseq [ch (vals (:channels @hub))]
      (try (send! ch {:type "ping"})
           (catch InterruptedException e (throw e))
           (catch Exception t (println (str "[documents-live] ping failed: " (ex-message t))))))))

(defn- start! []
  (reset! ticker
          (doto (Thread. ^Runnable
                 (fn []
                   (loop [n 1]
                     (java.util.concurrent.locks.LockSupport/parkNanos (* 1000000 (long flush-ms)))
                     (when-not (Thread/interrupted)
                       (try (tick! n)
                            (catch InterruptedException e (throw e))
                            (catch Exception t
                              (println (str "[documents-live] tick failed: " (ex-message t)))))
                       (recur (inc n)))))
                         "sutra-documents-live")
            (.setDaemon true)
            (.start)))
  nil)

(defn- stop! []
  (some-> @ticker .interrupt)
  (reset! ticker nil)
  (flush-all!)
  (doseq [ch (vals (:channels @hub))]
    (try (http-kit/close ch)
         (catch InterruptedException e (throw e))
         (catch Exception t (println (str "[documents-live] close failed: " (ex-message t)))))))

(def module
  {:name        :documents-live
   :jobs        [{:name :documents-live :start start! :stop stop!}]
   :contributes {:http/routes {:document-live {:get open}}}})
