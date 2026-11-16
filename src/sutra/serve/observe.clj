(ns sutra.serve.observe
  (:require [sutra.kit.coll :as coll]
            [sutra.kit.attempt :as attempt]
            [sutra.foundation.decision-trace :as dtrace]
            [sutra.foundation.fn-trace :as fntrace]
            [sutra.foundation.periodic :as periodic]
            [sutra.foundation.profile :as prof]
            [sutra.foundation.profile.gecko :as gecko]
            [sutra.foundation.profile.report :as report]
            [sutra.foundation.sql-trace :as sqltrace]
            [sutra.serve.ports :as ports]
            [sutra.store.storage.node :as node]))

(set! *warn-on-reflection* true)

(def ^:private publish-ms 3000)

(def ^:private events-kept 300)

(def ^:private slowest-kept 50)

(defn kept-events
  [events]
  (let [cut (max 0 (- (count events) events-kept))
        [older newest] (split-at cut events)]
    (concat (sort-by :seq (take slowest-kept (sort-by #(- (long (or (:ms %) 0))) older)))
            newest)))

(defn- remote? [] (not (#{:api :all} node/role)))

(defn- own-role [] (if (remote?) (name node/role) "backend"))

(defn arm-sql!
  [on opts]
  (sqltrace/set-role! (own-role))
  (if on (sqltrace/arm! opts) (sqltrace/disarm!)))

(defn arm-profile!
  [on {:keys [interval depth]}]
  (sqltrace/set-role! (own-role))
  (if on
    (do (sqltrace/arm! {:capture-params? false :shapes? true})
        (merge (prof/arm! {:interval-ms interval :depth depth}) {:sql-trace true}))
    (do (sqltrace/disarm!)
        (merge (prof/disarm!) (prof/status)))))

(defonce ^:private fn-answer (atom nil))

(defonce ^:private decision-answer (atom nil))

(defn- arm-fns-here!
  [on {:keys [request] :as opts}]
  (reset! fn-answer
          (try (if on (fntrace/arm! opts) (fntrace/disarm!))
               {:request request}
               (catch Throwable t
                 (attempt/rethrow-fatal! t)
                 {:request request :error (ex-message t)}))))

(defn tell!
  [tracer on opts]
  (when-let [broadcast (ports/cluster-broadcast)]
    (broadcast :observe/arm {:tracer tracer :on (boolean on)
                             :opts (not-empty (dissoc opts :file))})))

(defn- event-brief
  [e]
  (-> (select-keys e [:seq :t :ms :conn :role :thread :span :update-count :rows :failed :error
                      :explainable])
      (assoc :sql (some-> (:sql e) str (subs 0 (min 300 (count (str (:sql e)))))))
      (cond-> (:sql-shape e) (assoc :sql-shape (select-keys (:sql-shape e)
                                                            [:chars :folds :repeat])))))

(defn report
  []
  (let [sql  (sqltrace/since 0)
        snap (when (prof/armed?) (prof/snapshot))]
    (cond-> {:node (node/id) :role node/role :at (System/currentTimeMillis)
             :sql  (-> (dissoc sql :events)
                       (assoc :events (mapv event-brief (kept-events (:events sql))))
                       (update :inflight #(mapv event-brief %)))
             :shapes (sqltrace/shapes-report {:limit 25 :sort :total})
             :fns (merge (fntrace/node-report) @fn-answer)
             :decisions (merge (dtrace/node-report) @decision-answer)}
      snap (assoc :profile (merge (prof/status snap)
                                  {:cpu     (report/busiest snap {:limit 15})
                                   :markers (report/marker-summary snap)})))))

(defn- publish!
  []
  (when-let [{:keys [publish!]} (ports/observe-reports)]
    (try (publish! (report))
         (catch Throwable t
           (attempt/rethrow-fatal! t)
           (println (str "[observe] report not kept: " (.getMessage t)))))))

(defonce ^:private was-armed (atom false))

(defn- tick! []
  (let [armed (or (sqltrace/armed?) (prof/armed?) (fntrace/armed?) (dtrace/armed?))]
    (when (or armed @was-armed) (publish!))
    (reset! was-armed armed)))

(defn reports
  []
  (when-let [{:keys [recent]} (ports/observe-reports)]
    (try (vec (remove #(= (node/id) (:node %)) (recent)))
         (catch Throwable t
           (attempt/rethrow-fatal! t)
           (println (str "[observe] other nodes' reports unreadable: " (ex-message t)))
           []))))

(defn- arm-here!
  [{:keys [tracer on opts]}]
  (when (remote?)
    (case tracer
      :sql     (arm-sql! on opts)
      :profile (arm-profile! on opts))
    (println (str "[observe] " (name tracer) " " (if on "armed" "disarmed") " here"))
    (publish!)
    (reset! was-armed (boolean on))))

(defn- arm-fn-heard!
  [{:keys [on opts]}]
  (when (remote?)
    (arm-fns-here! on opts)
    (println (str "[observe] fn " (if on "armed" "disarmed") " here"))
    (publish!)
    (reset! was-armed (boolean on))))

(def ^:private collect-wait-ms 10000)

(defonce ^:private answered (atom {}))

(defn- collect-here!
  [{:keys [request]}]
  (when (remote?)
    (future
      (try
        (let [snap (prof/snapshot)
              any?  (or (pos? (long (or (:sample-count snap) 0)))
                        (pos? (long (or (:marker-count snap) 0))))
              proc (str "sutra " (name node/role))]
          (when-let [{:keys [publish!]} (ports/observe-recordings)]
            (publish! {:node (node/id) :role node/role :request request
                       :gecko (when any? (gecko/profile snap {:label proc :process proc}))
                       :status (prof/status snap)
                       :decisions (dtrace/records-text)})))
        (catch Throwable t
          (attempt/rethrow-fatal! t)
          (println (str "[observe] recording not kept: " (.getMessage t))))
        (finally
          (when-let [broadcast (ports/cluster-broadcast)]
            (broadcast :observe/collected {:request request :node (node/id)})))))))

(defn- collected! [{:keys [request node]}]
  (swap! answered (fn [m] (if (contains? m request) (update m request conj node) m))))

(defn- alive-samples []
  (when-let [{:keys [recent]} (ports/telemetry-samples)]
    (try (filterv #(< (long (or (:age-ms %) 0)) 120000) (recent))
         (catch Throwable t
           (attempt/rethrow-fatal! t)
           (println (str "[observe] telemetry unreadable: " (ex-message t)))
           nil))))

(defn- alive-nodes []
  (some->> (alive-samples) (into #{} (map :node))))

(defn recordings
  []
  (let [alive    (alive-nodes)
        expected (into #{} (comp (map :node) (filter #(or (nil? alive) (alive %))))
                       (reports))
        broadcast (ports/cluster-broadcast)
        port      (ports/observe-recordings)]
    (if (or (empty? expected) (nil? broadcast) (nil? port))
      {:processes [] :nodes []}
      (let [request (str (java.util.UUID/randomUUID))
            until   (+ (System/currentTimeMillis) collect-wait-ms)]
        (swap! answered assoc request #{})
        (try
          (broadcast :observe/collect {:request request})
          (while (and (not (every? (get @answered request) expected))
                      (< (System/currentTimeMillis) until))
            (Thread/sleep 100))
          (let [got (into {} (for [r ((:recent port)) :when (= request (:request r))]
                               [(:node r) r]))]
            {:processes (into [] (keep :gecko) (vals got))
             :decisions (vec (for [[n r] (sort-by key got) :when (:decisions r)]
                               {:node n :role (:role r) :text (:decisions r)}))
             :nodes (vec (for [n (sort expected) :let [r (get got n)]]
                           (if r
                             (cond-> {:node n :role (:role r) :status (:status r)}
                               (nil? (:gecko r)) (assoc :empty true))
                             {:node n :missing true})))})
          (finally (swap! answered dissoc request)))))))

(def not-covered
  ["Python-Compute: eigener Prozess, nur seine Zeit als Build-Phase"
   "Vorgänge in Postgres selbst: Locks, Autovacuum, Plan-Wahl (nur `trace --explain`)"
   "Verbindungen, die vor dem Scharfschalten aus dem Pool geholt wurden"
   "Prozesse, die während des Fensters starten (neue Deploy-Farbe): sie sind nicht scharf"])

(defn side-row
  [process node {:keys [armed seen shapes? in-ring events age-ms]}]
  (cond-> {:process process :node node :answered true :armed (boolean armed)
           :statements (long (or seen 0)) :shapes (boolean shapes?)}
    in-ring (assoc :in-ring in-ring)
    events  (assoc :in-report (count events))
    age-ms  (assoc :report-age-ms age-ms)))

(defn scope
  [{:keys [own egress reports alive]}]
  (let [live?   #(or (nil? alive) (contains? alive (:node %)))
        by-node (coll/index-by :node reports)
        expect  (for [[node role] alive :when (not (#{"api" "all"} (name role)))] [node role])]
    {:processes
     (vec (concat
           [(side-row "api" (:node own) (dissoc own :events))]
           (for [[agent side] egress]
             (if side
               (side-row "egress" (name agent) (dissoc side :events))
               {:process "egress" :node (name agent) :answered false}))
           (for [{:keys [node role] :as r} (filter live? reports)]
             (side-row (name role) node r))
           (for [[node role] expect :when (not (by-node node))]
             {:process (name role) :node node :answered false})))
     :past-reports (vec (sort (map :node (remove live? reports))))
     :not-covered  not-covered}))

(defn alive-others
  []
  (some->> (alive-samples)
           (remove #(= (node/id) (:node %)))
           (into {} (map (juxt :node :role)))))

(def ^:private obey-wait-ms 4000)

(defn- await-reports
  [obeyed?]
  (let [until  (+ (System/currentTimeMillis) obey-wait-ms)
        alive  (alive-others)
        expect (into #{} (keep (fn [[n role]] (when-not (#{"api" "all"} (name role)) n))) alive)]
    (loop []
      (let [rs      (reports)
            obeyed  (into #{} (keep #(when (obeyed? %) (:node %))) rs)]
        (if (or (every? obeyed expect) (>= (System/currentTimeMillis) until))
          {:reports rs :alive alive}
          (do (Thread/sleep 150) (recur)))))))

(defn await-obeyed
  [on]
  (await-reports #(= (boolean on) (boolean (get-in % [:sql :armed])))))

(def fn-not-covered
  ["Egress und Python-Compute: eigene Prozesse ohne Function-Tracer"
   "Funktionen mit Primitiv-Signatur, Protokoll-Methoden, Makros, Lokale"
   "Namespaces, die erst nach dem Scharfschalten geladen werden"
   "Prozesse, die während des Fensters starten (neue Deploy-Farbe): sie sind nicht scharf"])

(defn switch-fns!
  [on opts]
  (let [own     (if on (fntrace/arm! opts) (fntrace/disarm!))
        request (str (java.util.UUID/randomUUID))]
    (when-let [broadcast (ports/cluster-broadcast)]
      (broadcast :observe/arm-fn {:on (boolean on) :opts (assoc opts :request request)}))
    (await-reports #(= request (get-in % [:fns :request])))
    (assoc own :request request)))

(defn- arm-decisions-heard!
  [{:keys [on opts]}]
  (when (remote?)
    (reset! decision-answer
            (try (if on (dtrace/arm! opts) (dtrace/disarm!))
                 {:request (:request opts)}
                 (catch Throwable t
                   (attempt/rethrow-fatal! t)
                   {:request (:request opts) :error (ex-message t)})))
    (println (str "[observe] decisions " (if on "recorded" "no longer recorded") " here"))
    (publish!)
    (reset! was-armed (boolean on))))

(defn switch-decisions!
  [on {:keys [kinds]}]
  (let [own     (if on (dtrace/arm! {:kinds kinds}) (dtrace/disarm!))
        request (str (java.util.UUID/randomUUID))]
    (when-let [broadcast (ports/cluster-broadcast)]
      (broadcast :observe/arm-decisions {:on (boolean on)
                                         :opts {:kinds (vec kinds) :request request}}))
    (let [{:keys [reports]} (await-reports #(= request (get-in % [:decisions :request])))]
      (assoc own :request request
             :nodes (mapv #(merge (select-keys % [:node :role :age-ms]) (:decisions %))
                          reports)))))

(defn- report-now!
  [{:keys [request]}]
  (when (remote?)
    (swap! fn-answer assoc :read request)
    (publish!)))

(defn refresh-fns!
  []
  (when-let [broadcast (ports/cluster-broadcast)]
    (let [request (str (java.util.UUID/randomUUID))]
      (broadcast :observe/report-now {:request request})
      (await-reports #(= request (get-in % [:fns :read])))
      request)))

(defn fn-scope
  [{:keys [reports alive request refresh limit sort-key]}]
  (let [counts? (if request #(= request (get-in % [:fns :request])) :fns)
        live?   #(or (nil? alive) (contains? alive (:node %)))
        nodes   (filter (every-pred live? counts?) reports)
        heard   (into #{} (map :node) nodes)]
    {:nodes (mapv (fn [{:keys [fns node role age-ms]}]
                    (-> fns
                        (update :rows fntrace/sorted-rows sort-key limit)
                        (assoc :node node :role role :age-ms age-ms)
                        (cond-> (and refresh (not= refresh (:read fns))) (assoc :stale true))))
                  nodes)
     :silent (vec (for [[node role] alive
                        :when (and (not (heard node)) (not (#{"api" "all"} (name role))))]
                    {:node node :role role}))
     :not-covered fn-not-covered}))

(defn fn-nodes
  [opts]
  (fn-scope (assoc opts :reports (reports) :alive (alive-others))))

(defonce ^:private publisher (periodic/slot))

(defn start! []
  (when (remote?)
    (periodic/start! publisher "sutra-observe" publish-ms publish-ms tick!)))

(defn stop! [] (periodic/stop! publisher))

(def module
  {:name        :observe
   :jobs        [{:name :observe :start start! :stop stop!}]
   :contributes {:cluster/listeners {:observe/arm       arm-here!
                                     :observe/arm-fn    arm-fn-heard!
                                     :observe/arm-decisions arm-decisions-heard!
                                     :observe/report-now report-now!
                                     :observe/collect   collect-here!
                                     :observe/collected collected!}}})
