(ns sutra.foundation.sql-trace
  (:require [sutra.kit.attempt :as attempt]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.foundation.armed :as armed]
            [sutra.foundation.confine :as confine]
            [sutra.foundation.encoding :as enc]
            [sutra.foundation.inflight :as inflight]
            [sutra.foundation.profile :as prof]
            [sutra.kit.effects :as effects])
  (:import (java.lang.reflect Proxy InvocationHandler Method InvocationTargetException)
           (java.sql Connection Statement PreparedStatement CallableStatement ResultSet)
           (java.util.concurrent ConcurrentHashMap)
           (java.util.function BiFunction)
           (clojure.lang Reflector)
           (org.postgresql PGConnection)
           (org.postgresql.copy CopyManager)
           (org.postgresql.core BaseConnection)))

(set! *warn-on-reflection* true)

(def ^:const ring-capacity
  5000)

(defonce ^:private state
  (atom {:role "backend" :events [] :file nil :capture-params? true :armed-at nil}))

(defonce ^:private running
  (inflight/registry))
(defonce ^:private shapes (ConcurrentHashMap.))
(defonce ^:private span-shapes (ConcurrentHashMap.))
(defonce ^:private sources (ConcurrentHashMap.))
(defonce ^:private pending (ConcurrentHashMap.))
(defonce ^:private kept (atom []))
(def ^:private kept-capacity 30)
(def ^:private default-keep-ms 500)
(defonce ^:private write-lock (Object.))
(defonce ^:private writer* (atom nil))

(declare on-arm! on-disarm!)

(defonce ^:private switch
  (armed/switch {:label "sql-trace" :on-arm #'on-arm! :on-disarm #'on-disarm!}))

(defn armed?
  []
  (armed/armed? switch))

(defn- trunc [s n]
  (let [s (str s)] (if (> (count s) n) (str (subs s 0 n) "…") s)))

(defn- fmt-param
  [v]
  (cond
    (nil? v)      "NULL"
    (bytes? v)    (str "<bytes " (alength ^bytes v) ">")
    (string? v)   (trunc v 200)
    :else         (trunc (str v) 200)))

(defn- write-line!
  [ev]
  (locking write-lock
    (let [{:keys [file]} @state]
      (when (and (armed?) file)
        (try
          (let [{:keys [p ^java.io.Writer w]} @writer*
                ^java.io.Writer w (if (and w (= p file)) w
                                      (let [nw (io/writer file :append true)]
                                        (reset! writer* {:p file :w nw}) nw))]
            (.write w ^String (json/write-str ev))
            (.write w "\n")
            (.flush w))
          (catch Throwable t
            (attempt/rethrow-fatal! t)
            (swap! state (fn [s] (cond-> (update s :write-errors (fnil inc 0))
                                   (= file (:file s)) (assoc :file nil))))))))))

(defn- close-writer! []
  (locking write-lock
    (when-let [{:keys [^java.io.Writer w]} @writer*]
      (attempt/silently (.flush w) (.close w)))
    (reset! writer* nil)))

(defn- template [sql]
  (-> (str sql)
      (str/replace #"\s+" " ")
      (str/replace #"_[0-9a-f]{32}" "_<id>")
      (trunc 160)))

(def ^:private fold-head
  #"\(SELECT\s+(?:COUNT|AVG|MIN|MAX|COALESCE\(SUM)\b")

(def ^:private join-head
  #"LEFT JOIN \(SELECT\s")

(defn- closing
  [^String s ^long i]
  (let [n (.length s)]
    (loop [j i depth 0 quoted? false]
      (if (>= j n)
        nil
        (let [c (.charAt s j)]
          (cond
            quoted?       (recur (inc j) depth (not= \' c))
            (= \' c)      (recur (inc j) depth true)
            (= \( c)      (recur (inc j) (inc depth) false)
            (= \) c)      (if (= 1 depth) (inc j) (recur (inc j) (dec depth) false))
            :else         (recur (inc j) depth false)))))))

(defn- folds
  [^String s]
  (let [m (re-matcher fold-head s)]
    (loop [acc []]
      (if-not (.find m)
        acc
        (let [i (.start m)
              e (closing s i)]
          (recur (cond-> acc e (conj (subs s i e)))))))))

(defn shape
  [sql]
  (let [s  (str sql)
        fs (folds s)
        d  (count (set fs))
        js (count (re-seq join-head s))]
    (cond-> {:chars (count s) :folds (count fs) :distinct d}
      (pos? d)  (assoc :repeat (long (Math/round (double (/ (count fs) d)))))
      (pos? js) (assoc :fold-joins js))))

(defn absorb-shape
  [m {:keys [sql path ms chars]}]
  (update m [(template sql) path]
          (fn [[c t n]]
            [(inc (long (or c 0)))
             (+ (long (or t 0)) (long (or ms 0)))
             (max (long (or n 0)) (long (or chars 0)))])))

(defn- tally! [ev]
  (.compute ^ConcurrentHashMap shapes [(template (:sql ev)) (:path ev)]
            (reify BiFunction
              (apply [_ k v] (get (absorb-shape {k v} ev) k))))
  (when-let [watched (some->> (:span ev) (.get ^ConcurrentHashMap span-shapes))]
    (swap! watched absorb-shape ev)))

(def ^:private shape-orders
  {"total" (comp - :total-ms)
   "chars" (comp - :sql-chars)
   "calls" (comp - :calls)})

(defn- shape-rows
  [tallied {:keys [limit sort]}]
  (let [rows (for [[[sql path] [c t n]] tallied]
               {:calls c :total-ms t :sql-chars n :sql sql :path path})]
    {:shapes (count rows)
     :rows   (vec (take (or limit 15)
                        (sort-by (get shape-orders (some-> sort name) (comp - :calls)) rows)))}))

(defn shapes-report
  [opts]
  (shape-rows shapes opts))

(defn watch-span!
  [span]
  (.put ^ConcurrentHashMap span-shapes span (atom {}))
  nil)

(defn unwatch-span! [span]
  (.remove ^ConcurrentHashMap span-shapes span)
  nil)

(defn span-shapes-report
  [span opts]
  (shape-rows (some-> (.get ^ConcurrentHashMap span-shapes span) deref) opts))

(defn- begin! [label sql params binds ^Statement stmt]
  (let [id (inflight/open! running
                           (cond-> {:ev "begin" :conn label :role (:role @state)
                                    :thread (.getName (Thread/currentThread))
                                    :span (prof/current-span)
                                    :sql (some-> sql (trunc 4000))
                                    :sql-shape (some-> sql shape)
                                    :params (when (:capture-params? @state) (mapv fmt-param params))
                                    :stmt stmt}
                             (:shapes? @state) (assoc :path (inflight/stack))))]
    (write-line! (-> (inflight/entry running id) (dissoc :t0 :stmt :path)))
    (when (and sql (not (re-find #"(?i)^\s*COPY\b" sql)))
      (.put ^ConcurrentHashMap pending id {:sql sql :binds binds}))
    id))

(defn- profile-marker!
  [{:keys [sql sql-shape conn update-count] :as ev} ms]
  (prof/mark-ms! (if (:http ev) :HTTP :SQL) ms
                 (cond-> {:sql (trunc sql 300) :conn (str conn)}
                   (:folds sql-shape) (assoc :folds (:folds sql-shape))
                   (:chars sql-shape) (assoc :chars (:chars sql-shape))
                   update-count       (assoc :rows update-count)
                   (:path ev)         (assoc :path (str/join " <- " (take 4 (:path ev))))
                   (:failed ev)       (assoc :failed true))))

(defn- keep-whole!
  [id inf ms]
  (let [whole (.remove ^ConcurrentHashMap pending id)]
    (when (and whole (>= (long (or ms 0)) (long (:keep-ms @state default-keep-ms))))
      (swap! kept enc/conj-capped
             (assoc whole :seq id :ms ms :conn (:conn inf) :t (:t inf))
             kept-capacity)
      true)))

(defn- end! [id info error]
  (let [inf (inflight/close! running id)
        ms  (inflight/running-ms inf)
        exact (inflight/running-exact-ms inf)
        whole? (keep-whole! id inf ms)
        ev  (cond-> (-> (or inf {:seq id}) (dissoc :t0 :stmt)
                        (assoc :ev "end" :ms ms
                               :chars (:chars (:sql-shape inf))
                               :update-count (:update-count info)
                               :rows (:rows info))
                        (cond-> (:status info) (assoc :status (:status info))))
              whole? (assoc :explainable true)
              error (assoc :error (trunc (some-> ^Throwable error .getMessage) 500) :failed true))]
    (when (:path ev) (tally! ev))
    (swap! state (fn [s] (-> s
                             (update :events enc/conj-capped (dissoc ev :path :chars) ring-capacity)
                             (update :seen (fnil inc 0)))))
    (write-line! (dissoc ev :path :chars))
    (profile-marker! ev exact)
    nil))

(defn- call-begin
  [target]
  (let [id (inflight/open! running {:ev "begin" :conn "http" :http true :role (:role @state)
                                    :thread (.getName (Thread/currentThread))
                                    :span (prof/current-span)
                                    :sql (trunc target 400)})]
    (write-line! (dissoc (inflight/entry running id) :t0))
    (fn finish [result error]
      (end! id {:status (when (map? result) (or (:status result) (:exit result)))}
            (when (instance? Throwable error) error))
      result)))

(defn- result-info [^Statement stmt r]
  (try
    (cond
      (instance? ResultSet r)           {:rows "resultset"}
      (and (some? r) (.isArray (class r))) {:update-count (reduce + 0 (map long (seq r)))}
      (number? r)                       {:update-count (long r)}
      (instance? Boolean r)             (if r {:rows "resultset"}
                                            {:update-count (.getUpdateCount stmt)})
      :else                             nil)
    (catch java.sql.SQLException _ nil)))

(defn- invoke*
  [^Method m target args]
  (try (.invoke m target (object-array (or args [])))
       (catch InvocationTargetException e (throw (.getCause e)))))

(defn- run-traced
  [label sql params binds stmt f]
  (let [id      (begin! label sql params binds stmt)
        outcome (volatile! [nil nil])]
    (try
      (let [r (f)]
        (vreset! outcome [(result-info stmt r) nil])
        r)
      (catch Throwable e
        (vreset! outcome [nil e])
        (throw e))
      (finally
        (let [[info error] @outcome] (end! id info error))))))

(defn- statement-iface [stmt]
  (cond (instance? CallableStatement stmt) CallableStatement
        (instance? PreparedStatement stmt) PreparedStatement
        :else Statement))

(defn- wrap-statement [^Statement target label sql]
  (let [prepared? (instance? PreparedStatement target)
        params    (atom (sorted-map))
        binds     (atom (sorted-map))
        h (reify InvocationHandler
            (invoke [_ _proxy m args]
              (let [^Method m m
                    nm (.getName m)
                    n  (if args (alength args) 0)]
                (cond
                  (and prepared? (str/starts-with? nm "set") (>= n 2)
                       (integer? (aget args 0)))
                  (do (swap! params assoc (int (aget args 0)) (fmt-param (aget args 1)))
                      (swap! binds assoc (int (aget args 0)) [m (aclone args)])
                      (invoke* m target args))

                  (= nm "clearParameters")
                  (do (reset! params (sorted-map))
                      (reset! binds (sorted-map))
                      (invoke* m target args))

                  (str/starts-with? nm "execute")
                  (if-not (armed?)
                    (invoke* m target args)
                    (run-traced label
                                (or sql (when (and (pos? n) (string? (aget args 0)))
                                          (aget args 0)))
                                (vals @params) (vals @binds) target
                                #(invoke* m target args)))

                  :else (invoke* m target args)))))]
    (Proxy/newProxyInstance (clojure.lang.RT/baseLoader)
                            (into-array Class [(statement-iface target)]) h)))

(defn- traced-copy-manager
  [^Connection target ^CopyManager real label]
  (proxy [CopyManager] [(.unwrap target BaseConnection)]
    (copyIn [& args]
      (let [call #(Reflector/invokeInstanceMethod real "copyIn" (object-array args))]
        (if (and (armed?) (next args))
          (run-traced label (first args) nil nil nil call)
          (call))))))

(defn- wrap-pg-connection
  [^Connection target ^PGConnection pg label]
  (let [h (reify InvocationHandler
            (invoke [_ _proxy m args]
              (let [^Method m m
                    r (invoke* m pg args)]
                (if (= "getCopyAPI" (.getName m))
                  (traced-copy-manager target r label)
                  r))))]
    (Proxy/newProxyInstance (clojure.lang.RT/baseLoader)
                            (into-array Class [PGConnection]) h)))

(defn wrap-connection
  ^Connection [^Connection target label]
  (let [h (reify InvocationHandler
            (invoke [_ _proxy m args]
              (let [^Method m m
                    nm (.getName m)]
                (case nm
                  ("prepareStatement" "prepareCall")
                  (let [stmt  (invoke* m target args)
                        sql (when (and args (pos? (alength args)) (string? (aget args 0)))
                              (aget args 0))]
                    (wrap-statement stmt label sql))
                  "createStatement"
                  (wrap-statement (invoke* m target args) label nil)
                  "unwrap"
                  (let [r (invoke* m target args)]
                    (if (= PGConnection (aget ^objects args 0))
                      (wrap-pg-connection target r label)
                      r))
                  (invoke* m target args)))))]
    (Proxy/newProxyInstance (clojure.lang.RT/baseLoader)
                            (into-array Class [Connection]) h)))

(defn wrap-datasource
  (^javax.sql.DataSource [raw label] (wrap-datasource raw label true))
  (^javax.sql.DataSource [^javax.sql.DataSource raw label always?]
   (.put ^ConcurrentHashMap sources (str label) raw)
   (let [wrap (fn [^Connection c] (if (or always? (armed?)) (wrap-connection c label) c))
         acquire (fn [f] (prof/with-mark :Pool {:conn (str label)} (f)))]
     (reify javax.sql.DataSource
       (getConnection [_]   (wrap (acquire #(.getConnection raw))))
       (getConnection [_ u p] (wrap (acquire #(.getConnection raw u p))))
       (unwrap [_ iface] (.unwrap raw iface))
       (isWrapperFor [_ iface] (.isWrapperFor raw iface))
       (getLoginTimeout [_] (.getLoginTimeout raw))
       (setLoginTimeout [_ s] (.setLoginTimeout raw s))
       (getLogWriter [_] (.getLogWriter raw))
       (setLogWriter [_ w] (.setLogWriter raw w))
       (getParentLogger [_] (.getParentLogger raw))))))

(defn set-role!
  [role] (swap! state assoc :role role) role)

(defn trace-dir
  []
  (or (not-empty (System/getenv "SUTRA_TRACE_DIR"))
      (str (System/getProperty "java.io.tmpdir") "/sutra-trace")))

(defn trace-file
  [asked]
  (when (seq (str asked))
    (let [base (.getName (io/file (str asked)))
          base (if (re-matches #"[A-Za-z0-9][A-Za-z0-9._-]{0,99}" base)
                 base
                 (str "sql-" (System/currentTimeMillis) ".jsonl"))
          f    (confine/confine-path (trace-dir) base)]
      (io/make-parents f)
      (str f))))

(defn- on-arm!
  [{:keys [capture-params? shapes? keep-ms] :as opts}]
  (let [file (trace-file (:file opts))]
    (when (not= file (:file @state)) (close-writer!))
    (.clear ^ConcurrentHashMap shapes)
    (swap! state assoc
           :shapes? (boolean shapes?)
           :keep-ms (or keep-ms default-keep-ms)
           :armed-at (System/currentTimeMillis)
           :seen 0
           :file file
           :capture-params? (if (nil? capture-params?) true (boolean capture-params?))))
  (effects/observe-calls! call-begin)
  {:file (:file @state) :role (:role @state)})

(defn- on-disarm!
  []
  (effects/observe-calls! nil)
  (locking write-lock
    (swap! state assoc :file nil)
    (close-writer!))
  nil)

(defn arm!
  ([] (arm! nil))
  ([opts] (armed/arm! switch opts)))

(defn disarm!
  []
  (armed/disarm! switch))

(defn shapes?
  []
  (boolean (and (armed?) (:shapes? @state))))

(defn clear!
  []
  (swap! state assoc :events []
         :armed-at (when (armed?) (System/currentTimeMillis)))
  {:cleared true})

(defn- cancel-stmt!
  [entry]
  (when-let [^Statement stmt (:stmt entry)]
    (try (.cancel stmt) true (catch java.sql.SQLException _ false))))

(defn kill!
  [seq]
  (if-let [entry (inflight/entry running seq)]
    {:killed (long seq) :ok (boolean (cancel-stmt! entry))
     :sql (:sql entry) :conn (:conn entry)}
    {:killed nil}))

(defn kill-all!
  []
  {:killed (mapv (fn [e] {:seq (:seq e) :ok (boolean (cancel-stmt! e))
                          :sql (:sql e) :conn (:conn e)})
                 (inflight/entries running))})

(defn- merge-spans
  [spans]
  (->> (sort-by first spans)
       (reduce (fn [{:keys [total lo hi]} [s e]]
                 (cond
                   (nil? lo)  {:total total :lo s :hi e}
                   (<= s hi)  {:total total :lo lo :hi (max hi e)}
                   :else      {:total (+ total (- hi lo)) :lo s :hi e}))
               {:total 0 :lo nil :hi nil})
       ((fn [{:keys [total lo hi]}] (if lo (+ total (- hi lo)) 0)))))

(defn coverage
  ([] (coverage @state))
  ([{:keys [events armed-at]}]
   (when armed-at
     (let [inflight (inflight/entries running)
           now    (System/currentTimeMillis)
           window (max 1 (- now armed-at))
           spans  (into (vec (for [{:keys [t ms]} events
                                   :when (and t ms)
                                   :let [s (max (long t) armed-at)
                                         e (min (+ (long t) (long ms)) now)]
                                   :when (< s e)]
                               [s e]))
                        (for [{:keys [t]} inflight
                              :when t
                              :let [s (max (long t) armed-at)]
                              :when (< s now)]
                          [s now]))
           busy   (merge-spans spans)]
       {:window-ms window
        :busy-ms   busy
        :covered   (double (/ busy window))
        :spans     (count spans)}))))

(defn events->bus
  [agent events]
  (into []
        (comp (filter #(and (:t %) (:ms %)))
              (map (fn [{:keys [t ms sql sql-shape conn update-count span failed http status]}]
                     {:kind (if http :HTTP :SQL)
                      :epoch-ms t
                      :dur ms
                      :track (str "egress " agent)
                      :span span
                      :data (cond-> {:sql (trunc sql 300) :conn (str conn)}
                              (:folds sql-shape) (assoc :folds (:folds sql-shape))
                              (:chars sql-shape) (assoc :chars (:chars sql-shape))
                              update-count       (assoc :rows update-count)
                              status             (assoc :status status)
                              failed             (assoc :failed true))})))
        events))

(defn since
  [after]
  (let [{:keys [events role file] :as snap} @state
        after (long (or after 0))
        newer (filterv #(> (long (:seq %)) after) events)
        cursor (reduce (fn [m e] (max m (long (:seq e)))) after events)]
    {:armed (armed?) :lifecycle (armed/status switch)
     :role role :file file :cursor cursor
     :seen (long (or (:seen snap) 0))
     :shapes? (boolean (:shapes? snap))
     :in-ring (count events)
     :events newer
     :inflight (inflight/live running #(dissoc % :t0 :stmt :path))
     :coverage (coverage snap)}))

(defn- loop-ms [node]
  (* (long (or (get node "Actual Loops") 1)) (double (or (get node "Actual Total Time") 0))))

(defn- plan-nodes
  [node depth]
  (let [total (loop-ms node)
        kids  (get node "Plans")
        self  (- total (reduce + 0.0 (map loop-ms kids)))
        on    (or (get node "Relation Name") (get node "Subplan Name")
                  (get node "CTE Name") (get node "Index Name"))]
    (cons (cond-> {:node     (get node "Node Type")
                   :depth    depth
                   :self-ms  (Math/round (double (max 0.0 self)))
                   :total-ms (Math/round (double total))
                   :loops    (get node "Actual Loops")
                   :rows     (get node "Actual Rows")
                   :est-rows (get node "Plan Rows")}
            on                     (assoc :on on)
            (get node "Alias")     (assoc :alias (get node "Alias"))
            (get node "Join Type") (assoc :join (get node "Join Type"))
            (get node "Filter")    (assoc :filter (trunc (get node "Filter") 200))
            (get node "Hash Cond") (assoc :cond (trunc (get node "Hash Cond") 200))
            (get node "Shared Read Blocks")
            (assoc :read-blocks (get node "Shared Read Blocks")))
          (mapcat #(plan-nodes % (inc depth)) kids))))

(defn explain-summary
  [plan-json {:keys [limit]}]
  (let [[top] (json/read-str plan-json)
        nodes (plan-nodes (get top "Plan") 0)]
    {:planning-ms  (get top "Planning Time")
     :execution-ms (get top "Execution Time")
     :nodes        (count nodes)
     :top          (vec (take (or limit 12) (sort-by (comp - :self-ms) nodes)))}))

(defn kept-statements []
  (mapv #(select-keys % [:seq :ms :conn :t]) @kept))

(defn- explain-on
  [^Connection c sql binds timeout-ms]
  (with-open [s (.createStatement c)]
    (.execute s (str "SET LOCAL statement_timeout = " (long (or timeout-ms 25000)))))
  (with-open [ps (.prepareStatement
                  c (str "EXPLAIN (ANALYZE, BUFFERS, FORMAT JSON) " sql))]
    (doseq [[m args] binds] (invoke* m ps args))
    (with-open [rs (.executeQuery ps)]
      (.next rs)
      (.getString rs 1))))

(defn explain!
  [seq {:keys [limit raw timeout-ms]}]
  (let [hit (some #(when (= (long seq) (long (:seq %))) %) @kept)
        ^javax.sql.DataSource ds (some->> (:conn hit) str (.get ^ConcurrentHashMap sources))]
    (cond
      (nil? hit) {:refused (str "statement " seq " is not kept — only one slower than "
                                (:keep-ms @state default-keep-ms)
                                " ms while armed keeps its whole text")
                  :kept    (kept-statements)}
      (nil? ds)  {:refused (str "no pool " (:conn hit) " to explain on")}
      :else
      (with-open [c (.getConnection ds)]
        (let [auto (.getAutoCommit c)]
          (try
            (.setAutoCommit c false)
            (.setReadOnly c true)
            (let [plan (explain-on c (:sql hit) (:binds hit) timeout-ms)]
              (cond-> (assoc (explain-summary plan {:limit limit})
                             :seq (long seq) :traced-ms (:ms hit) :conn (:conn hit)
                             :sql (trunc (:sql hit) 300) :chars (count (:sql hit)))
                raw (assoc :plan plan :sql-full (:sql hit)
                           :params (mapv (fn [[_ args]] (mapv str args)) (:binds hit)))))
            (finally
              (try (.rollback c) (.setReadOnly c false) (.setAutoCommit c auto)
                   (catch Exception e
                     (attempt/rethrow-fatal! e)
                     (println (str "[sql-trace] connection not restored after explain: "
                                   (ex-message e))))))))))))
