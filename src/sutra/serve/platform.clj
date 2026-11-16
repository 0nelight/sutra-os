(ns sutra.serve.platform
  (:require [sutra.kit.refusal :as refusal]
            [sutra.foundation.egress :as egress]
            [sutra.foundation.ops :as op]
            [sutra.foundation.rpc :as rpc]
            [sutra.http.openapi :as openapi]
            [sutra.catalog.lookup :as lookup]
            [sutra.kit.anomaly :as anomaly]
            [sutra.kit.attempt :as attempt]
            [sutra.catalog.grants :as grants]
            [sutra.projects.api :as entity-rows]
            [sutra.actions.dispatch :as c]
            [sutra.objects.reads :as objects]
            [sutra.store.seat :as seat]
            [sutra.serve.lifecycle :as lc]
            [sutra.serve.hot-patches :as hot]
            [clojure.string :as str]
            [sutra.foundation.encoding :as enc]
            [sutra.parser.spec :as spec]
            [sutra.catalog.functions :as functions]
            [sutra.kit.system :as sys]
            [sutra.store.access.scope :as ctx]
            [sutra.foundation.sql-trace :as sqltrace]
            [sutra.foundation.fn-trace :as fntrace]
            [sutra.foundation.decision-trace :as dtrace]
            [sutra.kit.decisions :as decisions]
            [sutra.foundation.profile :as prof]
            [sutra.serve.observe :as observe]
            [sutra.store.storage.node :as node]
            [sutra.foundation.profile.gecko :as gecko]
            [sutra.foundation.profile.report :as report]
            [sutra.store.storage.instance-config :as ic]
            [sutra.store.migrations.world-history :as world-history]
            [sutra.access.sessions :as sessions]
            [sutra.sources.protocols :as api]
            [sutra.sources.datasources :as ds]
            [sutra.builds.run :as runs]
            [sutra.builds.ledger.stack-map :as stack-map]
            [sutra.evals.jobs :as eval-jobs]
            [sutra.evals.suites :as evals]
            [sutra.actions.effects :as action-effects]
            [sutra.authoring.http.oms :as oms]
            [sutra.sources.api :as sources-api]
            [sutra.sources.http :as source]
            [sutra.http.endpoint :as ep]
            [sutra.http.security-log :as seclog]
            [sutra.http.responses :refer [ok object-set-rows]]
            [sutra.store.access.run-log :as run-log]
            [sutra.builds.plan.outputs :as outputs]
            [sutra.foundation.env :as env]))

(set! *warn-on-reflection* true)

(defn schema-code-blobs
  [{:keys [body-params] store :sutra/store}]
  (let [opts {:blobs  (ctx/blobs)
              :apply? (boolean (:apply? body-params))
              :limit  (:limit body-params)}]
    (if-let [f (sys/provided (sys/of store) :store/legacy-maintenance)]
      (ok (f opts))
      (ok {:skipped "the store driver keeps no pre-store schema history"}))))

(defn replay-history
  [{:keys [request-method body-params] store :sutra/store}]
  (ok (if (= :get request-method)
        (world-history/status)
        (world-history/start! (:db store) {:apply?  (boolean (:apply? body-params))
                                           :verify? (boolean (:verify? body-params))
                                           :repair? (boolean (:repair? body-params))
                                           :drop-texts? (boolean (:drop-texts? body-params))
                                           :dedupe? (boolean (:dedupe? body-params))
                                           :threads (:threads body-params)}))))

(defn prune-resolution
  [{:keys [body-params] store :sutra/store schema :sutra/schema branch :sutra/branch}]
  (let [resolved (into #{} (filter #(outputs/resolved-object-type? schema %))
                       (keys (:object-types schema)))
        census   (entity-rows/entity-state-by-type store branch)]
    (ok {:branch    branch
         :resolved  (vec resolved)
         :by-type   (mapv (fn [{:keys [object-type rows entities]}]
                            {:object-type object-type
                             :name (get-in schema [:object-types object-type
                                                   :object-type-name])
                             :rows rows :entities entities
                             :resolved? (contains? resolved object-type)})
                          census)
         :elsewhere (vec (entity-rows/entity-state-elsewhere store branch))
         :swept     (entity-rows/sweep-entity-resolution! store branch resolved
                                                          {:apply? (boolean (:apply? body-params))})
         :applied?  (boolean (:apply? body-params))})))

(defn datasource-query
  [{:keys [path-params body-params] store :sutra/store schema :sutra/schema branch :sutra/branch}]
  (let [ds-name (:name path-params)
        sql     (:sql body-params)
        limit   (max 1 (min 1000 (or (enc/safe-long (:limit body-params)) 50)))]
    (if-not (and (string? sql) (seq sql))
      (refusal/refused :refusal.serve/missing-sql {})
      (source/with-datasource-spec schema branch ds-name
        (fn [spec]
          (ok {:datasource (keyword ds-name)
               :rows       (api/with-source [s (ctx/connect (grants/source-authority store)) spec]
                             (api/query s sql limit))}))))))

(defn- node-traces [reports]
  (mapv (fn [{:keys [sql] :as r}]
          (assoc sql :node (:node r) :role (:role r) :age-ms (:age-ms r) :shapes (:shapes r)))
        reports))

(defn- own-side [side]
  (assoc side :node (node/id)))

(defn- side-scope
  [backend egress reports alive]
  (observe/scope {:own (own-side backend) :egress egress
                  :reports (node-traces reports) :alive alive}))

(defn trace-arm
  [{:keys [body-params]}]
  (let [{:keys [on file egress-file capture-params shapes keep-ms]} body-params
        opts    {:file file :capture-params? capture-params :shapes? shapes
                 :keep-ms (enc/safe-long keep-ms)}
        backend (observe/arm-sql! on opts)
        _       (observe/tell! :sql on opts)
        egress  (ds/trace-arm! on {:file egress-file :capture-params? capture-params})
        {:keys [reports alive]} (observe/await-obeyed on)]
    (ok {:backend backend :egress egress
         :egress-reachable (every? some? (vals egress))
         :scope (side-scope (sqltrace/since Long/MAX_VALUE) egress reports alive)})))

(def trace-snapshot
  (ep/handler :trace
              {:call (fn [{:keys [b e]}]
                       (let [egress (ds/trace-since e)]
                         (when (prof/armed?)
                           (doseq [[agent side] egress]
                             (prof/ingest! (sqltrace/events->bus agent (:events side)))))
                         (let [backend (sqltrace/since b)
                               reports (observe/reports)]
                           {:backend backend :egress egress
                            :nodes (node-traces reports)
                            :scope (side-scope (sqltrace/since Long/MAX_VALUE) egress reports
                                               (observe/alive-others))})))}))

(defn trace-kill
  [{:keys [body-params]}]
  (let [{:keys [seq all]} body-params
        backend (if all (sqltrace/kill-all!) (sqltrace/kill! seq))
        egress  (ds/trace-kill! (if all {:all true} {:seq seq}))]
    (ok {:backend backend :egress egress})))

(defn trace-explain
  [{:keys [body-params]}]
  (let [{:keys [seq limit raw]} body-params]
    (if-let [n (enc/safe-long seq)]
      (ok (sqltrace/explain! n {:limit (enc/safe-long limit) :raw (boolean raw)}))
      (ok {:kept (sqltrace/kept-statements)}))))

(defn fn-trace
  [{:keys [body-params]}]
  (let [{:keys [on off prefixes limit sort shapes slow-ms]} body-params]
    (ok (if shapes
          (sqltrace/shapes-report {:limit limit :sort sort})
          (let [own (cond
                      on  (observe/switch-fns! true {:prefixes prefixes :slow-ms slow-ms})
                      off (observe/switch-fns! false {})
                      :else {})
                refresh (when-not (or on off) (observe/refresh-fns!))]
            (merge own
                   (fntrace/report {:limit limit :sort-by-key sort})
                   (observe/fn-nodes {:request (:request own) :refresh refresh :limit limit
                                      :sort-key sort})))))))

(defn- node-decisions
  [{:keys [node role text]}]
  (try {:records (mapv #(assoc % :node node :role role) (decisions/read-text text))}
       (catch Exception e {:unreadable {:node node :error (ex-message e)}})))

(defn decision-trace
  [{:keys [body-params]}]
  (let [{:keys [on off kinds save]} body-params]
    (ok (cond
          on   (observe/switch-decisions! true {:kinds kinds})
          off  (observe/switch-decisions! false {})
          save (let [own    {:node (node/id) :role node/role
                             :text (decisions/text (decisions/records))}
                     parts  (map node-decisions (cons own (:decisions (observe/recordings))))
                     all    (vec (sort-by :at (mapcat :records parts)))]
                 {:count     (count all)
                  :kinds     (frequencies (map :kind all))
                  :unreadable (vec (keep :unreadable parts))
                  :edn       (decisions/text all)})
          :else (assoc (dtrace/node-report)
                       :nodes (mapv #(merge (select-keys % [:node :role :age-ms])
                                            (:decisions %))
                                    (observe/reports)))))))

(defn profile-handler
  [{:keys [body-params]}]
  (let [{:keys [on off interval depth gecko label limit events]} body-params]
    (ok (cond
          events (merge {:ingested 0} (prof/ingest! events))

          (or on off)
          (let [opts {:interval interval :depth depth}]
            (observe/tell! :profile (boolean on) opts)
            (let [armed (observe/arm-profile! (boolean on) opts)
                  {:keys [reports alive]} (observe/await-obeyed (boolean on))]
              (assoc armed :scope (side-scope (sqltrace/since Long/MAX_VALUE) nil reports alive))))

          gecko (let [snap  (prof/snapshot)
                      other (observe/recordings)]
                  {:gecko (gecko/profile snap {:label label :processes (:processes other)})
                   :nodes (:nodes other)
                   :status (prof/status snap)})

          :else (let [snap (prof/snapshot)]
                  (merge (prof/status snap)
                         {:cpu     (report/busiest snap {:limit limit})
                          :markers (report/marker-summary snap)
                          :shapes  (sqltrace/shapes-report {:limit (or limit 10)})
                          :live    (:live snap)
                          :nodes   (into [] (keep (fn [{:keys [profile shapes] :as r}]
                                                    (when profile
                                                      (assoc profile :shapes shapes
                                                             :node (:node r) :role (:role r)
                                                             :age-ms (:age-ms r)))))
                                         (observe/reports))}))))))

(defn- egress-allowlist-data
  [datasources proxy-policies]
  (let [xf?   (fn [s] (or (:external-transform s)
                          (contains? spec/default-source-types (:type s))))
        norm  (fn [e] (some-> e str str/trim not-empty))
        hosts (fn [srcs]
                (vec (sort (distinct (for [s srcs e (:egress s) :let [e (norm e)] :when e] e)))))
        proxied? (fn [s] (= :agent-proxy (sources-api/connection s)))
        by    (group-by egress/agent-of (remove proxied? (vals datasources)))
        proxied (for [s (filter proxied? (vals datasources)) e (:egress s)
                      a (sources-api/proxy-agents proxy-policies (norm e))]
                  [a (norm e)])]
    {:egress (hosts (get by egress/default-agent))
     :agents (merge-with (comp vec sort distinct concat)
                         (into (sorted-map)
                               (for [[a srcs] (dissoc by egress/default-agent)] [a (hosts srcs)]))
                         (update-vals (group-by first proxied) #(mapv second %)))
     :xf     (hosts (filter xf? (remove proxied? (vals datasources))))}))

(def egress-allowlist
  (ep/handler :egress-allowlist
              {:respond
               (fn [{:keys [store schema branch format]}]
                 (if branch
                   (let [ps (sources-api/egress-policies store)
                         {:keys [egress agents xf]}
                         (-> (egress-allowlist-data (source/datasources schema) ps)
                             (update :egress #(sources-api/egress-effective ps %))
                             (update :xf #(sources-api/egress-effective ps %))
                             (update :agents update-vals #(sources-api/egress-effective ps %)))]
                     (if (= "text" format)
                       {:status  200
                        :headers {"Content-Type" "text/plain; charset=utf-8"}
                        :body    (str (str/join "\n" (concat (map #(str "ALL " %) egress)
                                                             (for [[a hs] agents h hs]
                                                               (str "AGENT " a " " h))
                                                             (map #(str "XF " %) xf)))
                                      "\n")}
                       (ok {:egress egress :agents agents :xf xf})))
                   (refusal/refused :refusal.http/no-branch {})))}))

(def list-audit
  (ep/handler :audit
              {:call (fn [{:keys [store branch limit]}]
                       {:audit (run-log/audit (ctx/at store branch) limit)})}))

(defn list-login-audit
  [{:keys [query-params] store :sutra/store}]
  (let [limit  (or (some-> (get query-params "limit") not-empty parse-long) 500)
        params (cond-> {}
                 (not-empty (get query-params
                                 "user"))     (assoc :username (get query-params "user"))
                 (not-empty (get query-params
                                 "outcome"))  (assoc :outcome  (get query-params "outcome"))
                 (not-empty (get query-params
                                 "provider")) (assoc :provider (get query-params "provider")))]
    (ok {:events (sessions/read-login-log store params limit)})))

(def list-eval-suites
  (ep/handler :eval-suites {:call #(evals/list-all (:store %) (:schema %) (:branch %))
                            :wrap :suites}))

(def upsert-eval-suite
  (ep/handler :eval-suites {:call #(evals/upsert! (:store %) (:schema %) (:branch %) (:body %))}))

(def add-eval-case
  (ep/handler
   :eval-suite-cases
   {:respond (fn [{:keys [store schema branch body req] nm :name}]
               (let [{:keys [declared spec suite]}
                     (evals/with-case store schema branch nm (:case body))]
                 (if declared
                   (oms/stage-schema req {:schema   {:eval-suites {declared spec}}
                                          :preview? (boolean (:preview body))}
                                     nil)
                   (ok (evals/upsert! store schema branch suite)))))}))

(def run-eval
  (ep/handler :eval-run
              {:respond (fn [{:keys [store schema branch body edge]}]
                          (let [{:keys [suite]} body
                                opts (assoc (select-keys body [:iterations :parallel :model
                                                               :version :meta :experiment])
                                            :edge edge)]
                            (evals/check! store schema branch suite opts)
                            (if (:preview body)
                              (ok (evals/preview store schema branch suite opts))
                              (ok (eval-jobs/start!
                                   store branch (if (map? suite) (:name suite) suite)
                                   #(evals/run-suite! store schema branch suite
                                                      (assoc opts :progress %)))))))}))

(def eval-job
  (ep/handler :eval-job
              {:respond (fn [{:keys [store branch id]}]
                          (if-let [j (eval-jobs/job store branch id)]
                            (ok j)
                            (refusal/refused :refusal.serve/no-eval-job-on-this {:id id} {:id id})))}))

(def eval-results
  (ep/handler :eval-results {:call #(evals/results (:store %) (:branch %) (:id %)) :wrap :results}))

(def eval-run-cases
  (ep/handler :eval-run-cases {:call #(evals/run-cases (:store %) (:branch %) (:id %))}))

(defn platform-experience
  [{store :sutra/store}]
  (ok (ic/platform-experience store)))

(defn set-platform-experience
  [{:keys [body-params] store :sutra/store}]
  (ok (ic/set-platform-experience! store body-params)))

(defn- run-query-function
  [store branch schema fn-name inputs]
  (let [args (into {} (map (fn [[k v]] [(keyword (name k)) v])) inputs)]
    (try
      (ok {:function fn-name
           :kind     :query
           :rows     (object-set-rows
                      (action-effects/run-query-function
                       (ctx/at store branch) schema fn-name args))})
      (catch clojure.lang.ExceptionInfo e
        (let [d (ex-data e)]
          (if (or (anomaly/client-error? d) (:function d) (= fn-name (:transform d)))
            (refusal/refused :refusal.serve/passed-getmessage-e (assoc (select-keys d [:alias :kind :scalars :language])
                                                :function fn-name) {:message (.getMessage e)})
            (throw e)))))))

(defn run-function
  [{:keys [body-params] store :sutra/store current :sutra/schema branch :sutra/branch
    auth :sutra/auth}]
  (let [{:keys [function-name language source types inputs target-type]} body-params
        entry  (when function-name
                 (functions/function-by-name current function-name))
        rid    (when function-name (lookup/->type-rid current :functions function-name))
        stored (when function-name (get-in current [:functions rid]))
        query  (when (= :query (:kind entry))
                 (lookup/->type-rid current :transforms (:name entry)))]
    (cond
      query         (grants/authorize-read! auth (get-in current [:transforms query]) query)
      function-name (grants/authorize-read! auth stored rid)
      :else         (grants/authorize! auth {:op :update} "ad-hoc code"))
    (cond
      (= :query (:kind entry))
      (if branch
        (run-query-function store branch current (:name entry) inputs)
        (refusal/refused :refusal.http/no-branch {}))

      (and function-name (nil? stored))
      (refusal/refused :refusal.serve/stored-function-not-found {:function-name function-name})

      :else
      (let [wf (if stored
                 {:language    (:language stored)
                  :source      (functions/function-code (functions/function-lookup current)
                                                        (:function-name stored) stored)
                  :types       (or types (:param-types stored))
                  :inputs      inputs
                  :target-type (:target-type stored)
                  :staged-writes (:staged-writes stored)
                  :timeout-ms    (:timeout-ms stored)
                  :snapshot      (:snapshot stored)
                  :function-name (:function-name stored)}
                 {:language    (keyword (name (or language :clojure)))
                  :source      source
                  :types       types
                  :inputs      inputs
                  :target-type (some-> target-type name keyword)})
            result (runs/with-run!
                     [[wst schema] (ctx/at store branch)
                      {:kind :function
                       :inputs {:function-name (some-> function-name name keyword)
                                :inputs inputs
                                :target-type (some-> target-type name keyword)}}]
                     (let [{:keys [compiled result]} (c/submit! store schema [(c/function wf)])
                           decisions
                           (when-let [drid
                                      (and result
                                           (lookup/->type-rid schema :object-types :decision))]
                             (objects/objects-of-type
                              (ctx/at wst (or (:branch result) (:branch wst))) schema drid))
                           step-count (count (:steps compiled))]
                       {:compiled-steps step-count
                        :object-sets (vec (keys (:object-sets compiled)))
                        :step-count step-count
                        :decisions (vec decisions)
                        :summary {:steps step-count
                                  :decisions (count decisions)}}))]
        (ok {:compiled {:steps (:compiled-steps result)
                        :object-sets (:object-sets result)}
             :decisions (:decisions result)
             :run-id (:run-id result)
             :snapshot-id (:snapshot-id result)})))))

(defn reboot [{store :sutra/store}]
  (let [h2  (lc/start!)
        branch-store (ctx/at store (:branch h2))]
    (ok {:ok true
         :branch (:branch h2)
         :counts (:counts (objects/app-snapshot branch-store (seat/branch-schema branch-store)))})))

(defn reload-schema
  [{store :sutra/store}]
  (let [s (lc/reload-schema! store)]
    (ok {:reloaded     true
         :object-types (count (:object-types s))
         :link-types   (count (:link-types s))
         :schema-error (seat/schema-load-error)})))

(defn code-reload
  [req]
  (if-not (hot/armed?)
    (refusal/refused :refusal.serve/hot-reload-disarmed-needs-sutra {})
    (let [body  (:body-params req)
          files (or (:files body) (get body "files"))]
      (if (empty? files)
        (refusal/refused :refusal.serve/no-files-to-reload {})
        (try
          (let [files (mapv (fn [f] {:path (or (:path f) (get f "path"))
                                     :code (or (:code f) (get f "code"))})
                            files)
                done  (mapv (fn [{:keys [path code]}] (hot/load-source! path code)) files)
                proof (hot/verify files)
                bad   (filterv (complement :ok) proof)]
            (when-not (some #{"sutra.serve.server"} done)
              (require 'sutra.serve.server :reload))
            (if (seq bad)
              (refusal/refused :refusal.serve/the-hot-patch-cannot-be {:unproven bad
                :verified proof
                :reloaded done})
              (do (hot/record! files)
                  (when-let [broadcast (sys/provided (sys/of (:sutra/store req))
                                                   :cluster/broadcast)]
                    (broadcast :hot/patched {:files (mapv :path files)}))
                  (ok {:reloaded done :count (count done) :verified proof}))))
          (catch Throwable t
            (attempt/rethrow-fatal! t)
            {:status 500
             :body {:error (str "hot reload failed: " (.getMessage t))
                    :class (.getName (class t))
                    :trace (mapv str (take 6 (.getStackTrace t)))}}))))))

(defn restore-point
  [req]
  (let [{:keys [take! roll-back!] points :list}
        (sys/provided (sys/of (:sutra/store req)) :datasets/restore-points)
        to (some-> (or (get-in req [:query-params "to"])
                       (get-in req [:query-params :to])
                       (get-in req [:parameters :query :to]))
                   str str/trim not-empty parse-long)
        label (or (get-in req [:body-params :label]) (get-in req [:body-params "label"]))]
    (cond
      (nil? points)                  {:status 501
                                      :body {:error "the datasets driver keeps no restore points"}}
      (= :get (:request-method req)) (ok (points))
      to                             (ok (roll-back! to))
      (str/blank? (str label))       (refusal/refused :refusal.serve/a-restore-point-needs-a {})
      :else                          (ok (take! label)))))

(def ^:private client-log-path (env/env "SUTRA_CLIENT_LOG" "/var/sutra/reach/client.log"))

(defn- map-client-stack
  [stack]
  (when (and (not-empty stack) (re-find stack-map/frame-re (str stack)))
    (attempt/silently (not-empty (stack-map/map-stack stack)))))

(def ^:private client-log-max-entries 50)

(defn- log-text
  [n v]
  (seclog/encode (enc/elide n (str v))))

(defn client-log-handler
  [{:keys [body-params principal]}]
  (let [given   (let [es (:entries body-params)]
                  (cond (sequential? es) es (some? es) [] :else [body-params]))
        entries (->> given (filter map?) (take client-log-max-entries))
        user    (or (:user-id principal) "anon")
        now     (str (java.time.Instant/now))
        mapped* (atom nil)
        lines   (for [{:keys [level message stack url ts ua]} entries]
                  (let [mapped (when (nil? @mapped*) (map-client-stack (str stack)))]
                    (when mapped (reset! mapped* mapped))
                    (str now " [" (log-text 20 (or level "log")) "] user=" (log-text 64 user)
                         " url=" (log-text 300 url)
                         (when ts (str " cts=" (log-text 24 ts)))
                         (when ua (str " ua=" (log-text 200 ua)))
                         " :: " (log-text 2000 message)
                         (when (not-empty (str stack)) (str " stack=" (log-text 6000 stack)))
                         (when mapped (str " mapped=" (log-text 6000 mapped)))
                         "\n")))]
    (try
      (spit client-log-path (apply str lines) :append true)
      (catch Exception _
        (binding [*out* *err*] (print (apply str "[client-log] " lines)) (flush))))
    (ok (cond-> {:logged (count entries)}
          (< (count entries) (count given)) (assoc :dropped (- (count given) (count entries)))
          @mapped* (assoc :mapped @mapped*)))))

(def ^:private client-log-line
  #"^(\S+) \[([^\]]*)\] user=(\S*) url=(\S*)(?: cts=\S*)?(?: ua=(.*?))? :: (.*)$")

(defn- client-log-entry
  [line]
  (when-let [[_ at level user url ua more] (re-matches client-log-line line)]
    (let [[body mapped] (str/split more #" mapped=" 2)
          [msg stack]   (str/split body #" stack=" 2)
          lines         #(str/replace % "\\n" "\n")]
      (cond-> {:at at :level level :user user :url url :message msg}
        ua     (assoc :ua ua)
        stack  (assoc :stack (lines stack))
        mapped (assoc :mapped (lines mapped))))))

(def ^:private client-log-tail-bytes (* 8 1024 1024))

(defn- client-log-tail
  []
  (let [f (java.io.File. ^String client-log-path)]
    (when (.exists f)
      (with-open [raf (java.io.RandomAccessFile. f "r")]
        (let [len  (.length raf)
              from (max 0 (- len client-log-tail-bytes))
              buf  (byte-array (- len from))]
          (.seek raf from)
          (.readFully raf buf)
          (cond->> (str/split-lines (String. buf "UTF-8")) (pos? from) rest))))))

(defn- since-instant
  [s]
  (when-not (str/blank? s)
    (if-let [[_ n u] (re-matches #"(\d+)([smhd])" s)]
      (str (.minusSeconds (java.time.Instant/now)
                          (* (parse-long n) ({"s" 1 "m" 60 "h" 3600 "d" 86400} u))))
      s)))

(defn list-client-log
  [{:keys [query-params]}]
  (let [q      #(not-empty (get query-params %))
        limit  (or (some-> (q "limit") parse-long) 50)
        levels (some-> (q "level") (str/split #",") set)
        since  (since-instant (q "since"))
        text   (q "q")
        keep?  (fn [{:keys [at level user url message]}]
                 (and (if levels (levels level) (not= "navigation" level))
                      (or (nil? (q "user")) (= (q "user") user))
                      (or (nil? since) (>= (compare at since) 0))
                      (or (nil? text) (str/includes? (str message " " url) text))))
        hits   (->> (reverse (client-log-tail)) (keep client-log-entry) (filter keep?)
                    (take limit) vec)]
    (ok {:entries (mapv (fn [e] (cond-> e
                                  (and (:stack e) (not (:mapped e)))
                                  (assoc :mapped (map-client-stack (:stack e)))))
                        hits)})))

(def ^:private openapi-text (delay (openapi/render (openapi/document))))

(defn- openapi-handler
  [_]
  {:status  200
   :headers {"Content-Type" "application/json; charset=utf-8"}
   :body    @openapi-text})

(defn ops-call
  [{:keys [uri] store :sutra/store :as req}]
  (if-not (op/accepts-remote? op/*topology*)
    {:status 404 :headers {"Content-Type" "application/edn"} :body "{:ok false}"}
    (rpc/handler
     {uri (fn [envelope]
            (let [{:keys [op kind in branch authority schema?]}
                  (op/open (op/peer-secret) (System/currentTimeMillis) envelope)
                  b   (or branch "main")
                  s   (ctx/act-for (ctx/at store b) authority)
                  ctx (cond-> {:store s}
                        schema? (assoc :schema (ep/request-schema
                                                (assoc req :sutra/store s :sutra/branch b))))]
              (op/serve ctx op kind in)))}
     (update req :headers dissoc rpc/request-key-header)
     {:token (op/peer-token)})))

(def module
  {:name        :platform
   :contributes {:http/routes
                 {:audit                   {:get list-audit}
                  :client-log              {:post client-log-handler}
                  :client-log-entries      {:get list-client-log}
                  :code-reload             {:post code-reload}
                  :datasource-query        {:post datasource-query}
                  :egress-allowlist        {:get egress-allowlist}
                  :eval-results            {:get eval-results}
                  :eval-run-cases          {:get eval-run-cases}
                  :eval-run                {:post run-eval}
                  :eval-job                {:get eval-job}
                  :eval-suites             {:get list-eval-suites :post upsert-eval-suite}
                  :eval-suite-cases        {:post add-eval-case}
                  :decision-trace          {:post decision-trace}
                  :fn-trace                {:post fn-trace}
                  :function-run            {:post run-function}
                  :login-audit             {:get list-login-audit}
                  :openapi                 {:get openapi-handler}
                  :ops-call                {:post ops-call}
                  :platform-experience     {:get platform-experience}
                  :platform-experience-set {:post set-platform-experience}
                  :profile                 {:post profile-handler}
                  :prune-resolution        {:post prune-resolution}
                  :replay-history          {:get replay-history :post replay-history}
                  :reboot                  {:post reboot}
                  :restore-point           {:get restore-point :post restore-point}
                  :schema-code-blobs       {:post schema-code-blobs}
                  :schema-reload           {:post reload-schema}
                  :trace                   {:get trace-snapshot}
                  :trace-arm               {:post trace-arm}
                  :trace-kill              {:post trace-kill}
                  :trace-explain           {:post trace-explain}}}})
