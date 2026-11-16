(ns sutra.sources.policy
  (:require [sutra.kit.refusal :as refusal]
            [sutra.kit.redact :as redact]
            [clojure.string :as str]
            [sutra.types.source :as source]))

(set! *warn-on-reflection* true)

(defn- connector? [type] (some? (source/connector type)))

(defn- network-type?
  [type]
  (let [e (:endpoint (source/profile type))]
    (boolean (if (connector? type) e (#{:jdbc-url :host} e)))))

(defn- jdbc-network-type? [type]
  (and (not (connector? type)) (= :jdbc-url (:endpoint (source/profile type)))))

(defn connector-route
  [{:keys [type secret-cipher]}]
  (let [r (:route (source/profile type))]
    (cond (= #{:agent} r)  :agent
          (= #{:direct} r) :direct
          secret-cipher    :agent
          :else            :direct)))

(defn- agent-reached-type? [type]
  (contains? (:capabilities (source/profile type)) :land))

(def default-ssh-port 22)

(defn egress-routed?
  [{:keys [type secret-cipher user password]}]
  (boolean (and (jdbc-network-type? type)
                secret-cipher
                (not (and user password)))))

(defn agent-routed?
  [{:keys [type secret-cipher]}]
  (boolean (or secret-cipher (agent-reached-type? type)
               (and (connector? type) (= :agent (connector-route {:type type}))))))

(def ^:private jdbc-host "([A-Za-z0-9._-]+|\\[[0-9A-Fa-f:.]+\\])")

(def ^:private jdbc-shapes
  {:postgres [(re-pattern (str "jdbc:postgresql://" jdbc-host
                               "(?::(\\d+))?(?:/[^?/;]*)?(?:\\?(.*))?"))
              #"&"]
   :mysql    [(re-pattern (str "jdbc:mysql://" jdbc-host "(?::(\\d+))?(?:/[^?/;]*)?(?:\\?(.*))?"))
              #"&"]
   :mssql    [(re-pattern (str "jdbc:sqlserver://" jdbc-host
                               "(?:\\\\[A-Za-z0-9_$-]+)?(?::(\\d+))?(?:;(.*))?"))
              #";"]})

(def ^:private refused-jdbc-options
  #{"socketfactory" "socketfactoryarg" "sslfactory" "sslfactoryarg" "sslhostnameverifier"
    "sslpasswordcallback" "authenticationpluginclassname" "loggerfile" "loggerlevel"
    "pghost" "pgport" "host" "port" "servername" "server" "address" "addr" "networkaddress"
    "failoverpartner" "socketfactoryclass" "socketfactoryconstructorarg"
    "accesstokencallbackclass" "trustmanagerclass" "trustmanagerconstructorarg"
    "keystorelocation" "allowloadlocalinfile" "allowloadlocalinfileinpath"
    "allowurlinlocalinfile" "autodeserialize" "queryinterceptors" "statementinterceptors"
    "exceptioninterceptors" "connectionlifecycleinterceptors" "propertiestransform"
    "serverconfigcachefactory" "clientinfoprovider" "authenticationplugins"
    "defaultauthenticationplugin" "localsocket"})

(defn jdbc-host-port
  [type jdbc-url]
  (when-let [[re] (jdbc-shapes type)]
    (when-let [[_ host port] (re-matches re (str jdbc-url))]
      [host (some-> port Integer/parseInt)])))

(defn- refused-option [opts sep]
  (some (fn [o]
          (let [k (str/lower-case (str/trim (first (str/split o #"=" 2))))]
            (when (or (refused-jdbc-options k) (str/includes? k "%"))
              (str "option " k " is not allowed"))))
        (remove str/blank? (str/split (or opts "") sep))))

(defn jdbc-url-problem
  [type jdbc-url]
  (let [url (str jdbc-url)]
    (if (= :sqlite type)
      (when-not (re-matches #"jdbc:sqlite:(?::memory:|/[^?]*)" url)
        "a sqlite source names an absolute file path, without options")
      (if-let [[re sep] (jdbc-shapes type)]
        (if-let [[_ _ _ opts] (re-matches re url)]
          (refused-option opts sep)
          (str "not a single-host " (name type) " JDBC URL"))
        (str "a source of type " (pr-str type) " opens no JDBC URL")))))

(defn assert-jdbc-url!
  [{:keys [type jdbc-url source-name] :as spec}]
  (when-let [why (jdbc-url-problem type jdbc-url)]
    (refusal/refuse! :refusal.foreign/jdbc-url-refused
                     {:source source-name :type type :reason why} {:reason why}))
  spec)

(defn read-mode [spec] (source/read-mode spec))

(defn capable?
  [spec c]
  (contains? (:capabilities (source/profile (:type spec))) c))

(defn jdbc-network? [spec] (jdbc-network-type? (:type spec)))

(defn csv? [spec] (source/csv-route? (source/profile (:type spec))))

(defn connection
  [spec]
  (or (:connection spec) (get-in spec [:config :connection]) :direct))

(defn route
  [spec]
  (cond
    (connector? (:type spec)) (connector-route spec)
    (csv? spec)           :csv
    (egress-routed? spec) :agent
    :else                 :direct))

(defn- url-host-port [url]
  (try (let [u (java.net.URI. (str url))
             p (.getPort u)]
         (when (.getHost u)
           [(.getHost u) (if (neg? p) (if (= "https" (.getScheme u)) 443 80) p)]))
       (catch java.net.URISyntaxException _ nil)))

(defn spec-endpoint [spec]
  (let [{:keys [type jdbc-url host port base-url]} (source/lifted spec)]
    (case (:endpoint (source/profile type))
      :host     [host (or port (when-not (connector? type) default-ssh-port))]
      :jdbc-url (jdbc-host-port type jdbc-url)
      :base-url (when (connector? type) (url-host-port base-url))
      nil)))

(defn- egress-target
  [spec]
  (let [[host port] (spec-endpoint spec)]
    (str host (when port (str ":" port)))))

(defn- egress-allowed?
  [{:keys [egress] :as spec}]
  (let [[host _] (spec-endpoint spec)
        allow    (set (map str egress))]
    (boolean (or (contains? allow (egress-target spec))
                 (and host (contains? allow host))))))

(defn egress-permits?
  [{:keys [egress]} host port]
  (let [allow (set (map str egress))]
    (boolean (and (some? host)
                  (or (contains? allow (str host (when port (str ":" port))))
                      (contains? allow (str host)))))))

(defn assert-egress-permits!
  [spec host port]
  (when-not (egress-permits? spec host port)
    (refusal/refuse! :refusal.foreign/egress-not-allowed
                     {:target (str host (when port (str ":" port)))
                      :allow  (set (map str (:egress spec)))
                      :spec   (redact/redact spec)}))
  true)

(defn assert-egress-allowed!
  [{:keys [type egress] :as spec}]
  (when (network-type? type)
    (when (empty? egress)
      (refusal/refuse! :refusal.foreign/egress-undeclared {:spec (redact/redact spec)}
                       {:type type}))
    (when-not (egress-allowed? spec)
      (refusal/refuse! :refusal.foreign/egress-not-allowed
                       {:target (egress-target spec)
                        :allow  (set (map str egress))
                        :spec   (redact/redact spec)})))
  spec)

(def ^:private requirements
  {:path           {:ok? (fn [{:keys [path]}] (seq path))
                    :code "missing-path"
                    :message (fn [t] (str (name t) " datasource spec missing :path"))}
   :host           {:ok? (fn [{:keys [host]}] (seq host))
                    :code "missing-host"
                    :message (fn [t] (str (name t) " datasource spec missing :host"))}
   :remote-path    {:ok? (fn [{:keys [remote-path]}] (seq remote-path))
                    :code "missing-remote-path"
                    :message (fn [t] (str (name t) " datasource spec missing :remote-path"))}
   :command        {:ok? (fn [{:keys [command]}] (seq command))
                    :code "missing-command"
                    :message (fn [t] (str (name t) " datasource spec missing :command"))}
   :allow-commands {:ok? (fn [{:keys [allow-commands]}] (seq allow-commands))
                    :code "missing-allow-commands"
                    :message (fn [t]
                               (str (name t)
                                    " datasource must declare a non-empty :allow-commands whitelist"))}
   :jdbc-url       {:ok? (fn [{:keys [jdbc-url]}] (seq jdbc-url))
                    :code "missing-jdbc-url"
                    :message (fn [_] "datasource spec missing :jdbc-url")}
   :egress         {:ok? (fn [{:keys [egress]}] (seq egress))
                    :code "missing-egress"
                    :message (fn [t]
                               (str (name t)
                                    " datasource must declare a non-empty :egress allowlist"))}
   :egress-allowed {:ok? egress-allowed?
                    :code "egress-blocked"
                    :message (fn [t]
                               (str (name t) " endpoint not on the source's :egress allowlist"))}})

(defn- health-needs [spec] (source/needs spec))

(defn- requirement
  [k]
  (or (requirements k)
      {:ok? #(some? (get % k))
       :code "missing-field"
       :message (fn [t] (str (name t) " datasource spec missing " k))}))

(defn spec-health
  [spec]
  (let [{:keys [type] :as spec} (source/lifted spec)]
    (if (nil? type)
      {:status "misconfigured" :code "missing-type" :message "datasource spec missing :type"}
      (or (some (fn [k]
                  (let [{:keys [ok? code message]} (requirement k)]
                    (when-not (ok? spec)
                      {:status "misconfigured" :code code :message (message type)})))
                (health-needs spec))
          {:status "healthy"}))))

(defn runtime-health
  [spec agent-up?]
  (let [h (spec-health spec)]
    (if (and (= "healthy" (:status h))
             (agent-routed? spec)
             (not agent-up?))
      {:status "unreachable" :code "agent-down"
       :message "egress agent unreachable"}
      h)))

(defn assert-export-allowed!
  [{:keys [markings export-markings source-name] :as spec}]
  (when (seq markings)
    (let [allowed (set export-markings)
          leaking (remove allowed markings)]
      (when (seq leaking)
        (refusal/refuse! :refusal.foreign/export-markings
                         {:source source-name :leaking (vec leaking) :export-markings allowed}
                         {:source (or source-name "?")}))))
  spec)

(defn assert-command-allowed!
  [{:keys [type command allow-commands] :as spec}]
  (when (= :command type)
    (when (empty? allow-commands)
      (refusal/refuse! :refusal.foreign/commands-undeclared {:spec (redact/redact spec)}))
    (when-not (contains? (set allow-commands) command)
      (refusal/refuse! :refusal.foreign/command-not-allowed
                       {:command command :allow-commands (set allow-commands)
                        :spec (redact/redact spec)})))
  spec)

(defn safe-ident
  [s]
  (let [s (str s)]
    (when-not (re-matches #"[A-Za-z_][A-Za-z0-9_]*" s)
      (refusal/refuse! :refusal.foreign/unsafe-identifier {:identifier s}))
    s))

(def pk-separator
  "_|_")

(defn pk-parts
  [pk-columns pk-value]
  (let [parts (str/split (str pk-value) #"_\|_" -1)]
    (when-not (= (count parts) (count pk-columns))
      (refusal/refuse! :refusal.foreign/composite-key-arity
                       {:pk-value pk-value :pk-columns pk-columns}
                       {:parts (count parts) :needs (count pk-columns)}))
    parts))

(defn scan-key
  [spec table]
  (let [wb   (let [w (:writeback spec)] (or (get w table) (get w (keyword table))))
        rs   (or (:rest spec) (get-in spec [:config :rest]))
        cols (cond (seq (:pk-columns wb)) (:pk-columns wb)
                   (:pk wb)               [(:pk wb)]
                   :else
                   (some-> (get-in rs [:resources (keyword table) :key]) vector))]
    (when (seq cols) (mapv #(keyword (str/lower-case (name %))) cols))))

(defn row-key
  [ks row]
  (if (= 1 (count ks))
    (get row (first ks))
    (str/join pk-separator (map #(str (get row %)) ks))))

(defn- log-rows
  [^java.time.LocalDateTime now {:keys [columns constants]} table pk-value changed expect actor
   correlation]
  (let [{actor-cols :actor date-col :date time-col :time corr-col :correlation} columns]
    (vec (for [[col v] (sort-by key changed)]
           (cond-> (merge (or constants {})
                          {(:table columns)    table
                           (:pk columns)       (str pk-value)
                           (:column columns)   col
                           (:previous columns) (some-> (get expect col) str)
                           (:next columns)     (some-> v str)}
                          (zipmap actor-cols (repeat (str/upper-case (str actor)))))
             date-col (assoc date-col (str (.toLocalDate now)))
             time-col (assoc time-col (format "%02d:%02d:%02d"
                                              (.getHour now) (.getMinute now) (.getSecond now)))
             (and corr-col correlation) (assoc corr-col (str correlation)))))))

(defn- actor-stamp
  [table allow op actor]
  (when-let [cols (seq (get-in allow [:actor-columns op]))]
    (when (str/blank? (str actor))
      (refusal/refuse! :refusal.foreign/actor-stamp {:table table :actor-columns (vec cols)}
                       {:op (name op)}))
    (zipmap cols (repeat (str/upper-case (str/trim (str actor)))))))

(defn- delete-plan
  [now table allow {:keys [pk-value expect actor correlation]}]
  (let [{:keys [pk deletable? delete-guard log column-map]} allow
        ->col  #(get column-map % %)
        expect (update-keys expect ->col)
        guard  (mapv name (or delete-guard []))]
    (when-not deletable?
      (refusal/refuse! :refusal.foreign/not-deletable {:table table}))
    (when (nil? pk-value)
      (refusal/refuse! :refusal.foreign/pk-value-missing {:table table}))
    (when (empty? guard)
      (refusal/refuse! :refusal.foreign/no-delete-guard {:table table}))
    (when-let [missing (seq (remove #(contains? expect %) guard))]
      (refusal/refuse! :refusal.foreign/expect-guard-missing {:table table :missing (vec missing)}))
    (when (and (map? log) (str/blank? (str actor)))
      (refusal/refuse! :refusal.foreign/logged-delete-needs-actor
                       {:table table :log-table (:table log)}))
    (let [before (select-keys expect guard)]
      {:op :delete :table table :pk pk :pk-value pk-value :expect before
       :pk-columns (:pk-columns allow)
       :log (when (map? log)
              {:table (:table log)
               :rows  (log-rows now (update log :constants merge (:delete-constants log))
                                table pk-value
                                (zipmap (keys before) (repeat nil))
                                before actor correlation)
               :expect-count (count before)})})))

(defn- insert-plan
  [now table allow {:keys [returning actor correlation] ins :insert :as write-spec}]
  (let [{:keys [insertable-columns identity log autonumber sequence]} allow
        ic (set (map name insertable-columns))]
    (when (empty? ins)
      (refusal/refuse! :refusal.foreign/insert-empty {:write-spec write-spec}))
    (when (empty? ic)
      (refusal/refuse! :refusal.foreign/no-insertable-columns {:table table}))
    (when-let [bad (seq (remove ic (map name (keys ins))))]
      (refusal/refuse! :refusal.foreign/columns-not-insertable
                       {:bad (vec bad) :insertable ic :table table}))
    (when (and returning (not= (name returning) (some-> identity name)))
      (refusal/refuse! :refusal.foreign/returning-not-identity
                       {:returning returning :identity identity :table table}))
    (let [stamp (actor-stamp table allow :insert actor)]
      (when (and sequence (contains? ins (:column sequence)))
        (refusal/refuse! :refusal.foreign/column-drawn {:table table :column (:column sequence)}))
      (when (and (map? log) (str/blank? (str actor)))
        (refusal/refuse! :refusal.foreign/logged-insert-needs-actor
                         {:table table :log-table (:table log)}))
      (doseq [a (if (sequential? autonumber) autonumber (when autonumber [autonumber]))]
        (when (contains? ins (:into a))
          (refusal/refuse! :refusal.foreign/column-minted {:table table :column (:into a)}
                           {:from (:table a)})))
      {:op :insert :table table
       :insert (merge ins stamp)
       :stamped (vec (keys stamp))
       :returning (some-> returning name)
       :returning-columns (when (and returning (:pk-columns allow)
                                     (= (name returning) (some-> (:pk allow) name)))
                            (:pk-columns allow))
       :autonumber autonumber
       :sequence (:sequence allow)
       :log (when (map? log)
              {:table (:table log) :config log :actor actor :correlation correlation :at now})})))

(defn- update-plan
  [now table allow {:keys [pk-value expect actor correlation] sets :set :as write-spec}]
  (let [{:keys [pk writable-columns column-values log column-map]} allow
        wc     (set writable-columns)
        ->col  #(get column-map % %)
        sets   (update-keys sets ->col)
        expect (update-keys expect ->col)]
    (when (empty? sets)
      (refusal/refuse! :refusal.foreign/set-empty {:write-spec write-spec}))
    (when (nil? pk-value)
      (refusal/refuse! :refusal.foreign/pk-value-missing {:write-spec write-spec}))
    (when-let [missing (seq (remove #(contains? expect %) (keys sets)))]
      (refusal/refuse! :refusal.foreign/expect-set-missing {:table table :missing (vec missing)}))
    (let [changed (into {} (remove (fn [[c v]] (= (str v) (str (get expect c)))) sets))]
      (when (empty? changed)
        (refusal/refuse! :refusal.foreign/changes-nothing {:table table :set sets}))
      (when-let [bad (seq (remove wc (keys changed)))]
        (refusal/refuse! :refusal.foreign/columns-not-writable
                         {:bad (vec bad) :writable wc :table table}))
      (doseq [[col allowed] column-values
              :when (contains? changed col)
              :let [v (get changed col)]]
        (when-not (contains? (set (map str allowed)) (str v))
          (refusal/refuse! :refusal.foreign/value-not-allowed
                           {:table table :column col :value v :allowed (vec allowed)})))
      (when (and (map? log) (str/blank? (str actor)))
        (refusal/refuse! :refusal.foreign/logged-writeback-needs-actor
                         {:table table :log-table (:table log)}))
      {:op :update :table table :pk pk :pk-value pk-value
       :pk-columns (:pk-columns allow)
       :set (merge changed (actor-stamp table allow :update actor))
       :expect (select-keys expect (keys changed))
       :log (when (map? log)
              {:table  (:table log)
               :rows   (log-rows now log table pk-value changed expect actor correlation)
               :expect-count (count changed)})})))

(defn- insert-rows-plan
  [table allow {:keys [rows] :as write-spec}]
  (let [{:keys [insertable-columns]} allow
        ic   (set (map name insertable-columns))
        cols (vec (sort (distinct (mapcat #(map name (keys %)) rows))))]
    (when (empty? rows)
      (refusal/refuse! :refusal.foreign/insert-empty {:write-spec (dissoc write-spec :rows)}))
    (when (empty? ic)
      (refusal/refuse! :refusal.foreign/columns-not-insertable
                       {:bad cols :insertable ic :table table}))
    (when-let [bad (seq (remove ic cols))]
      (refusal/refuse! :refusal.foreign/columns-not-insertable
                       {:bad (vec bad) :insertable ic :table table}))
    (when-let [ragged (seq (remove #(= (count cols) (count %)) rows))]
      (refusal/refuse! :refusal.foreign/export-rows-ragged
                       {:table table :columns cols :row (first ragged)}))
    {:op :insert-rows :table table :columns cols
     :rows (mapv #(update-keys % name) rows)}))

(defn- truncate-plan
  [table allow]
  (when-not (:truncatable? allow)
    (refusal/refuse! :refusal.foreign/not-truncatable {:table table}))
  {:op :truncate :table table})

(defn write-plan
  [now {:keys [access writeback]} {:keys [table] :as write-spec}]
  (when-not (= :read-write access)
    (refusal/refuse! :refusal.foreign/source-read-only {:access access}))
  (let [allow (get writeback table)]
    (when-not allow
      (refusal/refuse! :refusal.foreign/table-not-writable
                       {:table table :allowed (vec (keys writeback))}))
    (cond
      (contains? write-spec :truncate) (truncate-plan table allow)
      (contains? write-spec :rows)     (insert-rows-plan table allow write-spec)
      (contains? write-spec :delete)   (delete-plan now table allow write-spec)
      (contains? write-spec :insert)   (insert-plan now table allow write-spec)
      :else                            (update-plan now table allow write-spec))))

(defn insert-log-rows
  [{:keys [config actor correlation at]} table pk-value inserted]
  (log-rows at (update config :constants merge (:insert-constants config))
            table pk-value inserted {} actor correlation))
