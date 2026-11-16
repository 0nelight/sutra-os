(ns sutra.sources.http
  (:require [sutra.kit.refusal :as refusal]
            [sutra.kit.anomaly :as anomaly]
            [sutra.kit.attempt :as attempt]
            [sutra.kit.noted :as noted]
            [clojure.edn :as edn]
            [sutra.foundation.egress :as egress]
            [sutra.foundation.rpc :as rpc]
            [sutra.sources.egress-policies :as policies]
            [sutra.sources.proxy.seal :as seal]
            [sutra.store.seat :as seat]
            [sutra.store.storage.instance-config :as ic]
            [sutra.store.access.scope :as ctx]
            [sutra.store.world :as world]
            [clojure.string :as str]
            [sutra.catalog.values :as values]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.parser.spec :as spec]
            [sutra.sources.protocols :as api]
            [sutra.sources.datasources :as ds]
            [sutra.sources.jdbc :as fjdbc]
            [sutra.sources.policy :as policy]
            [sutra.sources.writeback-check :as wb-check]
            [sutra.types.source :as source]
            [sutra.store.access.datasets :as dsets]
            [sutra.http.endpoint :as ep]
            [sutra.http.responses :refer [ok fault]])
  (:import (java.io File)))

(set! *warn-on-reflection* true)

(defn- read-staging-tables
  [store prefix limit]
  (mapv (fn [tname]
          (try
            (let [{:keys [columns rows]} (dsets/sample store tname limit)]
              {:source     :staging
               :table      tname
               :columns    columns
               :rows       rows
               :row-count  (count rows)
               :truncated? (= (count rows) limit)})
            (catch InterruptedException e (throw e))
            (catch Exception e
              {:source :staging :table tname :error (.getMessage e) :rows [] :columns []})))
        (dsets/dataset-names store prefix)))

(defn datasources [schema]
  (values/sources->datasources schema))

(def list-datasources
  (ep/handler :datasources
              {:call (fn [{:keys [schema]}] {:datasources (values/public-datasources schema)})}))

(defn request->source-spec
  [{:keys [name type jdbc-url path secret-cipher agent external-transform config
           access writeback markings export-markings]}]
  (let [type-kw    (when type (->kw type))
        descriptor (get (spec/source-types (source/connectors)) type-kw)
        connector? (some? (source/connector type-kw))
        config     (or config {})
        profile    (source/profile type-kw)
        csv?       (source/csv-route? profile)
        local?     (source/local? profile)
        unmet      (when local?
                     (first (remove #(seq (str (get {:path path :jdbc-url jdbc-url} %)))
                                    (filter #{:path :jdbc-url} (:needs profile)))))
        missing    (when descriptor
                     (->> (:fields descriptor)
                          (filter #(and (:required %) (not= :secret (:kind %))))
                          (remove #(seq (str (get config (:key %)))))
                          (mapv :key)))]
    (cond
      (not (and name type)) {:error "datasource needs :name :type"}
      unmet
      {:error (str (clojure.core/name type-kw) " datasource needs " unmet)}
      (and descriptor (seq missing))
      {:error (str (clojure.core/name type-kw) " datasource is missing required field(s): "
                   (str/join ", " (map clojure.core/name missing)))}
      (and (not local?) (not descriptor) (not (seq jdbc-url)))
      {:error "datasource needs :jdbc-url"}
      (and (not csv?) (not descriptor) (policy/jdbc-url-problem type-kw jdbc-url))
      {:error (str "datasource :jdbc-url refused: "
                   (policy/jdbc-url-problem type-kw jdbc-url))}
      :else
      (let [egress (cond
                     local?     nil
                     descriptor (spec/descriptor-egress descriptor config)
                     :else      (let [[h p] (policy/jdbc-host-port type-kw jdbc-url)]
                                  (when h [(str h (when p (str ":" p)))])))]
        {:ok (cond-> {:name (->kw name) :type type-kw
                      :access (or (some-> access ->kw) :read-only)}
               (seq secret-cipher) (assoc :secret-cipher secret-cipher)
               (seq (some-> agent clojure.core/name)) (assoc :agent (->kw agent))
               csv?         (assoc :path path)
               (and descriptor connector?)       (merge config)
               (and descriptor (not connector?)) (assoc :config config :external-transform true)
               (and (not csv?) (not descriptor)) (assoc :jdbc-url jdbc-url)
               external-transform (assoc :external-transform true)
               (seq egress) (assoc :egress egress)
               (map? writeback)      (assoc :writeback writeback)
               (seq markings)        (assoc :markings (mapv ->kw markings))
               (seq export-markings) (assoc :export-markings (mapv ->kw export-markings)))}))))

(defn datasources-after
  [{:keys [status body] :as resp}]
  (if (= 200 status)
    (ok {:datasources (values/public-datasources (:schema body))})
    resp))

(defn- connection-error
  [ds-name health]
  (refusal/refused :refusal.sources/passed-message-health
                   {:connection-error true :datasource ds-name :health health}
                   {:message (:message health)}))

(defn with-datasource-spec
  [schema branch ds-name f]
  (if branch
    (if-let [spec (get (datasources schema) (keyword ds-name))]
      (let [health (policy/spec-health spec)]
        (if (= "healthy" (:status health))
          (try (f spec)
               (catch InterruptedException e (throw e))
               (catch Exception t
                 (when (some-> (anomaly/of t) anomaly/client-error?) (throw t))
                 (connection-error ds-name
                                   {:status "unreachable" :code "connect-failed"
                                    :message (.getMessage t)
                                    :class   (str (class t))})))
          (connection-error ds-name health)))
      (refusal/refused :refusal.sources/unknown-datasource-for-this-branch {:datasource ds-name}))
    (refusal/refused :refusal.http/no-branch {})))

(defn- probe-response
  [store auth spec via]
  (let [health  (policy/spec-health spec)
        agent   (egress/agent-of spec)
        refusal (try (ds/assert-source-cleared! auth spec)
                     (policies/assert-permitted! store spec)
                     nil
                     (catch clojure.lang.ExceptionInfo e (.getMessage e)))]
    (cond
      (not= "healthy" (:status health))
      (ok {:ok false :code (:code health) :error (:message health)})

      refusal
      (ok {:ok false :code "not-cleared" :error refusal})

      :else
      (try
        (let [s ((ctx/connect auth) spec via)]
          (try (api/probe s) (finally (api/close! s))))
        (ok {:ok true :via (name via)})
        (catch InterruptedException e (throw e))
        (catch Exception t
          (if (and (= :agent via)
                   (not (attempt/or-answer true (ds/agent-reachable? agent))))
            (ok {:ok false :code "agent-down" :agent agent
                 :error (str "egress agent " agent " unreachable")})
            (ok {:ok false :error (.getMessage t)})))))))

(defn- sealed-for-saved-source
  [schema branch cipher]
  (when (and (seq cipher) branch)
    (some #(when (= cipher (:secret-cipher %)) %) (vals (datasources schema)))))

(defn- candidate-spec
  [body]
  (let [{src :ok error :error}
        (request->source-spec (-> (select-keys body [:type :jdbc-url :path :secret-cipher :agent])
                                  (assoc :name "candidate")))]
    (if error
      {:error error}
      {:ok (assoc src :source-name :candidate)})))

(def test-datasource
  (ep/handler :datasource-test
              {:respond
               (fn [{:keys [store auth schema branch body]}]
                 (let [{:keys [source]} body]
                   (cond
                     (contains? body :password)
                     (refusal/refused :refusal.sources/datasource-test-refuses-a-plaintext {})

                     (seq (some-> source clojure.core/name))
                     (if branch
                       (if-let [spec (get (datasources schema) (keyword source))]
                         (probe-response store auth spec (policy/route spec))
                         (refusal/refused :refusal.sources/unknown-datasource-for-this-branch
                                          {:datasource source}))
                       (refusal/refused :refusal.http/no-branch {}))

                     :else
                     (let [{spec :ok error :error} (candidate-spec body)]
                       (if error
                         (refusal/refused :refusal.sources/passed-error {} {:message error})
                         (let [spec (merge spec
                                           (select-keys
                                            (sealed-for-saved-source
                                             schema branch (:secret-cipher spec))
                                            [:source-name :markings]))]
                           (probe-response store auth spec
                                           (if (policy/jdbc-network? spec) :agent
                                               (policy/route spec)))))))))}))

(def list-datasource-tables
  (ep/handler :datasource-tables
              {:respond (fn [{:keys [auth schema branch name]}]
                          (with-datasource-spec schema branch name
                            (fn [spec]
                              (ok {:tables
                                   (api/with-source
                                     [s (ctx/connect auth) spec]
                                     (api/tables s))}))))}))

(def list-datasource-columns
  (ep/handler :datasource-columns
              {:respond (fn [{:keys [auth schema branch name table]}]
                          (if-not table
                            (refusal/refused :refusal.sources/missing-table-name {})
                            (with-datasource-spec schema branch name
                              (fn [spec]
                                (ok {:columns
                                     (api/with-source
                                       [s (ctx/connect auth) spec]
                                       (api/columns s table))})))))}))

(def datasource-writeback-check
  (ep/handler :datasource-writeback-check
              {:respond (fn [{:keys [auth schema branch name table]}]
                          (if-not table
                            (refusal/refused :refusal.sources/missing-table-name {})
                            (with-datasource-spec schema branch name
                              (fn [spec]
                                (ok (api/with-source
                                      [s (ctx/connect auth) spec]
                                      (wb-check/check schema name spec s table)))))))}))

(def datasource-sample
  (ep/handler :datasource-sample
              {:respond (fn [{:keys [auth schema branch name table limit]}]
                          (if-not table
                            (refusal/refused :refusal.sources/missing-table-name {})
                            (with-datasource-spec schema branch name
                              (fn [spec]
                                (ok {:datasource (keyword name) :table table
                                     :rows
                                     (api/with-source
                                       [s (ctx/connect auth) spec]
                                       (api/sample s table limit))})))))}))

(def datasource-relations
  (ep/handler :datasource-relations
              {:respond (fn [{:keys [auth schema branch name]}]
                          (with-datasource-spec schema branch name
                            (fn [spec]
                              (ok {:datasource (keyword name)
                                   :relations
                                   (api/with-source
                                     [s (ctx/connect auth) spec]
                                     (api/relations s))}))))}))

(defn- sanitize-ident [s]
  (str/replace (str s) #"[^A-Za-z0-9_]" "_"))

(defn- live-csv-preview
  [spec limit]
  (let [rows-of-maps (ds/read-csv-rows spec)
        header (vec (or (some-> (first rows-of-maps) keys vec) []))
        capped (vec (take limit rows-of-maps))]
    [{:source     :csv
      :table      (str (some-> ^String (:path spec) (File.) .getName))
      :columns    header
      :rows       (mapv (fn [r] (mapv #(get r %) header)) capped)
      :row-count  (count capped)
      :truncated? (and (= (count capped) limit)
                       (> (count rows-of-maps) limit))}]))

(def datasource-spec-at
  (ep/handler :datasource-spec
              {:path {:name :kw}
               :respond (fn [{:keys [store branch name as-of]}]
                          (try
                            (let [ds-map
                                  (values/sources->datasources
                                   (world/as-of (world/at store branch) as-of))]
                              (if-let [spec (get ds-map name)]
                                (ok {:datasource name :as-of as-of
                                     :spec (values/public-source spec)})
                                (refusal/refused :refusal.sources/datasource-did-not-exist-at
                                                 {:datasource name :as-of as-of})))
                            (catch InterruptedException e (throw e))
                            (catch Exception e
                              (fault e {:datasource name :as-of as-of}))))}))

(def datasource-preview
  (ep/handler :datasource-preview
              {:respond
               (fn [{:keys [store schema branch name limit as-of]}]
                 (with-datasource-spec schema branch name
                   (fn [spec]
                     (let [prefix  (str "s_" (sanitize-ident name) "_")
                           staging (if as-of
                                     (mapv (fn [t]
                                             {:source  :staging
                                              :table   t
                                              :error   "staging tables have no history"
                                              :rows    []
                                              :columns []})
                                           (dsets/dataset-names (ctx/at store branch) prefix))
                                     (read-staging-tables (ctx/at store branch) prefix limit))
                           tables  (cond
                                     (seq staging)                       staging
                                     (and (not as-of)
                                          (= :csv
                                             (:type spec)))         (live-csv-preview spec limit)
                                     :else                               [])]
                       (ok {:datasource (keyword name)
                            :tables     tables
                            :as-of      as-of})))))}))

(def create-datasource-table
  (ep/handler :datasource-tables
              {:respond (fn [{:keys [schema branch name body]}]
                          (let [{:keys [table columns primary-key]} body]
                            (cond
                              (not (seq table))
                              (refusal/refused :refusal.sources/missing-table {})
                              (not (seq columns))
                              (refusal/refused :refusal.sources/missing-columns {})
                              :else
                              (with-datasource-spec schema branch name
                                (fn [spec]
                                  (fjdbc/with-open-conn [conn spec]
                                    (ok
                                     (fjdbc/create-table! conn table columns primary-key))))))))}))

(def egress-pubkey
  (ep/handler :egress-pubkey
              {:call (fn [{:keys [schema agent source]}]
                       (let [src (when (seq source) (get (datasources schema) (keyword source)))]
                         (if (and src (= :agent-proxy (policy/connection src)))
                           (seal/public-key (ctx/system-store))
                           (ds/agent-pubkey
                            (cond
                              (seq agent) agent
                              src         (egress/agent-of src)
                              :else       egress/default-agent)))))}))

(defn- agent-allowlist-key [agent] (str "agent-proxy-allowlist:" agent))

(defn- agent-allowlist
  [agent]
  (some-> (ic/get-cfg (ctx/system-store) (agent-allowlist-key agent)) edn/read-string))

(defn- authorize-stream
  [{:keys [source host port]}]
  (let [store (ctx/system-store)
        src   (get (datasources (seat/rebuild-schema-from-storage store)) (keyword source))
        entry (str host ":" port)]
    (cond
      (nil? src)
      {:refused (str "no source " source)}
      (not= :agent-proxy (policy/connection src))
      {:refused (str "source " source " does not use the agent proxy")}
      (not (policy/egress-permits? src host port))
      {:refused (str entry " is not on source " source)}
      :else
      (if-let [agents (policies/proxy-agents (policies/all store) entry)]
        {:agents (vec agents)}
        {:refused (str "no active agent proxy policy for " entry)}))))

(defn- to-hub
  [path payload]
  (rpc/call! {:url     (str (egress/proxy-hub-url) path)
              :peer    "agent proxy hub"
              :timeout 2000
              :token   (egress/agent-token egress/proxy-hub-peer)}
             payload))

(defn- from-hub
  [req judge]
  (if (rpc/authorized? (egress/agent-token egress/proxy-hub-peer) req)
    {:status  200
     :headers {"Content-Type" "application/edn"}
     :body    (pr-str (judge (rpc/query-params req)))}
    {:status 403 :body "hub token refused"}))

(defn agent-proxy-judge
  [req]
  (from-hub req #(authorize-stream (update % :port parse-long))))

(defn agent-proxy-config
  [req]
  (from-hub req (fn [{:keys [agent]}] {:allow-listed-cidrs (agent-allowlist agent)})))

(defn- hub-linked
  []
  (try {:linked (:linked (to-hub "/linked" {}))}
       (catch InterruptedException e (throw e))
       (catch Exception e {:linked [] :hub-unreachable (ex-message e)})))

(def set-agent-proxy-allowlist
  (ep/handler :agent-proxy-allowlist
              {:call (fn [{:keys [store body]}]
                       (ic/set-cfg! store (agent-allowlist-key (:agent body))
                                    (pr-str (vec (:allow-listed-cidrs body))))
                       (let [reached (try (:reached (to-hub "/config" (select-keys
                                                                       body
                                                                       [:agent
                                                                        :allow-listed-cidrs])))
                                          (catch InterruptedException e (throw e))
                                          (catch Exception e
                                            (noted/note "agent-proxy allowlist" e)
                                            0))]
                         {:agent (:agent body) :allow-listed-cidrs (:allow-listed-cidrs body)
                          :applies (if (pos? reached)
                                     "now, on the linked agent"
                                     "the next time the agent links")}))}))

(def source-types
  (ep/handler :source-types
              {:call (fn [_] {:source-types (spec/source-types (source/connectors))})}))

(defn- declared-egress
  [schema]
  (reduce (fn [m s]
            (update m (egress/agent-of s) (fnil into #{})
                    (keep #(some-> % str str/trim not-empty) (:egress s))))
          {} (vals (datasources schema))))

(defn- used-by
  [schema]
  (reduce-kv (fn [m nm s]
               (reduce #(update %1 (some-> %2 str str/trim) (fnil conj []) (name nm))
                       m (:egress s)))
             {} (datasources schema)))

(defn- policy-row [uses {:keys [id] :as p}]
  (assoc p :effective (policies/state-of p) :used-by (get uses id [])))

(defn- policies-view
  [store schema]
  (let [ps   (policies/all store)
        uses (used-by schema)
        ids  (set (map :id ps))]
    {:policies  (mapv #(policy-row uses %) ps)
     :uncovered (vec (sort (remove ids (keys uses))))
     :enforced  (boolean (seq ps))}))

(def egress-policies
  (ep/handler :egress-policies
              {:call (fn [{:keys [store schema]}] (policies-view store schema))}))

(def propose-egress-policy
  (ep/handler :egress-policies
              {:call (fn [{:keys [store body]}]
                       (policies/propose!
                        store
                        (select-keys body [:address :port :agents :justification])))}))

(def decide-egress-policy
  (ep/handler :egress-policy-decide
              {:call (fn [{:keys [store body]}]
                       (policies/decide! store (:id body) (:decision body)))}))

(def adopt-egress-policies
  (ep/handler :egress-policies-adopt
              {:call (fn [{:keys [store schema body]}]
                       {:policies (policies/adopt! store (keys (used-by schema))
                                                   (:except body))})}))

(def report-egress-dropped
  (ep/handler :egress-policies-report
              {:call (fn [{:keys [store body]}]
                       (policies/report-dropped! store (:dropped body)))}))

(def egress-status
  (ep/handler :egress-status
              {:call (fn [{:keys [schema]}]
                       (let [want   (declared-egress schema)
                             agents (->> (egress/agent-names)
                                         (mapv (fn [a]
                                                 (future
                                                   (cond-> {:agent a :address (ds/agent-address a)
                                                            :reachable (ds/agent-reachable? a)
                                                            :status (ds/agent-status a 1500)}
                                                     (seq (get want a))
                                                     (assoc :endpoints
                                                            (ds/agent-connect
                                                             a (sort (get want a)) 8000))))))
                                         (mapv deref))]
                         {:reachable (every? :reachable agents)
                          :agent-url (ds/agent-address)
                          :agents    agents
                          :agent-proxy (hub-linked)}))}))

(def module
  {:name        :source
   :contributes {:http/routes
                 {:datasource-columns   {:get list-datasource-columns}
                  :datasource-preview   {:get datasource-preview}
                  :datasource-relations {:get datasource-relations}
                  :datasource-sample    {:get datasource-sample}
                  :datasource-spec      {:get datasource-spec-at}
                  :datasource-tables    {:get list-datasource-tables :post create-datasource-table}
                  :datasource-test      {:post test-datasource}
                  :datasource-writeback-check {:get datasource-writeback-check}
                  :datasources          {:get list-datasources}
                  :egress-policies      {:get egress-policies :post propose-egress-policy}
                  :egress-policy-decide {:post decide-egress-policy}
                  :egress-policies-adopt {:post adopt-egress-policies}
                  :egress-policies-report {:post report-egress-dropped}
                  :egress-pubkey        {:get egress-pubkey}
                  :agent-proxy-judge    {:get agent-proxy-judge}
                  :agent-proxy-config   {:get agent-proxy-config}
                  :agent-proxy-allowlist {:post set-agent-proxy-allowlist}
                  :egress-status        {:get egress-status}
                  :source-types         {:get source-types}}}})
