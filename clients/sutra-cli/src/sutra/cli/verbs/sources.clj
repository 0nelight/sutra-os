(ns sutra.cli.verbs.sources
  (:require [sutra.cli.auth :refer [seal-schema-sources secret-sentinel]]
            [sutra.cli.dispatch :refer [step!]]
            [sutra.cli.errors :refer [ok? req!]]
            [sutra.cli.live :refer [commit-unless-staged! schema-sections]]
            [sutra.cli.out :refer [emit! mark say! warn!]]
            [clojure.string :as str]
            [sutra.cli.instance :as inst]
            [sutra.i18n.core :as i18n]
            [sutra.wire.api :as api]))

(defn- redact-source
  [spec]
  (-> (select-keys spec [:type :jdbc-url :path :access :egress :capability :external-transform])
      (cond-> (or (:secret-cipher spec) (:password spec)) (assoc :sealed? true))))

(defmethod step! :sources [ctx [_ _]]
  (let [{:keys [datasources]} (req! "sources" (api/get! :datasources))]
    (emit! ctx {:sources (vec (for [[nm spec] (sort-by key datasources)]
                                (assoc (redact-source spec) :name (clojure.core/name nm))))})
    nil))

(defmethod step! :source [ctx [_ {:keys [name]}]]
  (let [{:keys [datasources]} (req! "source" (api/get! :datasources))
        spec (get datasources (keyword name))]
    (if-not spec
      (say! ctx (mark :yellow "•" (i18n/t :cli/no-such-source {:name (pr-str name)})))
      (let [probe (api/get! :datasource-tables {:name name})
            live? (ok? probe)]
        (emit! ctx {:source name :spec (redact-source spec)
                    :reachable live?
                    :table-count (when live? (clojure.core/count (:tables (:body probe))))})
        nil))))

(defmethod step! :source-tables [ctx [_ {:keys [name like]}]]
  (let [tables (:tables (req! "source-tables" (api/get! :datasource-tables {:name name})))]
    (emit! ctx {:tables (cond->> (vec tables)
                          like (filterv #(str/includes? (str/lower-case (str %))
                                                        (str/lower-case like))))})
    nil))

(defmethod step! :peek [ctx [_ {:keys [name table limit]}]]
  (emit! ctx {:rows (:rows (req! "peek"
                                 (api/get! :datasource-sample
                                           (merge {:name name}
                                                  (cond-> {:table table}
                                                    limit (assoc :limit limit))))))})
  nil)

(defmethod step! :source-sql [ctx [_ {:keys [name sql limit]}]]
  (let [r (api/post! :datasource-query {:name name} (cond-> {:sql sql} limit (assoc :limit limit)))]
    (if (ok? r)
      (emit! ctx {:rows (:rows (:body r))})
      (warn! (mark :red "✗" (i18n/t :cli/source-sql-refused {:grund (:error (:body r))}))))
    nil))

(defn- said
  [{:keys [rule severity params message]}]
  (let [code (some-> rule keyword)
        text (when code (i18n/t code params))]
    {:level (if (= "error" (some-> severity clojure.core/name)) :fehler :warnung)
     :text  (if (or (nil? text) (str/starts-with? text "⟦")) message text)}))

(defmethod step! :writeback-check [ctx [_ {:keys [name table]}]]
  (let [r (api/get! :datasource-writeback-check {:name name :table table})]
    (if-not (ok? r)
      (let [{:keys [error data]} (:body r)
            code (some-> (:refusal/code data) keyword)]
        (warn! (mark :red "✗" (if code
                                (:text (said {:rule code :message error
                                              :params (merge data (:refusal/params data))}))
                                (or error (str "HTTP " (:status r))))))
        :abort)
      (let [found (mapv said (:findings (:body r)))]
        (emit! ctx (assoc (:body r) :findings found))
        (when (some #(= :fehler (:level %)) found) :abort)))))

(defmethod step! :source-graph [ctx [_ {:keys [name table]}]]
  (let [rels (:relations (req! "source-graph" (api/get! :datasource-relations {:name name})))]
    (emit! ctx {:relations (cond->> (vec rels)
                             table (filterv #(or (= (str (:from-table %)) table)
                                                 (= (str (:to-table %)) table))))})
    nil))

(def ^:private jdbc-add-types
  #{:postgres :mysql :mssql})

(defn- jdbc-types
  []
  (if-let [ps (:connector-profiles (inst/facts))]
    (into (sorted-set) (keep (fn [[t p]] (when (= :jdbc-url (keyword (:endpoint p))) (keyword t))))
          ps)
    jdbc-add-types))

(defn- url->egress
  [url]
  (when-let [[_ h p] (re-find #"//([^:/;]+):(\d+)" (str url))]
    (str h ":" p)))

(defmethod step! :source-add [ctx [_ {:keys [name type url user password egress access stage?]}]]
  (when-not (and type url)
    (throw (ex-info (str "source-add needs --type <" (str/join "|" (map name (jdbc-types)))
                         "> and --url <jdbc-url>") {})))
  (when-not (contains? (jdbc-types) type)
    (throw (ex-info (str "source-add supports the JDBC types this server knows, " (vec (jdbc-types))
                         " — author a " type " source in an .ent instead")
                    {:type type})))
  (let [eg (or egress (url->egress url))]
    (when-not eg
      (throw (ex-info "could not derive egress host:port from --url — pass --egress host:port"
                      {:url url})))
    (let [spec   {:type type :jdbc-url url :egress [eg]
                  :access (or access :read-only)
                  :secrets {:user     (or user secret-sentinel)
                            :password (or password secret-sentinel)}}
          schema (seal-schema-sources ctx {:sources {(keyword name) spec}})
          tally  (req! "source-add" (api/post! :install {:schema schema}))]
      (emit! ctx (assoc tally :source name :type type :egress eg :access (or access :read-only)))
      (when (zero? (+ (:created tally 0) (:updated tally 0)))
        (say! ctx (mark :yellow "!" (i18n/t :cli/note-source-exists))))
      (commit-unless-staged! ctx stage? (i18n/t :cli/committed-probe-source {:name name}))
      nil)))

(defn- from-stdin
  [v]
  (if (= "-" v)
    (or (not-empty (first (line-seq (java.io.BufferedReader. *in*))))
        (throw (ex-info "--value - / --password -: stdin brought no line" {})))
    v))

(defmethod step! :source-secret [ctx [_ {:keys [name user field cipher stage?] :as args}]]
  (let [password (from-stdin (:password args))
        value    (from-stdin (:value args))
        secrets (cond-> {}
                  (or user password)
                  (merge {:user     (or user secret-sentinel)
                          :password (or password secret-sentinel)})
                  field (assoc-in (mapv keyword (str/split (str field) #"\."))
                                  (or value secret-sentinel)))]
    (when (and field (str/includes? (str field) ".") (nil? value))
      (throw (ex-info "a nested --field (a.b.c) needs --value (or --value - for stdin)"
                      {:field field})))
    (when (and (empty? secrets) (not cipher))
      (throw (ex-info (str "source-secret needs --user/--password (a database source) "
                           "or --field <name> [--value <v>] (a rest/imap source)")
                      {:source name})))
    (let [live   (some #(when (= (clojure.core/name name) (clojure.core/name (:source-name %))) %)
                       (:sources (req! "source-secret" (api/get! :sources))))
          _      (when-not live
                   (throw (ex-info (str "no source named " name) {:source name})))
          typ    (keyword (:type live))
          sealed (when-not cipher
                   (-> (seal-schema-sources ctx {:sources {(keyword name) {:secrets secrets}}})
                       (assoc-in [:sources (keyword name) :type] typ)))
          before (when (and field (not (or user password)))
                   (some #(when (= (clojure.core/name name)
                                   (some-> (:source-name %) clojure.core/name))
                            (not-empty (:secret-cipher %)))
                         (vals (:sources (schema-sections :sources)))))
          schema (cond
                   cipher {:sources {(keyword name) {:type typ :secret-cipher cipher}}}
                   (and before (get-in sealed [:sources (keyword name) :secret-cipher]))
                   (update-in sealed [:sources (keyword name) :secret-cipher] #(str before " " %))
                   :else sealed)
          tally  (req! "source-secret" (api/post! :install {:schema schema}))]
      (when (zero? (+ (:created tally 0) (:updated tally 0)))
        (say! ctx (mark :yellow "!" (i18n/t :cli/note-check-source-name))))
      (emit! ctx (assoc tally :source name :sealed (vec (keys secrets))))
      (commit-unless-staged! ctx stage? (i18n/t :cli/committed-sealed {:name name}))
      nil)))
