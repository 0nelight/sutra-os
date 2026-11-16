(ns sutra.foundation.settings
  (:require [sutra.kit.coll :as coll]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [sutra.foundation.env :as env]
            [sutra.kit.refusal :as refusal]
            [sutra.types.run-limits :as limits]))

(set! *warn-on-reflection* true)

(def groups
  [[:throughput "how much runs at once"]
   [:fault-tolerance "how a failing call is tried again"]
   [:intervals "how often a job ticks — 0 stops it"]
   [:retention "how long what a run leaves is kept"]
   [:limits "what one request or one transform may take"]
   [:diagnostics "what a process measures besides its work"]])

(def default-profiles
  {:small   {:wall-seconds 30 :cpu-seconds 15 :address-space-bytes (* 1024 1024 1024)
             :max-open-files 128}
   :default {:wall-seconds 60 :cpu-seconds 30 :address-space-bytes (* 2048 1024 1024)
             :max-open-files 256}
   :large   {:wall-seconds 300 :cpu-seconds 180 :address-space-bytes (* 8192 1024 1024)
             :max-open-files 512}
   :xlarge  {:wall-seconds 900 :cpu-seconds 600 :address-space-bytes (* 32768 1024 1024)
             :max-open-files 1024}})

(def registry
  [{:key :build-concurrency :group :throughput :type :long :min 1 :default 4
    :env "SUTRA_BUILD_CONCURRENCY"
    :doc "builds running at once — admission counts them, intake counts all but the waiting"}
   {:key :build-waiting-max :group :throughput :type :long :min 0 :default 32
    :env "SUTRA_BUILD_WAITING_MAX"
    :doc "builds one worker holds waiting for admission, beyond those it runs"}
   {:key :build-parallelism :group :throughput :type :long :min 1 :default 1
    :env "SUTRA_BUILD_PARALLELISM"
    :doc "jobs of one build run side by side"}
   {:key :logic-parallelism-max :group :throughput :type :long :min 1
    :default limits/logic-concurrency-default
    :env "SUTRA_LOGIC_PARALLELISM_MAX"
    :doc "automation logic calls at once, the cap over each automation's own parallelism"}
   {:key :agent-runs :group :throughput :type :long :min 1 :default 4
    :doc "agent runs one process answers at once; the next ones wait their turn"}
   {:key :agent-run-queue :group :limits :type :long :min 0 :default 32
    :doc "agent runs that may wait for a free place; one more is refused"}
   {:key :eval-parallel :group :throughput :type :long :min 1 :default 10
    :doc "test cases an eval run answers at once when the suite names none"}
   {:key :transform-retries :group :fault-tolerance :type :long :min 0 :default 4
    :env "SUTRA_TRANSFORM_RETRIES"
    :doc "times a transform dispatch to the compute service is tried again"}
   {:key :transform-backoff-ms :group :fault-tolerance :type :long :min 0 :default 1500
    :env "SUTRA_TRANSFORM_BACKOFF_MS"
    :doc "wait before the first retry of a dispatch, doubled each time"}
   {:key :source-query-seconds :group :fault-tolerance :type :long :min 1 :default 180
    :env "SUTRA_SOURCE_QUERY_SECONDS"
    :doc "how long a query against a JDBC source may run"}
   {:key :scheduler-ms :group :intervals :type :long :min 0 :env "SUTRA_SCHEDULER_MS"
    :doc "the schedule poller's tick"}
   {:key :automate-ms :group :intervals :type :long :min 0 :env "SUTRA_AUTOMATE_MS"
    :doc "the automation poller's tick"}
   {:key :stream-ms :group :intervals :type :long :min 0 :env "SUTRA_STREAM_MS"
    :doc "the stream indexer's tick"}
   {:key :telemetry-ms :group :intervals :type :long :min 0 :default 30000
    :env "SUTRA_TELEMETRY_MS"
    :doc "how often each process samples what it costs"}
   {:key :retention-ms :group :intervals :type :long :min 0 :env "SUTRA_RETENTION_MS"
    :doc "the branch and dataset retention sweep's tick"}
   {:key :git-gc-ms :group :intervals :type :long :min 0 :env "SUTRA_GIT_GC_MS"
    :doc "the git garbage collection's tick"}
   {:key :run-payload-days :group :retention :type :long :min 1 :default 30
    :env "SUTRA_RUN_PAYLOAD_DAYS"
    :doc "days a build run's payload is kept before it is pruned"}
   {:key :request-concurrency :group :limits :type :long :min 1 :default 24
    :env "SUTRA_REQUEST_CONCURRENCY"
    :doc "requests one API process answers at once"}
   {:key :request-admission-wait-ms :group :limits :type :long :min 0 :default 5000
    :env "SUTRA_REQUEST_ADMISSION_WAIT_MS"
    :doc "how long a request waits for a free slot before it is refused"}
   {:key :app-bind-max :group :limits :type :long :min 1 :default 50000
    :env "SUTRA_APP_BIND_MAX"
    :doc "objects an app may bind at most"}
   {:key :app-page :group :limits :type :long :min 1 :default 500
    :env "SUTRA_APP_PAGE"
    :doc "rows an app page delivers when it asks no limit"}
   {:key :llm-requests-per-minute :group :limits :type :long :min 0 :default 240
    :doc "calls the LLM relay sends one model per minute — 0 is no cap"}
   {:key :llm-tokens-per-minute :group :limits :type :long :min 0 :default 1000000
    :doc (str "tokens (prompt without cache reads, plus answer) the LLM relay sends one"
              " model per minute — 0 is no cap")}
   {:key :llm-concurrency :group :limits :type :long :min 0 :default 16
    :doc (str "calls the LLM relay has in flight through one source at once — a provider"
              " account caps them across its models; 0 is no cap")}
   {:key :llm-batch-share-pct :group :limits :type :long :min 1 :default 80
    :doc (str "percent of each LLM rate cap a transform's calls may take; the rest stays"
              " free for interactive use")}
   {:key :refuse-capped-replace :group :limits :type :boolean :default true
    :doc "a run that replaces its output may not cap its batch: it would lose the rest"}
   {:key :transform-profiles :group :limits :type :profiles :default default-profiles
    :doc (str "the resource profiles a transform names (:profile): per profile :wall-seconds"
              " :cpu-seconds :address-space-bytes :max-open-files — a stored map is merged"
              " over the defaults, profile by profile. Time limits only a run outside a build:"
              " a job has one only if its transform states @configure(allowed_run_duration)")}
   {:key :container-workflows :group :limits :type :boolean :default false
    :doc (str "a transform may run in an image of its own (with_container), pushed with"
              " `image push` — off until an administrator turns it on")}
   {:key :home-folders :group :limits :type :boolean :default true
    :doc (str "files may be saved privately in a person's home folder; off, a saved exploration or"
              " list needs a project (object-finder/save-explorations.md)")}
   {:key :import-timing :group :diagnostics :type :boolean :default false
    :env "SUTRA_IMPORT_TIMING"
    :doc "a build prints how long each phase of its import took"}])

(def ^:private by-key (coll/index-by :key registry))

(def row-key "instance_settings")

(defonce ^:private stored (atom {}))

(defn spec
  [k]
  (or (by-key k)
      (refusal/refuse! :refusal.foundation/unknown-setting
                       {:setting k}
                       {:known (str/join ", " (map (comp name :key) registry))})))

(def ^:private profile-keys [:wall-seconds :cpu-seconds :address-space-bytes :max-open-files])

(defn- profiles-problem
  [v]
  (cond
    (not (map? v)) "is a map {profile {limit n}}"
    :else
    (some (fn [[p limits]]
            (cond
              (not (keyword? p)) (str "names its profiles as keywords, not " (pr-str p))
              (not (map? limits)) (str "gives " p " a map of limits")
              :else (some (fn [[l n]]
                            (cond
                              (not (some #{l} profile-keys))
                              (str p " names " l " — a limit is one of "
                                   (str/join " " profile-keys))
                              (not (pos-int? n)) (str p " " l " is a positive integer")))
                          limits)))
          v)))

(defn- invalid [why] {::invalid (or why "")})

(defn- invalid? [v] (and (map? v) (contains? v ::invalid)))

(defn- parse-text
  [{:keys [type]} s]
  (let [s (str/trim (str s))]
    (case type
      :long     (or (parse-long s) (invalid nil))
      :boolean  (case (str/lower-case s)
                  ("true" "on" "1" "yes") true
                  ("false" "off" "0" "no" "") false
                  (invalid nil))
      :profiles (try (edn/read-string s) (catch Exception e (invalid (ex-message e)))))))

(defn checked
  [k v]
  (let [{:keys [type min] :as s} (spec k)
        v   (if (string? v) (parse-text s v) v)
        bad (fn [why] (refusal/refuse! :refusal.foundation/setting-invalid
                                       {:setting k :value v}
                                       {:name (name k) :why why}))]
    (cond
      (invalid? v)      (bad (str "is no " (name type)
                               (some->> (::invalid v) not-empty (str ": "))))
      (= :long type)    (cond (not (integer? v)) (bad "is a whole number")
                              (and min (< (long v) (long min))) (bad (str "is at least " min))
                              :else (long v))
      (= :boolean type) (if (boolean? v) v (bad "is true or false"))
      :else             (if-let [p (profiles-problem v)] (bad p) v))))

(defn- from-env
  [{:keys [env] :as s}]
  (when-let [raw (some-> env env/getenv str/trim not-empty)]
    (let [v (parse-text s raw)]
      (when-not (invalid? v) v))))

(defn- source-of
  [{k :key :as s}]
  (cond (contains? @stored k) :stored
        (some? (from-env s))  :env
        :else                 :default))

(defn value
  [k]
  (let [{:keys [type default] :as s} (spec k)
        v (get-in @stored [k :value])]
    (if (= :profiles type)
      (merge-with merge default (or v (from-env s)))
      (if (some? v) v (let [e (from-env s)] (if (some? e) e default))))))

(defn lookup
  [k]
  (if (keyword? k) (value k) (env/getenv k)))

(defn view
  []
  (let [order (into {} (map-indexed (fn [i [g]] [g i])) groups)]
    {:groups   (mapv (fn [[g doc]] {:group g :doc doc}) groups)
     :settings (vec (for [{k :key :as s} (sort-by (comp order :group) registry)
                          :let [{:keys [by at]} (get @stored k)]]
                      (cond-> (-> (select-keys s [:key :group :type :min :default :env :doc])
                                  (assoc :value (value k) :source (source-of s)))
                        by (assoc :by by)
                        at (assoc :at at))))}))

(defn read-row
  [text]
  (let [m (try (some-> text not-empty edn/read-string)
               (catch Exception e
                 (println (str "[settings] the stored row does not read, defaults apply: "
                               (ex-message e)))
                 nil))]
    (if (map? m) (select-keys m (keys by-key)) {})))

(defn install!
  [rows]
  (let [before @stored]
    (reset! stored rows)
    (into #{} (filter #(not= (get-in before [% :value]) (get-in rows [% :value])))
          (set (concat (keys before) (keys rows))))))

(defn changed-rows
  [rows {:keys [sets resets by at]}]
  (let [resets (mapv #(:key (spec (keyword (name %)))) resets)
        sets   (into {} (map (fn [[k v]] (let [k (keyword (name k))] [k (checked k v)]))) sets)]
    (reduce-kv (fn [m k v] (assoc m k {:value v :by by :at at}))
               (apply dissoc rows resets)
               sets)))
