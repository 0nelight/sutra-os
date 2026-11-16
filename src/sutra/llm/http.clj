(ns sutra.llm.http
  (:require [sutra.kit.refusal :as refusal]
            [sutra.http.request :as http-request]
            [sutra.store.access.scope :as ctx]
            [sutra.store.control :as ctl]
            [sutra.kit.digest :as digest]
            [sutra.kit.effects :as effects]
            [sutra.llm.ports :as ports]
            [sutra.llm.rates :as llm-rates]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [sutra.foundation.encoding :as enc]
            [sutra.catalog.lookup :as lookup]
            [sutra.catalog.llm-models :as llm-models]
            [sutra.llm.budget :as budget]
            [sutra.llm.rate :as rate]
            [sutra.types.llm-providers :as llmp]
            [sutra.store.access.datasets :as dsets]
            [sutra.foundation.egress :as egress]
            [sutra.kit.attempt :as attempt]
            [sutra.kit.noted :as noted]
            [sutra.llm.wire :as wire]
            [sutra.foundation.live-trace :as live]
            [clojure.data.json :as json]
            [sutra.catalog.values :as cvalues]
            [sutra.http.responses :refer [ok json-response]]
            [sutra.http.endpoint :as ep]
            [sutra.types.rid :as rid])
  (:import (java.util UUID)))

(set! *warn-on-reflection* true)

(defn- elide-llm-media
  [x]
  (cond
    (string? x)
    (if (and (> (count x) 4096)
             (or (str/starts-with? x "data:")
                 (re-matches #"[A-Za-z0-9+/=\s]+" x)))
      {:_sutra_elided_sha256 (digest/sha256-hex x) :_sutra_bytes (count x)}
      x)
    (map? x)    (into {} (map (fn [[k v]] [k (elide-llm-media v)])) x)
    (vector? x) (mapv elide-llm-media x)
    :else x))

(defn- llm-prompt-preview
  [request]
  (attempt/silently
    (some->> (:messages request)
             (filter #(= "user" (str (:role %))))
             last
             :content
             live/clip)))

(defn- llm-completion-preview
  [output-json]
  (attempt/silently
    (when-let [s (not-empty (str output-json))]
      (let [msg (-> (json/read-str s :key-fn keyword) :choices first :message)]
        (live/clip (or (not-empty (str (:content msg)))
                       (some-> msg :tool_calls first :function :arguments)))))))

(def ^:private provider-error-max
  2000)

(defn provider-error-text
  [message body]
  (let [b (some-> body str not-empty (as-> s (subs s 0 (min provider-error-max (count s)))))]
    [(str message (when b (str " — " b))) b]))

(defn- log-llm-call!
  [store branch schema values]
  (live/emit! {:kind        :llm/response
               :level       (cond
                              (nil? (:error values)) :info
                              (re-find #"daily LLM budget|^llm budget wait:"
                                       (str (:error values)))
                              :warn
                              :else :error)
               :request-id  (:request_id values)
               :caller      (:caller values)
               :run-id      (:run_id values)
               :model       (:model values)
               :ms          (:latency_ms values)
               :prompt-tokens     (:prompt_tokens values)
               :completion-tokens (:completion_tokens values)
               :error       (live/clip (:error values))
               :answer      (llm-completion-preview (:output_json values))})
  (try
    (when branch
      (let [rid    (lookup/rid-of schema :datasets :llm_call_log)
            ds     (get-in schema [:datasets rid])]
        (if (nil? ds)
          (binding [*out* *err*]
            (println "[llm-audit] dataset llm_call_log missing — call not logged"
                     (:request_id values)))
          (let [store (ctx/at store branch)]
            (dsets/ensure-datasets! store {rid (:columns ds)})
            (dsets/write-system-rows! store rid :append
                               [{:id (:request_id values) :values values}])))))
    (catch Throwable e
      (attempt/rethrow-fatal! e)
      (binding [*out* *err*]
        (println "[llm-audit] log write failed (fail-open):" (.getMessage e))))))

(defn admission
  [spec registered? source model]
  (cond
    (str/blank? (str source))
    {:status 400
     :logged (str "model " model " names no source — neither the call nor its registration")
     :error  (str "model " model " has no source: register it with one — sutra llm register "
                  model " --source <source> --in <rate> --out <rate>")}

    (nil? spec)
    {:status 400 :logged (str "unknown language-model source " source)
     :error (str "unknown source " source)}

    (not registered?)
    {:status 400
     :logged (str "unregistered model " model
                  " — not in the LLM model registry; register it (with its rate) to allow use")
     :error  (str "unregistered model " model " — register it: sutra register-model "
                  model " --in <rate> --out <rate>")}))

(def ^:private budget-wait-ms
  30000)

(defn- admit-or-wait!
  [store reqid eur timeout-ms pool source]
  (let [until (+ (System/currentTimeMillis) budget-wait-ms)]
    (loop []
      (let [r (budget/admit! store reqid eur timeout-ms pool source)]
        (if (and r (:waitable? r) (< (System/currentTimeMillis) until))
          (do (Thread/sleep 1000) (recur))
          r)))))

(defn budget-wait-text
  [{:keys [limit cap used project source until detail]}]
  (str "llm budget wait: "
       (case limit
         :account (str "provider " source "'s account takes no calls — it answered that its"
                       " credits are used or its spending limit is reached"
                       (when detail (str " (" detail ")"))
                       "; calls through it stay home until "
                       (java.time.Instant/ofEpochMilli (long until))
                       ", then one tries again. Top the account up, then"
                       " `llm budget --source " source " --resume`")
         :source  (str "provider " source "'s daily budget of " cap " EUR is used")
         :project (str "project " project "'s daily quota of " cap " EUR is used ("
                       (format "%.4f" (double used)) ")")
         :batch   (str "the batch share of the daily budget (" (format "%.2f" (double cap))
                       " EUR, the rest is reserved for interactive use) is used")
         (str "the daily LLM budget of " cap " EUR is used"))
       " — the call waits for the budget; a transform keeps what it has and resumes later."
       " Raise it with `llm budget`, or in the GUI under Settings → AI models."))

(defn- call-row
  [store reqid {:keys [caller run-id]} source model request]
  (let [input-json (json/write-str (elide-llm-media request))]
    {:ts         (ctl/stamp store)
     :request_id reqid
     :caller     caller
     :run_id     run-id
     :source     source
     :model      model
     :input_json input-json
     :input_sha  (digest/sha256-hex input-json)}))

(defn- source-spec
  [schema source model]
  (let [provider (:provider (llm-models/rate-of schema model))]
    (cond-> (get (cvalues/sources->datasources schema) (keyword source))
      provider (assoc :wire provider))))

(defn- requested!
  [reqid {:keys [caller run-id]} source model request]
  (live/emit! {:kind       :llm/request
               :level      :debug
               :request-id reqid
               :caller     caller
               :run-id     run-id
               :source     source
               :model      model
               :prompt     (llm-prompt-preview request)}))

(defn- budget-refusal!
  [store schema reqid {:keys [caller timeout-ms]} source model request]
  (when-let [held (admit-or-wait! store reqid (budget/max-cost schema model request)
                                  timeout-ms (budget/pool-of schema caller) source)]
    (let [t (budget-wait-text held)]
      {:status 429 :logged t :error t})))

(defn- usage-of
  [body]
  (let [usage (:usage (json/read-str body :key-fn keyword))]
    {:prompt     (:prompt_tokens usage)
     :completion (:completion_tokens usage)
     :cached     (get-in usage [:prompt_tokens_details :cached_tokens])
     :written    (get-in usage [:prompt_tokens_details :cache_write_tokens])}))

(defn rate-tokens
  [{:keys [prompt completion cached]}]
  (+ (- (long (or prompt 0)) (long (or cached 0))) (long (or completion 0))))

(defn prompt-estimate
  [request]
  (quot (count (pr-str (:messages request request))) 4))

(defn retry-after-ms
  [header fallback-ms]
  (if-let [s (some->> header str str/trim (re-matches #"\d+(\.\d+)?") first)]
    (max 1000 (long (* 1000 (Double/parseDouble s))))
    fallback-ms))

(defn rate-refusal
  [reqid source model wait-ms]
  {:status 429 :request-id reqid :retry-after-ms wait-ms
   :error (str "llm rate limit: " source "/" model " is at its rate for this minute"
               " — retry in " (int (Math/ceil (/ wait-ms 1000.0))) " s")})

(def ^:private rate-wait-max-ms
  60000)

(def ^:private short-wait-ms
  2000)

(defn waits-here?
  [on-rate w]
  (or (= :wait on-rate) (<= w short-wait-ms)))

(defn- rate-held!
  [key reqid batch? tokens on-rate]
  (let [until (+ (System/currentTimeMillis) rate-wait-max-ms)]
    (loop []
      (let [w (rate/admit! key reqid batch? tokens)]
        (if (and w (waits-here? on-rate w) (< (+ (System/currentTimeMillis) w) until))
          (do (Thread/sleep (long w)) (recur))
          w)))))

(defn- refused!
  [store {:keys [branch schema]} row reqid {:keys [status logged error]}]
  (log-llm-call! store branch schema (assoc row :error logged))
  {:status status :request-id reqid :error error})

(defn- answered!
  [store {:keys [branch schema]} row reqid model body latency-ms]
  (let [{:keys [prompt completion cached written]} (usage-of body)]
    (when-let [s (:source row)]
      (when (contains? (budget/exhausted-sources store) s) (budget/recovered! store s)))
    (log-llm-call! store branch schema
                   (assoc row :output_json body :prompt_tokens prompt
                          :completion_tokens completion :latency_ms latency-ms :error nil))
    (budget/settle! store schema model prompt completion reqid
                    {:cached cached :written written})
    {:status 200 :request-id reqid :body body
     :tokens (rate-tokens {:prompt prompt :completion completion :cached cached})}))

(defn- failed!
  [store {:keys [branch schema]} row reqid e]
  (noted/or-else (str "releasing the budget hold of " reqid) nil
    (budget/release! store reqid))
  (let [d           (ex-data e)
        [full body] (provider-error-text (ex-message e) (:body d))
        source      (:source row)
        held        (when (and source (budget/account-exhausted? full))
                      (assoc (budget/exhausted! store source (or body full))
                             :limit :account :source source))]
    (log-llm-call! store branch schema (assoc row :error full :latency_ms (:latency-ms d)))
    (cond
      held
      (let [t (budget-wait-text held)]
        (cond-> {:status 429 :request-id reqid :error t}
          body (assoc :provider-error body)))

      (= 429 (:provider-status d))
      (cond-> {:status 429 :request-id reqid :error full
               :retry-after-ms (retry-after-ms (:retry-after d) 5000)}
        body (assoc :provider-error body))

      :else
      (cond-> {:status 502 :request-id reqid :error full}
        body (assoc :provider-error body)))))

(defn relay-chat!
  [store {:keys [source model request schema timeout-ms call caller on-rate] :as opts}]
  (let [request (walk/keywordize-keys request)
        model   (or model (:model request))
        source  (some-> (or (not-empty (some-> source name))
                            (llm-models/source-of schema model))
                        name)
        spec    (source-spec schema source model)
        reqid   (str (UUID/randomUUID))
        row     (call-row store reqid opts source model request)
        rkey    [source model]]
    (effects/check! effects/*work*)
    (requested! reqid opts source model request)
    (if-let [refusal (admission spec (llm-models/registered? schema model) source model)]
      (refused! store opts row reqid refusal)
      (if-let [wait (rate-held! rkey reqid (budget/batch? (budget/pool-of schema caller))
                                (prompt-estimate request) (or on-rate :wait))]
        (rate-refusal reqid source model wait)
        (if-let [refusal (budget-refusal! store schema reqid opts source model request)]
          (do (rate/forget! rkey reqid)
              (refused! store opts row reqid refusal))
          (let [measured (volatile! nil)]
            (try
              (let [wire-req (assoc request :model (llm-models/provider-model schema model))
                    {:keys [body latency-ms]} (effects/call!
                                               #((or call egress/chat!) spec wire-req
                                                                    {:timeout-ms timeout-ms}))]
                (let [r (answered! store opts row reqid model body latency-ms)]
                  (vreset! measured (:tokens r))
                  r))
              (catch Throwable e
                (attempt/rethrow-fatal! e)
                (when (or (effects/cancellation e) (attempt/interruption? e))
                  (budget/release! store reqid)
                  (attempt/rethrow-fatal! e)
                  (throw e))
                (failed! store opts row reqid e))
              (finally (rate/land! rkey reqid @measured)))))))))

(defn llm-chat
  [req]
  (let [store       (:sutra/store req)
        want-tok ((-> req :sutra/host :config :relay-token) store)
        got-tok  (http-request/bearer-token req)]
    (if-not (digest/same? want-tok got-tok)
      (json-response 401 {:error "llm relay: bad token"})
      (let [body (json/read-str (slurp (:body req)) :key-fn keyword)
            {:keys [status request-id error provider-error retry-after-ms] :as r}
            (relay-chat! store
                         {:source (:source body) :model (:model body) :request (:request body)
                          :caller (:caller body) :run-id (:run_id body)
                          :branch (ep/request-branch req)
                          :schema (ep/request-schema req)
                          :on-rate :refuse})]
        (if (= 200 status)
          {:status 200 :headers {"Content-Type" "application/json"} :body (:body r)}
          (cond-> (json-response status (cond-> {:error error :request_id request-id}
                                           provider-error
                                           (assoc :provider_error provider-error)))
            retry-after-ms
            (assoc-in [:headers "Retry-After"]
                      (str (long (Math/ceil (/ retry-after-ms 1000.0)))))))))))

(defn relay-embed!
  [store {:keys [text] :as opts}]
  (let [{:keys [status
                body]} (relay-chat! store (assoc (dissoc opts :text) :request {:input (str text)}
                                                     :call egress/embed!))]
    (when (= 200 status)
      (some-> body (json/read-str :key-fn keyword) :data first :embedding))))

(defn kernel-complete
  [{:keys [store source model request branch schema timeout-ms caller run-id]}]
  (let [{:keys [status body error request-id]}
        (relay-chat! store
                     {:source source :model model :request request :caller (or caller "use-llm")
                   :run-id run-id :branch branch :schema schema :timeout-ms timeout-ms})]
    (if (= 200 status)
      (let [b (json/read-str body :key-fn keyword)]
        {:request-id request-id
         :message    (-> b :choices first :message)
         :usage      (select-keys (:usage b) [:prompt_tokens :completion_tokens])})
      (refusal/refuse! :refusal.llm/call-refused {:status status :request-id request-id}
                                 {:why error}))))

(defn- instant [ms] (str (java.time.Instant/ofEpochMilli (long ms))))

(defn- budget-view
  [store schema]
  (let [b   (budget/budget-eur store)
        pct (budget/reserve-pct store)]
    {:budget-eur              b
     :spent-today             (budget/spent-today store)
     :day                     (budget/today store)
     :zone                    budget/day-zone
     :interactive-reserve-pct pct
     :batch-cap-eur           (* b (- 1.0 (/ pct 100.0)))
     :project-quotas          (budget/project-quotas store)
     :pools                   (budget/pools-today store)
     :source-budgets          (budget/source-budgets store)
     :exhausted-sources       (update-vals (budget/exhausted-sources store)
                                           #(-> % (update :since instant) (update :until instant)))
     :sources                 (budget/sources-today store)
     :llm-sources
       (vec (sort (distinct (keep (comp #(some-> % name) :source val) (:llm-models schema)))))
     :projects                (vec (sort (keep (comp #(some-> % rid/->name) :project-name val)
                                               (:projects schema))))}))

(defn llm-budget
  [{store :sutra/store schema :sutra/schema branch :sutra/branch}]
  (if branch
    (ok (budget-view store schema))
    (refusal/refused :refusal.http/no-branch {})))

(defn set-llm-budget
  [{:keys [body-params] store :sutra/store schema :sutra/schema}]
  (let [{:keys [eur reserve-pct project source resume]} body-params]
    (if (or (contains? body-params :eur) (contains? body-params :reserve-pct)
            (and (or project source) (contains? body-params :quota))
            (and source resume))
      (do (when (and source resume) (budget/recovered! store source))
          (when (and source (contains? body-params :quota))
            (budget/set-source-budget! store source (:quota body-params)))
          (when (contains? body-params :eur) (budget/set-budget! store eur))
          (when (contains? body-params :reserve-pct) (budget/set-reserve-pct! store reserve-pct))
          (when (and project (contains? body-params :quota))
            (budget/set-project-quota! store project (:quota body-params)))
          (ok (budget-view store schema)))
      (refusal/refused :refusal.llm/set-budget-needs-eur-n body-params))))

(defn llm-reprice
  [{:keys [body-params] store :sutra/store schema :sutra/schema branch :sutra/branch}]
  (if branch
    (ok (llm-rates/reprice-llm-model! store schema body-params))
    (refusal/refused :refusal.http/no-branch {})))

(def ^:private llm-hosts
  #{"api.x.ai" "api.openai.com" "api.anthropic.com" "api.mistral.ai" "openrouter.ai"
    "api.groq.com" "api.deepseek.com" "generativelanguage.googleapis.com" "api.together.xyz"})

(defn- model-id [k] (if (keyword? k) (subs (str k) 1) (str k)))

(defn- provider-source? [schema k s]
  (let [base (llmp/spec-base-url s)
        ^java.net.URI u (try (java.net.URI. (str base))
                             (catch java.net.URISyntaxException _ nil))
        host (some-> u .getHost str/lower-case)]
    (and (:external-transform s) base
         (or (some #(= k (some-> (:source %) name keyword)) (vals (:llm-models schema)))
             (get-in s [:config :wire])
             (llm-hosts host)
             (wire/local-endpoint? base)
             (some (fn [[_ t]] (some #(and (map? %) (llm-models/language-model-input? %)
                                           (= (name k) (some-> (:source %) name)))
                                     (vals (:inputs t))))
                   (:transforms schema))))))

(defn llm-providers
  [{schema :sutra/schema}]
  (let [models (vec (for [[k v] (sort-by (comp model-id key) (:llm-models schema))]
                      (assoc (select-keys v [:in :out :currency :source :provider :provider-model
                                             :capabilities :default :deprecated :description
                                             :context-window])
                             :id (model-id k))))]
    (ok {:wires    (vec (sort (map name (keys wire/builtin))))
         :projects (vec (sort-by :name
                                 (keep (fn [[r p]]
                                         (when-let [n (:project-name p)]
                                           {:rid (str r) :name (name n)}))
                                       (:projects schema))))
         :models  models
         :sources (vec (for [[k s] (sort-by key (cvalues/sources->datasources schema))
                             :when (provider-source? schema k s)
                             :let [base (llmp/spec-base-url s)]]
                         (cond-> {:name      (name k)
                                  :type      (some-> (:type s) name)
                                  :project
                                    (some-> (get-in schema [:projects (:project s) :project-name])
                                            name)
                                  :base-url  base
                                  :wire      (name (first (wire/wire-for wire/builtin s base)))
                                  :has-key?  (boolean (seq (str (:secret-cipher s))))
                                  :local?    (wire/local-endpoint? base)
                                  :models
                                    (filterv #(= (name k) (some-> (:source %) name)) models)}
                           (:description s) (assoc :description (:description s)))))})))

(defn llm-provider-models
  [{:keys [query-params] schema :sutra/schema}]
  (let [nm   (get query-params "source")
        spec (get (cvalues/sources->datasources schema) (some-> nm keyword))]
    (if-not spec
      (refusal/refused :refusal.llm/unknown-source {:source nm} {:nm nm})
      (try
        (let [base (llmp/spec-base-url spec)
              [_ w] (wire/wire-for wire/builtin spec base)
              body (json/read-str (str (egress/list-models! spec (:models-path w))))]
          (ok {:source nm
               :models (vec (sort (keep #(get % "id")
                                        (or (get body "data") (get body "models")))))}))
        (catch Throwable e
          (attempt/rethrow-fatal! e)
          (json-response 502
                         {:error (str "listing the provider's models failed: "
                                      (.getMessage e))}))))))

(defn llm-provider-test
  [{:keys [body-params] store :sutra/store schema :sutra/schema branch :sutra/branch}]
  (let [{:keys [source model]} body-params
        t0 (System/currentTimeMillis)
        r  (relay-chat! store
                        {:source source :model model :caller "provider-test" :branch branch
                         :schema schema
                         :timeout-ms 60000
                         :request
                           {:messages [{:role "user" :content "Reply with the single word OK."}]
                            :max_tokens 200}})
        ms (- (System/currentTimeMillis) t0)]
    (ok (if (= 200 (:status r))
          {:ok true :ms ms
           :answer (some-> (:body r) (json/read-str :key-fn keyword) :choices first :message
                           :content str str/trim)}
          {:ok false :ms ms :error (:error r)}))))

(defn llm-catalog
  [{:keys [query-params] store :sutra/store :as req}]
  (let [cats   (ports/llm-catalogues store)
        dss    (cvalues/sources->datasources (ep/request-schema req))
        source (or (get query-params "source")
                   (some (fn [[k s]]
                           (when (llmp/adapter-for cats s) (name k)))
                         (sort-by key dss)))
        spec   (get dss (some-> source keyword))]
    (cond
      (nil? spec)
      (json-response 400
                     {:error (if source
                               (str "unknown language-model source " (pr-str source))
                               "no source of this instance has a language-model catalogue — name one with ?source=")})
      (nil? (llmp/adapter-for cats spec))
      (json-response 400 {:error (str "source " (pr-str source) " has no LLM-catalog adapter"
                                      " (base-url " (pr-str (llmp/spec-base-url spec)) ")")})
      :else
      (try
        (let [path   (:models-path (llmp/adapter-for cats spec))
              body   (egress/list-models! spec path)
              models (llmp/parse-catalog cats spec body)]
          (if (empty? models)
            (json-response 502 {:error (str "provider catalog for " source
                                            " parsed to zero priced models — fetch/parse failed")
                                :source source
                                :body-snippet (enc/clip 600 body)})
            (ok {:source source :models models})))
        (catch Throwable e
          (attempt/rethrow-fatal! e)
          (json-response 502 {:error (str "catalog fetch failed: " (.getMessage e))}))))))

(def module
  {:name        :llm
   :provides    {:llm/complete kernel-complete
                 :llm/relay-embed relay-embed!}
   :contributes {:http/routes
                 {:llm-budget   {:get llm-budget :post set-llm-budget}
                  :llm-providers {:get llm-providers}
                  :llm-provider-test {:post llm-provider-test}
                  :llm-provider-models {:get llm-provider-models}
                  :llm-catalog  {:get llm-catalog}
                  :llm-chat     {:post llm-chat}
                  :llm-rates    {:post llm-reprice}}}})
