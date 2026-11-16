(ns sutra.actions.effects
  (:require [clojure.string :as str]
            [sutra.catalog.function-contract :as contract]
            [sutra.media.api :as media]
            [sutra.store.access.run-log :as run-log]
            [sutra.store.control :as ctl]
            [sutra.store.access.scope :as ctx]
            [sutra.actions.ports :as ports]
            [sutra.store.world :as world]
            [sutra.actions.dispatch :as c]
            [sutra.store.seat :as seat]
            [sutra.actions.action :as action]
            [sutra.catalog.program :as prog]
            [sutra.catalog.functions :as functions]
            [sutra.catalog.values :as values]
            [sutra.kit.anomaly :as anomaly]
            [sutra.kit.refusal :as refusal]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [sutra.sources.api :as sources]
            [sutra.compute.api :as compute]
            [sutra.store.storage.instance-config :as ic]
            [sutra.types.languages :as langs]
            [sutra.types.run-limits :as limits]
            [sutra.types.transform-io :as tio]
            [sutra.types.kinds :as kinds]
            [sutra.types.table :as tbl]
            [sutra.types.attribute :as attribute]
            [sutra.types.naming :as naming]
            [sutra.catalog.kernel :as kernel]
            [sutra.actions.execution :as x]
            [sutra.ref.value :as rv]
            [sutra.parser.spec :as spec]))

(set! *warn-on-reflection* true)

(defn- ok-2xx? [res]
  (let [s (:status res)] (and (integer? s) (<= 200 s 299))))

(defn- webhook-answer
  [src-name wh ps {:keys [outcome result side-effect request]}]
  (let [event {:source src-name :webhook wh :params ps}]
    (when (and (= :performed outcome) (not (ok-2xx? result)))
      (refusal/refuse! :refusal.actions/webhook-failed
                       {:source src-name :webhook wh :result result}
                       {:source-name (name src-name) :webhook-name (name wh)
                        :status (:status result)
                        :detail (if-let [e (:error result)] (str ": " e) "")}))
    {:binding (case outcome
                :performed (or (:outputs result) result)
                :queued    {:status 202 :queued true}
                :planned   {:status 200 :dry-run true}
                :held      {:status 200 :suppressed-on-branch true})
     :event   (kernel/event :webhook (case outcome
                                       :performed (assoc event :result result)
                                       :queued    (assoc event :outbox request)
                                       :planned   (assoc event :planned? true)
                                       :held      (assoc event :side-effect side-effect)))}))

(defn- call-webhook
  [ctx io world src src-name wh ps]
  (webhook-answer
   src-name wh ps
   (x/ask ctx io {:kind :webhook :source src :webhook wh :params ps :world world
                  :actor (some-> (grants/acting-user (:authority ctx)) name)
                  :correlation (str (x/fresh-uuid io))
                  :session? (not= :side-effect (:phase ctx))
                  :effect [:webhooks {:source src-name :webhook wh}]})))

(defn- webhook-target
  [{:keys [source webhook params]} world bindings]
  (let [src-name (keyword (rv/resolve-arg bindings source))
        src      (get (values/sources->datasources world) src-name)]
    (when-not src
      (refusal/refuse! :refusal.actions/webhook-unknown-source {:source src-name}))
    {:src-name src-name
     :wh       (keyword (rv/resolve-arg bindings webhook))
     :ps       (let [ps (rv/resolve-arg bindings (or params {}))]
                 (if (map? ps)
                   (update-vals ps #(if (map? %) (rv/resolve-bindings bindings %) %))
                   ps))
     :src      src}))

(defn- invoke-webhook-verb
  [{:keys [as] :as step} world bindings ctx io]
  (let [{:keys [src-name wh ps src]} (webhook-target step world bindings)]
    (sources/assert-source-write-cleared! (:authority ctx) src)
    (sources/assert-export-allowed! src)
    (let [fan? (and (= :side-effect (:phase ctx)) (sequential? ps) (every? map? ps))
          rs   (mapv #(call-webhook ctx io world src src-name wh %) (if fan? ps [ps]))]
      {:world        world
       :bindings     (assoc bindings (or as :webhook-result)
                            (if fan? (mapv :binding rs) (:binding (first rs))))
       :side-effects (mapv :event rs)})))

(defn- coerce-to-declared
  [world rid attr v]
  (let [typ (some (fn [a] (when (= attr (:attribute-name a)) (:attribute-type a)))
                  (get-in world [:object-types rid :attribute-list]))]
    (cond
      (nil? v)                       v
      (= :string typ)                (str v)
      (and (#{:integer :long} typ)
           (string? v))              (or (parse-long v) v)
      :else                          v)))

(def ^:private table-capability
  {:insert :writeback/create :update :writeback/update :delete :writeback/delete})

(defn- table-request!
  [{:keys [table op on-duplicate]} {:keys [source world params actor correlation session?]}
   {:keys [store write-session]}]
  (let [world (or world (seat/branch-schema store))
        ot    (some-> (get-in source [:writeback table :object-type]) keyword)
        _     (when-not ot
                (refusal/refuse! :refusal.actions/webhook-table-not-allowed {:table table}))
        f     (ports/required store (table-capability op))
        opts  {:actor actor :correlation correlation :store store
               :write-session (when session? write-session)
               :effect-gate {:branch nil :allowed #{}}
               :on-duplicate (some-> on-duplicate keyword)}
        one   (fn [row]
                (let [row (cond->> row (= :update op) (into {} (remove (comp nil? val))))
                      r (f world ot row
                           (cond-> opts (= :update op) (assoc :only (set (keys row)))))]
                  (if (= :insert op)
                    (let [{:keys [identity pk-attr number drawn-attrs deduped?]} r
                          rid (lookup/rid-of world :object-types ot)]
                      (merge drawn-attrs
                             {:identity (coerce-to-declared world rid pk-attr identity)
                              :number   number
                              :drawn    drawn-attrs
                              :deduped? (boolean deduped?)}))
                    r)))]
    (if (sequential? params)
      (mapv one params)
      (one params))))

(defn chained-row
  [as from params answers]
  (when-not (map? (get params as))
    (refusal/refuse! :refusal.actions/webhook-request-no-input
                     {:request as :given (vec (keys params))}
                     {:request (name as)}))
  (reduce-kv (fn [row col [src k]]
               (let [answer (get answers src)]
                 (cond
                   (and (not (contains? answers src)) (contains? row col)) row

                   (not (contains? answer k))
                   (refusal/refuse! :refusal.actions/webhook-request-unanswered
                                   {:request as :from src :key k}
                                   {:request (name as) :key (name k) :from (name src)})

                   :else (assoc row col (get answer k)))))
             (get params as) from))

(defn chain-answers
  [requests params send!]
  (reduce (fn [answers {:keys [as from optional?] :as req}]
            (let [as (keyword as)]
              (if (and optional? (nil? (get params as)))
                answers
                (assoc answers as (send! req (chained-row as from params answers))))))
          {} requests))

(defn chain-binding
  [requests answers]
  (merge (get answers (keyword (:as (peek (vec requests)))))
         answers
         (into {} (for [[as a] answers :when (map? a) [k v] a]
                    [(keyword (str (name as) "-" (name k))) v]))))

(defn- table-webhook!
  [{:keys [requests] :as decl} request env]
  (let [chain (fn [params]
                (chain-binding requests
                               (chain-answers requests params
                                              #(table-request! %1 (assoc request :params %2)
                                                               env))))]
    (cond
      (not (seq requests))               (table-request! decl request env)
      (sequential? (:params request))    (mapv chain (:params request))
      :else                              (chain (:params request)))))

(defn- perform-webhook
  [{:keys [source webhook params] :as request} {:keys [timeout-ms store] :as env}]
  (sources/assert-egress-permitted! store source)
  (let [decl (get-in source [:webhooks webhook])]
    (if (= :table (some-> (:kind decl) keyword))
      (try {:status 200 :outputs (table-webhook! decl request env)}
           (catch clojure.lang.ExceptionInfo e
             {:status 502 :error (ex-message e) :data (ex-data e)}))
      (sources/invoke-webhook! source webhook params
                               {:timeout-ms      timeout-ms
                                :idempotency-key (:idempotency-key request)
                                :event-type      "com.sutra.actions.webhook"}))))

(defn- version-dep
  [step]
  (select-keys step [:version :auto-upgrade]))

(defn- unsatisfied-version!
  [fn-name {:keys [version auto-upgrade]} available]
  (refusal/refuse! (if (seq available)
                     :refusal.actions/no-matching-release
                     :refusal.actions/no-release-published)
                   {:function fn-name :version version :auto-upgrade auto-upgrade
                    :available (vec available)}
                   {:fn        (name fn-name)
                    :wanted    (if auto-upgrade
                                 (str ">= " version " (auto-upgrade, same major)")
                                 (str "version " version))
                    :published (str/join ", " available)}))

(defn- resolve-query-function
  [world fn-name dep]
  (let [{:keys [kind unsatisfied available] :as entry}
        (functions/function-at-version world fn-name dep)]
    (when unsatisfied (unsatisfied-version! fn-name unsatisfied available))
    (case kind
      :query (select-keys entry [:spec :version])

      :edit
      (refusal/refuse! :refusal.actions/invoke-edit-function
                      {:function fn-name :lifecycle :edit} {:fn (name fn-name)})

      (if (functions/build-transform-by-name world fn-name)
        (refusal/refuse! :refusal.actions/invoke-build-transform
                        {:function fn-name :lifecycle :build-transform} {:fn (name fn-name)})
        (refusal/refuse! :refusal.actions/invoke-unknown-function {:function fn-name})))))

(defn- function-compute
  [fn-name spec]
  (cond-> {:transform-name fn-name
           :language       (:language spec)
           :code           (:code spec)
           :profile        (:profile spec)
           :inputs         (:inputs spec)}
    (:timeout-ms spec)
    (assoc :timeout-ms (limits/function-timeout-ms spec))
    (seq (:modules spec))
    (assoc :modules (:modules spec))
    (seq (:external-systems spec))
    (assoc :external-systems (mapv keyword (:external-systems spec)))))

(defn- refuse-egress-in-action
  [fn-name compute]
  (when (seq (:external-systems compute))
    (refusal/refuse! :refusal.actions/invoke-external-systems
                    {:function fn-name :external-systems (:external-systems compute)}
                    {:fn (name fn-name)}))
  compute)

(def ^:private source-problem-words
  {:unknown        "which is no source of this ontology."
   :not-released   "which is not released to code — give the source :external-transform true."
   :not-cleared    "whose markings the caller is not cleared for."
   :not-exportable "whose markings may not leave the platform (:export-markings), and a call sends its arguments out."})

(defn source-problem
  [clearances {:keys [external-transform markings export-markings] :as spec}]
  (cond
    (nil? spec)                                        :unknown
    (not external-transform)                           :not-released
    (not (grants/cleared? clearances markings))        :not-cleared
    (seq (remove (set export-markings) markings))      :not-exportable))

(defn source-grants
  [clearances world compute]
  (let [sources (values/sources->datasources world)]
    (mapv (fn [src]
            (let [spec (get sources src)]
              (cond-> {:source-id src :spec spec}
                (source-problem clearances
                                spec) (assoc :problem (source-problem clearances spec)))))
          (:external-systems compute))))

(defn- grant-sources
  [auth world fn-name compute]
  (if (empty? (:external-systems compute))
    compute
    (let [granted (source-grants (grants/clearances auth) world compute)]
      (when-let [{:keys [source-id problem]} (first (filter :problem granted))]
        (refusal/refuse! :refusal.actions/function-source-unusable
                        {:function fn-name :source source-id :problem problem}
                        {:fn (name fn-name) :source (name source-id)
                         :words (source-problem-words problem)}))
      (assoc compute :external-systems (mapv #(select-keys % [:source-id :spec]) granted)))))

(defn- function-rows
  [fn-name payload]
  (when (:outputs payload)
    (refusal/refuse! :refusal.actions/invoke-multiple-outputs
                    {:function fn-name :outputs (vec (keys (:outputs payload)))}
                    {:fn (name fn-name)}))
  (vec (:rows payload)))

(defn- object-type-rows
  [objects]
  (mapv (fn [{:keys [id values]}] (assoc values :id id)) objects))

(def ^:private scalar-remedies
  {:action-rule
   (fn [alias _site]
     (str "A Function argument defaults to the IDENTICALLY NAMED action parameter, so"
          " give the action a " (pr-str alias) " parameter, or map one on the rule with"
          " :inputs {" (pr-str alias) " :some-param}."))
   :app-variable
   (fn [alias {:keys [application]}]
     (str "An app passes a Function its arguments through the calling variable's :args,"
          " so bind " (pr-str alias) " there: {:function … :args {" (pr-str alias) " …}}"
          (when application (str " in app " (name application))) "."))})

(defn- scalar-remedy-unplaced
  [alias _site]
  (str "Bind " (pr-str alias) " where the Function is called."))

(defn- scalar-arg
  [fn-name alias args unbound site]
  (if-not (contains? unbound alias)
    (get args alias)
    (refusal/refuse! :refusal.actions/invoke-scalar-unbound
                    {:function fn-name :alias alias
                     :call-site (:call-site site)
                     :bound (vec (sort (keys args)))}
                    {:fn     (name fn-name)
                     :remedy ((scalar-remedies (:call-site site) scalar-remedy-unplaced)
                              alias site)})))

(defn- object-type-input
  [world {:keys [object-type]} objects]
  (let [aliases (naming/declared-aliases
                 (:names (attribute/of (get-in world [:object-types object-type]))))]
    (cond-> {:kind :object-type :rows (object-type-rows objects)}
      (seq aliases) (assoc :aliases aliases))))

(def ^:private function-inputs
  (tbl/per-key "actions/effects function-inputs" kinds/compute-input-kinds
               {:object-type
                (fn [{:keys [read world]} _ in]
                  (object-type-input world in (read (:object-type in))))
                :scalar
                (fn [{:keys [fn-name args unbound site]} alias in]
                  (if (and (tio/optional-input? in) (not (contains? args alias)))
                    ::unbound
                    {:kind :scalar :value (scalar-arg fn-name alias args unbound site)}))
                :language-model
                (fn [{:keys [world]} _ in] (media/language-model-entry world in))
                :media
                (fn [{:keys [store world fn-name args unbound site]} alias in]
                  (if store
                    (media/media-item-entry store world in
                                            (scalar-arg fn-name alias args unbound site))
                    (refusal/refuse! :refusal.actions/invoke-media-item
                                     {:function fn-name :alias alias :kind :media}
                                     {:fn (name fn-name)})))
                :media-set
                (fn [{:keys [store world fn-name]} alias in]
                  (if store
                    (media/whole-media-set store world in)
                    (refusal/refuse! :refusal.actions/invoke-media-set
                                     {:function fn-name :alias alias :kind :media-set}
                                     {:fn (name fn-name)})))
                #{:dataset :transform :datasource :media-set-items} nil}))

(defn- read-function-inputs
  [read store world fn-name inputs args site]
  (let [c {:read read :store store :world world :fn-name fn-name :args args :site site
           :unbound (set (tio/unbound-scalars {:inputs inputs} args))}]
    (reduce-kv
     (fn [acc alias in]
       (if-let [f (get function-inputs (:kind in))]
         (let [entry (f c alias in)]
           (if (= ::unbound entry) acc (assoc acc alias entry)))
         (refusal/refuse! :refusal.actions/invoke-input-kind
                          {:function fn-name :alias alias :kind (:kind in)})))
     {} inputs)))

(defn- runnable!
  [fn-name compute _inputs]
  (let [lang (:language compute)]
    (when-not (contains? compute/runners lang)
      (refusal/refuse! (if (langs/query-language? lang)
                       :refusal.actions/invoke-query-language
                       :refusal.actions/invoke-language)
                     {:function fn-name :language lang}
                     {:fn        (name fn-name)
                      :languages (pr-str (set (filter langs/code-language?
                                                      langs/compute-language-names)))}))
    compute))

(defn- run-function!
  [lm-service fn-name compute inputs]
  (let [lang (:language (runnable! fn-name compute inputs))]
    (compute/run-code! lang compute (assoc (compute/prepare-inputs lang inputs nil)
                                           :blobs (delay (ctx/blobs))
                                           :lm-service lm-service))))

(defn lm-service
  [store]
  (delay (ic/lm-service-config store)))

(defn- perform-function
  [{:keys [function compute inputs]} {:keys [store]}]
  (run-function! (lm-service store) function compute inputs))

(defn- declared-rows
  [fn-name returns rows]
  (when-let [problem (contract/returns-problem returns rows)]
    (refusal/refuse! :refusal.actions/function-returns
                     {:function fn-name :returns returns}
                     {:fn (name fn-name) :problem problem}))
  rows)

(defn- run-query-function*
  [world read store prepare call fn-name args dep site]
  (let [{:keys [spec version]} (resolve-query-function world fn-name dep)
        compute (prepare fn-name (function-compute fn-name spec))
        inputs  (read-function-inputs read store world fn-name (:inputs spec) args site)
        payload (call fn-name compute inputs)]
    {:rows    (declared-rows fn-name (:returns spec) (function-rows fn-name payload))
     :log     (:log payload)
     :version version}))

(declare run-query-function-logged)

(defn run-query-function
  ([store world fn-name] (run-query-function store world fn-name {}))
  ([store world fn-name args] (run-query-function store world fn-name args nil))
  ([store world fn-name args site]
   (:rows (run-query-function-logged store world fn-name args site))))

(defn object-reader
  [store]
  (partial x/objects-of
           (x/reader {:authority (:authority store) :branch (:branch store)}
                     (x/io (action/store-io store (ctx/code))))))

(defn run-query-function-logged
  ([store world fn-name args site]
   (run-query-function-logged store world fn-name args site (object-reader store)))
  ([store world fn-name args site read]
   (-> (run-query-function* world read store
                            (partial grant-sources (grants/read-rights store) world)
                            (partial run-function! (lm-service store)) fn-name args nil
                            site)
       (select-keys [:rows :log]))))

(defn rule-function
  [world ctx io fn-name args dep]
  (let [call (fn [f compute ins]
               (if (= :subprocess (langs/executor (:language (runnable! f compute ins))))
                 (:result (x/ask ctx io {:kind :function :function f :compute compute
                                         :inputs ins}))
                 (run-function! (:lm-service io) f compute ins)))]
    (run-query-function* world (partial x/objects-of (x/reader ctx io))
                         nil refuse-egress-in-action call fn-name args dep
                         {:call-site :action-rule})))

(defn- invoke-function-verb
  [{:keys [function as inputs] :as step} world bindings ctx io]
  (let [fn-name (keyword (rv/resolve-arg bindings function))
        args    (if (seq inputs)
                  (update-vals inputs #(rv/resolve-arg bindings %))
                  bindings)
        {:keys [rows version]} (rule-function world ctx io fn-name args (version-dep step))]
    {:world    world
     :bindings (assoc bindings (or as :function-result) rows)
     :side-effects [(kernel/event :function (cond-> {:function fn-name :result-count (count rows)}
                                              version (assoc :version version)))]}))

(defn- run-schedule-verb
  [{:keys [schedule parameters as]} world bindings ctx io]
  (let [s  (name (rv/resolve-arg bindings schedule))
        ps (update-vals (or parameters {}) #(rv/resolve-arg bindings %))
        branch (str (or (:branch (:effect-gate ctx)) "main"))
        {:keys [outcome result]} (x/ask ctx io {:kind :schedule-run :schedule s :parameters ps
                                                :branch branch :effect [:builds {:schedule s}]})
        run-id (:run-id result)]
    {:world        world
     :bindings     (assoc bindings (or as :schedule-run) run-id)
     :side-effects [(kernel/event :schedule-run
                                  (cond-> {:schedule s :parameters ps :outcome outcome}
                                    run-id (assoc :run-id run-id)))]}))

(defn- perform-schedule-run
  [{:keys [schedule parameters branch]} {:keys [store]}]
  ((ports/schedule-start store) store branch {:schedule schedule :parameters parameters}))

(defn- resolve-edit-function
  [world fn-name dep]
  (let [{:keys [kind unsatisfied available] :as entry}
        (functions/function-at-version world fn-name dep)]
    (when unsatisfied (unsatisfied-version! fn-name unsatisfied available))
    (case kind
      :edit (if (:library (:spec entry))
              (refusal/refuse! :refusal.actions/edit-function-library
                              {:function fn-name} {:fn (name fn-name)})
              (select-keys entry [:spec :version]))

      :query
      (refusal/refuse! :refusal.actions/edit-function-query
                      {:function fn-name :lifecycle :query} {:fn (name fn-name)})

      (refusal/refuse! :refusal.actions/edit-function-unknown {:function fn-name}))))

(defn- edit-function-inputs
  [param-bindings inputs bindings]
  (if (seq inputs)
    (update-vals inputs #(rv/resolve-arg bindings %))
    (into {} (for [p (keys param-bindings)
                   :when (contains? bindings p)]
               [p (get bindings p)]))))

(defn- batch-param
  [fn-name param-bindings]
  (let [ps (keys param-bindings)]
    (when-not (= 1 (count ps))
      (refusal/refuse! :refusal.actions/edit-function-batched-params
                      {:function fn-name :params (vec ps)}
                      {:fn (name fn-name) :count (count ps)}))
    (first ps)))

(defn- batch-entries
  [fn-name inputs entries action world read]
  (when-not (seq inputs)
    (refusal/refuse! :refusal.actions/edit-function-batched-no-inputs
                    {:function fn-name} {:fn (name fn-name)}))
  (let [field-types (into {} (for [[field src] inputs
                                   :let [t (when (keyword? src)
                                             (get-in action [:params src :type]))]
                                   :when (and t (lookup/rid-of world :object-types t))]
                               [field t]))]
    (mapv (fn [b]
            (reduce-kv
             (fn [entry field src]
               (let [v (rv/resolve-arg b src)]
                 (assoc entry field
                        (if-let [t (and (not (map? v)) (some? v) (get field-types field))]
                          (or (read (lookup/rid-of world :object-types t) v)
                              (refusal/refuse! :refusal.actions/edit-function-entry-unknown
                                              {:field field :object-type t :id v}
                                              {:type (name t)}))
                          v))))
             {} inputs))
          entries)))

(defn- deref-object-params
  [world read param-bindings supplied]
  (reduce-kv
   (fn [acc p v]
     (let [t     (:type (get param-bindings p))
           rid   (when t (lookup/rid-of world :object-types t))
           deref (fn [x]
                   (if (or (nil? rid) (map? x) (nil? x))
                     x
                     (or (read rid x)
                         (refusal/refuse! :refusal.actions/edit-function-param-unknown
                                         {:param p :object-type t :id x}
                                         {:type (name t)}))))]
       (assoc acc p (if (sequential? v) (mapv deref v) (deref v)))))
   {} supplied))

(defn- edit-function-step
  [world bindings result fn-name version step-count]
  {:world        world
   :bindings     bindings
   :ops          (:ops result)
   :proposals    (:proposals result)
   :collected    (:collected result)
   :decisions    (:decisions result)
   :installed    (:installed result)
   :side-effects (conj (vec (:side-effects result))
                       (kernel/event :function (cond-> {:function fn-name :lifecycle :edit
                                                        :step-count step-count}
                                                 version (assoc :version version))))})

(defn- apply-edit-function-verb
  [{:keys [function inputs] :as step} world bindings ctx io]
  (let [fn-name  (keyword (rv/resolve-arg bindings function))
        {:keys [spec version]} (resolve-edit-function world fn-name (version-dep step))
        ctx      (cond-> ctx (:staged-writes spec) (assoc :staged? true))
        {:keys [compiled]}
        (c/compile-edit-function world {:language    (:language spec)
                                        :source      (functions/function-code
                                                      (functions/function-lookup world) fn-name
                                                      spec)
                                        :types       (:param-types spec)
                                        :target-type (:target-type spec)})
        params   (:param-bindings compiled)
        supplied (if (:batched step)
                   {(batch-param fn-name params)
                    (batch-entries fn-name inputs (or (:batch ctx) [bindings])
                                   (:action ctx) world (partial x/object (x/reader ctx io)))}
                   (->> (edit-function-inputs params inputs bindings)
                        (deref-object-params world (partial x/object (x/reader ctx io)) params)))
        input-bindings (prog/bind-inputs params supplied)
        world*   (update world :object-sets merge (:object-sets compiled))
        result   (kernel/eval-steps (:steps compiled) world*
                                    (merge bindings input-bindings) ctx io)]
    (edit-function-step world bindings result fn-name version (count (:steps compiled)))))

(def ^:private dispatch-attempts 3)

(def ^:private dispatch-timeout-ms 60000)

(defn- dispatched
  [perform {:keys [id request]}]
  (loop [n 1]
    (let [request (cond-> request
                    (= :webhook (:kind request)) (assoc :idempotency-key (str id)))
          e (try (let [a (perform request dispatch-timeout-ms)]
                   (when (and (= :webhook (:kind request)) (not (ok-2xx? a)))
                     (anomaly/ex (refusal/refused :refusal.actions/webhook-answer
                                                   {:answer a}
                                                   {:status (:status a)
                                                    :detail (if-let [m (:error a)]
                                                              (str ": " m)
                                                              "")}))))
                 (catch InterruptedException e (throw e))
                 (catch Exception t t))]
      (if (and e (< n dispatch-attempts)) (recur (inc n)) e))))

(def ^:private dead-backoff-ms
  60000)

(defn- dead!
  [branch-store b {:keys [id] :as entry} ^Throwable e rounds]
  (let [wait (min 3600000 (* dead-backoff-ms (bit-shift-left 1 (min 16 (dec rounds)))))]
    (ctl/upsert! branch-store :outbox-dead
                 {:branch b :id (str id) :state "dead"
                  :entry (dissoc entry ::run-log/at) :error (ex-message e)
                  :attempts rounds
                  :next-at (java.util.Date. (long (+ (ctl/millis branch-store) wait)))})))

(defn- dispatch-dead!
  [branch-store b]
  (let [now (ctl/millis branch-store)]
    (doseq [{:keys [entry attempts
                    next-at]} (ctl/list branch-store :outbox-dead {:branch b :state "dead"})
            :when (<= (.getTime ^java.util.Date next-at) now)]
      (if-let [e (dispatched (action/performer
                              (ctx/act-for branch-store (grants/stored (:authority entry))) nil)
                             entry)]
        (dead! branch-store b entry e (inc attempts))
        (ctl/delete! branch-store :outbox-dead {:branch b :id (str (:id entry))})))))

(defn dispatch-outbox!
  [store]
  (doseq [{b :name} (world/branches store)
          :let [branch-store (ctx/at store b)]]
    (doseq [{:keys [id authority] :as entry} (run-log/pending branch-store :outbox/effect)]
      (when-let [e (dispatched
                    (action/performer (ctx/act-for branch-store (grants/stored authority)) nil)
                    entry)]
        (println (str "[outbox] " b " " (:kind (:request entry)) " " id " failed: " (ex-message e)))
        (run-log/append! branch-store [(run-log/side-effect-failed-entry entry e)])
        (dead! branch-store b entry e 1))
      (run-log/advance! branch-store :outbox/effect [id]))
    (dispatch-dead! branch-store b)))

(def module
  {:name        :action-effects
   :provides    {:functions/run-query run-query-function}
   :jobs        [{:name :outbox :every "SUTRA_OUTBOX_MS" :tick dispatch-outbox! :default 5000}]
   :requires    #{:writeback/create :writeback/update :writeback/delete}
   :contributes {:action/verbs
                 {:invoke-webhook        invoke-webhook-verb
                  :invoke-function       invoke-function-verb
                  :run-schedule          run-schedule-verb
                  spec/function-rule-op  apply-edit-function-verb}
                 :run/performers
                 {:webhook      perform-webhook
                  :function     perform-function
                  :schedule-run perform-schedule-run}}})
