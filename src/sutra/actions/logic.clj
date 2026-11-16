(ns sutra.actions.logic
  (:require [sutra.kit.coll :as coll]
            [sutra.kit.anomaly :as anomaly]
            [sutra.kit.refusal :as refusal]
            [sutra.store.access.scope :as scope]
            [sutra.kit.digest :as digest]
            [sutra.actions.ports :as ports]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [sutra.catalog.actions :as actions]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [sutra.catalog.kernel :as kernel]
            [sutra.actions.execution :as x]
            [sutra.llm.api :as llm]
            [sutra.actions.operations :as ops]
            [sutra.objects.api :as objects]
            [sutra.types.object-set :as os]
            [sutra.types.llm-output :as llm-out]
            [sutra.types.run-limits :as limits]
            [sutra.ref.value :as rv]))

(set! *warn-on-reflection* true)

(def default-iterations 8)
(def max-iterations-cap 20)
(def query-limit-cap limits/query-tool-rows-max)

(defn- perform-llm
  [request {:keys [store timeout-ms]}]
  ((ports/llm-complete store) (assoc (dissoc request :kind) :store store :timeout-ms timeout-ms)))

(defn- json-value [v]
  (cond
    (rv/handle? v)  (assoc (update-keys (:values v) name) "id" (str (:id v)))
    (map? v)         (into {} (map (fn [[k x]] [(if (keyword? k) (name k) (str k)) (json-value x)]))
                           v)
    (sequential? v)  (mapv json-value v)
    (keyword? v)     (name v)
    (or (uuid? v) (inst? v)) (str v)
    :else v))

(defn- render [v]
  (cond
    (nil? v)    ""
    (string? v) v
    (or (number? v) (boolean? v)) (str v)
    :else (json/write-str (json-value v))))

(def ^:private var-re #"\{\{\s*([A-Za-z][\w-]*)((?:\.[\w-]+)*)\s*\}\}")

(defn- var-value [vars bindings v path]
  (let [k    (keyword (str/replace v "_" "-"))
        base (if-let [bk (get vars k)] (get bindings bk) (get bindings k))
        ks   (map keyword (remove str/blank? (str/split (or path "") #"\.")))]
    (reduce rv/attr-of base ks)))

(defn interpolate
  [prompt vars bindings]
  (str/replace (or prompt "")
               var-re
               (fn [[_ v path]] (render (var-value vars bindings v path)))))

(defn media-values
  [media vars bindings]
  (vec (mapcat (fn [entry]
                 (let [[_ v path] (re-find var-re (str entry))]
                   (llm/hashes-of (var-value vars bindings v path))))
               media)))

(defn- where-values
  [vars bindings tool]
  (update tool :where
          (fn [cs] (mapv (fn [c] (mapv #(if-let [[_ v path] (and (string? %)
                                                                 (re-matches var-re %))]
                                          (var-value vars bindings v path)
                                          %)
                                       c))
                         cs))))

(defn- tool-name [prefix n]
  (let [s (str prefix (str/replace (name n) #"[^A-Za-z0-9_-]" "_"))]
    (subs s 0 (min 64 (count s)))))

(defn- param-schema [world {:keys [type description]}]
  (let [t (some-> type name keyword)]
    (cond-> (cond
              (#{:integer :long :short :byte} t)            {"type" "integer"}
              (#{:number :double :float :decimal} t)        {"type" "number"}
              (= :boolean t)                                {"type" "boolean"}
              (and t (lookup/rid-of world :object-types t))   {"type" "string"
                                                               "description"
                                                               (str "id of a " (name t))}
              :else                                         {"type" "string"})
      description (assoc "description" description))))

(defn action-tool [world action]
  (let [a (or (actions/resolve-action world action)
              (refusal/refuse! :refusal.actions/llm-tool-unknown-action {:action action}))
        ps (:params a)]
    {:kind :apply-action :action action :name (tool-name "apply_" action)
     :def {"type" "function"
           "function" {"name" (tool-name "apply_" action)
                       "description" (or (:description a)
                                         (str "Apply the " (name action) " action."))
                       "parameters" {"type" "object"
                                     "properties"
                                     (assoc
                                      (into {} (for [[p s] ps] [(name p) (param-schema world s)]))
                                      "_reason" {"type" "string"
                                                 "description" "why — shown to the reviewer"})
                                     "required"
                                     (vec (for [[p s] ps :when (true? (:required s))]
                                            (name p)))}}}}))

(defn- fixed-where
  [where]
  (cond (nil? where)                 []
        (keyword? (first where))     [where]
        (every? vector? where)       (vec where)
        :else (refusal/refuse! :refusal.actions/llm-query-where {:where where})))

(defn- query-tool [world {t :query-objects props :properties limit :limit where :where}]
  (let [rid (or (lookup/rid-of world :object-types t)
                (refusal/refuse! :refusal.actions/llm-tool-unknown-type {:object-type t}))]
    {:kind :query-objects :type t :rid rid :properties (vec props)
     :where (fixed-where where)
     :limit (min query-limit-cap (or limit 20))
     :name (tool-name "query_" t)
     :def {"type" "function"
           "function" {"name" (tool-name "query_" t)
                       "description" (str "Read " (name t) " objects. Optional equality filters on "
                                          (str/join ", " (map name props)) ".")
                       "parameters" {"type" "object"
                                     "properties" {"where" {"type" "object"
                                                            "properties"
                                                            (into {}
                                                                  (for [p props] [(name p) {}]))
                                                            "additionalProperties" false}
                                                   "limit" {"type" "integer"}}}}}}))

(def ^:private calculator-tool
  {:kind :calculator :name "calculator"
   :def {"type" "function"
         "function" {"name" "calculator"
                     "description"
                     "Evaluate an arithmetic expression: numbers, + - * / and parentheses."
                     "parameters" {"type" "object"
                                   "properties" {"expression" {"type" "string"}}
                                   "required" ["expression"]}}}})

(defn tool-bindings
  [world tools]
  (mapv
   (fn [t]
     (cond
       (= :calculator t)       calculator-tool
       (:apply-action t)       (action-tool world (:apply-action t))
       (:query-objects t)      (query-tool world t)
       :else (refusal/refuse! :refusal.actions/llm-unknown-tool {:tool t})))
   tools))

(defn- final-tool [spec]
  {"type" "function"
   "function" {"name" "final_answer"
               "description" "Return the final answer. Call this exactly once, when done."
               "parameters" {"type" "object"
                             "properties" {"value" (llm-out/json-schema spec)}
                             "required" ["value"]}}})

(defn- tokens [s]
  (re-seq #"\d+(?:\.\d+)?|[-+*/()]" s))

(defn calculate
  [expression]
  (when-not (re-matches #"[\d\s.+\-*/()]+" (str expression))
    (refusal/refuse! :refusal.actions/calculator-characters {:expression expression}))
  (let [ts (vec (tokens expression))
        pos (volatile! 0)
        peek* #(get ts @pos)
        next* #(let [t (get ts @pos)] (vswap! pos inc) t)]
    (letfn [(atom* []
              (let [t (next*)]
                (cond
                  (= "(" t) (let [v (sum)]
                              (when-not (= ")" (next*))
                                (refusal/refuse! :refusal.actions/calculator-parentheses {})) v)
                  (= "-" t) (- (atom*))
                  (and t (re-matches #"\d+(?:\.\d+)?" t)) (bigdec t)
                  :else (refusal/refuse! :refusal.actions/calculator-unexpected
                                         {:expression expression} {:token t}))))
            (product []
              (loop [v (atom*)]
                (case (peek*)
                  "*" (do (next*) (recur (* v (atom*))))
                  "/" (do (next*) (recur (with-precision 20 (/ v (atom*)))))
                  v)))
            (sum []
              (loop [v (product)]
                (case (peek*)
                  "+" (do (next*) (recur (+ v (product))))
                  "-" (do (next*) (recur (- v (product))))
                  v)))]
      (let [v (sum)]
        (when (< @pos (count ts))
          (refusal/refuse! :refusal.actions/calculator-trailing {:expression expression}))
        (double v)))))

(defn- handle->id [v] (if (rv/handle? v) (:id v) v))

(defn- proposal
  [action args reason]
  (cond-> {:action action :args (update-vals args handle->id)}
    reason (assoc :reason reason)))

(defn- run-query-tool [world ctx io {:keys [rid properties limit] fixed :where} args]
  (grants/authorize-read! (:authority ctx) (get-in world [:object-types rid]) rid)
  (let [w     (get args "where")
        where (into (vec fixed)
                    (for [[k v] (when (map? w) w)
                          :let [p (keyword k)]
                          :when (and (some #{p} properties) (some? v))]
                      [:= p v]))
        l     (get args "limit")
        n     (max 1 (min limit (if (number? l) (long l) limit)))
        rows  (x/read-store ctx io [:select
                                    (os/qt->ir (cond-> {:root rid :limit n}
                                                 (seq where) (assoc :where (into [:and] where))))])
        rows  (if-let [dr (:derived-reader io)]
                (objects/attach-derived (dr (x/reader ctx io)) world rid rows properties)
                rows)]
    (x/loaded! io (count rows))
    {"objects" (mapv (fn [r]
                       (assoc (into {}
                                    (for [p properties]
                                      [(name p) (json-value (get-in r [:values p]))]))
                              "id" (str (:id r))))
                     rows)}))

(defn- collected
  [world ctx io action args]
  (let [v (ops/evaluate-action-params world ctx io action args)]
    (when (:blocked? v)
      (refusal/refuse! (:reason-code v) {:action action} {:why (:reason v)}))
    {:action action :args (update-vals args handle->id)}))

(defn- collecting? [ctx]
  (and (= :collect (:mode ctx)) (not (:staged? ctx))))

(defn- exec-tool
  [run-state ctx io tool args]
  (case (:kind tool)
    :calculator    {:state run-state :result {"result" (calculate (get args "expression"))}}
    :query-objects {:state run-state
                    :result (run-query-tool (:world run-state) ctx io tool args)}
    :apply-action
    (let [action (:action tool)
          reason (get args "_reason")
          params (update-keys (dissoc args "_reason") keyword)]
      (cond
        (= :propose (:mode ctx))
        {:state  (update run-state :proposals (fnil conj []) (proposal action params reason))
         :result {"status" "proposed" "note" "staged for human review, not applied"}}

        (collecting? ctx)
        {:state  (update run-state :collected (fnil conj [])
                         (collected (:world run-state) ctx io action params))
         :result {"status" "applied"}}

        :else
        (let [r (ops/execute-action (:world run-state) (:bindings run-state) ctx io action
                                    {:arg0 params})]
          {:state  (-> run-state
                       (assoc :world (:world r))
                       (update :ops (fnil into []) (:ops r))
                       (update :side-effects into (:side-effects r)))
           :result {"status" "applied"}})))))

(defn- seen
  [ctx run-state]
  (assoc ctx :ops (into (vec (:ops ctx)) (:ops run-state))))

(defn- run-tools
  [run-state ctx io by-name calls]
  (reduce (fn [acc {:keys [name arguments] :as c}]
            (let [{:keys [state result]}
                  (if-let [t (by-name name)]
                    (try (exec-tool (:state acc) (seen ctx (:state acc)) io t arguments)
                         (catch clojure.lang.ExceptionInfo e
                           (if (or (anomaly/acl-denied? (ex-data e)) (x/need e))
                             (throw e)
                             {:state (:state acc) :result {"error" (ex-message e)}})))
                    {:state (:state acc) :result {"error" (str "unknown tool " name)}})]
              (-> acc
                  (assoc :state state)
                  (update :results conj {:call c :result result}))))
          {:state run-state :results []}
          calls))

(defn- tool-message [call result]
  {"role" "tool" "tool_call_id" (:id call) "content" (json/write-str result)})

(defn- parse-arguments [a]
  (cond (map? a) (update-keys a name)
        (string? a) (let [v (json/read-str a)] (if (map? v) v {}))
        :else {}))

(defn- content-answer [spec content]
  (let [parsed (try (json/read-str (str content)) (catch Exception _ ::raw))
        v      (cond (= ::raw parsed) content
                     (and (map? parsed) (contains? parsed "value")
                          (not (:struct spec))) (get parsed "value")
                     :else parsed)
        r      (llm-out/coerce spec v)]
    (if (and (:error r) (= :string spec)) {:value (str content)} r)))

(defn- failure [msg log extra]
  (refusal/refusal :refusal.actions/llm-failed (merge {:decision-log log} extra) {:msg msg}))

(defn- transcript-within-limit!
  [msgs log]
  (let [n (count (json/write-str (llm/elide msgs)))]
    (when (> n limits/transcript-chars-max)
      (throw (failure (str "the conversation grew to " n " characters, past the "
                           limits/transcript-chars-max
                           " one run may carry — "
                           "narrow the tools' :properties and :limit, or split the block")
                      log {:sutra/run-limit :transcript})))))

(defn prompt-sha
  [system text shown]
  (digest/sha256-hex (pr-str [system text (mapv :hash shown)])))

(defn- opening
  [{:keys [model temperature]} spec tools* sys text shown]
  {:msgs    (cond-> []
              sys  (conj {"role" "system" "content" sys})
              true (conj {"role"    "user"
                          "content" (if (seq (:parts shown))
                                      (into [{"type" "text" "text" text}] (:parts shown))
                                      text)}))
   :request (cond-> {"model"       model
                     "tools"       (conj (mapv :def tools*) (final-tool spec))
                     "tool_choice" "required"}
              (some? temperature) (assoc "temperature" temperature))})

(defn- read-turn
  [message request-id i]
  (let [calls (mapv (fn [c] {:id (:id c) :name (get-in c [:function :name])
                             :arguments (parse-arguments (get-in c [:function :arguments]))})
                    (:tool_calls message))
        final? #(= "final_answer" (:name %))]
    {:final  (some #(when (final? %) %) calls)
     :others (remove final? calls)
     :asst   (cond-> {"role" "assistant" "content" (:content message)}
               (seq calls)                (assoc "tool_calls" (:tool_calls message))
               (:provider_blocks message) (assoc "provider_blocks" (:provider_blocks message)))
     :entry  {:turn i :request-id request-id :content (:content message)
              :tool-calls (mapv #(select-keys % [:name :arguments]) calls)}}))

(defn- next-step
  [spec final message i cap]
  (let [{:keys [error path value]} (if final
                                     (llm-out/coerce spec (get (:arguments final) "value"))
                                     (content-answer spec (:content message)))]
    (cond (nil? error)    [:answer value]
          (< (inc i) cap) [:retry error path]
          :else           [:fail error path])))

(defn- after-tools
  [results final]
  (cond-> (mapv #(tool-message (:call %) (:result %)) results)
    final (conj (tool-message final {"error"
                                     "call final_answer alone, after the tools have answered"}))))

(defn- asking-again
  [final err]
  (if final
    [(tool-message final {"error" err})]
    [{"role" "user" "content" (str "Invalid answer: " err ". Call final_answer.")}]))

(defn- answered
  [{:keys [model source as]} run-state bindings v {:keys [text psha shown]} log i]
  (let [media (seq (:media shown))]
    {:world        (:world run-state)
     :ops          (:ops run-state)
     :proposals    (:proposals run-state)
     :collected    (:collected run-state)
     :decisions    [(cond-> {:model model :source source :prompt text :prompt-sha psha :log log}
                      media (assoc :media (:media shown)))]
     :bindings     (cond-> bindings as (assoc as v))
     :side-effects (conj (:side-effects run-state)
                         (kernel/event :use-llm
                                       (cond-> {:model model :source source
                                                :request-ids (vec (keep :request-id log))
                                                :turns (inc i) :prompt-sha psha}
                                         media (assoc :media (mapv :hash (:media shown))))))
     :trace-extra  {:decision-log log :model model}}))

(defn do-use-llm
  [{:keys [model source prompt system output tools max-iterations vars media media-as
           media-pages] :as args} world bindings ctx io]
  (when (and (:dry-run? ctx) (not (:asks-models? ctx)))
    (refusal/refuse! :refusal.actions/plan-model-not-asked
                     {:plan/model-not-asked (str model)} {:model model}))
  (let [spec    (or output :string)
        tools*  (mapv #(cond->> % (seq (:where %)) (where-values vars bindings))
                      (tool-bindings world tools))
        by-name (coll/index-by :name tools*)
        cap     (min max-iterations-cap (or max-iterations default-iterations))
        text    (interpolate prompt vars bindings)
        sys     (when system (interpolate system vars bindings))
        shown   (when (seq media)
                  (llm/parts (scope/blobs) (media-values media vars bindings)
                             {:as media-as :pages media-pages}))
        asked   {:text text :psha (prompt-sha sys text (:media shown)) :shown shown}
        {:keys [msgs request]} (opening args spec tools* sys text shown)]
    (loop [msgs msgs, i 0, run-state {:world world :bindings bindings :side-effects [] :ops []},
           log []]
      (when (>= i cap)
        (throw (failure (str "no valid " (pr-str spec) " answer within " cap " turns") log
                        {:model model})))
      (x/checkpoint! io (assoc run-state :ops (:ops (seen ctx run-state))))
      (transcript-within-limit! msgs log)
      (let [{:keys [request-id message]}
            (:result (x/ask ctx io {:kind :llm :source source :model model
                                    :request (assoc request "messages" msgs)
                                    :caller "use-llm" :run-id (:run-id ctx)
                                    :branch (:branch ctx) :schema (:schema ctx)}))
            {:keys [final others asst entry]} (read-turn message request-id i)
            log (conj log entry)]
        (if (seq others)
          (let [{:keys [state results]} (run-tools run-state ctx io by-name others)]
            (recur (into (conj msgs asst) (after-tools results final)) (inc i) state
                   (conj log {:turn i
                              :tool-results (mapv (fn [{:keys [call result]}]
                                                    {:tool (:name call) :result result})
                                                  results)})))
          (let [[kind v path] (next-step spec final message i cap)]
            (case kind
              :answer (answered args run-state bindings v asked
                                (conj log {:turn i :output v}) i)
              :retry  (recur (into (conj msgs asst) (asking-again final v)) (inc i) run-state
                             (conj log {:turn i :invalid v :path path}))
              :fail   (throw (failure (str "the model's answer does not fit " (pr-str spec)
                                           ": " v)
                                      (conj log {:turn i :invalid v :path path})
                                      {:model model :path path})))))))))

(defn- refuse-edit [op]
  (fn [_ _ _ _ _]
    (refusal/refuse! :refusal.actions/propose-step-edits {:op op :mode :propose}
                     {:op (name op)})))

(def ^:private writeback-ops
  #{:invoke-webhook})

(defn proposing
  [ictx]
  (-> ictx
      (assoc :mode :propose)
      (update :verbs merge (into {} (for [op writeback-ops] [op (refuse-edit op)])))))

(def verbs {:use-llm do-use-llm})

(def performers {:llm perform-llm})

(def module
  {:name     :logic
   :wants #{:llm/complete}})
