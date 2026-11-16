(ns sutra.agents.runtime
  (:require [sutra.foundation.workers :as workers]
            [sutra.kit.coll :as coll]
            [sutra.kit.attempt :as attempt]
            [sutra.foundation.encoding :as enc]
            [sutra.kit.refusal :as refusal]
            [clojure.data.json :as json]
            [sutra.governance.api :as governance]
            [sutra.store.access.scope :as scope]
            [clojure.string :as str]
            [sutra.agents.ports :as ports]
            [sutra.agents.builder :as builder]
            [sutra.agents.threads :as threads]
            [sutra.agents.tools :as tools]
            [sutra.catalog.values :as values]
            [sutra.catalog.llm-models :as llm-models]
            [sutra.catalog.lookup :as lookup]
            [sutra.foundation.settings :as settings]
            [sutra.types.run-limits :as limits]
            [sutra.types.versions :as vers]
            [sutra.llm.api :as llm-api])
  (:import (java.time LocalDateTime ZoneId)
           (java.time.format DateTimeFormatter TextStyle)
           (java.util Locale)
           (java.util.concurrent ThreadPoolExecutor)))

(set! *warn-on-reflection* true)

(def default-turns 8)
(def max-turns-cap 20)
(def history-max 20)
(def call-timeout-ms 300000)
(def seen-max 40)

(def builtin-name :analyst)

(defn- model-name [k] (if (keyword? k) (subs (str k) 1) (str k)))

(defn- default-model
  [schema]
  (let [live   (remove (comp :deprecated val) (:llm-models schema))
        marked (filter (comp :default val) live)
        models (mapv (comp model-name key) (if (seq marked) marked live))]
    (when (= 1 (count models)) (first models))))

(defn builtin
  [schema]
  (when-let [model (default-model schema)]
    {:agent-name   builtin-name
     :builtin      true
     :display-name "Analyst"
     :description  "Answers questions about your data from every object type you may read."
     :model        model
     :tools        [{:kind :ontology} {:kind :calculator}]}))

(defn builtins
  [schema]
  (when-let [model (default-model schema)]
    [(builtin schema) (builder/builtin model)]))

(defn agent-of
  ([schema nm] (agent-of schema nm nil))
  ([schema nm version]
   (when nm
     (let [k (keyword (name nm))]
       (or (some-> (get-in schema [:agents (lookup/rid-of schema :agents k)])
                   (vers/as-version version))
           (some #(when (= k (:agent-name %)) %) (builtins schema)))))))

(defn- llm-sources
  [schema]
  (into (sorted-set)
        (for [[_ t] (:transforms schema)
              [_ in] (:inputs t)
              :let [in (when (map? in) (llm-models/with-source schema in))]
              :when (and (llm-models/language-model-input? in) (:source in))]
          (keyword (name (:source in))))))

(defn source-of
  [schema agent]
  (or (some-> (:source agent) name keyword)
      (some-> (llm-models/rate-of schema (:model agent)) :source name keyword)
      (let [used (llm-sources schema)]
        (when (= 1 (count used)) (first used)))
      (let [cands (for [[k s] (values/sources->datasources schema)
                        :when (and (:external-transform s)
                                   (or (:base-url s) (get-in s [:config :base-url])))]
                    k)]
        (when (= 1 (count cands)) (keyword (name (first cands)))))
      (refusal/refuse! :refusal.agents/no-source
                       {:agent (:agent-name agent)}
                       {:agent-name (name (:agent-name agent))})))

(defn- zone-of
  ^ZoneId [zone]
  (or (some-> zone str not-empty (#(try (ZoneId/of %) (catch java.time.DateTimeException _ nil))))
      (ZoneId/systemDefault)))

(defn- now-line [zone]
  (let [z   (zone-of zone)
        now (LocalDateTime/now z)]
    (str "Now: " (.getDisplayName (.getDayOfWeek now) TextStyle/FULL Locale/ENGLISH) " "
         (.format now (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm")) " (" z ").")))

(defn- system-prompt [agent tool-names writes zone]
  (str/join
   "\n"
   (remove nil?
           [(:system-prompt agent)
            (now-line zone)
            "Answer in the language of the question."
            (when (seq tool-names)
              (str "Call the tools you need right away instead of describing what you would do; "
                   "answer only once their results are in. "
                   "Answer questions about data only from what your tools return; never invent a "
                   "value. "
                   "If the tools find nothing, say so and say what you looked for. "
                   "Cite every object a statement rests on by the ref its tool result carries, "
                   "in double brackets: [[type:id]]. Dates are calendar days YYYY-MM-DD; "
                   "\"yesterday\" and \"last month\" are relative to now."))
            (when (some #(str/starts-with? % "apply_") tool-names)
              (if (= :apply writes)
                "An action you call is applied at once — call one only when the asker asked for that change."
                "An action you call is staged as a proposal the asker accepts or rejects; say what you proposed."))])))

(def ^:private cite-re #"\[\[([A-Za-z0-9_\-]+):([^\]\s]+)\]\]")

(defn- citations
  [answer refs]
  (let [by   (into {} (map (fn [r] [(str (:type r) ":" (:id r)) r])) refs)
        hits (distinct (map (fn [[_ t id]] (str t ":" id)) (re-seq cite-re (str answer))))]
    [(vec (keep by hits)) (vec (remove by hits))]))

(defn- strip-thinking
  [content]
  (let [i (str/last-index-of content "</think>")]
    (str/trim (if i (subs content (+ i (count "</think>"))) content))))

(defn- history-messages [msgs]
  (->> msgs
       (filter #(and (= "done" (:status %)) (not (str/blank? (:content %)))))
       (take-last history-max)
       (mapv (fn [{:keys [role content detail]}]
               {"role" role
                "content" (if-let [as (seq (:attachments detail))]
                            (str content "\n[attached: "
                                 (str/join ", " (map #(or (:name %) (:hash %)) as)) "]")
                            content)}))))

(defn- user-message [input attachments]
  (if (seq attachments)
    (let [{:keys [parts]} (llm-api/parts (scope/blobs) (mapv :hash attachments) {:as :auto})]
      {"role" "user" "content" (into [{"type" "text" "text" input}] parts)})
    {"role" "user" "content" input}))

(defn- arguments [a]
  (cond (map? a)    (update-keys a name)
        (string? a) (let [v (enc/json-or-nil a)] (if (map? v) v {}))
        :else       {}))

(defn- step-summary [{:keys [result]}]
  (cond
    (get result "error")   {:error (get result "error")}
    (get result "objects") {:count (get result "count") :returned (get result "returned")}
    (get result "groups")  {:groups (count (get result "groups"))}
    (get result "types")   {:types (count (get result "types"))}
    :else                  (update-vals (update-keys result keyword)
                                        #(if (and (string? %) (> (count %) 300))
                                           (str (subs % 0 300) "…") %))))

(defn- complete!
  [{:keys [store schema source model run-id agent]} request]
  ((ports/llm-complete store)
   {:store store :source source :model model :request request
    :caller (str "agent:" (name (:agent-name agent)))
    :run-id run-id :branch (:branch store) :schema schema :timeout-ms call-timeout-ms}))

(defn- turn-cap
  [agent max-turns]
  (min (if (:builtin agent) (max max-turns-cap (or (:max-turns agent) 0)) max-turns-cap)
       (or max-turns (:max-turns agent) default-turns)))

(defn- embedder
  [store schema agent run-id]
  (fn [m text]
    ((ports/llm-embed store) store
                       {:source     (source-of schema {:agent-name (:agent-name agent) :model m})
                        :model      m :text text
                        :caller     (str "agent:" (name (:agent-name agent)))
                        :run-id     run-id :branch (:branch store) :schema schema
                        :timeout-ms call-timeout-ms})))

(defn- request-base
  [agent model tools*]
  (cond-> {"model" model}
    (seq tools*)                 (assoc "tools" (mapv :def tools*))
    (some? (:temperature agent)) (assoc "temperature" (:temperature agent))))

(defn- run-limit
  [{:keys [turn msgs]} cap]
  (cond
    (>= turn cap)
    (str "no answer within " cap " model turns")

    (> (count (json/write-str (llm-api/elide msgs))) limits/transcript-chars-max)
    "the conversation grew past what one run may carry — ask a narrower question"))

(defn- reply-kind
  [{:keys [turn nudged?]} cap calls answer]
  (cond
    (and (empty? calls) (str/blank? answer) (< (inc turn) cap) (not nudged?)) :nudge
    (empty? calls)                                                          :answer
    :else                                                                   :tools))

(defn- reply-step
  [turn {:keys [request-id ms tokens]} m]
  (cond-> (merge {:turn turn :request-id request-id :ms ms} m)
    tokens (assoc :tokens tokens)))

(defn- run-tools!
  [store schema by opts calls]
  (mapv (fn [{:keys [id function]}]
          (let [nm   (:name function)
                args (arguments (:arguments function))
                t1   (System/currentTimeMillis)
                out  (if-let [tool (by nm)]
                       (try (tools/execute store schema tool args opts)
                            (catch Throwable e
                              (attempt/rethrow-fatal! e)
                              {:result {"error" (or (ex-message e) (str (class e)))}}))
                       {:result {"error" (str "unknown tool " nm)}})]
            (assoc out :id id :tool nm :args args :ms (- (System/currentTimeMillis) t1))))
        calls))

(defn- after-tools
  [{:keys [turn] :as state} message {:keys [request-id tokens]} ran]
  (-> state
      (update :msgs into
              (cons (cond-> {"role" "assistant" "content" (:content message)
                             "tool_calls" (:tool_calls message)}
                      (:provider_blocks message)
                      (assoc "provider_blocks" (:provider_blocks message)))
                    (map (fn [r] {"role" "tool" "tool_call_id" (:id r)
                                  "content" (json/write-str (:result r))})
                         ran)))
      (update :steps into
              (map-indexed (fn [i r]
                             (cond-> {:turn turn :request-id request-id :tool (:tool r)
                                      :args (:args r) :ms (:ms r) :summary (step-summary r)}
                               (and tokens (zero? i)) (assoc :tokens tokens))))
              ran)
      (update :refs into (mapcat :refs) ran)
      (update :props into (keep :proposal) ran)
      (update :turn inc)))

(defn- nudged
  [state reply]
  (-> state
      (update :msgs conj {"role" "user"
                          "content" "Give your answer now, from the tool results above."})
      (update :steps conj (reply-step (:turn state) reply {:empty true}))
      (update :turn inc)
      (assoc :nudged? true)))

(defn- answered
  [{:keys [turn steps refs]} reply answer]
  (let [[sources unverified] (citations answer refs)]
    {:answer answer :sources sources :unverified unverified :turns (inc turn)
     :seen   (vec (take seen-max (distinct refs)))
     :steps  (conj steps (reply-step turn reply {:answer true}))}))

(defn- answer-in-run!
  [store schema agent
   {:keys [input history attachments thread-id run-id max-turns on-step writes zone]}]
  (let [agent  (cond-> agent
                 (and (:builtin agent) (= builder/agent-name (:agent-name agent)))
                 (assoc :system-prompt (builder/system-prompt store)))
        source (source-of schema agent)
        model  (:model agent)
        writes (or writes (:writes agent) :propose)
        tools* (tools/bindings schema agent)
        by     (coll/index-by :name tools*)
        cap    (turn-cap agent max-turns)
        window (:context-window (llm-models/rate-of schema model))
        ctx    {:store store :schema schema :source source :model model :run-id run-id
                :agent agent}
        opts   {:writes writes :thread-id thread-id :run-id run-id :agent (:agent-name agent)
                :embed  (embedder store schema agent run-id)
                :writing (delay (governance/data-guarded store))}
        base   (request-base agent model tools*)
        done   (fn [{:keys [steps turn props]} m]
                 (merge (cond-> {:steps steps :turns turn :proposals props :model model
                                 :source (name source)}
                          window (assoc :context-window window))
                        m))]
    (when-not (llm-models/registered? schema model)
      (refusal/refuse! :refusal.agents/model-unregistered
                       {:model model}
                       {:agent-name (name (:agent-name agent))}))
    (loop [state {:msgs    (-> [{"role"    "system"
                                 "content" (system-prompt agent (map :name tools*) writes zone)}]
                               (into (history-messages history))
                               (conj (user-message input attachments)))
                  :turn 0 :steps [] :refs [] :props [] :nudged? false}]
      (if-let [why (run-limit state cap)]
        (done state {:error why})
        (let [t0     (System/currentTimeMillis)
              {:keys [request-id message usage]}
              (complete! ctx (assoc base "messages" (:msgs state)))
              reply  {:request-id request-id :ms (- (System/currentTimeMillis) t0)
                      :tokens     (when (:prompt_tokens usage)
                                    {:in (:prompt_tokens usage) :out (:completion_tokens usage)})}
              calls  (:tool_calls message)
              answer (strip-thinking (str (:content message)))]
          (case (reply-kind state cap calls answer)
            :nudge  (recur (nudged state reply))
            :answer (done state (answered state reply answer))
            :tools  (let [after (after-tools state message reply
                                             (run-tools! store schema by opts calls))]
                      (when on-step (on-step (:steps after)))
                      (recur after))))))))

(defn answer!
  [store schema agent opts]
  (if (some #(= :builder (:kind %)) (tools/bindings schema agent))
    (builder/with-api-token store #(answer-in-run! store schema agent opts))
    (answer-in-run! store schema agent opts)))

(defonce ^:private ^ThreadPoolExecutor pool
  (workers/pool "agent-run-" (settings/value :agent-runs)))

(defn- admitted
  ^ThreadPoolExecutor []
  (let [waiting (.size (.getQueue (workers/sized! pool (settings/value :agent-runs))))]
    (when (>= waiting (settings/value :agent-run-queue))
      (refusal/refuse! :refusal.agents/runs-queue-full
                       {:waiting waiting :running (.getActiveCount pool) :retry true}))
    pool))

(defn- detail-of [r attachments]
  (cond-> (select-keys r
                       [:steps :sources :unverified :seen :proposals :turns :model :source :error
                        :context-window])
    (seq attachments) (assoc :attachments attachments)))

(defn with-model
  [schema agent model]
  (if (or (str/blank? (str model)) (= (str model) (str (:model agent))))
    agent
    (do (when-not (llm-models/registered? schema model)
          (refusal/refuse! :refusal.agents/model-not-registered {:model model}))
        (cond-> (assoc agent :model (str model))
          (:source (llm-models/rate-of schema model)) (dissoc :source)))))

(defn ask!
  [store schema thread {:keys [input attachments max-turns sync? zone model version]}]
  (let [runs   (when-not sync? (admitted))
        agent  (with-model schema
                 (or (agent-of schema (:agent thread) version)
                     (refusal/refuse! :refusal.agents/no-agent {:agent (:agent thread)}))
                 model)
        source (source-of schema agent)
        tid    (:id thread)
        prior  (threads/messages store tid)
        _      (threads/titled! store thread input)
        q      (threads/add! store tid
                             (cond-> {:role "user" :content input}
                               (seq attachments) (assoc :detail {:attachments (vec attachments)})))
        run-id (str (random-uuid))
        a      (threads/add! store tid
                             {:role "assistant" :status "running" :run-id run-id
                              :detail
                              (cond-> {:steps [] :model (:model agent) :source (name source)}
                                version (assoc :agent-version version))})
        seen   (atom [])
        work   (fn []
                 (try
                   (let [r (answer! store schema agent
                                    {:input input :history prior :attachments attachments
                                     :thread-id tid :run-id run-id :max-turns max-turns :zone zone
                                     :on-step (fn [steps]
                                                (reset! seen steps)
                                                (threads/update-message! store tid (:seq a)
                                                                         {:detail {:steps steps
                                                                                   :model
                                                                                   (:model agent)
                                                                                   :source
                                                                                   (name
                                                                                    source)}}))})]
                     (threads/update-message! store tid (:seq a)
                                              {:status (if (:error r) "failed" "done")
                                               :content (:answer r)
                                               :detail (detail-of r nil)}))
                   (catch Throwable e
                     (threads/update-message! store tid (:seq a)
                                              {:status "failed" :detail {:steps @seen
                                                                         :error
                                                                         (or (ex-message e)
                                                                             (str
                                                                              (class e)))}})
                     (attempt/rethrow-fatal! e))))]
    (if sync?
      (work)
      (.submit runs ^Runnable work))
    {:question q :answer (assoc a :run-id run-id)}))
