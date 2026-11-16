(ns sutra.evals.suites
  (:require [sutra.foundation.settings :as settings]
            [sutra.kit.refusal :as refusal]
            [clojure.data.json :as json]
            [clojure.math :as math]
            [clojure.string :as str]
            [sutra.foundation.live-trace :as live]
            [sutra.store.control.cadence :as cadence]
            [sutra.compute.api :as compute]
            [sutra.foundation.encoding :as enc :refer [coerce-uuids]]
            [sutra.kit.anomaly :as anomaly]
            [sutra.kit.cache :as cache]
            [sutra.evals.ports :as ports]
            [sutra.parser.fn-params :as fp]
            [sutra.agents.api :as agents]
            [sutra.evals.cases :as cases]
            [sutra.evals.evaluators :as ev]
            [sutra.evals.experiments :as exp]
            [sutra.evals.validate :as validate]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.access.scope :as ctx]
            [sutra.catalog.functions :as functions]
            [sutra.catalog.llm-models :as llm-models]
            [sutra.store.control :as ctl]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [sutra.types.transform-io :as tio]
            [sutra.actions.api :as actions])
  (:import (java.util UUID)
           (java.util.concurrent ExecutionException Executors Future)))

(set! *warn-on-reflection* true)

(defn score-evaluator
  [e as es]
  (dissoc (ev/score e as es) :per-case))

(defn- target-label [{:keys [function agent action]}]
  (cond function  (str "function:" (name function))
        agent     (str "agent:" (name agent))
        action    (str "action:" (name action))))

(defn- declared-id [nm]
  (str (UUID/nameUUIDFromBytes (.getBytes (str "eval-suite:" (name nm)) "UTF-8"))))

(defn- declared-suites [schema]
  (for [[k spec] (sort-by key (:eval-suites schema))
        :let [nm (name (or (:name spec) k))]]
    {:id (declared-id nm) :name nm :transform (target-label spec) :declared true :enabled true
     :spec (assoc (dissoc spec :project) :name nm)}))

(defn- latest-run [store branch id]
  (some-> (first (ctl/list store :eval-runs {:branch branch :suite-id id} {:limit 1}))))

(defn list-all [store schema branch]
  (let [declared (declared-suites schema)
        names    (set (map :name declared))
        saved    (remove #(names (:name %)) (ctl/list store :eval-suites {:branch branch}))]
    (mapv (fn [{:keys [id spec] :as suite}]
            (let [f (exp/measured-function schema spec)]
              (cond-> (assoc suite :latest (latest-run store branch id)
                             :parameters (exp/parameters schema spec))
                f (assoc :versions (some-> (functions/function-by-name schema f) :spec
                                           functions/released-versions)))))
          (concat declared saved))))

(defn- suite-by-name [store schema branch nm]
  (or (some #(when (= (name nm) (:name %)) %) (declared-suites schema))
      (ctl/get store :eval-suites {:branch branch :name (name nm)})))

(defn results [store branch suite-id]
  (mapv (fn [{:keys [id] :as r}]
          (assoc r :metrics (ctl/list store :eval-results {:branch branch :run-id id})))
        (ctl/list store :eval-runs {:branch branch :suite-id suite-id} {:limit 200})))

(defn run-cases
  [store branch run-id]
  {:run   (some-> (ctl/get store :eval-runs {:branch branch :id run-id}))
   :cases (ctl/list store :eval-case-results {:branch branch :run-id run-id})})

(defn upsert!
  [store schema branch {:keys [name spec] :as suite}]
  (let [spec (assoc (or spec (dissoc suite :spec :enabled :id)) :name (clojure.core/name name))]
    (when (some #(= (clojure.core/name name) (:name %)) (declared-suites schema))
      (refusal/refuse! :refusal.eval/suite-declared {:suite name}))
    (validate/check! schema spec)
    (let [existing (ctl/get store :eval-suites {:branch branch :name (clojure.core/name name)})
          id       (or (:id existing) (:id suite) (str (UUID/randomUUID)))]
      (cadence/upsert! store branch "eval_suites"
                       {:id id :name (clojure.core/name name) :transform (target-label spec)
                        :spec spec
                        :enabled (boolean (if (nil? (:enabled suite)) true (:enabled suite)))}))))

(defn- case-ids
  [store schema branch suite-spec]
  (into (set (map #(str (:id %)) (:cases suite-spec)))
        (comp (mapcat #(:cases (cases/object-set-cases (ctx/at store branch) schema %)))
              (map :id))
        (:cases-from suite-spec)))

(defn with-case
  [store schema branch nm {:keys [id input expected]}]
  (let [suite (or (suite-by-name store schema branch nm)
                  (refusal/refuse! :refusal.eval/unknown-suite {:suite nm}))
        id    (some-> id str str/trim not-empty)
        c     (cond-> {:id id :input input} (seq expected) (assoc :expected expected))]
    (when-not (and id (map? input) (or (nil? expected) (map? expected)))
      (refusal/refuse! :refusal.eval/case-shape {:case c}))
    (when (contains? (case-ids store schema branch (:spec suite)) id)
      (refusal/refuse! :refusal.eval/case-exists {:suite nm :case id} {:suite-name (name nm)}))
    (if (:declared suite)
      (let [k    (some (fn [[k s]] (when (= (name nm) (name (or (:name s) k))) k))
                       (:eval-suites schema))
            node (update (get-in schema [:eval-suites k]) :cases (fnil conj []) c)]
        (validate/check! schema (dissoc node :project))
        {:declared k :spec node})
      {:suite (update-in suite [:spec :cases] (fnil conj []) c)})))

(defn- resolve-suite
  [store schema branch suite-ref]
  (cond
    (map? suite-ref) suite-ref
    (nil? suite-ref) (refusal/refuse! :refusal.eval/suite-unnamed {})
    :else (or (:spec (suite-by-name store schema branch suite-ref))
              (refusal/refuse! :refusal.eval/unknown-suite-listed {:suite suite-ref}))))

(defn- agent-actual
  [store schema agent input]
  (try
    (let [r (agents/answer! store schema agent
                            {:input (if (map? input) (str (:question input)) (str input))
                             :history (when (map? input)
                                        (mapv (fn [{:keys [role content]}]
                                                {:role (str role) :content (str content)
                                                 :status "done"})
                                              (:history input)))
                             :run-id (str (UUID/randomUUID)) :writes :dry})]
      (cond-> {:answer   (:answer r)
               :tools    (vec (distinct (keep :tool (:steps r))))
               :proposed (mapv #(select-keys % [:action :args]) (:proposals r))
               :sources  (mapv #(str (:type %) ":" (:id %)) (:sources r))
               :turns    (:turns r)}
        (:error r) (assoc :error (:error r))))
    (catch InterruptedException e (throw e))
    (catch Exception e {:error (or (ex-message e) (str (class e)))})))

(defn- action-actual
  [store branch edge action input]
  (try
    (let [r (actions/apply-action
             {:authority (grants/request-authority store) :branch branch
              :store (ctx/at store branch) :edge edge :action action :mode :dry-run
              :dry-run? true :asks-models? true
              :args (coerce-uuids (actions/with-fresh-id action (or input {})))})]
      (if (anomaly/refusal? r) {:error (anomaly/message r)} {:plan (:plan r)}))
    (catch InterruptedException e (throw e))
    (catch Exception e {:error (or (ex-message e) (str (class e)))})))

(defn- ms-since [t0]
  (quot (- (System/nanoTime) t0) 1000000))

(def ^:private ^:dynamic *case-trace*
  nil)

(def ^:private log-lines-kept 40)

(defn- note-trace!
  [err]
  (when (and *case-trace* (seq (str err)))
    (let [{:keys [spans log intermediates]} (compute/spans err)]
      (vreset! *case-trace*
               (cond-> {}
                 (seq spans)         (assoc :spans spans)
                 (seq log)           (assoc :log (mapv live/clip (take-last log-lines-kept log)))
                 (seq intermediates) (assoc :intermediates intermediates))))))

(defn- spans-brief
  [spans]
  (when (seq spans)
    (str/join ", " (map #(str (:name %) " "
                              (if (:open %) "open" (str (:ms %) "ms")))
                        (take 3 (sort-by (juxt (comp not :open) (comp - #(or (:ms %) 0)))
                                         spans))))))

(defn default-parallel
  []
  (settings/value :eval-parallel))

(defn- in-parallel
  [n f xs]
  (if (or (<= n 1) (< (count xs) 2))
    (mapv f xs)
    (let [pool (Executors/newFixedThreadPool (min n (count xs)))]
      (try
        (mapv #(try (.get ^Future %)
                    (catch ExecutionException e (throw (or (.getCause e) e))))
              (mapv (fn [x] (let [^Callable task (bound-fn [] (f x))] (.submit pool task))) xs))
        (finally (.shutdownNow pool))))))

(defn- each
  [{:keys [progress timings traces intermediates iteration parallel]} suite f ins]
  (in-parallel
   (or parallel 1)
   (fn [[i in]]
     (let [t0  (System/nanoTime)
           tr  (volatile! nil)
           r   (binding [*case-trace* tr] (f in))
           ms  (ms-since t0)
           p   (when progress (progress {:tick 1}))
           err (when (map? r) (:error r))]
       (some-> timings (swap! conj ms))
       (some-> traces (swap! assoc [iteration i]
                             (assoc (dissoc @tr :intermediates) :ms ms)))
       (when-some [m (:intermediates @tr)]
         (some-> intermediates (swap! assoc [iteration i] m)))
       (live/emit! (cond-> {:kind :eval/case :suite (some-> (:name suite) str)
                            :case (str (or (:id (nth (:cases suite) i nil)) i))
                            :iteration iteration :ms ms
                            :done (:done p) :total (:total p)
                            :spans (spans-brief (:spans @tr))}
                     err (assoc :level :warn :error (live/clip err))))
       r))
   (vec (map-indexed vector ins))))

(defn- release-version
  [version]
  (when (and (some? version) (not (#{"published" "last-saved"} (str version))))
    (str version)))

(defn- at-version
  [schema suite version]
  (let [v (release-version version)
        f (when v (exp/measured-function schema suite))
        e (when f (functions/function-at-version schema f {:version v}))]
    (cond
      (nil? e) schema
      (:unsatisfied e)
      (refusal/refuse! :refusal.eval/no-release {:function f :version version}
                       {:function-name (name f)
                        :published (if (seq (:available e)) (str/join ", " (:available e)) "–")})
      :else
      (assoc-in schema [(if (= :query (:kind e)) :transforms :functions) (:rid e)]
                (:spec e)))))

(defn- read-actuals
  [store schema branch suite {:keys [model version edge read] :as o}]
  (let [ins (mapv #(cases/mapped-input (:input-map suite) (:input %)) (:cases suite))]
    (cond
      (:agent suite)
      (let [agent (agents/with-model
                    schema
                    (or (agents/agent-of schema (:agent suite) version)
                        (refusal/refuse! :refusal.eval/unknown-agent {:agent (:agent suite)}))
                    model)
            store (ctx/at store branch)]
        (each o suite #(agent-actual store schema agent %) ins))

      (:function suite)
      (let [store   (ctx/at store branch)
            fn-name (keyword (name (:function suite)))]
        (each o suite
              (fn [input]
                (try (let [{:keys [rows log]} (actions/run-query-function-logged
                                               store schema fn-name input nil read)]
                       (note-trace! log)
                       (first rows))
                     (catch clojure.lang.ExceptionInfo e
                       (note-trace! (:stderr (ex-data e)))
                       {:error (ex-message e)})))
              ins))

      (:action suite)
      (let [action (keyword (name (:action suite)))]
        (when-not edge
          (refusal/refuse! :refusal.eval/action-suite-local {:action action}))
        (each o suite #(action-actual store branch edge action %) ins)))))

(def ^:private judge-system
  (str "You grade one answer of an AI system. Score how well the ACTUAL answer meets the RUBRIC "
       "and, when given, matches the EXPECTED answer in substance (wording may differ; numbers "
       "must "
       "agree). Write the reason in the language of the EXPECTED answer. "
       "Reply with JSON only: {\"score\": <number 0..1>, \"reason\": \"<one sentence>\"}."))

(defn- judge-score [content]
  (let [s (str content)
        j (or (re-find #"(?s)\{.*\}" s) s)
        v (enc/json-or-nil j)
        n (some-> (get v "score") double)]
    {:score  (if n (max 0.0 (min 1.0 n)) 0.0)
     :reason (or (get v "reason")
                 (when-not n (str "unparseable verdict: " (subs s 0 (min 200 (count s))))))}))

(defn- judge-fn
  [store schema branch suite]
  (fn [ev i actual expected]
    (let [model (or (:model ev)
                    (some->> (:agent suite) (agents/agent-of schema) :model)
                    (:model (agents/builtin schema))
                    (refusal/refuse! :refusal.eval/judge-without-model {:evaluator (:name ev)}))
          input (:input (nth (:cases suite) i nil))
          text  (str "RUBRIC: " (or (:rubric ev) "Is the answer correct and complete?")
                     "\nINPUT: " (if (string? input) input (json/write-str input))
                     (when (some? expected)
                       (str "\nEXPECTED: "
                            (if (string? expected) expected (json/write-str expected))))
                     "\nACTUAL: " (if (string? actual) actual (json/write-str actual)))
          {:keys [message]}
          ((ports/llm-complete store)
           {:store store :schema schema :branch branch :model model :caller "eval-judge"
            :timeout-ms 120000
            :source (or (some-> (:source ev) name keyword)
                        (agents/source-of schema
                                          (merge (some->> (:agent suite) (agents/agent-of schema))
                                                 {:agent-name :judge :model model})))
            :request {"model" model "max_tokens" 2000
                      "messages" [{"role" "system" "content" judge-system}
                                  {"role" "user" "content" text}]}})]
      (judge-score (:content message)))))

(defn- score-actuals
  [suite actuals ints]
  (let [expecteds (mapv :expected (:cases suite))
        ins       (mapv :input (:cases suite))]
    (into {} (map (fn [e]
                    [(:name e)
                     (ev/score e
                               (if-some [p (:intermediate e)]
                                 (mapv #(get % (keyword (name p))) ints)
                                 actuals)
                               expecteds ins)]))
          (:evaluators suite))))

(defn- intermediates-at
  [suite ints it]
  (mapv #(get ints [it %]) (range (count (:cases suite)))))

(defn- unexposed
  [suite ints]
  (let [seen (into #{} (comp (mapcat keys) (map name)) (vals ints))]
    (set (keep #(when-some [p (:intermediate %)] (when-not (seen (name p)) (:name %)))
               (:evaluators suite)))))

(defn- stddev [xs]
  (if (< (count xs) 2)
    0.0
    (let [m (/ (reduce + xs) (count xs))]
      (math/sqrt (/ (reduce + (map #(let [d (- % m)] (* d d)) xs)) (count xs))))))

(defn- iteration-passed?
  [scores i]
  (let [verdicts (keep #(:pass? (nth (:per-case %) i nil)) (vals scores))]
    (when (seq verdicts) (every? true? verdicts))))

(defn- case-passed?
  [per-iter i]
  (let [vs (keep #(iteration-passed? % i) per-iter)]
    (when (seq vs) (every? true? vs))))

(defn- metric-of [e per-iter]
  (let [nm     (:name e)
        scores (mapv #(get % nm) per-iter)
        vals   (keep :value scores)
        mean   (when (seq vals) (/ (reduce + vals) (count vals)))
        latest (peek scores)
        graded (keep :pass? (remove nil? (:per-case latest)))
        n      (get-in latest [:detail :n] (count (remove nil? (:per-case latest))))]
    (cond-> {:name      nm
             :kind      (ev/kind-of e)
             :value     mean
             :variance  (stddev vals)
             :status    (if (and (zero? n) (not= :abstain (ev/kind-of e))) "no-cases" "ungraded")
             :pass-rate (when (seq graded)
                          (double (/ (count (filter true? graded)) (count graded))))
             :detail    (:detail latest)}
      (:objective e) (assoc :objective (:objective e)))))

(defn- case-row [fields actual]
  (let [s (pr-str actual)]
    (if (and (map? actual) (> (count s) 4000) (seq fields))
      (select-keys actual (conj (mapv keyword fields) :error))
      actual)))

(defn- iteration-traces
  [fields per-acts traces i]
  (let [several? (> (count per-acts) 1)]
    (vec (keep (fn [it]
                 (when-let [t (get traces [it i])]
                   (cond-> t
                     several?
                     (assoc :actual (case-row fields (nth (nth per-acts (dec it)) i nil))))))
               (range 1 (inc (count per-acts)))))))

(defn- case-results [suite per-acts per-iter traces ints]
  (let [fields (vec (distinct (keep :field (:evaluators suite))))]
    (vec (map-indexed
          (fn [i {:keys [id input expected]}]
            (let [latest (nth (peek per-acts) i nil)]
              (cond-> {:id       (or id (str i))
                       :input    input
                       :expected expected
                       :actual   (case-row fields latest)
                       :passed   (case-passed? per-iter i)
                       :metrics  (into {} (for [e (:evaluators suite)]
                                            [(:name e)
                                             (mapv #(nth (:per-case (get % (:name e))) i nil)
                                                   per-iter)]))}
                (and (map? latest) (:error latest)) (assoc :error (str (:error latest)))
                (get ints [(count per-acts) i])
                (assoc :intermediates (get ints [(count per-acts) i]))
                (some #(get traces [% i]) (range 1 (inc (count per-acts))))
                (assoc :trace (iteration-traces fields per-acts traces i)))))
          (:cases suite)))))

(defn- write-results-dataset!
  [store schema branch suite run-id ref meta case-rows]
  (when-let [nm (:results-dataset suite)]
    (let [rid   (lookup/rid-of schema :datasets (keyword (name nm)))
          cols  (get-in schema [:datasets rid :columns])
          store (ctx/at store branch)
          now   (ctl/stamp store)
          js    #(when (some? %) (json/write-str % :key-fn name :value-fn (fn [_ v]
                                                                            (if (keyword? v)
                                                                              (name v) v))))]
      (dsets/ensure-datasets! store {rid cols})
      (dsets/write-system-rows!
       store rid :append
       (mapv (fn [{:keys [id input expected actual passed metrics intermediates]}]
               {:id     (str (UUID/randomUUID))
                :values (cond-> {:run_id run-id :suite (str (:name suite)) :ref (str ref)
                                 :case_id (str id) :passed passed :metrics (js metrics)
                                 :input (js input) :expected (js expected)
                                 :actual (js actual) :meta (js meta) :run_at now}
                          (some #(= "intermediates" (name (:attribute-name %))) cols)
                          (assoc :intermediates (js intermediates)))})
             case-rows))
      (count case-rows))))

(defn- models-of
  [schema spec]
  (let [label (fn [prefix m]
                (let [wire (llm-models/provider-model schema m)]
                  (cond-> (str/join ":" (keep #(some-> % name not-empty) [prefix m]))
                    (not= wire m) (str " (" wire ")"))))]
    (into (vec (keep #(when (llm-models/language-model-input? %)
                        (let [in (llm-models/with-source schema %)]
                          (label (:source in) (some-> (:model in) name))))
                     (tio/inputs-of spec)))
          (map #(label nil %))
          (some-> (:source spec) fp/llm-models))))

(defn- llm-backed?
  [schema suite]
  (boolean
   (or (:agent suite)
       (when-let [f (exp/measured-function schema suite)]
         (seq (models-of schema (:spec (functions/function-at-version schema f nil))))))))

(defn- iterations-of [schema suite {:keys [iterations]}]
  (max 1 (or iterations (:iterations suite) (if (llm-backed? schema suite) 3 1))))

(defn- parallel-of [suite {:keys [parallel]}]
  (max 1 (or parallel (:parallel suite) (default-parallel))))

(defn- cases-to-answer
  [schema suite opts]
  (* (count (:cases suite)) (iterations-of schema suite opts)))

(defn- captured
  [schema branch suite {:keys [model version]}]
  (cond
    (:agent suite)
    {:branch branch :agent (name (:agent suite)) :version (or version "published")
     :model (or model (some->> (:agent suite) (agents/agent-of schema) :model))}

    :else
    (let [f (exp/measured-function schema suite)
          e (when f (functions/function-at-version schema f nil))
          v (or (release-version version) (:version e))
          m (when e (first (models-of schema (:spec e))))]
      (cond-> {:branch branch}
        (:action suite) (assoc :action (name (:action suite)))
        f               (assoc :function (name f))
        v               (assoc :version (str v))
        m               (assoc :model m)))))

(defn- note-call! [calls {:keys [kind model prompt-tokens completion-tokens]}]
  (when (and (= :llm/response kind) prompt-tokens)
    (swap! calls conj {:model model :prompt prompt-tokens :completion completion-tokens})))

(defn- usage-of
  [schema calls]
  (let [costs (map #(llm-models/cost schema (:model %) (:prompt %) (:completion %)) calls)]
    (cond-> {:calls             (count calls)
             :prompt-tokens     (reduce + (map :prompt calls))
             :completion-tokens (reduce + (keep :completion calls))}
      (every? some? costs)
      (assoc :cost (reduce + costs)
             :currency (or (some #(:currency (llm-models/rate-of schema (:model %))) calls)
                           "USD")))))

(defn- run-suite*
  [store schema branch suite-id suite
   {:keys [model version meta experiment model-inputs] :as opts}]
  (let [function? (not (:agent suite))
        measured  #(-> % (at-version suite version) (exp/at-model suite model model-inputs))
        schema    (cond-> schema function? measured)
        iters     (iterations-of schema suite opts)
        o         (cond-> (assoc opts :timings (atom []) :traces (atom {}) :intermediates (atom {})
                                 :parallel (parallel-of suite opts))
                    (and function? (:edge opts)) (update-in [:edge :schema] measured))
        calls     (atom [])
        _         (when-let [p (:progress opts)] (p {:phase "cases"}))
        _         (live/emit! {:kind :eval/start :suite (some-> (:name suite) str)
                               :cases (count (:cases suite)) :iterations iters
                               :model model :params (:params experiment)})
        t0        (System/nanoTime)
        per-acts  (binding [live/*listener* (partial note-call! calls)]
                    (vec (for [i (range iters)]
                           (read-actuals store schema branch suite
                                         (assoc o :iteration (inc i))))))
        answer-ms (ms-since t0)
        _         (when-let [p (:progress opts)] (p {:phase "scoring"}))
        _         (live/emit! {:kind :eval/phase :suite (some-> (:name suite) str)
                               :phase "scoring"})
        t1        (System/nanoTime)
        auto      (captured schema branch suite o)
        per-iter  (vec (map-indexed (fn [i acts]
                                      (score-actuals suite acts
                                                     (intermediates-at
                                                      suite @(:intermediates o) (inc i))))
                                    per-acts))
        missing   (unexposed suite @(:intermediates o))
        metrics   (mapv #(cond-> (metric-of % per-iter)
                           (missing (:name %))
                           (assoc :status "no-intermediate"
                                  :detail {:intermediate (name (:intermediate %))
                                           :exposed-by-no-case true}))
                        (:evaluators suite))
        rows      (case-results suite per-acts per-iter @(:traces o) @(:intermediates o))
        verdicts  (map :passed rows)
        n-pass    (count (filter true? verdicts))
        n-fail    (count (filter false? verdicts))
        summary   (cond-> {:by-metric  (into {} (map (juxt :name :value)) metrics)
                           :case-count (count rows)
                           :passed     n-pass
                           :failed     n-fail
                           :ungraded   (count (filter nil? verdicts))
                           :total      (+ n-pass n-fail)
                           :pass-rate  (when (pos? (+ n-pass n-fail))
                                         (double (/ n-pass (+ n-pass n-fail))))
                           :targets-failed (vec (keep #(when (#{"no-cases" "no-intermediate"}
                                                              (:status %))
                                                         (:name %))
                                                      metrics))
                           :timing      (let [ts @(:timings o)]
                                          {:answer-ms answer-ms :scoring-ms (ms-since t1)
                                           :answers (count ts)
                                           :mean-ms (when (seq ts) (quot (reduce + ts) (count ts)))
                                           :max-ms  (when (seq ts) (reduce max ts))})
                           :captured    auto}
                    (seq @calls)            (assoc :usage (usage-of schema @calls))
                    (seq meta)              (assoc :meta meta)
                    experiment              (assoc :experiment experiment)
                    (:skipped-cases suite)  (assoc :skipped-cases (:skipped-cases suite))
                    (or model (:agent suite))
                    (assoc :model (or model (some->> (:agent suite) (agents/agent-of schema)
                                                     :model)))
                    (:agent suite)
                    (assoc :agent-version (or version "published")
                           :cases
                           (vec (map-indexed
                                 (fn [i {:keys [id input]}]
                                   (let [a (nth (peek per-acts) i nil)]
                                     (cond-> {:id (or id (str i)) :input input
                                              :answer
                                              (some-> (:answer a) str
                                                      (#(subs % 0 (min 6000 (count %)))))
                                              :tools (:tools a) :proposed (:proposed a)}
                                       (:error a) (assoc :error (:error a)))))
                                 (:cases suite)))))
        run-id    (str (UUID/randomUUID))
        _         (live/emit! (merge {:kind :eval/end :suite (some-> (:name suite) str)
                                      :run-id run-id :passed n-pass :failed n-fail
                                      :pass-rate (:pass-rate summary)}
                                     (:timing summary)))
        b         branch]
    (ctl/commit! store
                 [(fn [store _]
                    {:records
                     (-> [(ctl/entry store :eval-runs
                                     {:branch b :id run-id :suite-id suite-id :ref branch
                                      :iterations iters :summary summary})]
                         (into (map (fn [{:keys [name value variance status] :as m}]
                                      (ctl/entry store :eval-results
                                                 {:branch b :run-id run-id
                                                  :metric (clojure.core/name name)
                                                  :value value :variance variance :status status
                                                  :detail (select-keys m [:detail :pass-rate
                                                                          :objective :kind])})))
                               metrics)
                         (into (map (fn [{:keys [id input expected actual passed metrics error
                                                 trace intermediates]}]
                                      (ctl/entry store :eval-case-results
                                                 {:branch b :run-id run-id :case-id (str id)
                                                  :passed passed :input input :expected expected
                                                  :actual actual :metrics metrics
                                                  :error error :trace trace
                                                  :intermediates intermediates})))
                               rows))})])
    (let [written (write-results-dataset! store schema branch suite run-id branch
                                          (cond-> (merge {:captured auto} meta)
                                            experiment (assoc :experiment experiment))
                                          rows)]
      (cond-> {:suite (:name suite) :ref branch
               :iterations iters :run-id run-id :cases (count rows)
               :summary summary :metrics metrics :case-results rows}
        written (assoc :results-dataset {:name (name (:results-dataset suite))
                                         :written written})))))

(defn- with-cases [store schema branch suite]
  (let [drawn (mapv #(cases/object-set-cases (ctx/at store branch) schema %) (:cases-from suite))
        all   (into (vec (:cases suite)) (mapcat :cases) drawn)
        skip  (reduce + 0 (map :skipped drawn))]
    (when (empty? all)
      (if (seq drawn)
        (refusal/refuse! :refusal.evals/no-cases-drawn {:suite (:name suite)}
                         {:found (str/join ", " (map #(str (:found %) " object(s), "
                                                           (:skipped %) " without input")
                                                     drawn))})
        (refusal/refuse! :refusal.evals/no-cases {:suite (:name suite)})))
    (cond-> (assoc suite :cases all) (pos? skip) (assoc :skipped-cases skip))))

(defn check!
  ([store schema branch suite-ref] (check! store schema branch suite-ref nil))
  ([store schema branch suite-ref opts]
   (doto (resolve-suite store schema branch suite-ref)
     (->> (#(validate/check! schema % opts))))))

(defn preview
  [store schema branch suite-ref opts]
  (let [suite (with-cases store schema branch (check! store schema branch suite-ref opts))
        e     (or (:experiment opts) (:experiment suite))]
    (exp/preview (or e {:params {}}) (cases-to-answer schema suite opts))))

(defn run-suite!
  [store schema branch suite-ref opts]
  (let [suite    (resolve-suite store schema branch suite-ref)
        _        (validate/check! schema suite opts)
        suite    (with-cases store schema branch suite)
        suite-id (or (some->> (if (map? suite-ref) (:name suite-ref) suite-ref)
                              (suite-by-name store schema branch)
                              :id)
                     (str (UUID/randomUUID)))
        e        (or (:experiment opts) (:experiment suite))]
    (when-let [p (:progress opts)]
      (p {:total (* (if e (count (exp/grid (:params e))) 1)
                    (cases-to-answer schema suite opts))}))
    (let [at   (ctx/at store branch)
          read (cache/once (actions/object-reader at))
          opts (assoc opts :read read)]
      (binding [ev/*judge*         (when (some #(= :llm-judge (ev/kind-of %))
                                               (:evaluators suite))
                                     (judge-fn store schema branch suite))
                ev/*call-function* (fn [fn-name args]
                                     (:rows (actions/run-query-function-logged
                                             at schema fn-name args nil read)))
                ev/*map-io-cases*  (partial in-parallel (parallel-of suite opts))]
        (if-not e
          (run-suite* store schema branch suite-id suite opts)
          (let [exp-id (str (UUID/randomUUID))
                head   {:id exp-id :name (:name e) :params (:params e)}
                runs   (vec (for [combo (exp/grid (:params e))
                                  :let [{s :suite :as at-combo}
                                        (exp/apply-combination schema suite combo)]]
                              (run-suite* store schema branch suite-id s
                                          (merge opts
                                                 (select-keys at-combo
                                                              [:model :version :model-inputs])
                                                 {:experiment {:id exp-id :name (:name e)
                                                               :params combo}}))))]
            {:experiment head
             :runs       runs
             :detail     {:experiment head :run-ids (mapv :run-id runs)}}))))))
