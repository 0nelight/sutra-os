(ns sutra.evals.experiments
  (:require [sutra.kit.coll :as coll]
            [clojure.string :as str]
            [sutra.agents.api :as agents]
            [sutra.catalog.functions :as functions]
            [sutra.catalog.llm-models :as llm-models]
            [sutra.catalog.lookup :as lookup]
            [sutra.kit.refusal :as refusal]
            [sutra.parser.fn-params :as fp]
            [sutra.parser.spec :as spec]
            [sutra.types.transform-io :as tio]))

(set! *warn-on-reflection* true)

(defn measured-function
  [schema suite]
  (cond
    (:function suite) (keyword (name (:function suite)))
    (:action suite)   (some-> (lookup/action schema (keyword (name (:action suite))))
                              spec/function-rule :function name keyword)))

(defn grid
  [params]
  (reduce (fn [combos [k vs]] (for [c combos v vs] (assoc c k v)))
          [{}] (sort-by key params)))

(def ^:private last-saved "last-saved")

(defn- registered-models [schema]
  (vec (sort (map #(if (keyword? %) (name %) (str %)) (keys (:llm-models schema))))))

(defn- measured-entry [schema suite]
  (when-let [f (measured-function schema suite)]
    (functions/function-at-version schema f nil)))

(defn- libraries-of [schema f spec]
  (try (functions/bound-libraries #(:spec (functions/function-by-name schema %)) f spec)
       (catch clojure.lang.ExceptionInfo e
         (println (str "[experiments] libraries of " f " not bound: " (ex-message e)))
         [])))

(defn- body-models
  [schema f spec]
  (fp/llm-models (try (functions/function-code (functions/function-lookup schema) f spec)
                      (catch clojure.lang.ExceptionInfo e
                        (println (str "[experiments] reading " f " from its own source: "
                                      (ex-message e)))
                        (:source spec)))))

(defn- model-inputs [spec]
  (filter (comp llm-models/language-model-input? val) (:inputs spec)))

(defn- action-param [[k p]]
  (cond-> {:name (name k) :kind :input :required (boolean (:required p))}
    (:type p) (assoc :type (:type p))))

(defn- exp-target [schema suite]
  {:name   (measured-function schema suite)
   :entry  (measured-entry schema suite)
   :action (when (:action suite) (lookup/action schema (keyword (name (:action suite)))))})

(defn- input-param [[alias in]]
  (cond-> {:name (name alias) :kind :input
           :required (not (or (tio/optional-input? in) (:optional in)))}
    (:type in) (assoc :type (:type in))))

(defn- function-params
  [schema suite {:keys [spec]} f]
  (let [models   (registered-models schema)
        versions (functions/released-versions spec)
        action   (when (:action suite)
                   (lookup/action schema (keyword (name (:action suite)))))]
    (vec
     (concat
      (when (or (seq (model-inputs spec)) (seq (body-models schema f spec)))
        [{:name "model" :kind :model :options models}])
      (when (seq versions)
        [{:name "version" :kind :version :options (into [last-saved] versions)}])
      (for [[alias in] (model-inputs spec)]
        {:name (name alias) :kind :language-model :options models
         :current (some-> (:model in) name)})
      (if action
        (map action-param (:params action))
        (map input-param (tio/call-inputs spec)))))))

(defn parameters
  [schema suite]
  (cond
    (:agent suite)
    [{:name "model" :kind :model :options (registered-models schema)}
     {:name "version" :kind :version :options ["published" "draft"]}]

    :else
    (let [f (exp-target schema suite)]
      (cond
        (:entry f)   (function-params schema suite (:entry f) (:name f))
        (:action f)  (mapv action-param (:params (:action f)))))))

(defn- open-inputs?
  [schema suite]
  (and (:function suite) (empty? (:inputs (:spec (measured-entry schema suite))))))

(defn- agent-model? [schema suite v]
  (= (str v) (some->> (:agent suite) (agents/agent-of schema) :model str)))

(defn- value-problems [schema suite {:keys [kind options]} k vs]
  (case kind
    (:model :language-model)
    (for [v vs :when (not (or (llm-models/registered? schema v) (agent-model? schema suite v)))]
      (str "experiment parameter " (name k) ": model " (pr-str (str v))
           " is not in the LLM registry — `llm list`"))
    :version
    (for [v vs :when (not (some #{(str v)} options))]
      (str "experiment parameter " (name k) ": no version " (pr-str (str v)) " — it has "
           (str/join ", " options)))
    nil))

(defn- param-problems [schema suite known k vs]
  (let [p (get known (name k))]
    (cond
      (not (and (sequential? vs) (seq vs)))
      [(str "experiment parameter " (name k) " needs a list of values, not " (pr-str vs))]

      p (value-problems schema suite p k vs)

      (#{:model "model"} k)
      [(str "the target names no model an experiment could vary — add a :language-model input "
            "or name one in a use-llm body")]

      (open-inputs? schema suite) nil

      :else
      [(str "experiment parameter " (name k) " is no input of the target — it has "
            (str/join ", " (sort (keys known))))])))

(defn problems
  [schema suite {:keys [params]}]
  (if-not (and (map? params) (seq params))
    [":experiment needs :params {param [value …]}"]
    (let [known (coll/index-by :name (parameters schema suite))]
      (mapcat (fn [[k vs]] (param-problems schema suite known k vs)) params))))

(defn model-problems
  [schema suite model]
  (when model (problems schema suite {:params {:model [model]}})))

(defn- typed
  [in v]
  (if (#{:string "string"} (:type in)) (str v) v))

(defn- with-overrides
  [suite over]
  (let [input-map (:input-map suite)
        moved     (keep #(some-> (get input-map %) name keyword) (keys over))]
    (cond-> (update suite :cases
                    (fn [cs] (mapv #(update % :input
                                            (fn [in] (merge (apply dissoc in moved) over)))
                                   cs)))
      (seq input-map) (assoc :input-map (apply dissoc input-map (keys over))))))

(defn apply-combination
  [schema suite combo]
  (let [spec     (:spec (measured-entry schema suite))
        lm-names (set (map (comp keyword name key) (model-inputs spec)))
        lm       (select-keys combo lm-names)
        inputs   (:inputs spec)
        over     (into {}
                       (map (fn [[k v]] [k (typed (get inputs k) v)]))
                       (apply dissoc combo :model :version lm-names))]
    (cond-> {:suite (if (seq over) (with-overrides suite over) suite)}
      (:model combo)   (assoc :model (str (:model combo)))
      (:version combo) (assoc :version (str (:version combo)))
      (seq lm)         (assoc :model-inputs (update-vals lm str)))))

(defn- with-input-model [in model]
  (-> in (assoc :model model) (dissoc :source :provider-model)))

(defn- spec-at-model
  [spec model model-inputs]
  (let [source (fn [x] (cond-> x (string? (:source x)) (update :source fp/with-llm-model model)))
        input (fn [[alias in]]
                [alias (cond
                         (not (llm-models/language-model-input? in)) in
                         (get model-inputs (keyword (name alias)))
                         (with-input-model in (get model-inputs (keyword (name alias))))
                         model (with-input-model in model)
                         :else in)])
        one   (fn [s]
                (cond-> s
                  (map? (:inputs s)) (update :inputs #(into {} (map input) %))
                  model              source
                  (and model (:libraries s)) (update :libraries (partial mapv source))))]
    (cond-> (one spec)
      (map? (:releases spec)) (update :releases update-vals one))))

(defn- at-spec [schema {:keys [kind rid]} spec]
  (assoc-in schema [(if (= :query kind) :transforms :functions) rid] spec))

(defn at-model
  [schema suite model model-inputs]
  (let [f (when (or model (seq model-inputs)) (measured-function schema suite))
        e (when f (functions/function-at-version schema f nil))]
    (if-not e
      schema
      (let [names  (map :name (libraries-of schema f (:spec e)))
            libs   (keep #(functions/function-by-name schema %) names)
            result (reduce (fn [s {:keys [spec] :as entry}]
                             (at-spec s entry (spec-at-model spec model nil)))
                           (at-spec schema e (spec-at-model (:spec e) model model-inputs))
                           libs)
            left   (when model
                     (remove #{model}
                             (body-models result f
                                          (:spec (functions/function-at-version result f nil)))))]
        (when (seq left)
          (refusal/refuse! :refusal.evals/model-not-literal
                           {:function f :left (vec left)}
                           {:fn (name f) :left (str/join ", " left)}))
        result))))

(defn preview
  [{:keys [name params]} answers-per-run]
  (let [combos (vec (grid params))]
    {:experiment name :params params :combinations combos :runs (count combos)
     :answers (* (count combos) answers-per-run)}))
