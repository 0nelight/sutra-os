(ns sutra.evals.validate
  (:require [sutra.types.plan :as cp]
            [clojure.string :as str]
            [sutra.agents.api :as agents]
            [sutra.evals.cases :as cases]
            [sutra.evals.evaluators :as ev]
            [sutra.evals.experiments :as exp]
            [sutra.catalog.functions :as functions]
            [sutra.catalog.lookup :as lookup]
            [sutra.types.link-side :as ls]))

(set! *warn-on-reflection* true)

(def results-columns
  [:run_id :suite :ref :case_id :passed :metrics :input :expected :actual :meta :run_at])

(def agent-columns #{:answer :tools :proposed :sources :turns :error})

(def action-columns #{:plan :error})

(defn targets [suite] (filterv #(some? (get suite %)) [:function :agent :action]))

(defn- object-columns [schema ot]
  (when-let [rid (lookup/rid-of schema :object-types (keyword (name ot)))]
    (into (set (keys (lookup/attr-type-map schema rid)))
          (map first) (:derived (lookup/attrs-index schema rid)))))

(defn- dataset-columns [schema rid]
  (some->> (get-in schema [:datasets rid :columns]) (map (comp keyword name :attribute-name))
           set))

(defn target-columns
  [{:keys [agent action]}]
  (cond agent  agent-columns
        action action-columns))

(defn- squash [k] (str/replace (str/lower-case (name k)) #"[-_\s]" ""))

(defn- did-you-mean [k known]
  (let [hit (or (some #(when (= (squash k) (squash %)) %) known)
                (first (sort-by #(ev/levenshtein (name k) (name %))
                                (filter #(<= (ev/levenshtein (name k) (name %)) 2) known))))]
    (if hit
      (str " — did you mean " (pr-str (name hit)) "?")
      (str " — it has " (str/join ", " (map name (sort-by name known)))))))

(defn- unknown-column [what k known]
  (when (and (some? k) known (not (contains? known (keyword (name k)))))
    (str what " " (pr-str (name k)) " is no column of the target" (did-you-mean k known))))

(defn- objective-problems [{:keys [name objective] :as e}]
  (concat
   (when (and objective (or (not (map? objective)) (contains? objective :op)))
     [(str "evaluator " name ": :objective is per test case — {:expect true}, {:min n} or "
           "{:max n}; a threshold on the whole metric, like "
           (pr-str objective) ", does not exist")])
   (when (and objective (map? objective)
              (seq (remove #{:expect :min :max} (keys objective))))
     [(str "evaluator " name ": :objective takes only :expect, :min, :max")])
   (when (contains? e :target)
     [(str "evaluator " name ": :target is removed — a threshold on the metric's mean has "
           "no counterpart in Evals; set the objective per test case in :objective")])))

(defn- intermediate-problems [suite {:keys [name intermediate]}]
  (when (some? intermediate)
    (cond
      (not (or (string? intermediate) (keyword? intermediate)))
      [(str "evaluator " name ": :intermediate names a parameter — a string, not "
            (pr-str intermediate))]
      (not (:function suite))
      [(str "evaluator " name ": :intermediate reads a parameter a Function exposes "
            "(intermediate(name, value) in its code) — only a :function target has them")])))

(defn- evaluator-problems [cols {:keys [name field] :as e}]
  (let [kind (ev/kind-of e)
        spec (get ev/catalog kind)]
    (cond
      (nil? name) [(str "an evaluator needs a :name: " (pr-str e))]
      (nil? spec) [(str "evaluator " name ": unknown :kind " (pr-str (:kind e)) " — one of "
                        (str/join ", " (map clojure.core/name (sort (keys ev/catalog)))))]
      :else
      (concat
       (for [k (:needs spec) :when (nil? (get e k))]
         (str "evaluator " name " (" (clojure.core/name kind) ") needs " k))
       (when (and (:field? spec) cols (nil? field) (not= :function kind)
                 (nil? (:intermediate e)))
         [(str "evaluator " name " (" (clojure.core/name kind) ") needs :field — which "
               "column of the actual it reads")])
       (when (:field? spec)
         (keep identity [(unknown-column (str "evaluator " name " :field") field cols)]))
       (objective-problems e)))))

(defn- property-problems [schema ot src]
  (let [known (object-columns schema ot)
        k     (keyword (name src))]
    (when (and known (not= :id k) (not (contains? known k)))
      [(str ":cases-from " (name ot) ": no property " (pr-str (name src))
            (did-you-mean k known))])))

(defn- link-problems [schema ot {:keys [link property]}]
  (let [rid (lookup/rid-of schema :object-types (keyword (name ot)))]
    (if-let [side (try (ls/resolve-side schema rid link)
                       (catch clojure.lang.ExceptionInfo _ nil))]
      (when property
        (property-problems schema (or (lookup/name-of schema :object-types (:far-type side))
                                      (:far-type side))
                           property))
      [(str ":cases-from " (name ot) ": no link " (pr-str link) " — `describe " (name ot)
            "` lists its links")])))

(defn- cases-from-problems [schema {:keys [object-type input expected id] :as from}]
  (cond
    (nil? object-type) [(str ":cases-from needs :object-type: " (pr-str from))]
    (nil? (lookup/rid-of schema :object-types (keyword (name object-type))))
    [(str ":cases-from: no object type " (pr-str (name object-type)))]
    :else
    (mapcat (fn [src]
              (cond (and (map? src) (contains? src :val)) nil
                    (cases/link-source? src) (link-problems schema object-type src)
                    :else                    (property-problems schema object-type src)))
            (concat (vals input) (vals expected) (when id [id])))))

(defn- function-problems [schema {:keys [function cases input-map]}]
  (let [{:keys [kind spec]} (functions/function-at-version schema (keyword (name function)) nil)]
    (cond
      (nil? kind)     [(str "no function " (pr-str (name function)) " — `function list`")]
      (not= :query kind)
      [(str "function " (name function) " is an " (name kind) " function; a suite tests a "
            "query function (an action's edits: target :action)")]
      :else
      (let [declared (set (map (comp keyword name) (keys (:inputs spec))))
            given    (set (mapcat (comp keys #(cases/mapped-input input-map %) :input) cases))]
        (for [k given :when (and (seq declared) (not (contains? declared k)))]
          (str "case input " (pr-str (name k)) " is no input of function " (name function)
               (did-you-mean k declared) " (map it with :input-map {input column})"))))))

(defn- results-dataset-problems [schema nm]
  (when nm
    (if-let [rid (lookup/rid-of schema :datasets (keyword (name nm)))]
      (let [have (dataset-columns schema rid)
            miss (remove have results-columns)]
        (when (seq miss)
          [(str "results dataset " (name nm) " lacks the columns "
                (str/join ", " (map name miss)) " — declare them (passed :boolean, the rest "
                ":string)")]))
      [(str "no results dataset " (pr-str (name nm)) " — declare it under :datasets first")])))

(defn problems
  ([schema suite] (problems schema suite nil))
  ([schema {:keys [name agent action evaluators cases-from] :as suite}
    {:keys [model experiment]}]
   (let [ts   (targets suite)
         cols (target-columns suite)]
     (vec
      (concat
       (when (nil? name) ["a suite needs a :name"])
       (when (contains? suite :transform)
         [(str "a transform as eval target is removed — Evals measures Functions: measure "
               "a Function that shares the transform's core module")])
       (when (some #(contains? suite %) [:match :execute? :build?])
         [":match, :execute? and :build? belonged to the removed transform target"])
       (case (count ts)
         1 nil
         0 ["a suite tests one target: :function, :agent or :action"]
         [(str "a suite tests one target, not " (str/join " and " (map clojure.core/name ts))
               " — add a second suite (or compare implementations as an experiment)")])
       (when (and agent (nil? (agents/agent-of schema agent)))
         [(str "no agent " (pr-str (clojure.core/name agent)))])
       (when (and action (nil? (or (get-in schema [:actions (keyword (clojure.core/name action))])
                                   (lookup/rid-of schema :actions
                                                  (keyword (clojure.core/name action))))))
         [(str "no action " (pr-str (clojure.core/name action)))])
       (when (:function suite) (function-problems schema suite))
       (when (empty? evaluators)
         ["a suite needs :evaluators — without one nothing is measured"])
       (let [dups (for [[n c] (frequencies (map :name evaluators)) :when (> c 1)] n)]
         (when (seq dups) [(str "evaluator names repeat: " (str/join ", " dups))]))
       (mapcat #(evaluator-problems cols %) evaluators)
       (mapcat #(intermediate-problems suite %) evaluators)
       (mapcat #(cases-from-problems schema %) cases-from)
       (results-dataset-problems schema (:results-dataset suite))
       (when (= 1 (count ts))
         (concat (exp/model-problems schema suite model)
                 (when-let [e (or experiment (:experiment suite))]
                   (exp/problems schema suite e)))))))))

(defn check!
  ([schema suite] (check! schema suite nil))
  ([schema suite opts]
   (cp/check! (cp/of-findings (cp/problems :evals/suite (problems schema suite opts)))
              {:code   :refusal.evals/suite-cannot-run
               :data   {:suite (:name suite)}
               :params (fn [bs] {:lines (str/join "\n  - " (map :message bs))})})
   nil))
