(ns sutra.evals.http
  (:require [sutra.kit.refusal :as refusal]
            [clojure.string :as str]
            [sutra.catalog.grants :as grants]
            [sutra.agents.api :as agents]
            [sutra.evals.jobs :as jobs]
            [sutra.evals.suites :as evals]
            [sutra.http.responses :refer [ok]]
            [sutra.access.api :as access]
            [sutra.store.access.scope :as ctx]))

(set! *warn-on-reflection* true)

(def judge-rubric
  (str "Does the ACTUAL answer agree in substance with the EXPECTED answer? Every number, date, "
       "amount and named thing the expected answer gives must be in the actual one and agree; "
       "extra detail is fine; a missing, wrong or invented figure is a failure."))

(defn- evaluators []
  [{:name :richter :kind :llm-judge :field :answer :expected :reference :rubric judge-rubric}
   {:name :enthaelt :kind :contains :field :answer}
   {:name :werkzeuge :kind :tool-use :field :tools}])

(defn suite-name [agent] (str (name agent) "-tests"))

(defn- granted [store schema nm]
  (let [a (agents/agent-of schema nm)]
    (when (and a (or (:builtin a) (access/granted? (grants/read-rights store) schema a))) a)))

(defn- agent-suites [store schema branch]
  (filterv #(get-in % [:spec :agent]) (evals/list-all store schema branch)))

(defn- suite-of [store schema branch agent]
  (some #(when (= (suite-name agent) (:name %)) %) (agent-suites store schema branch)))

(defn- suite-state [store branch suite]
  (when-let [{:keys [status detail]} (jobs/latest store branch suite)]
    (if (= "running" status)
      {:running (vec (sort (:models detail)))}
      {:failed (not-empty (:failures detail))})))

(defn list-tests
  [{store :sutra/store schema :sutra/schema branch :sutra/branch}]
  (ok {:suites (vec (for [s (agent-suites store schema branch)
                          :when (granted store schema (get-in s [:spec :agent]))]
                      (let [st (suite-state store branch (:name s))]
                        {:name    (:name s)
                         :agent   (name (get-in s [:spec :agent]))
                         :cases   (vec (get-in s [:spec :cases]))
                         :runs    (vec (take 20 (evals/results store branch (:id s))))
                         :running (or (:running st) [])
                         :failed  (:failed st)})))}))

(defn add-case
  [{:keys [body-params] store :sutra/store schema :sutra/schema branch :sutra/branch}]
  (let [{:keys [agent input reference must-contain thread run history]} body-params
        history (vec (for [{:keys [role content]} history
                           :when (and (#{"user" "assistant"} (str role))
                                      (not (str/blank? (str content))))]
                       {:role (str role) :content (str content)}))
        must (vec (remove str/blank? (map (comp str/trim str) must-contain)))]
    (cond
      (not (granted store schema
                    agent))  (refusal/refused :refusal.evals/no-such-agent {:agent agent} {:agent agent})
      (str/blank? (str input))         (refusal/refused :refusal.evals/a-test-case-needs-the {})
      (and (str/blank? (str reference)) (empty? must))
      (refusal/refused :refusal.evals/a-test-case-needs-the-2 {})
      :else
      (let [existing (suite-of store schema branch agent)
            spec     (or (:spec existing)
                         {:name (suite-name agent) :agent (keyword (name agent)) :cases []
                          :evaluators (evaluators)})
            kase     (cond-> {:id (str (random-uuid))
                              :input (if (seq history) {:question (str input) :history history}
                                       (str input))
                              :expected (cond-> {}
                                          (not (str/blank? (str reference)))
                                            (assoc :reference (str reference))
                                          (seq must)                          (assoc :answer must))}
                       (or thread
                           run) (assoc :from
                                       (cond-> {} thread (assoc :thread (str thread))
                                         run (assoc :run (str run)))))]
        (evals/upsert! store schema branch
                        {:name (suite-name agent) :spec (update spec :cases (fnil conj []) kase)})
        (ok {:suite (suite-name agent) :case kase})))))

(defn delete-case
  [{:keys [path-params] store :sutra/store schema :sutra/schema branch :sutra/branch}]
  (let [{:keys [suite] case-id :case} path-params
        s (some #(when (= suite (:name %)) %) (agent-suites store schema branch))]
    (if-not (and s (granted store schema (get-in s [:spec :agent])))
      (refusal/refused :refusal.evals/no-test-suite {:suite suite} {:suite suite})
      (do (evals/upsert! store schema branch
                          {:name suite :spec (update (:spec s) :cases
                                                     (fn [cs]
                                                       (vec (remove #(= case-id (:id %)) cs))))})
          (ok {:deleted case-id})))))

(defn run-tests
  [{:keys [path-params body-params] store :sutra/store schema :sutra/schema branch :sutra/branch}]
  (let [suite  (:suite path-params)
        s      (some #(when (= suite (:name %)) %) (agent-suites store schema branch))
        agent  (when s (granted store schema (get-in s [:spec :agent])))
        models (vec (distinct (remove str/blank?
                                      (map str (or (seq (:models body-params)) [(:model agent)])))))
        busy?  (when agent (seq (:running (suite-state store branch suite))))]
    (cond
      (nil? agent)                     (refusal/refused :refusal.evals/no-test-suite {:suite suite} {:suite suite})
      (empty? (get-in s
                      [:spec :cases])) (refusal/refused :refusal.evals/the-suite-has-no-test {:suite suite})
      busy?                            (refusal/refused :refusal.evals/this-suite-is-running-already {:suite suite})
      :else
      (do (doseq [m models] (agents/with-model schema agent m))
          (jobs/start!
           store branch suite {:models models}
           (fn [_]
             {:detail
              {:failures
               (into {}
                     (keep (fn [m]
                             (try (evals/run-suite!
                                   (ctx/at store branch) schema branch suite
                                   {:model m :iterations 1
                                    :version (not-empty (str (:version body-params)))})
                                  nil
                                  (catch InterruptedException e (throw e))
                                  (catch Exception e
                                    [m (or (ex-message e) (str (class e)))]))))
                     models)}}))
          (ok {:suite suite :started models})))))

(def module
  {:name        :agent-evals
   :contributes {:http/routes {:agent-tests      {:get list-tests}
                               :agent-test-cases {:post add-case}
                               :agent-test-case  {:delete delete-case}
                               :agent-test-run   {:post run-tests}}}})
