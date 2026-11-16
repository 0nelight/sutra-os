(ns sutra.parser.lint
  (:require [sutra.kit.tree :as tree]
            [sutra.types.finding :as finding]
            [sutra.parser.lint.naming :as naming]
            [sutra.parser.lint.pipeline :as pipeline]
            [sutra.parser.lint.apps :as apps]
            [sutra.parser.lint.backing :as backing]
            [sutra.parser.lint.actions :as actions]
            [sutra.parser.lint.references :as references]
            [sutra.parser.lint.ooux :as ooux]))

(set! *warn-on-reflection* true)

(def ooux-rules
  #{:title-is-identifier :object-type-unnamed :object-type-undescribed :object-type-isolated
    :attributes-unprioritized
    :per-period-type :status-lookup-type :type-name-collision :link-side-names :lifecycle})

(def rules
  (into [] cat [naming/rules pipeline/rules apps/rules backing/rules actions/rules
                references/rules ooux/rules]))

(defn- actions-by-name
  [schema]
  (cond-> schema
    (map? (:actions schema))
    (update :actions #(into {} (for [[k a] %] [(or (:action-name a) k) a])))))

(defn- run-rules
  [rules schema* vocab]
  (binding [tree/*nodes-memo* (atom {})]
    (let [schema (actions-by-name schema*)]
      (->> rules
           (mapcat (fn [{:keys [id severity doc scan vocab? gate?]}]
                     (try
                       (mapv #(finding/ranked (merge {:doc doc} % {:rule id}) severity)
                             (if vocab? (scan schema vocab) (scan schema)))
                       (catch InterruptedException e
                         (.interrupt (Thread/currentThread))
                         (throw e))
                       (catch Exception e
                         [{:rule id :entity (name id) :entity-class :lint
                           :severity (if gate? :error :warn)
                           :message (str "Lint rule " id " crashed and judged nothing: "
                                         (or (ex-message e) (.getName (class e))))}]))))
           (into [])
           finding/worst-first))))

(defn run-lint
  [schema vocab]
  (run-rules rules schema vocab))

(defn advisory?
  [{:keys [severity rule]}]
  (or (= :warn severity) (and (= :error severity) (contains? ooux-rules rule))))

(def advisory-rules
  (into #{} (keep (fn [{:keys [id severity]}]
                    (when (or (= :warn severity)
                              (and (= :error severity) (contains? ooux-rules id))
                              (#{:app-source :source-config} id))
                      id)))
        rules))

(defn advisories
  [schema vocab]
  (filterv advisory? (run-rules (filter (comp advisory-rules :id) rules) schema vocab)))

(defn rule-findings
  [id schema vocab]
  (run-rules (filter #(= id (:id %)) rules) schema vocab))

(def gated-rules
  (into #{} (keep #(when (:gate? %) (:id %))) rules))

(defn gate-violations
  [schema vocab]
  (let [gated gated-rules]
    (into [] (filter (comp gated :rule))
          (run-rules (filter (comp gated :id) rules) schema vocab))))

(defn value-gate-violations
  [schema vocab]
  (let [change (into #{} (keep #(when (= :change (:scope %)) (:id %))) rules)
        gated  (into #{} (remove change) gated-rules)]
    (into [] (filter (comp gated :rule))
          (run-rules (filter (comp gated :id) rules) schema vocab))))

(defn summary
  [findings]
  (let [by (frequencies (map :severity findings))]
    (assoc (select-keys by (keys finding/severities))
           :total (count findings))))
