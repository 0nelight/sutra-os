(ns sutra.cli.tree
  (:require [clojure.string :as str]
            [sutra.cli.table :as table]
            [sutra.types.endpoint-verbs :as ev]))

(def sections
  [{:key :orient    :label "ORIENT — what exists · where am I · what does this do"}
   {:key :ontology  :label "ONTOLOGY — author the TYPE layer"}
   {:key :data      :label "DATA — the INSTANCE plane: objects, links, behaviour"}
   {:key :pipeline  :label "PIPELINE — transforms, datasets, builds"}
   {:key :graph     :label "GRAPH — saved canvas graphs as named resources"}
   {:key :catalog   :label "CATALOG — substrate ① DATA: typed transactions, deltas, rollback"}
   {:key :code      :label "CODE — substrate ② LOGIC: a resource's code, git-shaped over the store"}
   {:key :versions
    :label "VERSIONS — substrate ③ RESOURCE: one resource's saved versions — read-at, restore"}
   {:key :changeset
    :label "CHANGESET — DRAFT→PUBLISH across all three: stage · preview · rebase · commit"}
   {:key :branch
    :label "BRANCH — the UMBRELLA over all three: one branch + one proposal spanning data, logic & resources"}
   {:key :source    :label "SOURCE — external systems & the data they sync"}
   {:key :project   :label "PROJECT — organize, share & package"}
   {:key :operate   :label "OPERATE — cadence: schedules, automations, health"}
   {:key :llm       :label "LLM — models, spend, evals, audit"}
   {:key :access    :label "ACCESS — identity & who-can-do-what"}
   {:key :debug     :label "DEBUG — diagnostics"}
   {:key :repl      :label "REPL — session-only, never touches the server"}])

(def command-tree
  (-> []
      (into (for [{:keys [verb status leaves]} table/verbs
                  leaf leaves]
              (cond-> (assoc leaf :verb verb) status (assoc :status status))))
      (into (map #(assoc % :status :planned)) table/planned)
      (into ev/cli-leaves)))

(def leaf-index
  (merge (into {} (for [l command-tree
                        a (:aliases l)
                        :when (not= a (some-> (:verb l) name))]
                    [[a] l]))
         (into {} (map (juxt :path identity)) command-tree)))

(def max-path-len
  (transduce (map (comp count :path)) (completing max) 1 command-tree))

(def namespace-roots
  (into #{} (comp (filter #(> (count (:path %)) 1)) (map (comp first :path))) command-tree))

(defn match
  [toks]
  (loop [n (min (count toks) max-path-len)]
    (when (pos? n)
      (if-let [leaf (leaf-index (vec (take n toks)))]
        {:leaf leaf :remaining (vec (drop n toks))}
        (recur (dec n))))))

(defn subcommands
  [root]
  (into (sorted-set)
        (comp (filter #(and (> (count (:path %)) 1) (= root (first (:path %)))))
              (map #(second (:path %))))
        command-tree))

(defn leaf-for-verb
  [verb]
  (first (filter #(= verb (:verb %)) command-tree)))

(defn names-for-verb
  [verb]
  (let [ls (filter #(= verb (:verb %)) command-tree)]
    {:paths   (vec (distinct (map #(str/join " " (:path %)) ls)))
     :aliases (vec (distinct (mapcat :aliases ls)))}))

(defn status [leaf] (:status leaf :live))
