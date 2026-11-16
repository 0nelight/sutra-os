(ns sutra.builds.run.exports
  (:require [sutra.kit.refusal :as refusal]
            [clojure.string :as str]
            [sutra.store.control :as ctl]
            [sutra.catalog.grants :as grants]
            [sutra.builds.ledger :as ledger]
            [sutra.builds.ports :as ports]
            [sutra.store.access.datasets :as dsets]
            [sutra.catalog.transforms :as transforms]
            [sutra.types.column :as column]
            [sutra.types.export :as export]
            [sutra.types.transform-io :as tio]))

(set! *warn-on-reflection* true)

(defn export-of
  [t]
  (some :export (tio/outputs-of t)))

(defn export-nodes
  [schema in-scope?]
  (for [t-kw (apply concat (transforms/topo-levels schema))
        :let [exp (some-> (get-in schema [:transforms t-kw]) export-of)]
        :when (and exp (in-scope? t-kw))]
    {:kind :export :key t-kw :t-kw t-kw :spec {:export exp}}))

(defn- unexported-transactions
  [store ds branch last-txn-rid]
  (let [txns (ledger/list-transactions store ds branch 500)]
    (if (nil? last-txn-rid)
      txns
      (vec (take-while #(not= (str last-txn-rid) (str (:txn_rid %))) txns)))))

(defn- destination-row
  [columns at row]
  (into {} (for [{:keys [attribute-name]} columns
                 :let [col (column/attr->col attribute-name)]]
             [col (at row col)])))

(defn- kebab [col] (keyword (str/replace (name col) "_" "-")))

(defn- export-rows
  [store schema exp-rid dataset branch whole?]
  (let [columns (get-in schema [:datasets dataset :columns])]
    (if whole?
      (mapv #(destination-row columns (fn [r c] (get (:values r) (kebab c))) %)
            (dsets/rows store dataset))
      (let [rows (ledger/added-changed store exp-rid dataset branch)]
        (if (= :bootstrap rows)
          (mapv #(destination-row columns (fn [r c] (get (:values r) (kebab c))) %)
                (dsets/rows store dataset))
          (mapv #(destination-row columns (fn [r c] (get r c)) %) rows))))))

(defn plan-export
  [{:keys [mode]} {:keys [first-run? types]}]
  (let [mode (or mode export/default-mode)]
    (when-not (export/mode-row mode)
      (let [u (export/unknown-mode mode)]
        (refusal/refuse! :refusal.builds/export-unknown-mode (dissoc u :message)
                         {:mode mode :modes (vec (sort (:known u)))})))
    (if-let [bad (export/refused-transaction mode types first-run?)]
      {:mode mode :refused bad}
      (let [rows (if (or first-run? (= :all (:rows (export/mode-row mode)))) :all :unexported)]
        {:mode         mode
         :truncate?    (export/truncates? mode (or first-run? (some #{"SNAPSHOT"} types)))
         :rows         rows
         :nothing-new? (and (empty? types) (= :unexported rows))}))))

(defn export-misconfiguration
  [nm source src table]
  (cond
    (nil? src) [:refusal.builds/export-source-missing {:export nm :source source}]
    (str/blank? (str table)) [:refusal.builds/export-no-table {:export nm}]))

(defn transaction-refusal
  [nm dataset {:keys [mode refused]}]
  [:refusal.builds/export-transaction-unsupported
   {:export nm :mode mode :transaction refused}
   {:name (name nm) :dataset (name dataset)}])

(defn run-export!
  [store schema branch exp-rid]
  (let [{:keys [dataset source destination] :as spec} (get-in schema [:exports exp-rid])
        nm      (or (:export-name spec) exp-rid)
        src     (get-in schema [:sources source])
        ds-spec (get-in schema [:datasets dataset])
        table   (:table destination)]
    (when-let [[code data] (export-misconfiguration nm source src table)]
      (refusal/refuse! code data))
    (let [pointer    (ledger/consumer-pointer store exp-rid dataset branch)
          first-run? (nil? (:last_txn_rid pointer))
          txns       (unexported-transactions store dataset branch (:last_txn_rid pointer))
          types      (mapv #(str/upper-case (str (:type %))) txns)
          plan       (plan-export spec {:first-run? first-run? :types types})]
      (cond
        (:refused plan)
        (let [[code data params] (transaction-refusal nm dataset plan)]
          (refusal/refuse! code data params))

        (:nothing-new? plan)
        {:written 0 :truncated? false :skipped :nothing-new}

        :else
        (let [rows (export-rows store schema exp-rid dataset branch (= :all (:rows plan)))
              out  ((ports/foreign-export-rows store)
                    (grants/source-authority store) src
                    {:table table :rows rows :truncate? (:truncate? plan)
                     :markings (:markings ds-spec)})]
          (ledger/advance-pointer!
           store {:consumer-rid exp-rid :input-dataset-rid dataset :branch branch
                  :last-txn-rid (some-> (first txns) :txn_rid)
                  :last-snapshot-id (str (dsets/current-version store))
                  :updated-at (ctl/stamp store)})
          (assoc out :mode (:mode plan)))))))

(defn export-job
  [{:keys [ctx schema build-branch]} _ {_t-kw :key {:keys [export]} :spec}]
  (let [{:keys [store]} ctx
        nm  (or (get-in schema [:exports export :export-name]) export)
        res (run-export! store schema (str (or build-branch "main")) export)]
    (println (format "  → export %s: %s"
                     (name nm)
                     (if (:skipped res)
                       "nichts Neues"
                       (str (:written res) " Zeilen"
                            (when (:truncated? res) ", Ziel vorher geleert")))))
    {:facts []}))
