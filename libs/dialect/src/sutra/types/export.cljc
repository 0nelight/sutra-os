(ns sutra.types.export
  (:require [sutra.types.table :as tbl]))

(def modes
  {:mirror
   {:truncate :on-snapshot :rows :unexported :refuses #{"UPDATE" "DELETE"}
    :recommended? true
    :summary "mirror the dataset: unexported transactions appended, the destination truncated when the dataset was snapshot"}

   :full
   {:truncate nil :rows :all :refuses #{}
    :summary
      "the whole view appended, without truncating — almost always duplicates in the destination"}

   :full-truncate
   {:truncate :always :rows :all :refuses #{}
    :summary "truncate the destination, then write the whole view"}

   :incremental
   {:truncate nil :rows :unexported :refuses #{"UPDATE" "DELETE"}
    :summary "only unexported transactions, without truncating"}

   :incremental-truncate
   {:truncate :always :rows :unexported :refuses #{"UPDATE" "DELETE"}
    :summary "truncate the destination, then write only unexported transactions — the destination as a queue"}

   :incremental-strict
   {:truncate nil :rows :unexported :refuses #{"SNAPSHOT" "UPDATE" "DELETE"}
    :summary "only unexported transactions, failing on any transaction that is not APPEND — never exports a row twice"}})

(def default-mode :mirror)

(defn mode-row
  [mode]
  (get modes mode))

(defn unknown-mode
  [mode]
  (tbl/unknown :export/unknown-mode "export mode" modes mode))

(def truncating-modes (tbl/where modes (comp #{:always} :truncate)))
(def incremental-modes (tbl/where modes (comp #{:unexported} :rows)))

(defn truncates?
  [mode snapshot?]
  (case (:truncate (mode-row mode))
    :always      true
    :on-snapshot (boolean snapshot?)
    false))

(defn refused-transaction
  [mode types first-run?]
  (let [refuses (:refuses (mode-row mode))]
    (first (filter (fn [t] (and (contains? refuses t)
                                (not (and first-run? (= "SNAPSHOT" t)))))
                   types))))

(tbl/holds! "every mode says what it truncates and which rows it writes"
            (for [[k {:keys [rows truncate]}] modes
                  :when (not (and (#{:all :unexported} rows)
                                  (contains? #{:always :on-snapshot nil} truncate)))]
              k))

(tbl/holds!
 "only an incremental mode refuses a transaction type — a full export rewrites whatever it finds"
 (for [[k {:keys [rows refuses]}] modes
       :when (and (seq refuses) (not= :unexported rows))]
   k))

(def Destination
  [:map {:closed true}
   [:table :string]
   [:schema {:optional true} :string]
   [:database {:optional true} :string]])

(def Mode
  (into [:enum] (sort (keys modes))))
