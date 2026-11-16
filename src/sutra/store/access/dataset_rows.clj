(ns sutra.store.access.dataset-rows
  (:require [sutra.catalog.system-datasets :as sysds]
            [sutra.foundation.encoding :as enc]
            [sutra.types.virtual :as vtypes]))

(set! *warn-on-reflection* true)

(defn write-problem
  [rid ds nm rows mode key-col]
  (let [allowed (set (map :attribute-name (:columns ds)))
        unknown (distinct (for [r rows, k (keys (:values r)) :when (not (contains? allowed k))] k))]
    (cond
      (and (not (seq rows)) (not= mode :snapshot))
      {:message "no :rows to write (append needs rows; an empty snapshot is a truncate)"
       :data {:name nm :mode mode}}

      (seq unknown)
      {:message "unknown column(s) for dataset"
       :data {:name nm :unknown (vec unknown) :declared (vec allowed)}}

      (and key-col (not (contains? allowed key-col)))
      {:message "unknown :key column for dataset"
       :data {:name nm :key key-col :declared (vec allowed)}}

      (and key-col (some #(nil? (get (:values %) key-col)) rows))
      {:message "row(s) with a null :key value — a key that is null cannot identify a row"
       :data {:name nm :key key-col}}

      (vtypes/virtual? ds)
      {:message (str "dataset " nm " is virtual: it holds no rows, its source "
                     (name (vtypes/source-name ds)) " does — write the source table"
                     " through a :writeback action")
       :data {:name nm :kind :virtual :source (vtypes/source-name ds)}}

      (sysds/control-resident? rid)
      {:message (str "dataset is framework-owned and control-plane resident — it cannot be "
                     "written through the generic dataset endpoint")
       :data {:name nm :store :control}})))

(defn records
  [nm key-col rows]
  (mapv (fn [r] {:id     (or (:id r)
                             (when key-col (enc/pk-uuid nm (get (:values r) key-col)))
                             (str (java.util.UUID/randomUUID)))
                 :values (:values r)})
        rows))
