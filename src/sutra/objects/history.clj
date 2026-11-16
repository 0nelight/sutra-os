(ns sutra.objects.history
  (:require [sutra.access.api :as access]
            [sutra.llm.api :as llm]
            [sutra.store.access.substrate :as substrate]
            [sutra.store.world :as world]))

(set! *warn-on-reflection* true)

(defn- at-branch [store] (world/at store (or (:branch store) "main")))

(defn- named
  [store es]
  (let [names (access/usernames-by-object-id store)]
    (mapv (fn [{:keys [edited-by] :as e}]
            (cond-> e (get names edited-by) (assoc :edited-by-name (get names edited-by))))
          es)))

(defn page
  [store T {:keys [authority sort] :as opts}]
  (-> (world/edit-history-page (at-branch store) T
                               (assoc opts
                                      :authority (substrate/require-read-authority authority store)
                                      :oldest-first? (= "asc" (some-> sort name))))
      (update :history (partial named store))))

(defn- run-of
  [store schema {:keys [agent run]}]
  (let [calls (llm/run-calls store schema run)]
    (when (seq calls)
      {:run        run
       :agent      agent
       :calls      (count calls)
       :started-at (:ts (first calls))
       :ended-at   (:ts (peek calls))
       :tokens     (reduce + 0 (mapcat (juxt #(or (:prompt-tokens %) 0)
                                             #(or (:completion-tokens %) 0))
                                       calls))})))

(defn timeline
  [store schema T {:keys [pk authority]}]
  (let [es (named store
                  (world/edit-history (at-branch store) T
                                      {:pk pk :limit Integer/MAX_VALUE
                                       :authority (substrate/require-read-authority
                                                   authority store)}))]
    {:edits (vec (rseq es))
     :runs  (into [] (keep (partial run-of store schema))
                  (distinct (for [{:keys [agent agent-run]} es :when agent-run]
                              {:agent agent :run agent-run})))}))

(defn row
  [e]
  (cond-> {:pk             (:pk e)
           :updated_at     (:edited-at e)
           :edit_kind      (:edit-kind e)
           :edited_by      (:edited-by e)
           :values         (:edited-values e)
           :previous       (:previous-values e)}
    (:edited-by-name e) (assoc :edited_by_name (:edited-by-name e))
    (:action e)         (assoc :action (:action e))))
