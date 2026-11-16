(ns sutra.builds.plan.expectations
  (:require [clojure.string :as str]
            [sutra.builds.plan.inputs :as inputs]
            [sutra.builds.ports :as ports]
            [sutra.foundation.ops :as op]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.access.row-shape :as row-shape]
            [sutra.store.control :as ctl]
            [sutra.types.expectations :as xp]
            [sutra.types.naming :as naming]
            [sutra.types.opid :as opid]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]))

(set! *warn-on-reflection* true)

(defn check-id
  [t-name check-name]
  (opid/name-id :health-check (str "expectation/" (name t-name) "/" check-name)))

(defn- status [{:keys [status on-error]}]
  (cond (= :passed status) "passed" (= :warn on-error) "warning" :else "failed"))

(defn- record!
  [store branch t-name scope {:keys [expectation] :as c} r]
  (let [id (check-id t-name (:name c))]
    (op/run! {:store store} :health/record-expectation
             {:branch branch
              :check  {:id id :name (:name c) :node-key (name t-name)
                       :expects {:severity    (if (= :warn (:on-error r)) :moderate :high)
                                 :on-error    (:on-error r)
                                 :scope       (mapv #(if (keyword? %) (name %) (str %)) scope)
                                 :expectation (pr-str expectation)}}
              :result {:status (status r) :value (str (:failing r)) :detail (:detail r)}})
    (ports/health-result-recorded! store branch id)))

(defn- ref-fn
  [{:keys [row-cache] :as ctx} t-name t]
  (fn [alias]
    (let [in (get (:inputs t) alias)]
      (inputs/memo! row-cache [::whole t-name alias] #(inputs/whole-input-rows ctx in)))))

(defn- scope-label [[side nm]] (str (name side) " " (some-> nm name)))

(defn verify!
  [{:keys [store schema build-branch] :as ctx} t-ref t scope checks rows]
  (when (seq checks)
    (let [t-name  (or (rid/resolve-rid->name (sch/rid-name-index schema) t-ref) t-ref)
          env     {:now (ctl/instant store) :ref (ref-fn ctx t-name t)}
          branch  (or build-branch (:branch store) ctl/trunk)
          results (mapv (fn [c] [c (xp/evaluate c rows env)]) checks)
          failed  (filterv (fn [[_ r]] (and (= :failed (:status r)) (= :fail (:on-error r))))
                           results)]
      (doseq [[c r] results]
        (record! store branch t-name scope c r)
        (when (and (= :failed (:status r)) (= :warn (:on-error r)))
          (println (format "  ⚠ expectation WARN %s · %s · %s: %s" (name t-name)
                           (scope-label scope) (:name c) (:detail r)))))
      (when (seq failed)
        (throw (ex-info
                (str "data expectation(s) FAILED on " (name t-name) " " (scope-label scope)
                     " — the job is aborted, its outputs stay as they were:\n"
                     (str/join "\n" (for [[c r] failed]
                                      (str "· " (:name c) " " (pr-str (:expectation c))
                                           " — " (:detail r)))))
                {:transform t-name :scope scope
                 :expectation-failures (mapv (fn [[c r]] (assoc r :name (:name c))) failed)})))
      (mapv second results))))

(defn- transform-of [schema t-name]
  (when t-name (second (inputs/transform-entry schema t-name))))

(defn pre-conditions!
  [{:keys [schema row-cache] :as ctx} t-kw]
  (let [t (get-in schema [:transforms t-kw])]
    (when-let [by-alias (not-empty (xp/input-checks t))]
      (inputs/memo! row-cache [::pre t-kw]
                    (fn []
                      (let [t-name (or (:transform-name t) t-kw)
                            whole  (ref-fn ctx t-name t)]
                        (doseq [[alias checks] by-alias]
                          (verify! ctx t-name t [:input alias] checks (whole alias)))
                        :checked))))))

(defn checked-output
  [{:keys [schema] :as ctx} from out-name rows]
  (let [t-name (get-in from [:compute :transform-name])
        t      (transform-of schema t-name)]
    (verify! ctx t-name t [:output (or out-name t-name)] (xp/output-checks t out-name) rows)
    rows))

(defn check-landed-dataset!
  [{:keys [store schema] :as ctx} t-kw t {:keys [dataset name]}]
  (when-let [checks (seq (xp/output-checks t name))]
    (let [tbl (naming/dataset-table dataset)]
      (verify! ctx (or (:transform-name t) t-kw) t [:output (or name dataset)] checks
               (dsets/row-values store tbl (row-shape/array-keys store schema tbl))))))
