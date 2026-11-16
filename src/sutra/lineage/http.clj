(ns sutra.lineage.http
  (:require [sutra.kit.coll :as coll]
            [sutra.kit.refusal :as refusal]
            [sutra.kit.attempt :as attempt]
            [sutra.store.control :as ctl]
            [clojure.string :as str]
            [sutra.foundation.ops :as op]
            [sutra.authoring.api :as authoring]
            [sutra.builds.api :as builds]
            [sutra.lineage.facts :as lineage-facts]
            [sutra.lineage.reach :as reach]
            [sutra.lineage.render :as render]
            [sutra.schedules.api :as schedules]
            [sutra.catalog.lookup :as lookup]
            [sutra.http.endpoint :as ep]
            [sutra.http.responses :refer [ok]]
            [sutra.lineage.graph :as lineage]
            [sutra.lineage.ports :as ports]
            [sutra.objects.api :as objects]
            [sutra.store.access.scope :as ctx]
            [sutra.viz.lineage-branch :as lineage-branch]
            [sutra.viz.lineage-projection :as lp]))

(set! *warn-on-reflection* true)

(defn- with-issues
  [status store]
  (reduce (fn [st [[kind nm] n]]
            (update-in st [:nodes (lp/status-key kind nm)] assoc :issues n))
          status
          (ports/open-issues store)))

(defn- automations [store branch]
  (ports/automations store branch))

(defn- placement-of
  [ctx projection]
  (try (:placement (authoring/projection-memory ctx projection))
       (catch InterruptedException e (throw e))
       (catch Exception e
         (println (str "[lineage] no placement of " projection ": " (ex-message e)))
         {})))

(defn answers
  [{:keys [store schema branch] :as a}]
  (let [at (ctx/at store branch)]
    {:automations (automations store branch)
     :schedules   (schedules/list-all store branch)
     :jobs        (builds/last-jobs-per-transform at branch)
     :txns        (builds/last-txn-per-dataset at branch)
     :jobspecs    (delay (builds/jobspecs at))
     :placements  {:ontologie       (placement-of a :ontologie)
                   :code-repository (placement-of a :code-repository)}
     :status      (delay (:nodes (with-issues (lineage/status store schema branch) store)))
     :runs        (delay (coll/index-by (comp str :id) (builds/list-recent at 500 nil)))
     :modified    (delay (lineage-branch/modified-node-ids (ports/branch-tasks store branch)))
     :now         (ctl/millis store)}))

(def lineage-status
  (ep/handler :lineage-status
              {:call (fn [{:keys [store schema branch] :as a}]
                       (lineage-facts/canvas-status
                        schema (answers a)
                        (with-issues (lineage/status store schema branch) store)))}))

(defn- saved-graph [store schema handle]
  (ports/saved-graph store schema handle))

(def lineage-facts
  (ep/handler
   :lineage-facts
   {:call (fn [{:keys [store schema branch graph] :as a}]
            (let [spec (when graph
                         (or (second (saved-graph (ctx/at store branch) schema graph))
                             (refusal/refuse! :refusal.lineage/no-saved-graph {:graph graph} {:graph graph})))]
              (lineage-facts/facts schema (assoc (answers a) :graph-spec spec) a)))}))

(defn- node-name
  [schema automations handle]
  (when (seq (str handle))
    (let [es (lineage-facts/edges schema automations)
          id (first (lineage-facts/resolve-node schema (lineage-facts/nodes schema es) handle))]
      (second (str/split id #"/" 2)))))

(def lineage-graph
  (ep/handler :lineage-graph
              {:call (fn [{:keys [schema store branch node]}]
                       (lineage/graph schema
                                      (when node
                                        (node-name schema (automations store branch) node))))}))

(def lineage-dependents
  (ep/handler :lineage-dependents
              {:call (fn [{:keys [store branch schema] handle :type}]
                       (let [autos (automations store branch)
                             type  (or (attempt/or-answer nil (node-name schema autos handle))
                                       handle)
                             known?
                             (contains? (:object-types schema)
                                        (lookup/rid-of schema :object-types (keyword type)))]
                         (if known?
                           (assoc (lineage/dependents schema type)
                                  :readers (lineage-facts/readers-by-kind
                                            schema autos (str "object-type/" type))
                                  :count
                                  (get-in (objects/object-counts (ctx/at store branch) schema)
                                          [:by-type (keyword type)]))
                           {:object-type handle :unknown? true})))}))

(def lineage-related
  (ep/handler :lineage-related
              {:respond (fn [{:keys [schema node kind]}]
                          (if-not node
                            (refusal/refused :refusal.lineage/node-is-required {})
                            (ok (lineage/related schema kind node))))}))

(def lineage-rollback
  (ep/handler :lineage-rollback
              {:respond (fn [{:keys [store schema branch body]}]
                          (ok (lineage/rollback! store schema branch body)))}))

(def schedule-candidates
  (ep/handler :schedule-candidates
              {:call (fn [{:keys [schema target]}]
                       {:target     target
                        :candidates (lineage/watch-candidates (lineage/edges schema) target)})}))

(op/defop :lineage/drawing [{:keys [store]} {:keys [format drawing]}]
  (let [drawing (assoc drawing :store store)]
    (if (= :dot format) (render/graph-dot drawing) (render/graph-svg drawing))))

(op/defop :lineage/inputs-around [{:keys [schema]} {:keys [trunk nodes]}]
  (reach/inputs-around schema trunk nodes))

(op/defop :lineage/push-report [{:keys [schema]} {:keys [trunk around] :as bases}]
  (reach/report schema trunk around bases))

(op/defop :lineage/feeds-line [{:keys [schema]} {:keys [ids]}]
  (reach/feeds-line schema ids))

(def module
  {:name        :lineage-http
   :contributes {:http/routes
                 {:lineage-dependents  {:get lineage-dependents}
                  :lineage-facts       {:get lineage-facts}
                  :lineage-graph       {:get lineage-graph}
                  :lineage-related     {:get lineage-related}
                  :lineage-rollback    {:post lineage-rollback}
                  :lineage-status      {:get lineage-status}
                  :schedule-candidates {:get schedule-candidates}}}})
