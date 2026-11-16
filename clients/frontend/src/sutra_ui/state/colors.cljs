(ns sutra-ui.state.colors
  (:require [sutra.types.languages :as langs]
            [sutra.viz.palette :as palette]
            [sutra.viz.lineage-branch :as lb]
            [sutra-ui.api :as api]
            [sutra-ui.object.card :as oc]
            [sutra-ui.state.branches :as branches]
            [sutra-ui.state.canvas :as canvas]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.instances :as instances]
            [sutra-ui.state.lineage-status :as lineage-status]
            [sutra-ui.state.shell :as shell]))

(def ^:private hash-palette
  ["#2d72d2" "#238551" "#c87619" "#cd4246" "#9d3f9d"
   "#00a396" "#db2c6f" "#8eb125" "#946638" "#147eb3"])

(defn hash-color [type-kw]
  (nth hash-palette (mod (hash type-kw) (count hash-palette))))

(defn- active-scheme
  [view active-id]
  (let [schemes (:color-schemes view)]
    (or (some #(when (= (str (:id %)) (str active-id)) %) schemes)
        (some #(when (:is-default %) %) schemes))))

(defn- scheme-color
  [scheme type-kw node]
  (let [{:keys [kind colors attribute map other]} (:rules scheme)]
    (case kind
      :by-type
      (or (get colors type-kw) (hash-color type-kw))

      :by-attribute
      (or (get map (some-> node :values (get attribute)))
          other
          (hash-color type-kw))

      (hash-color type-kw))))

(defn current-color-scheme
  []
  (active-scheme @canvas/current-view (:active-color-scheme-id @shell/ui-state)))

(defn type-color
  ([type-kw] (type-color type-kw nil))
  ([type-kw node]
   (let [view   @canvas/current-view
         active (active-scheme view (:active-color-scheme-id @shell/ui-state))]
     (if active
       (scheme-color active type-kw node)
       (hash-color type-kw)))))

(defn node-kind-color
  [kind]
  (get-in palette/node-kinds [kind :color]))

(defn node-kind-icon [kind] (get-in palette/node-kinds [kind :icon]))

(def build-status-colors
  {"succeeded" "#238551" "failed" "#cd4246" "running" "#c87619"
   "waiting"   "#8abbff" "skipped" "#abb3bf" "aborted" "#9d3f9d"})

(def build-status-none "#c5cbd3")

(def duration-buckets
  [[1000    "< 1s"  "#72ca9b"]
   [10000   "< 10s" "#32a467"]
   [60000   "< 1m"  "#f0b726"]
   [300000  "< 5m"  "#ec9a3c"]
   [nil     "≥ 5m"  "#cd4246"]])

(def freshness-buckets
  [[3600000    "< 1h"  "#238551"]
   [86400000   "< 1d"  "#72ca9b"]
   [604800000  "< 7d"  "#f0b726"]
   [2592000000 "< 30d" "#ec9a3c"]
   [nil        "≥ 30d" "#cd4246"]])

(def model-usage-buckets
  [["active"   "All calls succeeded" "#238551"]
   ["degraded" "Some calls refused"  "#c87619"]
   ["refused"  "All calls refused"   "#cd4246"]
   [nil        "No model calls"      "#c5cbd3"]])

(def freshness-states
  [["up-to-date"  "Up to date"                "#238551"]
   ["parent"      "Out of date with parent"   "#c87619"]
   ["ancestor"    "Out of date with ancestor" "#fbd065"]
   ["logic"       "Logic out of date"         "#7961db"]
   ["read-time"   "Joined when read"          "#238551"]
   ["never-built" "Never built"               "#c5cbd3"]])

(def freshness-colors (into {} (map (juxt first last)) freshness-states))

(def freshness-labels (into {} (map (juxt first second)) freshness-states))

(def health-colors
  {"failed" "#cd4246" "warning" "#c87619" "passed" "#238551" nil "#c5cbd3"})

(defn schedule-color [n]
  (cond (nil? n) build-status-none
        (zero? n) "#c5cbd3"
        (= 1 n)   "#8abbff"
        (= 2 n)   "#4c90f0"
        :else     "#215db0"))

(def ontology-status-colors
  {"active"       "#238551"
   "endorsed"     "#147eb3"
   "experimental" "#c87619"
   "example"      "#bd6bbd"
   "deprecated"   "#cd4246"
   "retired"      "#5f6b7c"})

(def ontology-status-unknown "#c5cbd3")

(def app-view-colors
  {"published" "#238551" "draft" "#c87619"})

(defn resource-permission-color
  [project]
  (if project (hash-color (keyword (name project))) "#cd4246"))

(def categorical-color-modes
  {:branch
   {:label "Branch"
    :desc  "The branch each node is viewed on: changed there, otherwise main"
    :of    (fn [kind kw]
             (lb/viewed-branch (branches/branch-modified-refs) (api/selected-page-branch)
                               (keyword kind) kw))}

   :project
   {:label "Project"
    :desc  "The project (folder) each resource belongs to"
    :of    (fn [kind kw] (catalog/node-project kind kw))}

   :repository
   {:label "Repository"
    :desc  "The code repository a transform was authored in"
    :of    (fn [kind kw]
             (or (some-> (get-in @catalog/canonical-schema [:transforms kw :repository]) name)
                 (if (= :transform kind) "No repository" "Not code-backed")))}

   :resource-overview
   {:label "Resource overview"
    :desc  "How the resource was created — sync, transform, object backing"
    :of    (fn [kind kw]
             (let [sch @catalog/canonical-schema]
               (case (keyword kind)
                 :datasource  (str "Source · "
                                   (or (some-> (get-in (data/datasource-map) [kw :type]) name)
                                       "unknown"))
                 :dataset     (cond
                                (get-in sch [:datasets kw :external-source]) "Synced dataset"
                                (get-in sch [:datasets kw :object-type])     "Object backing"
                                :else                                        "Derived dataset")
                 :transform   (str "Transform · "
                                   (or (some-> (get-in sch [:transforms kw :language]) name)
                                       "mapping"))
                 :object-type "Object type"
                 (name kind))))}

   :jobspec
   {:label "JobSpec"
    :desc  "Which branch publishes the JobSpec that builds each dataset (gray: none)"
    :of    (fn [kind kw]
             (if (#{:dataset :object-type :link-type} (keyword kind))
               (let [b (:jobspec (lineage-status/node-status kind kw))]
                 (cond (nil? b)        "—"
                       (= "none" b)    "No JobSpec"
                       :else           (str "JobSpec on " b)))
               "—"))}

   :compute
   {:label "Compute"
    :desc  "The engine each transform runs on (SQL / Python / model)"
    :of    (fn [kind kw]
             (if (= :transform kind)
               (let [lang (some-> (get-in @catalog/canonical-schema [:transforms kw :language])
                                  keyword)]
                 (or (langs/label lang)
                     (if (= :language-model lang) "Language model" "Declarative mapping")))
               "No compute"))}

   :write-mode
   {:label "Transaction type"
    :desc  "Append (:modify) vs snapshot (:replace) writes"
    :of    (fn [kind kw]
             (if (= :transform kind)
               (let [i (get-in @catalog/canonical-schema [:transforms kw :incremental])
                     m (if (map? i) (:write-mode i :modify) :modify)]
                 (if (= :replace m) "Snapshot (replace)" "Append (modify)"))
               "—"))}})

(defn categorical-value
  [mode kind kw]
  (when-let [{:keys [of]} (get categorical-color-modes mode)]
    (str (of kind kw))))

(def ^:private fixed-colors
  {"No JobSpec" "#abb3bf" "JobSpec on main" "#2d72d2" lb/trunk "#c5cbd3"})

(defn categorical-color
  [label]
  (or (get fixed-colors (str label)) (hash-color (keyword (str label)))))

(def status-backed-color-modes
  #{:build-status :build-duration :last-built :model-usage
    :out-of-date :data-health :schedule-count :jobspec})

(defn set-color-mode! [mode]
  (shell/edit! assoc :color-mode mode)
  (when (status-backed-color-modes mode)
    (lineage-status/ensure-lineage-status!))
  (catalog/bump-version!)
  (instances/bump-instances-version!))

(defn object-type-node
  [sch positions show-fields? ctrl-held? staged type-kw fields derived? paint]
  (let [nid     (name type-kw)
        spec    (get-in sch [:object-types type-kw])
        sym     (:symbol spec)
        display (oc/type-label type-kw)]
    {:id       nid
     :type     "objectType"
     :position (get positions type-kw {:x 0 :y 0})
     :data     {:label       nid
                :displayName display
                :symbol      sym
                :showFields  (boolean show-fields?)
                :ctrlHeld    (boolean ctrl-held?)
                :staged?     (contains? staged type-kw)
                :derived?    (boolean derived?)
                :color       (type-color type-kw)
                :paint       paint
                :fields      (mapv (fn [{fname :name ftype :type}]
                                     [(name fname) (catalog/value-type-label ftype)])
                                   fields)}}))

(defn categorical-legend
  [mode]
  (when (contains? categorical-color-modes mode)
    (->> (instances/canvas-node-ids)
         (keep (fn [id]
                 (let [{:keys [node-kind node-key]} (canvas/node-id->layout-spec id)]
                   (categorical-value mode node-kind (keyword node-key)))))
         frequencies
         (sort-by (comp - val))
         (mapv (fn [[label n]]
                 {:label (str label " · " n) :color (categorical-color label)})))))
