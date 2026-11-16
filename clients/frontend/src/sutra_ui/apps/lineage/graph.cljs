(ns sutra-ui.apps.lineage.graph
  (:require [clojure.string :as str]
            [sutra.viz.lineage-projection :as lp]
            [sutra.viz.palette :as palette]
            [sutra-ui.state.canvas :as canvas]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.colors :as colors]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.instances :as instances]
            [sutra-ui.state.lineage :as lineage]
            [sutra-ui.state.lineage-status :as lineage-status]
            [sutra-ui.state.shell :as shell]))

(defn group-node-id [g] (str "group/" (:id g)))

(defn node->group
  [groups node-id]
  (some (fn [g] (when (contains? (set (:members g)) node-id) g)) groups))

(def resource-kind-colors
  (into {} (map (fn [[k v]] [k (:color v)])) palette/node-kinds))

(defn duration-color [ms]
  (if (nil? ms)
    colors/build-status-none
    (or (some (fn [[ceil _ c]] (when (or (nil? ceil) (< ms ceil)) c)) colors/duration-buckets)
        colors/build-status-none)))

(defn ->epoch-ms
  [s]
  (when-let [s (some-> s str not-empty)]
    (let [n (.parse js/Date s)] (when-not (js/isNaN n) n))))

(defn freshness-color [last-build]
  (if-let [t (->epoch-ms last-build)]
    (let [age (- (.getTime (js/Date.)) t)]
      (or (some (fn [[ceil _ c]] (when (or (nil? ceil) (< age ceil)) c)) colors/freshness-buckets)
          colors/build-status-none))
    colors/build-status-none))

(defn model-usage-grade
  [{:keys [attempted successful rate-limited failed]}]
  (let [a (or attempted 0) ok (or successful 0)
        bad (+ (or rate-limited 0) (or failed 0))]
    (cond (zero? a)  nil
          (zero? ok) "refused"
          (pos? bad) "degraded"
          :else      "active")))

(defn model-usage-color [usage]
  (let [g (model-usage-grade usage)]
    (or (some (fn [[k _ c]] (when (= k g) c)) colors/model-usage-buckets)
        colors/build-status-none)))

(defn ontology-status-color [status]
  (get colors/ontology-status-colors (some-> status name str/lower-case)
       (if status colors/ontology-status-unknown (get colors/ontology-status-colors "active"))))

(defn resource-metadata
  [kind kw]
  (let [sch  @catalog/canonical-schema
        spec (case kind
               :object-type (get-in sch [:object-types kw])
               :action      (get-in sch [:actions kw])
               :function    (get-in sch [:functions kw])
               :application (get-in sch [:applications kw])
               :link-type   (get-in sch [:link-types kw])
               :dataset     (get-in sch [:datasets kw])
               :transform   (get-in sch [:transforms kw])
               :datasource  (get-in sch [:sources kw])
               :agent       (get-in sch [:agents kw])
               nil)]
    (select-keys spec [:status :project :repository])))

(defn node-paint
  [kind kw]
  (let [status (delay (lineage-status/node-status kind kw))
        mode (:color-mode @shell/ui-state :resource-kind)
        meta* (delay (resource-metadata kind kw))]
    (case mode
      :ontology-status (ontology-status-color (:status @meta*))
      :folder          (if-let [p (:project @meta*)]
                         (colors/hash-color (keyword (name p)))
                         "#c5cbd3")
      :function-repo   (if (= kind :function)
                         (if-let [r (:repository @meta*)]
                           (colors/hash-color (keyword (name r)))
                           "#c5cbd3")
                         (resource-kind-colors kind))
      :app-views       (if (= kind :application)
                         (get colors/app-view-colors
                              (if (some? (:published @meta*)) "published" "draft"))
                         (resource-kind-colors kind))
      :resource-permissions (colors/resource-permission-color (:project @meta*))

      (if (= kind :application)
        (resource-kind-colors :application)
        (case mode
          :build-status   (get colors/build-status-colors (:build-status @status)
                               colors/build-status-none)
          :build-duration (if (= kind :datasource)
                            (resource-kind-colors kind)
                            (duration-color (:duration-ms @status)))
          :last-built     (if (= kind :datasource)
                            (resource-kind-colors kind)
                            (freshness-color (:last-build @status)))
          :model-usage    (model-usage-color (:model-usage @status))
          :out-of-date    (if (= kind :datasource)
                            (resource-kind-colors kind)
                            (get colors/freshness-colors (:freshness @status)
                                 colors/build-status-none))
          :data-health    (colors/health-colors (some-> @status :health :status))
          :schedule-count (colors/schedule-color (some-> @status :schedules))
          :scheme         (when (= kind :object-type) (colors/type-color kw))
          (if (contains? colors/categorical-color-modes mode)
            (colors/categorical-color (colors/categorical-value mode kind kw))
            (resource-kind-colors kind)))))))

(defn ds-pos-key   [ds-kw]   (keyword "datasource"     (name ds-kw)))

(defn ac-pos-key   [a-kw]    (keyword "action"         (name a-kw)))

(def ds-node-id  (partial lp/node-id :datasource))

(def ac-node-id  (partial lp/node-id :action))

(defn apply-group-rings
  [nodes groups]
  (let [live (remove :collapsed? groups)]
    (mapv (fn [n]
            (if-let [g (node->group live (:id n))]
              (assoc n :className "lineage-node--grouped" :style {"--lineage-ring" (:color g)})
              n))
          nodes)))

(defn apply-group-collapse-nodes
  [nodes positions groups]
  (let [collapsed  (filter :collapsed? groups)
        member-ids (into #{} (mapcat (comp set :members) collapsed))
        by-id      (into {} (map (juxt :id identity) nodes))
        kept       (remove #(contains? member-ids (:id %)) nodes)
        gnodes (for [g collapsed
                     :let [pk      (keyword "group" (str (:id g)))
                           mpos    (keep #(some-> (by-id %) :position) (:members g))
                           default (if (seq mpos)
                                     {:x (/ (reduce + (map :x mpos)) (count mpos))
                                      :y (/ (reduce + (map :y mpos)) (count mpos))}
                                     {:x 0 :y 0})]]
                 {:id       (group-node-id g)
                  :type     "group"
                  :position (get positions pk default)
                  :data     {:label (:name g)
                             :color (:color g)
                             :count (count (:members g))}})]
    (vec (concat kept gnodes))))

(defn apply-group-collapse-edges
  [edges groups]
  (let [remap (into {} (for [g groups :when (:collapsed? g)
                             m (:members g)]
                         [m (group-node-id g)]))
        re    (fn [id] (get remap id id))]
    (->> edges
         (map (fn [e] (-> e (update :source re) (update :target re))))
         (remove (fn [e] (= (:source e) (:target e))))
         (group-by (fn [e] [(:source e) (:target e) (get-in e [:data :kind])]))
         (vals)
         (mapv first))))

(defn relation-edges
  [sch]
  (for [l (:links sch)
        :let [from (:from l) to (:to l) lname (:name l)]
        :when (and from to lname (not= from to)
                   (or (lineage/relations-open? from) (lineage/relations-open? to)))]
    {:id     (str "relation/" (name from) "=>" (name to) "@" (name lname))
     :source (name from) :target (name to) :type "relationEdge"
     :data   {:kind "link" :link (name lname) :label (name lname)}}))

(defn with-relation-axis
  [node sch type-kw]
  (-> node
      (assoc-in [:data :relCount] (count (lineage/ot-relations sch type-kw)))
      (assoc-in [:data :relOpen]  (lineage/relations-open? type-kw))
      (assoc-in [:data :actCount] (count (lineage/type-actions type-kw)))
      (assoc-in [:data :actOpen]  (boolean (lineage/type-actions-shown? type-kw)))))

(defn with-expand-axis
  [node]
  (assoc-in node [:data :expandId] (:id node)))

(defn with-indicators
  [node kind kw]
  (let [{:keys [issues syncs trashed?]} (lineage-status/node-status kind kw)]
    (update node :data assoc
            :issues  (or issues 0)
            :syncs   (or syncs 0)
            :trashed (boolean trashed?))))

(defn lineage->rf-nodes [sch positions show-fields? ctrl-held?]
  (let [staged (:object-types @instances/staged-types)
        scope  (canvas/lineage-scope)
        {dataset-kws :datasets ot-kws :object-types src-kws :datasources a-kws :actions
         app-kws :applications wb-kws :writebacks}
        (lineage/lineage-node-inventory sch scope)
        dset-specs (get @catalog/canonical-schema :datasets {})
        unstructured (lp/media-set-datasets @lineage/lineage-graph)
        app-nodes
        (for [a-kw app-kws
              :let [spec (get-in @catalog/canonical-schema [:applications a-kw])]]
          (-> {:id       (lp/node-id :application a-kw)
               :type     "application"
               :position (get positions (keyword "application" (name a-kw)) {:x 0 :y 0})
               :data     {:label    (or (:display-name spec) (name a-kw))
                          :status   (if (some? (:published spec)) "published" "draft")
                          :paint    (node-paint :application a-kw)
                          :ctrlHeld (boolean ctrl-held?)}}
              (with-indicators :application a-kw)))
        wb-nodes
        (for [ot-kw wb-kws]
          {:id       (lp/node-id :writeback ot-kw)
           :type     "writeback"
           :position (get positions (keyword "writeback" (name ot-kw)) {:x 0 :y 0})
           :data     {:label      (name ot-kw)
                      :definesOts 1
                      :paint      (node-paint :writeback ot-kw)
                      :ctrlHeld   (boolean ctrl-held?)}})
        ac-nodes
        (for [a-kw a-kws
              :let [spec (get-in @catalog/canonical-schema [:actions a-kw])]]
          (-> {:id       (ac-node-id a-kw)
               :type     "actionType"
               :position (get positions (ac-pos-key a-kw) {:x 0 :y 0})
               :data     {:label      (name a-kw)
                          :rulesCount (count (or (:rules spec) []))
                          :paint      (node-paint :action a-kw)
                          :ctrlHeld   (boolean ctrl-held?)}}
              (with-indicators :action a-kw)))
        ot-nodes
        (for [type-kw ot-kws
              :let [fields (get-in sch [:objects type-kw])]]
          (-> (colors/object-type-node sch positions show-fields? ctrl-held?
                                       staged type-kw fields true
                                       (node-paint :object-type type-kw))
              (with-relation-axis sch type-kw)
              (with-indicators :object-type type-kw)))
        dataset-nodes
        (for [d-kw dataset-kws
              :let [spec (get dset-specs d-kw)]]
          (-> {:id       (catalog/dataset-node-id d-kw)
               :type     "dataset"
               :position (get positions (keyword "dataset" (name d-kw)) {:x 0 :y 0})
               :data     {:label        (name d-kw)
                          :cols         (count (:columns spec))
                          :definesOts   (count (lineage/ots-defined-by-dataset sch d-kw))
                          :unstructured (contains? unstructured d-kw)
                          :paint        (node-paint :dataset d-kw)
                          :ctrlHeld     (boolean ctrl-held?)}}
              (with-indicators :dataset d-kw)))
        ds-nodes
        (for [ds-kw src-kws
              :let [spec (get (data/datasource-map) ds-kw)]]
          (-> {:id       (ds-node-id ds-kw)
               :type     "datasource"
               :position (get positions (ds-pos-key ds-kw) {:x 0 :y 0})
               :data     {:label    (name ds-kw)
                          :dbType   (some-> (:type spec) name)
                          :paint    (node-paint :datasource ds-kw)
                          :ctrlHeld (boolean ctrl-held?)}}
              (with-indicators :datasource ds-kw)))
        groups (canvas/current-color-groups)]
    (-> (mapv with-expand-axis
              (concat ds-nodes dataset-nodes wb-nodes ot-nodes ac-nodes app-nodes))
        (apply-group-collapse-nodes positions groups)
        (apply-group-rings groups))))

(def ^:private flow-arrow
  {:type "arrowclosed" :color "var(--gray2)" :width 12 :height 12})

(defn lineage->rf-edges [sch]
  (let [scope (canvas/lineage-scope)
        trs   (lineage/lineage-transforms-in-scope sch scope)
        ids   (lineage/lineage-rendered-node-ids sch scope)
        edges (for [e (lp/edges {:links (:links sch) :all-edges @lineage/lineage-graph} trs ids)]
                {:id     (:id e)
                 :source (:source e) :target (:target e)
                 :type   "lineageEdge"
                 :markerEnd flow-arrow
                 :data   {:kind "transform" :transform (:transform e)}})
        ac-edges (for [e (lp/action-edges (lineage/lineage-action-edits) ids)]
                   {:id     (:id e)
                    :source (:source e) :target (:target e)
                    :type   "lineageEdge"
                    :data   {:kind "edits"}})
        app-edges (for [e (lp/app-edges (lineage/lineage-app-uses) ids)]
                    {:id     (:id e)
                     :source (:source e) :target (:target e)
                     :type   "lineageEdge"
                     :data   {:kind "surfaces"}})
        wb-edges  (for [e (lp/writeback-edges ids)]
                    {:id     (:id e)
                     :source (:source e) :target (:target e)
                     :type   "lineageEdge"
                     :data   {:kind "writeback"}})
        rel-edges (relation-edges sch)]
    (-> (->> (concat edges ac-edges app-edges wb-edges rel-edges)
             (filter (fn [e] (and (contains? ids (:source e))
                                  (contains? ids (:target e)))))
             (group-by :id) vals (mapv first))
        (apply-group-collapse-edges (canvas/current-color-groups)))))
