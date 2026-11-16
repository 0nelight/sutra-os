(ns sutra-ui.apps.lineage.model
  (:require [clojure.set :as set]
            [sutra.viz.lineage-branch :as lb]
            [sutra.viz.lineage-projection :as lp]
            [sutra-ui.state.branches :as branches]
            [sutra-ui.state.canvas :as canvas]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.colors :as colors]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.graph :as graph]
            [sutra-ui.state.instances :as instances]
            [sutra-ui.state.lineage :as lineage]
            [sutra-ui.state.lineage-status :as lineage-status]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.selection :as selection]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.apps.lineage.graph :as lgraph]
            [clojure.string :as str]))

(defonce ui (shell/slice :data-lineage))

(defonce lineage-frame (machine/frame ui {:name :lineage}))

(defn send! [event] (machine/dispatch event {:frame lineage-frame}))

(defn edit!
  [path f & args]
  (send! (into [:lineage/edit path f] args)))

(defn can-undo? [] (boolean (some-> (peek @graph/graph-undo) graph/own-snapshot?)))

(defn can-redo? [] (boolean (some-> (peek @graph/graph-redo) graph/own-snapshot?)))

(defn seed-lineage-ref!
  [[kind kw :as ref]]
  (if (= kind :dataset)
    (let [{:keys [upstream
                  downstream]} (lineage/lineage-neighbors @catalog/schema
                                                         (catalog/dataset-node-id kw))]
      (graph/add-to-lineage-scope! (into [ref] (concat upstream downstream))))
    (graph/add-to-lineage-scope! [ref])))

(defn remove-nodes-from-lineage-scope!
  [node-ids]
  (when (seq node-ids)
    (graph/record-graph-state!)
    (graph/hide-nodes! node-ids)))

(defn- closure-refs
  [sch node-id direction]
  (lineage/lineage-expansion-refs sch node-id direction :all))

(defn lineage-between-refs
  [sch node-ids]
  (let [ids  (vec (distinct node-ids))
        down (into {} (map (fn [id] [id (closure-refs sch id :downstream)])) ids)
        up   (into {} (map (fn [id] [id (closure-refs sch id :upstream)])) ids)]
    (into #{}
          (for [a ids b ids
                :when (not= a b)
                ref (set/intersection (set (get down a)) (set (get up b)))]
            ref))))

(defn lineage-common-refs
  [sch node-ids direction]
  (let [closures (map #(set (closure-refs sch % direction)) (distinct node-ids))]
    (if (seq closures) (reduce set/intersection closures) #{})))

(defn lineage-between-count [node-ids]
  (lineage/refs-node-delta @catalog/schema (lineage-between-refs @catalog/schema node-ids)))

(defn lineage-common-count [node-ids direction]
  (let [sch @catalog/schema]
    (lineage/refs-node-delta sch (lineage-common-refs sch node-ids direction))))

(defn expand-between!
  [node-ids]
  (let [refs (lineage-between-refs @catalog/schema node-ids)]
    (when (seq refs) (graph/add-to-lineage-scope! (vec refs)))))

(defn expand-common!
  [node-ids direction]
  (let [refs (lineage-common-refs @catalog/schema node-ids direction)]
    (when (seq refs) (graph/add-to-lineage-scope! (vec refs)))))

(defn invert-selection! []
  (selection/select-nodes! (set/difference (instances/canvas-node-ids)
                                           @selection/canvas-selection)))

(defn select-adjacent!
  [direction]
  (let [sch @catalog/schema
        cur @selection/canvas-selection
        adj (into #{} (mapcat #(lineage/lineage-adjacent-node-ids sch % direction)) cur)]
    (selection/select-nodes! (set/union cur (set/intersection adj (instances/canvas-node-ids))))))

(defn- distribute
  [boxes axis]
  (let [size   (if (= axis :x) :w :h)
        sorted (sort-by axis boxes)
        lo     (first sorted)
        hi     (last sorted)
        span   (- (+ (axis hi) (size hi)) (axis lo))
        total  (reduce + (map size sorted))
        gap    (/ (- span total) (max 1 (dec (count sorted))))]
    (first
     (reduce (fn [[acc at] b]
               [(assoc acc (:id b) (if (= axis :x)
                                     {:x at :y (:y b)}
                                     {:x (:x b) :y at}))
                (+ at (size b) gap)])
             [{} (axis lo)]
             sorted))))

(defn align-selection!
  [how]
  (let [boxes (selection/selected-boxes)]
    (when (> (count boxes) 1)
      (graph/record-graph-state!)
      (let [xs      (map :x boxes)
            ys      (map :y boxes)
            rights  (map #(+ (:x %) (:w %)) boxes)
            bottoms (map #(+ (:y %) (:h %)) boxes)
            at      (fn [f] (into {} (map (fn [b] [(:id b) (f b)])) boxes))
            id->xy  (case how
                      :left     (let [v (apply min xs)]
                                  (at (fn [b] {:x v :y (:y b)})))
                      :right    (let [v (apply max rights)]
                                  (at (fn [b] {:x (- v (:w b)) :y (:y b)})))
                      :h-center (let [c (/ (+ (apply min xs) (apply max rights)) 2)]
                                  (at (fn [b] {:x (- c (/ (:w b) 2)) :y (:y b)})))
                      :top      (let [v (apply min ys)]
                                  (at (fn [b] {:x (:x b) :y v})))
                      :bottom   (let [v (apply max bottoms)]
                                  (at (fn [b] {:x (:x b) :y (- v (:h b))})))
                      :v-middle (let [c (/ (+ (apply min ys) (apply max bottoms)) 2)]
                                  (at (fn [b] {:x (:x b) :y (- c (/ (:h b) 2))})))
                      :distribute-h (distribute boxes :x)
                      :distribute-v (distribute boxes :y))]
        (canvas/apply-ontology-positions! id->xy)))))

(defn- connected-node-ids
  []
  (into #{} (mapcat (juxt :source :target)) (lgraph/lineage->rf-edges @catalog/schema)))

(defn clean-candidates
  [what]
  (let [ids (instances/canvas-node-ids)]
    (vec (case what
           :isolated   (remove (connected-node-ids) ids)
           :unselected (remove @selection/canvas-selection ids)
           :all        ids))))

(defn clean-graph!
  [what]
  (let [ids (clean-candidates what)]
    (when (seq ids)
      (remove-nodes-from-lineage-scope! ids)
      (selection/select-nodes! (set/difference (instances/canvas-node-ids) (set ids))))))

(def ^:private histogram-facets
  [["Kind"            (fn [k _ _]  (name k))                                  false]
   ["Build status"    (fn [_ _ status] (or (:build-status status) "never built"))     false]
   ["Out of date"     (fn [_ _ status] (get colors/freshness-labels (:freshness status) "—"))
    false]
   ["Resource"        (fn [_ _ status] (or (:resource status) "—"))                  false]
   ["Storage"         (fn [_ _ status] (or (:storage status) "—"))                   false]
   ["Defines"         (fn [_ _ status] (:defines status))                            true]
   ["Data health"     (fn [_ _ status] (or (some-> status :health :status name) "—")) false]
   ["Project"         (fn [k kw _] (catalog/node-project k kw))                       false]
   ["Last Modified"   (fn [_ _ status] (let [s (some-> status :last-build str)]
                                         (cond (str/blank? s)  "never built"
                                               (> (count s) 10) (subs s 0 10)
                                               :else            s)))              false]
   ["Frequent Columns" (fn [k kw _] (catalog/node-columns k kw))                      true]])

(defn- node-facets
  [node-id]
  (let [{:keys [node-kind node-key]} (canvas/node-id->layout-spec node-id)
        kw (keyword node-key)
        status (lineage-status/node-status node-kind kw)]
    (into {} (for [[label f _] histogram-facets]
               [label (f node-kind kw status)]))))

(defn selection-histogram
  []
  (let [ids (vec @selection/canvas-selection)]
    (when (> (count ids) 1)
      (let [facets (mapv (juxt identity node-facets) ids)]
        (vec (for [[prop _ multi?] histogram-facets
                   :let [buckets
                         (->> (if multi?
                                (for [[id f] facets, v (get f prop)] [v id])
                                (for [[id f] facets] [(get f prop) id]))
                              (group-by first)
                              (map (fn [[v pairs]] [v (mapv second pairs)]))
                              (sort-by (comp - count second))
                              vec)]
                   :when (seq buckets)]
               [prop buckets]))))))

(defn selection-names
  []
  (str/join ", " (sort (map str @selection/canvas-selection))))

(defn highlight-bucket!
  [prop value ids]
  (let [cur (:histogram-highlight @ui)]
    (if (and (= prop (:prop cur)) (= value (:value cur)))
      (edit! [] dissoc :histogram-highlight)
      (do (edit! [] assoc :histogram-highlight
                 {:prop prop :value value :ids (vec ids)})
          (instances/focus-nodes! ids)))))

(defn highlighted-bucket [] (:histogram-highlight @ui))

(defn update-selection-to-highlight!
  []
  (when-let [{:keys [ids]} (:histogram-highlight @ui)]
    (selection/select-nodes! (set ids))
    (edit! [] dissoc :histogram-highlight)))

(defn build-targets-of
  [node-ids]
  (vec (distinct
        (keep (fn [id]
                (let [{:keys [node-kind node-key]} (canvas/node-id->layout-spec id)]
                  (when (contains? #{:dataset :object-type :transform} node-kind)
                    (keyword node-key))))
              node-ids))))

(defn- seed-match-rank
  [label ql]
  (cond (= label ql)                    0
        (str/starts-with? label ql)     1
        :else                           2))

(defn- round-robin
  [colls]
  (let [colls (remove empty? colls)]
    (when (seq colls)
      (lazy-seq (concat (map first colls)
                        (round-robin (map rest colls)))))))

(defn lineage-seed-candidates
  [q]
  (let [ql   (some-> q str/lower-case str/trim)
        mk   (fn [kind kw] {:kind kind :kw kw :label (name kw)})
        by-kind [(map #(mk :datasource %)  (keys (data/datasource-map)))
                 (map #(mk :dataset %)     (keys (get @catalog/canonical-schema :datasets {})))
                 (map #(mk :transform %)   (keys (get @catalog/canonical-schema :transforms {})))
                 (map #(mk :object-type %) (keys (:objects @catalog/schema)))
                 (map #(mk :action %)      (keys (get @catalog/canonical-schema :actions {})))
                 (map #(mk :application %)
                      (keys (get @catalog/canonical-schema :applications {})))]]
    (if (seq ql)
      (->> by-kind
           (map (fn [cands]
                  (->> cands
                       (keep (fn [{:keys [label] :as c}]
                               (let [l (str/lower-case label)]
                                 (when (str/includes? l ql)
                                   (assoc c :sutra-ui.state/rank (seed-match-rank l ql))))))
                       (sort-by (juxt :sutra-ui.state/rank :label)))))
           round-robin
           (sort-by :sutra-ui.state/rank)
           (map #(dissoc % :sutra-ui.state/rank)))
      (apply concat by-kind))))

(defn lineage-root-candidates
  []
  (let [sch      @catalog/schema
        edges    (lineage/graph-edges)
        produced (into #{} (mapcat (fn [[_ e]]
                                     (concat (map #(vector :dataset %)
                                                  (lp/of-kind e :outputs #{:dataset}))
                                             (map #(vector :object-type %)
                                                  (lineage/tr-output-otks sch e))))
                                   edges))
        consumed (into #{} (mapcat (fn [[_ e]]
                                     (concat (map #(vector :datasource %)
                                                  (concat (:external-systems e)
                                                          (lp/of-kind e :inputs #{:datasource})))
                                             (map #(vector :dataset %)
                                                  (lp/of-kind e :inputs #{:dataset :media-set}))
                                             (map #(vector :object-type %)
                                                  (lp/of-kind e :inputs #{:object-type}))))
                                   edges))]
    (->> consumed
         (remove produced)
         (sort-by (fn [[kind kw]]
                    [(case kind :datasource 0 :dataset 1 2) (name kw)]))
         (mapv (fn [[kind kw]] {:kind kind :kw kw :label (name kw)})))))

(defn- lineage-on-graph
  []
  (let [{:keys [datasets object-types datasources]}
        (lineage/lineage-node-inventory @catalog/schema (canvas/lineage-scope))
        on? (complement (canvas/lineage-hidden-ids))
        mk  (fn [kind kw] {:kind kind :kw kw :label (name kw)})]
    (concat (map #(mk :datasource %)  (filter #(on? (lgraph/ds-node-id %)) datasources))
            (map #(mk :dataset %)     (filter #(on? (catalog/dataset-node-id %)) datasets))
            (map #(mk :object-type %) (filter #(on? (name %)) object-types)))))

(defn modified-not-on-graph
  []
  (lb/missing-from-graph (branches/branch-modified-refs)
                         (canvas/lineage-scope)
                         (canvas/lineage-hidden-ids)))

(defn lineage-search-candidates
  [q]
  (let [ql  (some-> q str/lower-case str/trim)
        all (lineage-on-graph)]
    (if (seq ql)
      (filter #(str/includes? (str/lower-case (:label %)) ql) all)
      all)))

(defn lineage-column-candidates
  [q]
  (let [ql (some-> q str/lower-case str/trim)]
    (when (seq ql)
      (->> (lineage-on-graph)
           (keep (fn [{:keys [kind kw] :as n}]
                   (let [hits (filterv #(str/includes? (str/lower-case %) ql)
                                       (catalog/node-columns kind kw))]
                     (when (seq hits) (assoc n :columns hits)))))
           vec))))

(defn delete-group! [group-id]
  (graph/set-color-groups! (vec (remove #(= (:id %) group-id) (canvas/current-color-groups)))))

(defn toggle-all-groups-collapsed!
  []
  (let [groups (canvas/current-color-groups)
        fold?  (not-every? :collapsed? groups)]
    (when (seq groups)
      (graph/set-color-groups! (mapv #(assoc % :collapsed? fold?) groups)))))
