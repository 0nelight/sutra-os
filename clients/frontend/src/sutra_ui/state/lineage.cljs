(ns sutra-ui.state.lineage
  (:require [sutra-ui.state.db :as db]
            [sutra.viz.lineage-projection :as lp]
            [sutra-ui.api :as api]
            [sutra-ui.state.canvas :as canvas]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.shell :as shell]))

(defonce lineage-graph
  (db/cursor [:lineage :lineage-graph] {}))

(defonce ^:private lineage-actions
  (db/cursor [:lineage :lineage-actions] {}))

(defn lineage-action-edits
  []
  @lineage-actions)

(defonce ^:private lineage-apps
  (db/cursor [:lineage :lineage-apps] {}))

(defn lineage-app-uses
  []
  @lineage-apps)

(defn- graph-node
  [n]
  (-> n
      (update :kind keyword)
      (update :name #(some-> % name keyword))))

(defn- uses [m] (into {} (for [[a ots] m] [(keyword a) (into #{} (map keyword) ots)])))

(defn graph-of
  [resp]
  {:lineage-graph   (into {} (for [e (:transforms resp)]
                               [(keyword (:name e))
                                {:inputs           (mapv graph-node (:inputs e))
                                 :outputs          (mapv graph-node (:outputs e))
                                 :external-systems (mapv keyword (:external-systems e))}]))
   :lineage-actions (uses (:action-edits resp))
   :lineage-apps    (uses (:app-uses resp))})

(db/reg :sutra.lineage/refresh
        (fn [_ _]
          {:fx [[:call {:call [api/get! :lineage-graph {:node nil}]
                        :ok [:sutra.lineage/loaded] :err [:sutra/ignored]}]]}))

(db/reg :sutra.lineage/loaded
        (fn [db [_ resp]]
          {:db (-> db
                   (update :lineage merge (graph-of resp))
                   (update-in [:catalog :schema-version] inc))}))

(defn refresh-lineage-graph! [] (db/send! [:sutra.lineage/refresh]))

(defn graph-edges
  []
  (seq @lineage-graph))

(defn tr-output-otks
  [sch edge]
  (lp/output-object-types (:links sch) edge))

(defn lineage-transforms-in-scope
  [_sch scope]
  (lp/transforms-in-scope (graph-edges) scope))

(defn lineage-node-inventory
  ([sch scope] (lineage-node-inventory sch scope (canvas/lineage-hidden-ids)))
  ([sch scope hidden]
   (-> (lp/inventory {:links (:links sch) :datasources (keys (data/datasource-map))}
                     (lineage-transforms-in-scope sch scope)
                     scope)
       (lp/visible-inventory hidden))))

(defn lineage-rendered-node-ids
  ([sch scope] (lineage-rendered-node-ids sch scope (canvas/lineage-hidden-ids)))
  ([sch scope hidden]
   (lp/rendered-node-ids (lineage-node-inventory sch scope hidden))))

(defn ot-relations
  [sch type-kw]
  (for [{lname :link :keys [api-name label dir far]} (catalog/link-sides sch type-kw)
        :let [{:keys [from to]} (catalog/link-spec lname)]]
    {:link lname :side api-name :label label :dir dir
     :from from :to to :other far}))

(defn relations-open?
  [type-kw]
  (contains? (:open-relations @shell/ui-state) type-kw))

(defn ots-defined-by-dataset
  [sch d-kw]
  (->> (graph-edges)
       (filter (fn [[_ e]] (some #{d-kw} (lp/of-kind e :inputs #{:dataset :media-set}))))
       (mapcat (fn [[_ e]] (tr-output-otks sch e)))
       distinct
       vec))

(def ref->node-id (partial apply lp/node-id))

(defn lineage-neighbors
  [sch node-id]
  (let [{:keys [node-kind node-key]} (canvas/node-id->layout-spec node-id)
        kw  (keyword node-key)
        writes-dataset? (fn [e] (some #{kw} (lp/of-kind e :outputs #{:dataset})))
        reads-dataset?  (fn [e] (some #{kw} (lp/of-kind e :inputs #{:dataset :media-set})))
        writes-ot?      (fn [e] (contains? (tr-output-otks sch e) kw))
        reads-ot?       (fn [e] (some #{kw} (lp/of-kind e :inputs #{:object-type})))
        uses-source?    (fn [e] (or (contains? (set (:external-systems e)) kw)
                                    (some #{kw} (lp/of-kind e :inputs #{:datasource}))))
        by (fn [pred] (->> (graph-edges) (filter (fn [[_ e]] (pred e)))
                           (map (fn [[t _]] [:transform t])) set))]
    (case node-kind
      :datasource  {:upstream #{}                  :downstream (by uses-source?)}
      :dataset     {:upstream (by writes-dataset?) :downstream (by reads-dataset?)}
      :object-type {:upstream   (cond-> (by writes-ot?)
                                  (some #(contains? % kw) (vals (lineage-action-edits)))
                                  (conj [:writeback kw]))
                    :downstream (-> (by reads-ot?)
                                    (into (for [[a ots] (lineage-action-edits)
                                                :when (contains? ots kw)]
                                            [:action a]))
                                    (into (for [[a ots] (lineage-app-uses)
                                                :when (contains? ots kw)]
                                            [:application a])))}
      :action      {:upstream   (into #{} (for [t (get (lineage-action-edits) kw)]
                                            [:object-type t]))
                    :downstream #{}}
      :application {:upstream   (into #{} (for [t (get (lineage-app-uses) kw)]
                                            [:object-type t]))
                    :downstream #{}}
      :writeback   {:upstream #{} :downstream #{[:object-type kw]}}
      :transform
      (let [self    (get @lineage-graph kw)
            in-ds   (set (lp/of-kind self :inputs #{:dataset :media-set}))
            in-ot   (set (lp/of-kind self :inputs #{:object-type}))
            out-ds  (set (lp/of-kind self :outputs #{:dataset}))
            out-ot  (tr-output-otks sch self)
            others  (fn [pred] (->> (graph-edges)
                                    (filter (fn [[t e]] (and (not= t kw) (pred e))))
                                    (map (fn [[t _]] [:transform t])) set))]
        {:upstream   (others (fn [e] (or (some in-ds (lp/of-kind e :outputs #{:dataset}))
                                         (some in-ot (tr-output-otks sch e)))))
         :downstream (others (fn [e] (or (some out-ds (lp/of-kind e :inputs #{:dataset :media-set}))
                                         (some out-ot (lp/of-kind e :inputs #{:object-type})))))})
      {:upstream #{} :downstream #{}})))

(defn dataset-producer-transform
  [sch d-kw]
  (some-> (:upstream (lineage-neighbors sch (catalog/dataset-node-id d-kw)))
          first second))

(defn type-actions
  [type-kw]
  (sort (for [[a ots] (lineage-action-edits) :when (contains? ots type-kw)] a)))

(defn type-actions-shown?
  [type-kw]
  (let [scope  (canvas/lineage-scope)
        hidden (canvas/lineage-hidden-ids)
        acts   (type-actions type-kw)]
    (and (seq acts)
         (every? #(and (canvas/lineage-scoped? scope :actions %)
                       (not (hidden (lp/node-id :action %))))
                 acts))))

(defn lineage-expansion-refs
  [sch node-id direction levels]
  (let [max-depth (if (= levels :all) 100000 levels)]
    (loop [frontier #{node-id}
           seen     #{node-id}
           acc      #{}
           depth    0]
      (if (or (empty? frontier) (>= depth max-depth))
        acc
        (let [refs    (->> frontier
                           (mapcat #(get (lineage-neighbors sch %) direction))
                           set)
              fresh   (remove #(contains? seen (ref->node-id %)) refs)
              new-ids (set (map ref->node-id fresh))]
          (recur new-ids (into seen new-ids) (into acc fresh) (inc depth)))))))

(defn refs-node-delta
  [sch refs]
  (let [scope   (or (canvas/lineage-scope) {})
        scope'  (reduce (fn [m [kind kw]]
                          (if-let [sk (lp/scope-of kind)]
                            (update m sk (fnil conj #{}) kw)
                            m))
                        scope refs)
        hidden  (canvas/lineage-hidden-ids)
        hidden' (apply disj hidden (map ref->node-id refs))]
    (- (count (lineage-rendered-node-ids sch scope' hidden'))
       (count (lineage-rendered-node-ids sch scope hidden)))))

(defn multi-expansion-refs [sch node-ids direction levels]
  (into #{} (mapcat #(lineage-expansion-refs sch % direction levels)) node-ids))

(defn lineage-expand-count-of
  [node-ids direction levels]
  (let [sch @catalog/schema]
    (refs-node-delta sch (multi-expansion-refs sch node-ids direction levels))))

(defn- edge-node-refs
  [sch e side]
  (concat (map (fn [k] [:dataset k]) (lp/of-kind e side #{:dataset :media-set}))
          (when (= side :inputs)
            (map (fn [k] [:datasource k])
                 (concat (:external-systems e) (lp/of-kind e side #{:datasource}))))
          (map (fn [k] [:object-type k])
               (if (= side :outputs)
                 (tr-output-otks sch e)
                 (lp/of-kind e side #{:object-type})))))

(defn lineage-adjacent-node-ids
  [sch node-id direction]
  (let [side (if (= direction :upstream) :inputs :outputs)]
    (into #{} (comp (mapcat (fn [[kind kw :as ref]]
                              (if (= :transform kind)
                                (when-let [e (get @lineage-graph kw)]
                                  (edge-node-refs sch e side))
                                [ref])))
                    (map ref->node-id))
          (get (lineage-neighbors sch node-id) direction))))
