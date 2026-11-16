(ns sutra-ui.state.graph
  (:require [sutra-ui.ui.overlay :as overlay]
            [clojure.set :as set]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.requests :as rq]
            [sutra.types.rid :as rid]
            [sutra.viz.lineage-projection :as lp]
            [sutra.viz.lineage-branch :as lb]
            [sutra.i18n.core :as i18n]
            [sutra-ui.api :as api]
            [sutra-ui.feature :as feature]
            [sutra-ui.state.branches :as branches]
            [sutra-ui.state.canvas :as canvas]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.colors :as colors]
            [sutra-ui.state.instances :as instances]
            [sutra-ui.state.atlas :as kompass]
            [sutra-ui.state.lineage :as lineage]
            [sutra-ui.state.ontology :as ontology]
            [sutra-ui.state.selection :as selection]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.state.sync :as sync]))

(db/reg :sutra.graph/opened
        (fn [_ [_ view-id side reply]]
          (let [v (merge side (catalog/view-spec-by-rid view-id)
                         {:id            view-id
                          :layout        (:layout side)
                          :color-schemes (:color-schemes side)})]
            {:fx [[:invoke [canvas/open-drawing! v (:layout side)]] (rq/settle reply v)]})))

(defn- open-view!
  [view-id]
  (when-let [m (catalog/graph-mode (catalog/view-spec-by-rid view-id))]
    (when (not= (feature/mode->nav m) (shell/current-nav))
      (shell/enter! (feature/mode->nav m))))
  (db/promise! [:rf/request {:ask  [api/get! :graph-open {:handle (str view-id)}]
                             :then [[:sutra.graph/opened view-id]] :report "load-view"
                             :handover? true}]))

(defn- offer-saved-branch!
  [{:keys [mode branch]}]
  (if-let [to (when (lb/keeps-view? mode)
                (lb/switch-to {:saved   branch
                               :current (api/selected-page-branch)
                               :known   (map :name (branches/branch-list))}))]
    (-> (overlay/confirm! {:title   (i18n/t :ui/lineage-switch-branch-title)
                           :body    (i18n/t :ui/lineage-switch-branch-body {:name to})})
        (.then (fn [ok?] (when ok? (sync/checkout-branch! to)))))
    (js/Promise.resolve nil)))

(defn load-view!
  [view-id]
  (-> (offer-saved-branch! (get @catalog/views (str view-id)))
      (.then (fn [_] (open-view! view-id)))))

(defn save-as-view!
  [{:keys [area name folder-rid]}]
  (let [mode (shell/canvas-mode)
        nm   (rid/slug-kw name)]
    (-> (ontology/mutate! :create-graph
                          {:name nm :area area :mode mode
                           :branch (when (lb/keeps-view? mode)
                                     (lb/selected (api/selected-page-branch)))
                           :payload {:filter {}
                                     :color-palettes []
                                     :active-palette nil}})
        (.then (fn [_]
                 (when-let [vrid (get-in @catalog/canonical-schema [:graphs nm :id])]
                   (when folder-rid
                     (kompass/kompass-place! (rid/resource-ref :graphs vrid)
                                             folder-rid (clojure.core/name nm)))
                   (-> (canvas/save-layout-to-view! vrid)
                       (.then (fn [_] (open-view! vrid))))))))))

(defn delete-view!
  [view-name]
  (-> (ontology/mutate! :delete-graph {:name (rid/slug-kw view-name)})
      (.then (fn [_]
               (canvas/close-view!)))
      (.catch (partial shell/fail! "delete-view"))))

(defn- view->action-args
  [v overrides]
  (merge {:name              (rid/slug-kw (:name v))
          :title             (or (:title v) (some-> (:name v) name))
          :area              (:area v)
          :mode              (:mode v)
          :description       (:description v)
          :branch            (if (lb/keeps-view? (:mode v))
                               (lb/selected (api/selected-page-branch))
                               (:branch v))
          :fallback-branches (:fallback-branches v)
          :project           (:project v)
          :payload           (:payload v)
          :visible-types     (:visible-types v)}
         overrides))

(defn- redrawn [db instances?]
  (cond-> (update-in db [:catalog :schema-version] inc)
    instances? (update-in [:instances :instances-version] inc)))

(db/reg :sutra.graph/update
        (fn [db [_ k v args label instances?]]
          (rq/request (-> db (assoc-in [:canvas :current-view k] v) (redrawn instances?))
                      {:ask  [ontology/mutate! :update-graph args]
                       :then [[:sutra.graph/updated instances?]] :report label})))

(db/reg :sutra.graph/updated (fn [db [_ instances?]] {:db (redrawn db instances?)}))

(defn set-visible-types!
  [visible-types]
  (when-let [v @canvas/current-view]
    (db/send! [:sutra.graph/update :visible-types visible-types
               (view->action-args v {:visible-types visible-types}) "set-visible-types" true])))

(defn set-payload-key!
  [k value]
  (when-let [v @canvas/current-view]
    (let [payload' (assoc (or (:payload v) {}) k value)]
      (db/send! [:sutra.graph/update :payload payload'
                 (view->action-args v {:payload payload'}) (str "set-payload " k) false]))))

(defn hide-nodes! [node-ids]
  (set-payload-key! :hidden-nodes (vec (into (canvas/lineage-hidden-ids) node-ids))))

(defn unhide-nodes! [node-ids]
  (let [hidden (canvas/lineage-hidden-ids)
        left   (apply disj hidden node-ids)]
    (when (not= hidden left)
      (set-payload-key! :hidden-nodes (vec left)))))

(defn toggle-visible-type!
  [kind type-kw]
  (when-let [v @canvas/current-view]
    (let [vt   (:visible-types v)
          all-of-kind (case kind
                        :object-types     (set (keys (:objects @catalog/schema)))
                        :link-types (set (mapv :name (:links @catalog/schema))))
          curr (or (get vt kind) all-of-kind)
          next-set (if (contains? curr type-kw)
                     (disj curr type-kw)
                     (conj curr type-kw))
          next-vt (let [base (or vt {})
                        with-other-kinds (dissoc base kind)
                        with-this  (if (= next-set all-of-kind)
                                     with-other-kinds
                                     (assoc with-other-kinds kind next-set))]
                    (when (seq with-this) with-this))]
      (set-visible-types! next-vt))))

(defn set-active-color-scheme! [scheme-id]
  (shell/edit! assoc :active-color-scheme-id (some-> scheme-id str))
  (catalog/bump-version!)
  (instances/bump-instances-version!))

(defn- with-scheme
  [xs stored]
  (let [others (remove #(= (str (:id %)) (str (:id stored))) xs)]
    (vec (cons stored
               (mapv (fn [s]
                       (if (:is-default stored)
                         (assoc s :is-default false)
                         s))
                     others)))))

(db/reg :sutra.graph/scheme-stored
        (fn [db [_ stored]]
          {:db (update-in db [:canvas :current-view :color-schemes] with-scheme stored)
           :fx [[:invoke [set-active-color-scheme! (:id stored)]]]}))

(defn snapshot-by-type-scheme!
  [scheme-name]
  (when-let [v @canvas/current-view]
    (let [colors  (into {} (for [k (sort (keys (:objects @catalog/schema)))]
                             [k (colors/hash-color k)]))
          body    {:name scheme-name
                   :is-default true
                   :rules {:kind :by-type :colors colors}}]
      (db/send! [:rf/request {:ask    [api/post! :graph-schemes {:id (:id v)} body]
                              :then   [[:sutra.graph/scheme-stored]]
                              :report "create-color-scheme"}]))))

(db/reg :sutra.graph/scheme-deleted
        (fn [db [_ scheme-id active?]]
          {:db (update-in db [:canvas :current-view :color-schemes]
                          (fn [xs] (vec (remove #(= (str (:id %)) (str scheme-id)) xs))))
           :fx [(when active? [:invoke [set-active-color-scheme! nil]])]}))

(defn delete-color-scheme! [scheme-id]
  (when-let [v @canvas/current-view]
    (let [active? (= (str scheme-id) (:active-color-scheme-id @shell/ui-state))]
      (db/send! [:rf/request {:ask    [api/delete! :graph-scheme {:id (:id v)
                                                                  :scheme-id scheme-id}]
                              :then   [[:sutra.graph/scheme-deleted scheme-id active?]]
                              :report "delete-color-scheme"}]))))

(defn- ensure-canvas-view!
  [mode nm]
  (let [{:keys [open create wait reason] :as choice}
        (lb/canvas-view {:mode mode :current @canvas/current-view :catalog @catalog/rid->name
                         :graphs (vals @catalog/views)})]
    (cond
      (:use choice) (js/Promise.resolve @canvas/current-view)
      open (-> (open-view! open) (.then (fn [_] @canvas/current-view)))
      wait (js/Promise.reject
            (js/Error. (if (= :failed wait)
                         (i18n/t :ui/catalog-failed {:reason reason})
                         (i18n/t :ui/catalog-loading))))
      create
      (-> (ontology/mutate! :create-graph
                            {:name nm :area (name mode) :mode mode
                             :payload {:filter {} :color-palettes [] :active-palette nil}
                             :visible-types {}})
          (.then (fn [_]
                   (let [vrid (get-in @catalog/canonical-schema [:graphs nm :id])
                         v    (or (catalog/view-spec-by-rid vrid)
                                  {:id vrid :name nm :mode mode :visible-types {}})]
                     (canvas/show-view! v)
                     v)))))))

(defn- ensure-lineage-view! []
  (ensure-canvas-view! :lineage :untitled-lineage))

(defonce graph-undo (db/cursor [:graph :graph-undo] []))

(defonce graph-redo (db/cursor [:graph :graph-redo] []))

(defn- graph-snapshot []
  {:view      (:id @canvas/current-view)
   :scope     (:visible-types @canvas/current-view)
   :hidden    (get-in @canvas/current-view [:payload :hidden-nodes])
   :pinned    (get-in @canvas/current-view [:payload :pinned-nodes])
   :positions @canvas/node-positions})

(defn own-snapshot?
  [snap]
  (boolean (and @canvas/current-view (some? (:view snap))
                (= (:view snap) (:id @canvas/current-view)))))

(defn record-graph-state!
  []
  (when @canvas/current-view
    (db/edit! graph-undo (fn [s] (vec (take-last 50 (conj (vec s) (graph-snapshot))))))
    (db/put! graph-redo [])))

(defn clear-graph-history!
  []
  (db/put! graph-undo [])
  (db/put! graph-redo []))

(defn- restore-graph! [{:keys [scope hidden pinned positions]}]
  (canvas/restore-node-positions! positions)
  (when (not= hidden (get-in @canvas/current-view [:payload :hidden-nodes]))
    (set-payload-key! :hidden-nodes hidden))
  (when (not= pinned (get-in @canvas/current-view [:payload :pinned-nodes]))
    (set-payload-key! :pinned-nodes pinned))
  (set-visible-types! scope))

(defn graph-undo!
  []
  (when-let [snap (peek @graph-undo)]
    (when (own-snapshot? snap)
      (db/edit! graph-redo conj (graph-snapshot))
      (db/edit! graph-undo pop)
      (restore-graph! snap))))

(defn graph-redo!
  []
  (when-let [snap (peek @graph-redo)]
    (when (own-snapshot? snap)
      (db/edit! graph-undo conj (graph-snapshot))
      (db/edit! graph-redo pop)
      (restore-graph! snap))))

(defn- add-to-scope!
  [ensure-view! label refs]
  (record-graph-state!)
  (unhide-nodes! (mapv lineage/ref->node-id refs))
  (-> (ensure-view!)
      (.then (fn [v]
               (let [vt  (or (:visible-types v) {})
                     vt' (reduce (fn [m [kind kw]]
                                   (if-let [sk (lp/scope-of kind)]
                                     (update m sk (fnil conj #{}) kw)
                                     m))
                                 vt refs)]
                 (set-visible-types! vt'))))
      (.catch (partial shell/fail! label))))

(defn add-to-lineage-scope!
  [refs]
  (add-to-scope! ensure-lineage-view! "add-to-lineage-scope" refs))

(defn ensure-graph-view!
  [mode]
  (ensure-canvas-view! (keyword mode) (keyword (str "untitled-" (name mode)))))

(defn with-graph-view!
  [f]
  (let [mode (shell/canvas-mode)]
    (if (canvas/view-of-mode? mode)
      (do (f) (js/Promise.resolve @canvas/current-view))
      (-> (ensure-graph-view! mode)
          (.then (fn [v] (f) v))
          (.catch (partial shell/fail! "graph view"))))))

(defn remove-nodes-from-graph!
  [node-ids]
  (when (seq node-ids)
    (with-graph-view! (fn [] (record-graph-state!) (hide-nodes! node-ids)))))

(defn pin-nodes-on-graph!
  [node-ids]
  (when (seq node-ids)
    (with-graph-view!
      (fn []
        (record-graph-state!)
        (unhide-nodes! node-ids)
        (set-payload-key! :pinned-nodes
                          (vec (into (canvas/pinned-node-ids (shell/canvas-mode)) node-ids)))))))

(defn toggle-relations!
  [type-kw]
  (let [open? (lineage/relations-open? type-kw)]
    (shell/edit! update :open-relations
  (fnil (if open? disj conj) #{}) type-kw)
    (when-not open?
      (let [scope  (canvas/lineage-scope)
            others (->> (lineage/ot-relations @catalog/schema type-kw)
                        (map :other)
                        (remove #(= % type-kw))
                        (remove #(canvas/lineage-scoped? scope :object-types %))
                        distinct)]
        (when (seq others)
          (add-to-lineage-scope! (mapv (fn [o] [:object-type o]) others)))))
    (catalog/bump-version!)))

(defn toggle-type-actions!
  [type-kw]
  (let [acts (lineage/type-actions type-kw)]
    (if (lineage/type-actions-shown? type-kw)
      (remove-nodes-from-graph! (mapv #(lp/node-id :action %) acts))
      (add-to-lineage-scope! (mapv (fn [a] [:action a]) acts)))))

(defn expose-dataset-object-types!
  [d-kw]
  (let [scope (canvas/lineage-scope)
        ots   (remove #(canvas/lineage-scoped? scope :object-types %)
                      (lineage/ots-defined-by-dataset @catalog/schema d-kw))]
    (when (seq ots)
      (add-to-lineage-scope! (mapv (fn [o] [:object-type o]) ots)))))

(defn expand-node!
  [node-id direction levels]
  (let [refs (lineage/lineage-expansion-refs @catalog/schema node-id direction levels)]
    (when (seq refs) (add-to-lineage-scope! refs))))

(defn expand-nodes!
  [node-ids direction levels]
  (let [refs (lineage/multi-expansion-refs @catalog/schema node-ids direction levels)]
    (when (seq refs) (add-to-lineage-scope! (vec refs)))))

(defn- level-of-selection
  [ids]
  (let [sch  @catalog/schema
        sel  (set ids)
        ups  (into {} (for [id ids]
                        [id
                         (set/intersection sel
                                           (set (lineage/lineage-adjacent-node-ids
                                                 sch id :upstream)))]))]
    (loop [lvl (zipmap ids (repeat 0)), n 0]
      (let [nxt (into {} (for [id ids]
                           [id (if-let [ps (seq (get ups id))]
                                 (inc (apply max (map #(get lvl % 0) ps)))
                                 0)]))]
        (if (or (= nxt lvl) (>= n (count ids))) nxt (recur nxt (inc n)))))))

(defn layout-selection!
  [how]
  (let [boxes (selection/selected-boxes)]
    (when (> (count boxes) 1)
      (record-graph-state!)
      (let [x0     (apply min (map :x boxes))
            y0     (apply min (map :y boxes))
            gap-x  240
            gap-y  110
            by-id  (into {} (map (juxt :id identity)) boxes)
            ids    (mapv :id boxes)
            stack  (fn [axis]
                     (into {} (map-indexed
                               (fn [i id]
                                 [id (if (= axis :y)
                                       {:x x0 :y (+ y0 (* i gap-y))}
                                       {:x (+ x0 (* i gap-x)) :y y0})])
                               (sort ids))))
            banded (fn [lvl-of horizontal?]
                     (let [groups (group-by lvl-of ids)]
                       (into {} (for [[lvl members] groups
                                      [i id] (map-indexed vector (sort members))]
                                  [id (if horizontal?
                                        {:x (+ x0 (* lvl gap-x)) :y (+ y0 (* i gap-y))}
                                        {:x (+ x0 (* i gap-x))   :y (+ y0 (* lvl gap-y))})]))))
            levels (delay (level-of-selection ids))]
        (canvas/apply-ontology-positions!
         (case how
           :vertical     (stack :y)
           :horizontal   (stack :x)
           :hierarchical (banded #(get @levels % 0) true)
           :by-level     (banded #(get @levels % 0) false)
           (into {} (map (juxt :id #(select-keys % [:x :y]))) (vals by-id))))))))

(defn set-color-groups!
  [groups]
  (set-payload-key! :color-groups (vec groups)))

(defn remove-node-from-groups! [node-id]
  (set-color-groups!
   (mapv (fn [g] (update g :members #(vec (remove #{node-id} %))))
         (canvas/current-color-groups))))

(defn update-group! [group-id f]
  (set-color-groups!
   (mapv (fn [g] (if (= (:id g) group-id) (f g) g)) (canvas/current-color-groups))))

(defn toggle-group-collapsed! [group-id]
  (update-group! group-id #(update % :collapsed? not)))

(defn create-color-group-for-nodes!
  [nm color node-ids]
  (with-graph-view!
    (fn []
      (set-color-groups!
       (conj (vec (canvas/current-color-groups))
             {:id (str (random-uuid)) :name nm :color color
              :members (vec (distinct node-ids)) :collapsed? false})))))

(defn assign-nodes-to-group!
  [group-id node-ids]
  (with-graph-view!
    (fn []
      (let [ids (set node-ids)]
        (set-color-groups!
         (mapv (fn [g]
                 (if (= (:id g) group-id)
                   (update g :members #(vec (distinct (concat % node-ids))))
                   (update g :members #(vec (remove ids %)))))
               (canvas/current-color-groups)))))))

(def ^:private band-gap
  {:x 260 :y 180})

(defn layout-by-color!
  []
  (let [groups  (canvas/current-color-groups)
        ids     (instances/canvas-node-ids)
        grouped (for [g groups]
                  (filterv ids (:members g)))
        rest'   (vec (remove (set (apply concat grouped)) (sort ids)))
        bands   (vec (remove empty? (conj (vec grouped) rest')))]
    (when (seq bands)
      (record-graph-state!)
      (canvas/apply-ontology-positions!
       (into {} (for [[row band] (map-indexed vector bands)
                      [col id]   (map-indexed vector band)]
                  [id {:x (* col (:x band-gap))
                       :y (* row (:y band-gap))}])))
      (instances/focus-nodes! (apply concat bands)))))
