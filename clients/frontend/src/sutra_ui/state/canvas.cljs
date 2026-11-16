(ns sutra-ui.state.canvas
  (:require [clojure.string :as str]
            [sutra-ui.api :as api]
            [sutra-ui.feature :as feature]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.instances :as instances]
            [sutra-ui.state.requests :as rq]
            [sutra-ui.state.shell :as shell]))

(defonce node-positions (db/cursor [:canvas :node-positions] {}))

(defonce instance-positions (db/cursor [:canvas :instance-positions] {}))

(defonce current-view (db/cursor [:canvas :current-view] nil))

(defonce canvas-layout-loaded (db/cursor [:canvas :canvas-layout-loaded] #{}))

(defn node-id->layout-spec
  [node-id]
  (let [s (str node-id)]
    (if-let [slash-i (str/index-of s "/")]
      (let [kind (subs s 0 slash-i)
            key  (subs s (inc slash-i))]
        {:node-kind (keyword kind)
         :node-key  key
         :pos-key   (keyword kind key)})
      {:node-kind :object-type
       :node-key  s
       :pos-key   (keyword s)})))

(defn canvas-layout-id
  [mode]
  (str "mode:" (name (or mode :ontology))))

(defn layout-target
  []
  (or (:current-view-id @shell/ui-state)
      (canvas-layout-id (shell/canvas-mode))))

(defn- redraw [db] (update-in db [:instances :instances-version] inc))

(defn- reschema [db] (update-in db [:catalog :schema-version] inc))

(defn- write-layout
  [target body]
  (rq/ask {:ask [api/post! :graph-layout {:id target} body]}))

(defn- uncommitted [entries] {:entries (vec entries) :commit? false})

(db/reg :sutra.canvas/move-node
        (fn [db [_ node-id x y target]]
          (let [{:keys [node-kind node-key pos-key]} (node-id->layout-spec node-id)]
            {:db (assoc-in db [:canvas :node-positions pos-key] {:x x :y y})
             :fx [(write-layout target {:node-kind node-kind :node-key node-key :x x :y y})]})))

(defn update-node-position! [node-id x y]
  (when (and x y) (db/send! [:sutra.canvas/move-node node-id x y (layout-target)])))

(defn carry-node-position!
  [kind from to]
  (let [pk (fn [nm] (if (= :object-type kind) (keyword (name nm)) (keyword (name kind) (name nm))))]
    (db/edit! node-positions
              (fn [m] (if-let [p (get m (pk from))]
                        (-> m (dissoc (pk from)) (assoc (pk to) p))
                        m)))))

(db/reg :sutra.canvas/move-instance
        (fn [db [_ uuid-str x y target]]
          {:db (assoc-in db [:canvas :instance-positions uuid-str] {:x x :y y})
           :fx [(write-layout target {:node-kind :object :node-key uuid-str :x x :y y})]}))

(defn update-instance-position! [uuid-str x y]
  (when (and uuid-str x y)
    (db/send! [:sutra.canvas/move-instance uuid-str x y (layout-target)])))

(defn- moved? [old {:keys [x y]}]
  (or (nil? old) (not= (:x old) x) (not= (:y old) y)))

(defn- place-instances
  [db id->xy target]
  (let [prev    (get-in db [:canvas :instance-positions])
        changed (into {} (for [[id {:keys [x y] :as p}] id->xy
                               :when (and id (number? x) (number? y) (moved? (get prev id) p))]
                           [id p]))]
    (if (empty? changed)
      {:db db}
      {:db (redraw (update-in db [:canvas :instance-positions] merge changed))
       :fx [(write-layout target (uncommitted (for [[id {:keys [x y]}] changed]
                                                {:node-kind :object :node-key id :x x :y y})))]})))

(db/reg :sutra.canvas/place-instances
        (fn [db [_ id->xy target]] (place-instances db id->xy target)))

(defn apply-instance-positions!
  [id->xy]
  (when (seq id->xy) (db/send! [:sutra.canvas/place-instances id->xy (layout-target)])))

(db/reg :sutra.canvas/place-nodes
        (fn [db [_ id->xy target]]
          (let [prev    (get-in db [:canvas :node-positions])
                changed (for [[id {:keys [x y]}] id->xy
                              :when (and id (number? x) (number? y))
                              :let [p (assoc (node-id->layout-spec id) :x x :y y :id id)]
                              :when (moved? (get prev (:pos-key p)) p)]
                          p)]
            (when (seq changed)
              {:db (reschema
                    (update-in db [:canvas :node-positions]
                               (fn [pos] (reduce (fn [m {:keys [pos-key x y]}]
                                                   (assoc m pos-key {:x x :y y}))
                                                 pos changed))))
               :fx [(write-layout target
                                  (uncommitted (for [{:keys [node-kind node-key x y]} changed]
                                                 {:node-kind node-kind :node-key node-key
                                                  :x x :y y})))]}))))

(defn apply-ontology-positions!
  [id->xy]
  (when (seq id->xy) (db/send! [:sutra.canvas/place-nodes id->xy (layout-target)])))

(defn positions-apply-fn
  [mode]
  (if (feature/schema-positions? mode)
    apply-ontology-positions!
    apply-instance-positions!))

(defn position-update-fn
  [mode]
  (if (feature/schema-positions? mode)
    update-node-position!
    update-instance-position!))

(defn position-of
  [mode node-id]
  (if (feature/schema-positions? mode)
    (get @node-positions (:pos-key (node-id->layout-spec node-id)))
    (get @instance-positions (str node-id))))

(defn unpositioned-ids
  [mode rf-nodes]
  (into #{}
        (comp (map #(.-id ^js %))
              (remove #(position-of mode %)))
        (array-seq rf-nodes)))

(defn seed-add!
  [type-ref row]
  (let [add {:objects (instances/objects-by-name {type-ref [row]}) :links []}]
    (instances/add-instances! add)
    (let [id (str (:id row))]
      (when-not (contains? @instance-positions id)
        (apply-instance-positions! {id {:x 240 :y 160}}))
      (instances/bump-instances-version!)
      (instances/focus-nodes! [id]))))

(db/reg :sutra.canvas/expand
        (fn [db [_ id-str target type]]
          (rq/request db {:ask  [api/post! :instances-around
                                 {:ids [id-str] :types [(or type (instances/type-of db id-str))]}]
                          :then [[:sutra.canvas/expanded id-str target]] :report "search-around"})))

(db/reg :sutra.canvas/expanded
        (fn [db [_ id-str target resp]]
          (let [add     {:objects (instances/objects-by-name (:objects resp))
                         :links   (vec (:links resp))}
                present (set (keys (get-in db [:canvas :instance-positions])))
                new-ids (->> (:objects add) vals (apply concat)
                             (map #(str (:id %)))
                             (remove present) distinct vec)
                db      (update-in db [:instances :instances] instances/merge-instance-graph add)
                center  (get-in db [:canvas :instance-positions id-str] {:x 240 :y 160})
                placed  (if (seq new-ids)
                          (place-instances db (instances/ring-positions center new-ids 400 220)
                                           target)
                          {:db db})]
            {:db (redraw (:db placed))
             :fx (conj (vec (:fx placed))
                       [:invoke [instances/focus-nodes! (cons id-str new-ids)]])})))

(defn expand-around!
  ([id-str] (expand-around! id-str nil))
  ([id-str type] (db/send! [:sutra.canvas/expand id-str (layout-target) type])))

(defn- with-layout
  [db layout replace?]
  (let [instance-kind? #{:object :link}
        by-kind (group-by :node-kind layout)
        ins     (mapcat (fn [k] (get by-kind k)) instance-kind?)
        nodes   (remove (comp instance-kind? :node-kind) layout)
        node-xy (into {}
                      (for [{:keys [node-kind node-key x y]} nodes]
                        [(if (= :object-type node-kind)
                           (keyword node-key)
                           (keyword (name node-kind) node-key))
                         {:x (double x) :y (double y)}]))
        ins-xy  (into {}
                      (for [{:keys [node-key x y]} ins]
                        [node-key {:x (double x) :y (double y)}]))]
    (-> (if replace?
          (update db :canvas assoc :node-positions node-xy :instance-positions ins-xy)
          (cond-> db
            (seq node-xy) (update-in [:canvas :node-positions] merge node-xy)
            (seq ins-xy)  (update-in [:canvas :instance-positions] merge ins-xy)))
        reschema
        redraw)))

(db/reg :sutra.canvas/layout
        (fn [db [_ layout replace?]] {:db (with-layout db layout replace?)}))

(defn apply-layout-entries!
  ([layout] (apply-layout-entries! layout false))
  ([layout replace?] (db/send! [:sutra.canvas/layout layout replace?])))

(defn forget-open-drawing!
  []
  (db/put! node-positions {})
  (db/put! instance-positions {})
  (db/put! canvas-layout-loaded #{})
  (catalog/bump-version!)
  (instances/bump-instances-version!))

(db/reg :sutra.canvas/load-layout
        (fn [db [_ mode reply]]
          (if (contains? (get-in db [:canvas :canvas-layout-loaded]) mode)
            {:fx [(rq/settle reply nil)]}
            {:fx [[:call {:call [api/get! :graph-open {:handle (canvas-layout-id mode)}]
                          :ok [:sutra.canvas/layout-loaded mode reply]
                          :err [:sutra.canvas/layout-loaded mode reply nil]}]]})))

(db/reg :sutra.canvas/layout-loaded
        (fn [db [_ mode reply side]]
          {:db (cond-> (update-in db [:canvas :canvas-layout-loaded] conj mode)
                 side (with-layout (:layout side) false))
           :fx [(rq/settle reply nil)]}))

(defn load-canvas-layout!
  [mode]
  (db/promise! [:sutra.canvas/load-layout mode]))

(defn layout-ready?
  [mode]
  (or (some? (:current-view-id @shell/ui-state))
      (contains? @canvas-layout-loaded mode)))

(defn close-view! []
  (db/put! current-view nil)
  (shell/edit! assoc :current-view-id nil)
  (forget-open-drawing!))

(add-watch shell/ui-state ::graph-of-its-app
           (fn [_ _ old new]
             (let [canvas (feature/nav->mode (:nav new))]
               (when (and canvas (not= canvas (feature/nav->mode (:nav old))))
                 (let [vid (:current-view-id new)
                       m   (catalog/graph-mode
                            (or @current-view (some-> vid catalog/view-spec-by-rid)))]
                   (when (and vid m (not= m canvas))
                     (close-view!)))))))

(defn- positions->layout-entries
  [positions node-kind name-fn]
  (vec (for [[k {:keys [x y]}] positions
             :when (and (some? x) (some? y))]
         {:node-kind node-kind :node-key (name-fn k) :x x :y y})))

(defn save-layout-to-view!
  [view-id]
  (let [mode (shell/canvas-mode)
        ontology-entries
        (when (feature/schema-positions? mode)
          (let [pos @node-positions]
            (concat
             (for [[k v] pos
                   :when (and (keyword? k) (nil? (namespace k))
                              (some? (:x v)) (some? (:y v)))]
               {:node-kind :object-type :node-key (name k)
                :x (:x v) :y (:y v)})
             (for [[k v] pos
                   :let [ns (namespace k)]
                   :when (and (keyword? k) ns
                              (some? (:x v)) (some? (:y v)))]
               {:node-kind (keyword ns) :node-key (name k)
                :x (:x v) :y (:y v)}))))
        entries (if (feature/schema-positions? mode)
                  ontology-entries
                  (positions->layout-entries @instance-positions :object str))]
    (api/post! :graph-layout {:id view-id} {:entries (vec entries) :commit? true})))

(defn lineage-hidden-ids []
  (set (get-in @current-view [:payload :hidden-nodes])))

(defn view-of-mode?
  [mode]
  (= (keyword mode) (:mode @current-view)))

(defn mode-color-groups
  [mode]
  (if (view-of-mode? mode) (get-in @current-view [:payload :color-groups] []) []))

(defn pinned-node-ids
  [mode]
  (if (view-of-mode? mode)
    (set (get-in @current-view [:payload :pinned-nodes]))
    #{}))

(defn visible?
  [visible-types kind type-kw]
  (let [s (get visible-types kind)]
    (or (nil? s) (contains? s type-kw))))

(defn current-visible-types []
  (:visible-types @current-view))

(defn lineage-scoped?
  [scope kind kw]
  (contains? (get scope kind) kw))

(defn lineage-scope
  []
  (when (= :lineage (:mode @current-view))
    (current-visible-types)))

(defn current-color-groups []
  (get-in @current-view [:payload :color-groups] []))

(defn- pos-key->layout-entry
  [k {:keys [x y]}]
  (if-let [kind (namespace k)]
    {:node-kind (keyword kind) :node-key (name k) :x x :y y}
    {:node-kind :object-type :node-key (name k) :x x :y y}))

(db/reg :sutra.canvas/restore
        (fn [db [_ positions target]]
          (let [prev (get-in db [:canvas :node-positions])]
            (when (not= prev positions)
              {:db (reschema (assoc-in db [:canvas :node-positions] positions))
               :fx [(when-let [entries (seq (for [[k v] positions :when (not= v (get prev k))]
                                              (pos-key->layout-entry k v)))]
                      (write-layout target (uncommitted entries)))]}))))

(defn restore-node-positions! [positions]
  (db/send! [:sutra.canvas/restore positions (layout-target)]))

(defn show-view!
  [v]
  (db/put! current-view v)
  (shell/edit! assoc :current-view-id (some-> (:id v) str)))

(defn open-drawing!
  [v layout]
  (db/put! current-view v)
  (shell/edit! assoc
:current-view-id (some-> (:id v) str)
:open-relations #{})
  (apply-layout-entries! layout true)
  (db/put! canvas-layout-loaded #{}))

(defn update-current-view!
  [f & args]
  (apply swap! current-view f args))
