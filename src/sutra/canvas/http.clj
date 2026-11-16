(ns sutra.canvas.http
  (:require [sutra.kit.coll :as coll]
            [sutra.kit.refusal :as refusal]
            [sutra.canvas.ports :as ports]
            [sutra.canvas.layouts]
            [sutra.foundation.ops :as op]
            [clojure.string :as str]
            [sutra.foundation.encoding :refer [->uuid]]
            [sutra.types.rid :as rid]
            [sutra.catalog.lookup :as lookup]
            [sutra.canvas.graphs :as cg]
            [sutra.viz.layouts :as layouts]
            [sutra.store.access.scope :as ctx]
            [sutra.store.world :as world]
            [sutra.types.entities :as ents]
            [sutra.projects.api :as projects]
            [sutra.objects.api :as objects]
            [sutra.catalog.grants :as grants]
            [sutra.http.endpoint :as ep]
            [sutra.http.responses :refer [ok]]))

(set! *warn-on-reflection* true)

(declare get-graph default-canvas resolve-graph-handle)

(defn- on-branch
  [f]
  (fn [{:keys [store branch] :as a}]
    (f (assoc a :store (ctx/at store branch)))))

(defn- on-graph
  [f]
  (fn [{:keys [store schema branch id auth req] :as a}]
    (let [at   (ctx/at store branch)
          [grid found] (when-not (default-canvas id) (resolve-graph-handle at schema id))
          op   (get-in req [:sutra/requirement :op])
          _    (when (and op grid)
                 (grants/authorize! auth {:op op :project (:project found)
                                          :ignore-inherited (:ignore-inherited found)
                                          :resource grid}
                                    grid))
          spec (or (default-canvas id) found)
          home (when (and branch spec (ents/authored-instance? :graphs spec))
                 (world/instance-home (world/at store branch)))]
      (f (assoc a :store (if home (ctx/at store home) at))))))

(defn- upsert-layout! [store args]
  (cg/upsert-layout! store args))

(defn- save-layout!
  [store graph-id entries commit?]
  (doseq [e entries]
    (cg/upsert-layout! store (assoc e :graph-id graph-id)))
  {:saved (count entries) :graph-id graph-id :committed (boolean commit?)})

(defn- commit-graph! [graph-id]
  {:committed graph-id})

(defn- query-color-schemes [store graph-id]
  (cg/query-color-schemes store graph-id))

(defn- delete-color-scheme! [store scheme-id]
  (cg/delete-color-scheme! store scheme-id))

(defn- kompass-path-index
  [store]
  (let [nodes  (projects/read-kompass-nodes store)
        by-rid (coll/index-by :rid nodes)
        chain  (fn chain [n depth]
                 (if (or (nil? n) (> depth 32))
                   []
                   (conj (chain (get by-rid (:parent-rid n)) (inc depth))
                         (:name n))))]
    (into {}
          (for [n nodes :when (and (= :placement (:node-kind n)) (:ref n))]
            [(:ref n) (str "/" (str/join "/" (chain n 0)))]))))

(defn- graph-summary
  ([grid spec] (graph-summary grid spec nil))
  ([grid spec path]
   {:id                grid
    :name              (:graph-name spec)
    :title             (or (:title spec) (some-> (:graph-name spec) name))
    :mode              (:mode spec)
    :area              (:area spec)
    :path              path
    :project           (:project spec)
    :branch            (:branch spec)
    :fallback-branches (:fallback-branches spec)}))

(defn resolve-graph-handle
  [store schema handle]
  (let [graphs (:graphs schema)
        h      (str/trim (str handle))]
    (or (when-let [spec (get graphs h)] [h spec])
        (let [kw (rid/slug-kw h)]
          (first (for [[grid spec] graphs
                       :when (or (= kw (:graph-name spec))
                                 (= kw (some-> (:title spec) rid/slug-kw)))]
                   [grid spec])))
        (let [want (str/lower-case (if (str/starts-with? h "/") h (str "/" h)))
              idx  (kompass-path-index store)]
          (first (for [[grid spec] graphs
                       :let [p (get idx grid)]
                       :when (and p (= want (str/lower-case p)))]
                   [grid spec]))))))

(def list-graphs
  (ep/handler :graphs
              {:call (on-branch
                      (fn [{:keys [store area mode project] sch :schema}]
                        (let [proj-rid (when project
                                         (lookup/rid-of sch :projects (rid/slug-kw project)))
                              paths    (kompass-path-index store)]
                          {:graphs (vec (for [[grid spec] (:graphs sch)
                                              :when (and (or (nil? area)    (= area (:area spec)))
                                                         (or (nil? mode)    (= mode (:mode spec)))
                                                         (or (nil? project)
                                                             (= proj-rid (:project spec))))]
                                          (graph-summary grid spec (get paths grid))))})))}))

(def ^:private default-canvas-re #"^mode:([a-z][a-z0-9-]{0,31})$")

(defn- default-canvas
  [id]
  (let [s (str id)]
    (when-let [[_ mode] (re-matches default-canvas-re s)]
      {:handle s :mode (keyword mode)})))

(def open-graph
  (ep/handler :graph-open
              {:respond (fn [{:keys [store schema branch handle]}]
                          (get-graph {:path-params {:id handle} :sutra/store store
                                      :sutra/schema schema
                                      :sutra/branch branch}))}))

(def get-graph
  (ep/handler :graph
              {:respond
               (on-graph
                (fn [{:keys [store schema id]}]
                  (if-let [{:keys [handle mode]} (default-canvas id)]
                    (ok (merge {:id handle :mode mode :default-canvas? true}
                               (cg/read-graph-side-data store handle)))
                    (let [[grid spec] (resolve-graph-handle store schema id)
                          grid        (or grid (when (rid/well-formed-for? :graphs id) id))]
                      (if (nil? grid)
                        (refusal/refused :refusal.canvas/no-such-graph {:handle id
                                            :hint "address a graph by RID, name, or kompass path"})
                        (ok (merge {:id grid}
                                   (when spec
                                     (merge
                                      (graph-summary grid spec
                                                     (get (kompass-path-index store) grid))
                                      {:description   (:description spec)
                                       :visible-types (:visible-types spec)
                                       :payload       (:payload spec)}))
                                   (cg/read-graph-side-data store grid))))))))}))

(defn- drawn-graph
  [{:keys [store schema id layout richtung]} spec positions fmt]
  (if (not= :lineage (:mode spec))
    (refusal/refused :refusal.canvas/only-a-lineage-graph-can {:graph id :mode (:mode spec)})
    (let [drawing (cond-> {:schema    schema
                           :scope     (:visible-types spec)
                           :hidden    (get-in spec [:payload :hidden-nodes])
                           :positions positions
                           :title     (or (:title spec) (some-> (:graph-name spec) name))}
                    layout   (assoc :layout layout)
                    richtung (assoc :richtung richtung))]
      {:status  200
       :headers {"Content-Type" (if (= :dot fmt)
                                  "text/vnd.graphviz; charset=utf-8"
                                  "image/svg+xml; charset=utf-8")}
       :body    (ports/lineage-drawing store fmt drawing)})))

(defn- draw-graph
  [{:keys [store schema branch id] :as req} fmt]
  (if-not branch
    (refusal/refused :refusal.http/no-branch {})
    (if-let [[grid spec] (resolve-graph-handle store schema id)]
      (drawn-graph (select-keys req [:store :schema :id :layout :richtung]) spec
                   (when (= :lineage (:mode spec))
                     (:layout (cg/read-graph-side-data store grid)))
                   fmt)
      (refusal/refused :refusal.canvas/no-such-graph {:handle id
                          :hint "address a graph by RID, name, or kompass path"}))))

(def graph-svg (ep/handler :graph-svg {:respond (on-graph #(draw-graph % :svg))}))

(def graph-dot (ep/handler :graph-dot {:respond (on-graph #(draw-graph % :dot))}))

(def get-dashboard-layout
  (ep/handler :explorer-dashboard
              {:respond (fn [{:keys [store object-type]}]
                          (if-let [row (objects/read-layout store (keyword object-type))]
                            (ok row)
                            (refusal/refused :refusal.canvas/no-layout {:object-type object-type :layout nil})))}))

(def save-dashboard-layout
  (ep/handler :explorer-dashboard
              {:respond (fn [{:keys [store schema object-type body]}]
                          (if (contains? body :layout)
                            (do (objects/readable-type! store schema (keyword object-type))
                                (ok
                                 (objects/upsert-layout!
                                  store
                                  {:object-type (keyword object-type) :layout (:layout body)})))
                            (refusal/refused :refusal.canvas/a-dashboard-layout-needs-layout {:body body})))}))

(defn- ->layout-target
  [store schema id]
  (or (:handle (default-canvas id))
      (first (resolve-graph-handle store schema id))
      (when (rid/well-formed-for? :graphs id) id)))

(def upsert-layout
  (ep/handler :graph-layout
              {:respond (on-graph (fn [{:keys [store schema id body req]}]
                                     (if-let [grid (->layout-target store schema id)]
                                       (if-let [entries (:entries body)]
                                         (ok
                                          (save-layout!
                                           store grid entries (get body :commit? true)))
                                         (ok (upsert-layout! store (assoc body :graph-id grid))))
                                       (refusal/refused :refusal.canvas/no-such-graph (:path-params req)))))}))

(def commit-graph
  (ep/handler :graph-commit
              {:respond (on-graph (fn [{:keys [store schema id]}]
                                     (if-let [grid (->layout-target store schema id)]
                                       (ok (commit-graph! grid))
                                       (refusal/refused :refusal.canvas/no-such-graph {:id id}))))}))

(def list-color-schemes
  (ep/handler :graph-schemes
              {:respond (on-graph (fn [{:keys [store schema id]}]
                                     (if-let [grid (->layout-target store schema id)]
                                       (ok {:color-schemes (query-color-schemes store grid)})
                                       (refusal/refused :refusal.canvas/no-such-graph {:id id}))))}))

(def create-color-scheme
  (ep/handler :graph-schemes
              {:respond (on-graph (fn [{:keys [store schema id body req]}]
                                     (if-let [grid (->layout-target store schema id)]
                                       (ok
                                        (cg/upsert-color-scheme!
                                         store (assoc body :id nil :graph-id grid)))
                                       (refusal/refused :refusal.canvas/no-such-graph (:path-params req)))))}))

(def update-color-scheme
  (ep/handler :graph-scheme
              {:respond (on-graph (fn [{:keys [store schema id scheme-id body]}]
                                     (let [grid (->layout-target store schema id)
                                           su   (->uuid scheme-id)]
                                       (if (and grid su)
                                         (ok
                                          (cg/upsert-color-scheme!
                                           store (assoc body :id su :graph-id grid)))
                                         (refusal/refused :refusal.canvas/no-such-graph-invalid-scheme {:id id :scheme-id scheme-id})))))}))

(def delete-color-scheme
  (ep/handler :graph-scheme
              {:respond (on-graph (fn [{:keys [store scheme-id]}]
                                     (if-let [su (->uuid scheme-id)]
                                       (ok (delete-color-scheme! store su))
                                       (refusal/refused :refusal.canvas/invalid-uuid {:scheme-id scheme-id}))))}))

(op/defop :canvas/saved-graph [{:keys [store schema]} {:keys [handle]}]
  (resolve-graph-handle store schema handle))

(def module
  {:name        :canvas
   :contributes {
                 :viz/layouts layouts/built-in
                 :http/routes
                 {:explorer-dashboard {:get get-dashboard-layout :put save-dashboard-layout}
                  :graph              {:get get-graph}
                  :graph-commit       {:post commit-graph}
                  :graph-dot          {:get graph-dot}
                  :graph-layout       {:post upsert-layout}
                  :graph-open         {:get open-graph}
                  :graph-scheme       {:put update-color-scheme :delete delete-color-scheme}
                  :graph-schemes      {:get list-color-schemes :post create-color-scheme}
                  :graph-svg          {:get graph-svg}
                  :graphs             {:get list-graphs}
                  :view               {:get get-graph}
                  :view-commit        {:post commit-graph}
                  :view-layout        {:post upsert-layout}
                  :view-scheme        {:put update-color-scheme :delete delete-color-scheme}
                  :view-schemes       {:get list-color-schemes :post create-color-scheme}
                  :views              {:get list-graphs}}}})
