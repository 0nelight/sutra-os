(ns sutra.lib
  (:require [sutra.parser.ontology :as onto]
            [sutra.parser.resolve :as resolve]
            [sutra.parser.sugar :as sugar]
            [sutra.ref.expr :as rexpr]
            [sutra.ref.install :as ri]
            [sutra.ref.objectset :as ros]
            [sutra.ref.run :as run]
            [sutra.ref.sides :as sides]
            [sutra.ref.clock :as clock]
            [sutra.store.derived :as derived]
            [sutra.types.world-rules :as rules]
            [sutra.types.lifecycle :as lifecycle]
            [sutra.types.object-set :as os]))

(set! *warn-on-reflection* true)

(defn lower
  ([ent] (lower ent "<value>"))
  ([ent source]
   (when-let [extra (seq (remove #{:schema} (keys ent)))]
     (throw (ex-info (str "an ontology is {:schema …}; " (vec extra) " belong to the platform")
                     {:source source :keys (vec extra)})))
   (let [schema (-> (:schema ent) (onto/validate! source) sugar/lower-schema)]
     (resolve/check! schema source)
     schema)))

(defn install-steps
  [schema]
  (let [{:keys [objects links transforms object-sets actions]} schema]
    (vec (concat
          (for [[k spec] objects] (assoc spec :op :create-object-type :name k))
          (for [[k from to card opts] links]
            (merge opts {:op :create-link-type :name k :from from :to to :cardinality card}))
          (for [[k spec] transforms] (assoc spec :op :create-transform :name k))
          (for [[k spec] object-sets] (assoc spec :op :create-object-set :name k))
          (for [[k spec] actions] (assoc spec :op :create-action :name k))))))

(defn- step-tag [op] (keyword (subs (name op) (count "create-"))))

(defn- installed-names
  [W tags]
  (into {} (for [tag tags
                 :let [sec (:section (ri/class-by-tag tag))]]
             [tag (if-let [f (ri/name-field sec)]
                    (into #{} (keep f) (vals (get W sec)))
                    (set (keys (get W sec))))])))

(defn reinstall-steps
  [schema W]
  (let [steps (install-steps schema)
        tags  [:action :object-set :transform :link-type :object-type]
        known (installed-names W tags)
        named (into {} (for [tag tags]
                         [tag
                          (into #{} (comp (filter #(= tag (step-tag (:op %)))) (map :name))
                                steps)]))]
    (vec (concat
          (for [tag tags, n (sort (remove (named tag) (known tag)))]
            {:op (keyword (str "delete-" (name tag))) :name n})
          (for [{:keys [op name] :as step} steps
                :let [tag (step-tag op)]]
            (cond-> step
              (contains? (known tag)
                         name) (assoc :op (keyword (str "update-" (clojure.core/name tag))))))))))

(defn world
  ([schema] (world schema clock/system nil))
  ([schema clock] (world schema clock nil))
  ([schema clock installed]
   (let [r (if (seq installed)
             (run/run-rules (assoc clock :persisted (constantly installed)) {} installed {}
                            (reinstall-steps schema installed))
             (run/run-rules clock {} {} (install-steps schema)))]
     (if-let [e (:error r)]
       (throw (ex-info (str "install refused: " (:message e)) e))
       (:world r)))))

(defn type-rid
  [W nm]
  (some (fn [[rid ot]] (when (= nm (:object-type-name ot)) rid)) (:object-types W)))

(defn- with-lifecycle
  [W an action]
  (let [types (for [[_ ot] (:object-types W) :when (:lifecycle ot)] [(:object-type-name ot) ot])]
    (if-let [tr (and action (seq types) (lifecycle/transition-for types an))]
      (let [trid (type-rid W (:object-type tr))]
        (lifecycle/expand-action action (assoc tr :object-type trid)
                                 #(and (some? %) (= trid (if (keyword? %) (type-rid W %) %)))))
      action)))

(defn- action-named
  [W an]
  (or (get-in W [:actions an])
      (some (fn [[_ a]] (when (= an (:action-name a)) a)) (:actions W))))

(defn traverse
  [W ask self over]
  (let [{:keys [link dir far]} (sides/resolve-side W (:object-type-ref self) over)
        [near far-end] (if (= :to dir) [:from :to-object-id] [:to :from-object-id])
        ids (mapv far-end (ask [:links link {near [(str (:id self))]}]))]
    (if (seq ids)
      (let [by-id (into {} (map (juxt :id identity))
                        (ask [:select (os/qt->ir {:root far :where [:id-in ids]})]))]
        (into [] (keep by-id) ids))
      [])))

(declare query)

(defn sigma
  [W ask clock]
  (merge clock
         {:object     (fn [T id] (first (ask [:objects T [(str id)]])))
          :lookup     (fn [_W T {[k v] :key id :id}]
                        (ask [:select
                              (os/qt->ir (if k
                                           {:root T :where [:= k v] :limit 2}
                                           {:root T :where [:id-in [(str id)]] :limit 1}))]))
          :link-ends  (fn [_W L id]
                        (when-let [l (first (ask [:links L {:ids [(str id)]}]))]
                          {:from-id (:from-object-id l) :to-id (:to-object-id l)}))
          :traverse   (fn [self over] (traverse W ask self over))
          :query      (fn [W* os args installed] (query (or installed W*) ask os args))
          :object-key rules/object-key
          :action     (fn [W an] (with-lifecycle W an (action-named W an)))}))

(defn object-set-ir
  [W n args]
  (let [spec (or (ros/lookup W n)
                 (throw (ex-info (str "Unknown object set: " n) {:object-set n})))]
    (ros/instantiate (apply dissoc spec ros/resource-keys) args)))

(declare select)

(defn read-objects
  [W ask T ids]
  (if (empty? ids)
    []
    (let [ids   (mapv str ids)
          by-id (into {} (map (juxt (comp str :id) identity))
                      (select W ask {:op :static :root T :ids ids}))]
      (into [] (keep #(get by-id %)) ids))))

(defn select
  [W ask ir]
  (let [lowered (derived/plan W ir)]
    (when-let [[T a] (derived/unstated W lowered)]
      (throw (ex-info (str "the plan filters or orders on " a ", a derived property of " T
                           " the plan language cannot state — the store cannot compute it")
                      {:type T :attribute a})))
    (let [rows (ask [:select lowered])]
      (if-let [read (and (every? :object-type-ref rows) (derived/reader W ir))]
        (let [ctx   {:traverse (fn [self over] (traverse W ask self over))}
              eval* (fn [r V e] (rexpr/eval V e (assoc ctx :self r)))]
          (mapv #(read % (:values %) eval*) rows))
        rows))))

(defn query
  [W ask n args]
  (select W ask (object-set-ir W n args)))

(defn run-action
  ([W sg an args] (run-action W sg {} an args))
  ([W sg kappa an args] (run/run-action sg kappa W an args)))
