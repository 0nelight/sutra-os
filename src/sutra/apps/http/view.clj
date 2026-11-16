(ns sutra.apps.http.view
  (:require [sutra.kit.refusal :as refusal]
            [clojure.string :as str]
            [sutra.capps.walk :as walk]
            [sutra.capps.catalog :as cat]
            [sutra.capps.layout :as lay]
            [sutra.capps.standard-view :as csv]
            [sutra.capps.variables :as vars]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.kit.tree :as tree]
            [sutra.parser.sugar :as sugar]
            [sutra.apps.http.delivery :as delivery]
            [sutra.apps.http.standard-view :as sv :refer [app-row app-type-rid]]
            [sutra.http.endpoint :as ep]
            [sutra.http.responses :refer [ok negotiated]]
            [sutra.foundation.profile :as prof]
            [sutra.store.access.scope :as ctx]
            [sutra.store.access.substrate :as sub]
            [sutra.store.edge :as store-edge]
            [sutra.actions.api :as actions-api]
            [sutra.types.schema :as sch]
            [sutra.types.schema-reach :as reach]
            [sutra.objects.api :as objects]
            [sutra.catalog.grants :as grants]
            [sutra.access.api :as access]
            [sutra.store.world :as world]
            [sutra.catalog.lookup :as lookup]
            [sutra.types.world-rules :as rules]
            [sutra.types.link-side :as ls]
            [sutra.types.naming :as naming]
            [sutra.types.timeseries :as ts]
            [sutra.types.value-types :as vt]
            [sutra.apps.http.scope :refer [in-scope]]
            [sutra.apps.http.session :refer [app-body scenario-at with-app]]))

(set! *warn-on-reflection* true)

(defn- resolve-var-refs
  [form vals]
  (let [holes (volatile! false)
        out   (walk/postwalk
               (fn [x]
                 (if (and (map? x) (= 1 (count x))
                          (or (contains? x :val) (contains? x :var)))
                   (let [k (->kw (or (:val x) (:var x)))]
                     (if (contains? vals k)
                       (get vals k)
                       (do (vreset! holes true) x)))
                   x))
               form)]
    (if @holes ::unresolved out)))

(defn- module-derived-properties
  [a vals]
  (into {}
        (for [[t props] (get-in a [:layout :derived-properties])
              :let [pairs (for [[nm spec] (sort-by first props)
                                :let [w (when (:where spec) (resolve-var-refs (:where spec) vals))]
                                :when (not= ::unresolved w)
                                :let [ex (sugar/derived-property->expr
                                          (cond-> spec w (assoc :where w)))]
                                :when ex]
                            [(->kw nm) ex])]
              :when (seq pairs)]
          [(->kw t) (vec pairs)])))

(defn app-with-properties
  [a vals]
  (reduce
   (fn [acc [t pairs]] (update acc t (fnil into []) pairs))
   (module-derived-properties a vals)
   (for [node (tree/nodes (:layout a))
         [_ v-spec] (:vars node)
         :let [src  (some-> (:source v-spec) ->kw)
               with (:with v-spec)]
         :when (and src (map? with))
         :let [pairs (for [[nm spec] with
                           :let [w (when (:where spec) (resolve-var-refs (:where spec) vals))]
                           :when (not= ::unresolved w)]
                       [(->kw nm) (sugar/rollup->expr (cond-> spec w (assoc :where w)))])]
         :when (seq pairs)]
     [src (vec pairs)])))

(defn- per-type-projection
  [schema scope props]
  (into {}
        (mapcat (fn [rid]
                  (when-let [ot (or (get-in schema [:object-types rid])
                                    (when-let [i (get-in schema [:interfaces rid])]
                                      (assoc i :attribute-list (:property-list i)
                                             :object-type-name (:interface-name i))))]
                    [[rid (props ot)]])))
        (:type scope)))

(defn- app-property-types
  [schema scope]
  (per-type-projection
   schema scope
   (fn [ot]
     (into {}
           (mapcat (fn [{:keys [attribute-name attribute-type]}]
                     (let [t (ts/base-type attribute-type)]
                       [[attribute-name t] [(naming/column-key attribute-name) t]])))
           (:attribute-list ot)))))

(defn- app-property-display
  [schema scope]
  (per-type-projection
   schema scope
   (fn [ot]
     (into {}
           (comp (map-indexed vector)
                 (mapcat (fn [[i {:keys [attribute-name attribute-type opts]}]]
                           (let [e (cond-> {:type (ts/base-type attribute-type)
                                            :prop attribute-name
                                            :order i}
                                     (seq (:name opts))   (assoc :name (:name opts))
                                     (seq (:format opts)) (assoc :format (:format opts))
                                     (seq (:conditional-format opts))
                                     (assoc :conditional-format (:conditional-format opts))
                                     (:visibility opts)
                                     (assoc :visibility (->kw (:visibility opts)))
                                     (seq (:render-hints opts))
                                     (assoc :render-hints (mapv ->kw (:render-hints opts))))]
                             [[attribute-name e] [(naming/column-key attribute-name) e]]))))
           (vt/resolve-value-types (:attribute-list ot) (:value-types schema))))))

(defn- type-visible?
  [schema auth rid]
  (let [spec (get-in schema [:object-types rid])]
    (and (not (contains? (grants/hidden-object-types auth) rid))
         (grants/read-allowed? auth (:project spec) (:ignore-inherited spec)))))

(defn- link-targets
  [schema link-kw]
  (let [k (keyword (name link-kw))]
    (if-let [t (lookup/rid-of schema :object-types k)]
      [t]
      (when-let [l (lookup/rid-of schema :link-types k)]
        ((juxt :from-object-type :to-object-type) (get-in schema [:link-types l]))))))

(defn- link-visible?
  [schema auth link-kw]
  (or (nil? link-kw)
      (every? (partial type-visible? schema auth) (link-targets schema link-kw))))

(defn prune-layout-visibility
  [layout schema auth]
  (walk/postwalk
   (fn [x]
     (if (and (map? x) (contains? x :kind))
       (reduce (fn [m k]
                 (cond-> m
                   (walk/children? (get m k))
                   (assoc k (filterv #(link-visible? schema auth (get-in % [:visibility :link]))
                                     (get m k)))))
               x [:tabs :pages :variants :items])
       x))
   layout))

(defn- app-object-titles
  [schema scope]
  (per-type-projection schema scope :title-key))

(defn- app-object-keys
  [schema scope]
  (into {}
        (mapcat (fn [rid]
                  (when-let [ks (and (get-in schema [:object-types rid])
                                     (:properties (rules/object-key schema rid)))]
                    [[rid ks]])))
        (:type scope)))

(defn- app-object-names
  [schema scope]
  (per-type-projection schema scope #(some-> (:object-type-name %) name)))

(defn- object-views-in
  [apps ->rid]
  (vec (for [[_ v] apps
             :when (:form-factor v)
             :let [rid (->rid (:object-type v))]
             :when rid]
         [rid v])))

(defn- app-object-views
  [auth schema scope]
  (let [by-rid (reduce (fn [m [rid v]]
                         (if (= :mobile (->kw (:form-factor v)))
                           m
                           (update m rid (fnil conj []) v)))
                       {}
                       (sch/column schema ::object-views
                                   #(object-views-in (:applications %)
                                                     (fn [t] (lookup/->type-rid
                                                              % :object-types t)))))]
    (into {}
          (mapcat (fn [rid]
                    (when-let [for-t (seq (get by-rid rid))]
                      (let [views (into {}
                                        (for [v for-t]
                                          [(->kw (:form-factor v))
                                           (prune-layout-visibility
                                            (lay/lower-view-body (:layout v))
                                            schema auth)]))]
                        [[rid views]]))))
          (:type scope))))

(defn view-reach
  [auth schema rids]
  (let [hidden (grants/hidden-object-types auth)
        views  (reduce (fn [m [r v]] (update m r (fnil conj []) (:layout v)))
                       {}
                       (sch/column schema ::object-views
                                   #(object-views-in (:applications %)
                                                     (fn [t] (lookup/->type-rid
                                                              % :object-types t)))))
        named  (fn [r] (into #{}
                             (comp (mapcat cat/referenced-object-types)
                                   (keep #(lookup/->type-rid schema :object-types %))
                                   (remove hidden))
                             (get views r)))]
    (set (tree/reach named rids))))

(defn app-view-rids
  [auth schema a]
  (view-reach auth schema
              (into #{} (keep #(some->> % (app-type-rid schema)))
                    (cons (:object-type a) (get-in a [:layout :object-types])))))

(defn view-layouts
  [schema rids]
  (for [[_ v] (:applications schema)
        :when (and (:form-factor v) (contains? rids (app-type-rid schema (:object-type v))))]
    (:layout v)))

(defn with-view-reach
  [auth schema scope]
  (let [reach (view-reach auth schema (:type scope))]
    (-> scope
        (update :type (fnil into #{}) reach)
        (update :object-type (fnil into #{}) reach))))

(defn- view-source-rids
  [auth schema layouts]
  (let [hidden (grants/hidden-object-types auth)]
    (into #{}
          (comp (mapcat tree/nodes)
                (keep :vars)
                (filter map?)
                (mapcat vals)
                (filter map?)
                (keep :source)
                (keep #(lookup/->type-rid schema :object-types (->kw %)))
                (remove hidden))
          layouts)))

(defn- app-object-rows
  [schema scope]
  (into {}
        (remove (comp nil? val))
        (per-type-projection schema scope #(some->> (:row %) seq (mapv ->kw)))))

(defn scope-tables
  [auth schema scope]
  {:property-types   (app-property-types schema scope)
   :property-display (app-property-display schema scope)
   :object-titles    (app-object-titles schema scope)
   :object-keys      (app-object-keys schema scope)
   :object-names     (app-object-names schema scope)
   :object-views     (app-object-views auth schema scope)
   :standard-views   (sv/app-standard-views auth schema scope)
   :object-rows      (app-object-rows schema scope)})

(defn- set-panel-want
  [schema scope layout]
  (when (some #(and (= :object-view (->kw (:widget %)))
                    (#{:adaptive :set} (->kw (:panel-behavior %))))
              (tree/nodes layout))
    (let [display (app-property-display schema scope)
          titles  (app-object-titles schema scope)]
      (into #{}
            (mapcat (fn [rid]
                      (let [d (get display rid)]
                        (concat (csv/set-groups d) (csv/set-cols d (get titles rid))))))
            (:type scope)))))

(defn- action-default-properties
  [schema layout]
  (into #{}
        (comp (mapcat #(vals (:params (lookup/action schema %))))
              (keep #(let [d (:default %)] (when (map? d) (some-> (:property d) ->kw)))))
        (cat/used-actions layout)))

(defn- layout-want
  [schema layout]
  (into (cat/referenced-properties layout) (action-default-properties schema layout)))

(defn app-row-want
  [auth schema scope a]
  (when-not (:source a)
    (-> (layout-want schema (:layout a))
        (into (set-panel-want schema scope (:layout a)))
        (into (mapcat val) (app-object-rows schema scope))
        (into (comp (mapcat (comp :links val)) (mapcat :cols) (map ->kw))
              (sv/app-standard-views auth schema scope)))))

(defn app-read-want
  [auth schema scope a]
  (some-> (app-row-want auth schema scope a)
          (into (mapcat #(layout-want schema %))
                (->> (app-object-views auth schema scope) vals (mapcat vals) distinct))))

(defn app-read
  [store branch sb f]
  (ep/branch-read (scenario-at store branch sb) f))

(defn- view-scope
  [schema layouts]
  {:types (into #{} (comp (mapcat cat/referenced-object-types)
                          (keep #(app-type-rid schema %)))
                layouts)
   :want  (into #{} (mapcat #(layout-want schema %)) layouts)})

(defn- side-hops
  [auth schema root-rid types sides]
  (let [hidden    (grants/hidden-object-types auth)
        ifaces-of (fn [far]
                    (into [] (comp (filter types)
                                   (keep #(get-in schema [:interfaces % :interface-name]))
                                   (map ->kw))
                          (get-in schema [:object-types far :implements])))]
    (for [[k lt] (:link-types schema)
          [dir far] [(when (= (:from-object-type lt) root-rid)
                       [:to (:to-object-type lt)])
                     (when (= (:to-object-type lt) root-rid)
                       [:from (:from-object-type lt)])]
          :when far
          :let  [ifaces (ifaces-of far)
                 side   (ls/api-name k lt dir)]
          :when (and (or (contains? types far) (seq ifaces) (contains? sides (->kw side)))
                     (not (contains? hidden far)))]
      {:far far :ifaces ifaces :side side})))

(defn- read-hop
  [query root-name id want {:keys [far side] :as hop}]
  (let [{:keys [object-type rows]}
        (query {:find root-name :ids [(str id)] :want want :around [side]})]
    (assoc hop :object-type object-type :rows (mapv #(app-row far %) rows))))

(defn- objects-by-type
  [types hops]
  (reduce (fn [acc {:keys [far ifaces object-type rows]}]
            (reduce (fn [acc k] (update acc k (fnil into []) rows))
                    acc (cond-> ifaces (contains? types far) (conj (->kw object-type)))))
          {}
          (filter (comp seq :rows) hops)))

(defn- linked-chain
  [schema vs spec depth]
  (when (and (:linked spec) (< depth 8))
    (when-let [rid (app-type-rid schema (:linked spec))]
      (if-some [of (some-> (:of spec) ->kw)]
        (let [up (get vs of)]
          (when (and up (empty? (:where up)) (nil? (:join up)))
            (some-> (linked-chain schema vs up (inc depth)) (conj rid))))
        [rid]))))

(defn- linked-chains
  [schema layouts]
  (into {}
        (for [vs   (keep :vars (mapcat tree/nodes layouts))
              :let [vs (update-keys vs ->kw)]
              spec (vals vs)
              :when (and (map? spec) (:linked spec) (:of spec))
              :let [chain (linked-chain schema vs spec 0)]
              :when chain]
          [(vars/linked-key (:of spec) (:linked spec)) chain])))

(defn- distinct-by-id [rows]
  (first (reduce (fn [[acc seen] r]
                   (if (seen (:id r)) [acc seen] [(conj acc r) (conj seen (:id r))]))
                 [[] #{}] rows)))

(defn- chained-objects
  [query auth schema root-rid id want layouts]
  (let [root-name (get-in schema [:object-types root-rid :object-type-name])
        want      (or (not-empty want) #{:id})]
    (into {}
          (for [[k chain] (linked-chains schema layouts)
                :let [sides (for [[a b] (map vector (cons root-rid chain) chain)]
                              (map :side (side-hops auth schema a #{b} nil)))
                      paths (reduce (fn [ps ss] (for [p ps s ss] (conj p s))) [[]] sides)
                      far   (peek chain)]]
            [k (distinct-by-id
                (for [p paths
                      r (:rows (query {:find root-name :ids [(str id)] :want want :around p}))]
                  (app-row far r)))]))))

(defn- app-around-objects
  [query auth schema root-rid id {:keys [types want]} layouts]
  (let [root-name (get-in schema [:object-types root-rid :object-type-name])
        reads     (mapv #(prof/span-future (read-hop query root-name id want %))
                        (side-hops auth schema root-rid types nil))]
    (merge (objects-by-type types (map deref reads))
           (chained-objects query auth schema root-rid id want layouts))))

(defn- door-neighbourhood
  [store sch auth rid id {:keys [types want link-sides layouts]}]
  (let [root-name (get-in sch [:object-types rid :object-type-name])
        want      (or (not-empty want) #{:id})
        [hops chained]
        (sub/with-read
          store
          (fn [store]
            (let [q (objects/object-query store sch)]
              [(mapv #(read-hop q root-name id want %)
                     (side-hops auth sch rid types link-sides))
               (chained-objects q auth sch rid id want layouts)])))]
    {:objects (merge (objects-by-type types hops) chained)
     :links   (into {} (keep (fn [{:keys [side rows]}]
                               (when (contains? link-sides (->kw side)) [(->kw side) rows])))
                    hops)}))

(defn- view-link-apis
  [layouts]
  (let [nodes (mapcat tree/nodes layouts)]
    (into (into #{}
                (comp (filter #(and (= 1 (count %)) (keyword? (:from %))
                                    (= "link" (namespace (:from %)))))
                      (map #(keyword (name (:from %)))))
                nodes)
          (comp (filter #(= :property-cards (->kw (:widget %))))
                (mapcat #(let [cs (:cards %)] (when (sequential? cs) cs)))
                (keep #(when (map? %) (some-> (:link %) ->kw))))
          nodes)))

(defn around-answer
  [{:keys [schema branch seg sb scope store auth]} spec where]
  (let [scope (with-view-reach auth schema scope)
        t    (:type spec)
        id   (some-> (:id spec) str str/trim not-empty)
        root (in-scope auth schema scope :object-type t)
        standard? (true? (:standard spec))
        views (when root (vals (get (app-object-views auth schema scope) root)))]
    (cond
      (or (nil? t) (nil? id))
      [400 {:error ":type and :id are required — the object whose neighbourhood to read"}]

      (nil? root)
      [404 {:error (str "no object type " (pr-str t) " in this " where)}]

      standard?
      (app-read store branch sb
                (fn [store sch]
                  [200 (merge {:object-type
                               (get-in schema [:object-types root :object-type-name])
                               :id          id
                               :objects     {}
                               :scenario    seg}
                              (sv/app-standard-around
                               (objects/object-query store sch) (grants/read-rights store)
                               sch scope root id))]))

      (empty? views)
      [404 {:error (str "this " where " ships no object view for " (pr-str t))}]

      :else
      (app-read store branch sb
                (fn [store sch]
                  (let [apis    (view-link-apis views)
                        q       (objects/object-query store sch)
                        tname   (get-in schema [:object-types root :object-type-name])
                        objects (prof/span-future (app-around-objects q auth sch root id
                                                                      (view-scope sch views)
                                                                      views))
                        own     (prof/span-future
                                 (mapv #(app-row root %)
                                       (:rows (q {:find tname :ids [id] :limit 1}))))
                        {:keys [answers answers-same inputs truncated refused]}
                        (delivery/view-answers store sch (grants/hidden-object-types auth)
                                               views
                                               (delay (cond-> @objects
                                                        (seq @own) (assoc (->kw tname) @own
                                                                          vars/view-object @own)))
                                               (:vars spec))
                        objects @objects
                        own     @own]
                    [200 (cond-> {:object-type tname
                                  :id          id
                                  :object      (first own)
                                  :objects     objects
                                  :answers     answers
                                  :inputs      (vec inputs)
                                  :scenario    seg}
                           (seq answers-same) (assoc :answers-same answers-same)
                           (seq truncated) (assoc :_truncated truncated)
                           (seq refused)   (assoc :_refused refused)
                           (seq apis)
                           (assoc :links
                                  (sv/standard-link-groups
                                   q (grants/read-rights store) sch scope root id
                                   apis)))]))))))

(defn standard-around?
  [{:keys [schema scope auth]} t form-factor]
  (let [scope (with-view-reach auth schema scope)
        root  (in-scope auth schema scope :object-type t)]
    (nil? (get-in (app-object-views auth schema scope) [root form-factor]))))

(defn scoped-around
  [req {:keys [body] :as ctx} where]
  (let [[status answer] (around-answer ctx body where)]
    (negotiated req status answer)))

(defn- view-specs
  [auth schema scope t]
  (let [root (in-scope auth schema scope :object-type t)]
    (reduce (fn [acc n] (merge acc (:vars n))) {}
            (mapcat tree/nodes (vals (get (app-object-views auth schema scope) root))))))

(defn- set-scope
  [{:keys [auth schema scope a]} object]
  (let [scope  (with-view-reach auth schema scope)
        hidden (grants/hidden-object-types auth)]
    {:specs  (if object
               (view-specs auth schema scope (:type object))
               (delivery/layout-vars a))
     :rid-of (fn [root]
               (let [r (if object
                         (or (lookup/->type-rid schema :object-types root)
                             (lookup/->type-rid schema :interfaces root))
                         (delivery/bound-rid schema a root))]
                 r))
     :hidden hidden}))

(defn my-application-facets
  [req]
  (with-app req {:seg :query :preview? true}
    (fn [{:keys [branch sb body store] :as app}]
      (let [{nm :var :keys [want vars object describe]} body
            {:keys [specs rid-of hidden]} (set-scope app object)]
        (if (str/blank? (str nm))
          (negotiated req 400 {:error ":var names the set to read"})
          (app-read store branch sb
                    (fn [store sch]
                      (let [{:keys [refused] :as a}
                            (delivery/facet-set store sch rid-of hidden specs nm want
                                                (update-keys (or vars {}) ->kw)
                                                (true? describe))]
                        (if refused
                          (negotiated req 400 {:error refused})
                          (negotiated req 200 a))))))))))

(defn- uses-widget?
  [a widget]
  (boolean (some #(= widget (some-> (:widget %) ->kw))
                 (tree/nodes (dissoc (:layout a) :vars)))))

(defn- in-groups-fn
  [store schema groups]
  (when (seq groups)
    (let [gs (into #{} (map str) groups)]
      (fn [id]
        (world/with-snapshot (world/at store access/live-branch)
          (fn [s] (some (comp gs second) (access/memberships s schema id))))))))

(defn my-application-users
  [req]
  (with-app req {:seg :query :preview? true}
    (fn [{:keys [body store a schema]}]
      (if-not (or (uses-widget? a :user-select) (uses-widget? a :comments))
        (negotiated req 403 {:error "this application shows no user selector"})
        (let [member? (in-groups-fn store schema (:groups body))]
          (negotiated req 200
                      {:users (into []
                                    (comp (filter :user-object-id)
                                          (filter #(or (nil? member?)
                                                       (member? (:user-object-id %))))
                                          (map (fn [p] {:id    (str (:user-object-id p))
                                                        :label (str (:username p))})))
                                    (access/principal-rows store))}))))))

(defn my-application-search
  [req]
  (with-app req {:seg :query :preview? true}
    (fn [{:keys [branch sb body store] :as app}]
      (let [{nm :var :keys [term fields show vars object]} body
            {:keys [specs rid-of hidden]} (set-scope app object)]
        (if (str/blank? (str nm))
          (negotiated req 400 {:error ":var names the set to search"})
          (app-read store branch sb
                    (fn [store sch]
                      (let [{:keys [rows total refused]}
                            (delivery/search-set store sch rid-of hidden specs nm (str term)
                                                 fields (update-keys (or vars {}) ->kw)
                                                 (into #{} (map ->kw) (concat fields show)))]
                        (if refused
                          (negotiated req 400 {:error refused})
                          (negotiated req 200 {:rows rows :total total}))))))))))

(defn- object-page
  [auth staged rid form]
  (when-let [a (let [of (fn [ff] (first (for [[_ v] (:applications staged)
                                               :when (and (= ff (->kw (:form-factor v)))
                                                          (= rid (app-type-rid
                                                                  staged (:object-type v))))]
                                           v)))]
                 (or (when (= :mobile (->kw form)) (of :mobile)) (of :full)))]
    (cond-> {:app (apply dissoc a (reach/bodies-of :applications))}
      (seq (:layout a))
      (assoc :view (prune-layout-visibility (lay/lower-view-body (:layout a)) staged auth)))))

(defn- door-want
  [schema near layouts info own?]
  (cond-> (into (:want (view-scope schema layouts))
                (mapcat val) (app-object-rows schema near))
    :always    (into (keep (fn [[_ t]] (some-> t ->kw))) (app-object-titles schema near))
    (not own?) (into (comp (mapcat :cols) (map ->kw)) (:links info))))

(defn object-view-standard
  [{store :sutra/store auth :sutra/auth schema :sutra/schema branch :sutra/branch
    edge :sutra/edge :as req}]
  (let [{:keys [type id vars standard form]} (app-body req)
        hidden (grants/hidden-object-types auth)
        rid    (some->> type ->kw (lookup/->type-rid schema :object-types))
        id     (some-> id str str/trim not-empty)]
    (cond
      (or (nil? type) (nil? id))
      (refusal/refused :refusal.apps/type-and-id-are-required {:type type})

      (or (nil? rid) (contains? hidden rid))
      (refusal/refused :refusal.apps/unknown-object-type-2 {} {:type type})

      :else
      (let [scope {:type (into #{} (remove hidden) (keys (:object-types schema)))}
            views (vals (get (app-object-views auth schema {:type #{rid}}) rid))
            reach (view-reach auth schema #{rid})
            near  {:type (-> (into #{rid} (map :far) (sv/standard-sides auth schema scope rid))
                             (into reach)
                             (into (view-source-rids auth schema views)))}
            info  (sv/standard-view-info auth schema scope rid)
            staged (actions-api/staging-schema
                    (ctx/at store (:read (store-edge/staging-view edge))))
            page   (object-page auth staged rid form)
            layouts (cond-> (vec views) (:view page) (conj (:view page)))
            own?   (boolean (and (:view page) (not (true? standard))))
            want   (door-want schema near layouts info own?)]
        (ep/branch-read (ctx/at store branch)
                        (fn [store sch]
                          (let [q     (objects/object-query store sch)
                                tname (get-in schema [:object-types rid :object-type-name])
                                {:keys [objects links]}
                                (door-neighbourhood
                                 store sch auth rid id
                                 {:types      (if own?
                                                (:types (view-scope schema layouts))
                                                (:type near))
                                  :want       want
                                  :layouts    (when own? layouts)
                                  :link-sides (if own?
                                                (view-link-apis layouts)
                                                (into #{} (map :api)
                                                      (sv/standard-sides
                                                       (grants/read-rights store) sch scope
                                                       rid)))})
                                own   (mapv #(app-row rid %)
                                            (:rows (q {:find tname :ids [id] :limit 1})))
                                {:keys [answers answers-same inputs truncated refused]}
                                (delivery/view-answers store sch hidden views
                                                       (cond-> objects
                                                         (seq own) (assoc (->kw tname) own
                                                                          vars/view-object own))
                                                       vars)]
                            (ok (merge (scope-tables auth schema near)
                                       {:object-type    (:type info)
                                        :id             id
                                        :object         (first own)
                                        :objects        objects
                                        :links          links
                                        :answers        answers
                                        :inputs         (vec inputs)
                                        :standard-view  info
                                        :object-page    page
                                        :viewer         {:role (get-in req [:principal :role])}
                                        :object-views   (app-object-views auth schema {:type reach})
                                        :standard-views (sv/app-standard-views
                                                         auth schema scope reach)}
                                       (when (seq answers-same)
                                         {:answers-same answers-same})
                                       (when (seq truncated) {:_truncated truncated})
                                       (when (seq refused) {:_refused refused}))))))))))
