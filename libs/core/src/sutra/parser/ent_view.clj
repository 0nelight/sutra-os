(ns sutra.parser.ent-view
  (:require [sutra.kit.coll :as coll]
            [sutra.parser.ontology :refer [compile-grammar]]
            [sutra.parser.spec :as spec]
            [clojure.walk :as walk]
            [sutra.capps.derived :as cd]
            [sutra.capps.bind :as bind]
            [sutra.capps.catalog :as cat]
            [sutra.capps.intent :as ci]
            [sutra.capps.events :as ce]
            [sutra.types.set-query :as sq]
            [sutra.capps.refs :as refs]
            [sutra.capps.variables :as vars]
            [sutra.types.aggregate :as agg]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def ^:private sdk-container-kinds (set (map :kind cat/container-specs)))

(def ^:private sdk-widget-names (mapv :widget cat/widget-specs))

(def ^:private Ref
  (into [:or] (map (fn [k] [:map {:closed true} [k :keyword]])) (sort bind/variable-keys)))

(def ^:private WhereClause
  [:tuple [:or :keyword [:vector :keyword]]
   (into [:enum] (sort (concat (keys vars/where-ops) vars/row-ops
                               [:within-distance :within-bounding-box])))
   :any])

(def ^:private Expr
  [:schema {:registry
            {::expr [:or number? :string :boolean :nil Ref [:set :any]
                     [:cat (into [:enum :+ :- :* :/ :min :max :abs :round :if :coalesce]
                                 (sort (concat vars/string-ops vars/logic-ops vars/date-ops
                                               vars/array-ops vars/array-checks)))
                      [:* [:schema [:ref ::expr]]]]]}}
   [:ref ::expr]])

(def rollup-entries
  [[:over  :keyword]
   [:agg   (into [:enum] cd/aggs)]
   [:of    {:optional true} :keyword]
   [:by    {:optional true} :keyword]
   [:pick  {:optional true} :keyword]
   [:where {:optional true} :any]])

(def ^:private WithProperty
  (into [:map {:closed true}] rollup-entries))

(defn- agg-problem
  [{a :aggregate :keys [group-by]}]
  (when (map? a)
    (agg/explain-aggregate {:aggs     (:aggs a)
                            :group-by (or (:group-by a) group-by)})))

(def ^:private Recompute (into [:enum] (map :id) vars/recompute-modes))

(def ^:private VarSpec
  [:multi {:dispatch (fn [m] (cond (:aggregate m)                        :aggregate
                                   (:function m)                         :function
                                   (:compare m)                          :compare
                                   (:expr m)                             :expr
                                   (or (contains? m :default)
                                       (contains? m :default-agg)
                                       (:in-url m)
                                       (:url? m)
                                       (:input? m))                      :state
                                   :else                                 :derived))}
   [:state
    [:and
     [:map {:closed true}
      [:type        {:optional true} :keyword]
      [:default     {:optional true} :any]
      [:default-agg {:optional true} [:maybe [:vector :any]]]
      [:url?        {:optional true} :any]
      [:in-url      {:optional true} (into [:enum] (sort vars/in-url-behaviors))]
      [:input?      {:optional true} :boolean]
      [:external-id {:optional true} :keyword]
      [:object-type {:optional true} :keyword]
      [:as          {:optional true} (into [:enum] (sort vars/url-coercions))]
      [:push?       {:optional true} :boolean]]
     [:fn {:error/fn (fn [{:keys [value]} _]
                       (vars/interface-problem (:external-id value) value))}
      (fn [m] (nil? (vars/interface-problem (:external-id m) m)))]
     [:fn {:error/fn (fn [_ _]
                       (str "eine :input?-Variable braucht :type — der Wirt reicht"
                            " einen Wert herein, und die Schnittstelle prueft ihn"
                            " gegen diesen Typ (" (str/join " " (sort vars/var-types))
                            "). Ohne Typ gaebe es nichts zu pruefen"))}
      (fn [m] (or (not (:input? m)) (some? (:type m))))]]]
   [:aggregate [:and
                [:map {:closed true}
                 [:aggregate map?]
                 [:type {:optional true} :keyword]
                 [:external-id {:optional true} :keyword]
                 [:group-by {:optional true} :any]
                 [:recompute {:optional true} Recompute]
                 [:scenario {:optional true} Ref]]
                [:fn {:error/fn (fn [{:keys [value]} _] (:message (agg-problem value)))}
                 (fn [v] (nil? (agg-problem v)))]]]
   [:function  [:map {:closed true}
                [:type {:optional true} :keyword]
                [:external-id {:optional true} :keyword]
                [:function :keyword]
                [:args {:optional true} [:map-of :keyword :any]]
                [:recompute {:optional true} Recompute]
                [:scenario {:optional true} Ref]]]
   [:compare   [:map {:closed true}
                [:type {:optional true} :keyword]
                [:compare [:map {:closed true}
                           [:of :keyword]
                           [:scenarios Ref]
                           [:key {:optional true} :keyword]]]]]
   [:expr      [:map {:closed true}
                [:type {:optional true} :keyword]
                [:expr Expr]]]
   [:derived   [:map {:closed true}
                [:type     {:optional true} :keyword]
                [:external-id {:optional true} :keyword]
                [:object-property {:optional true} :keyword]
                [:source   {:optional true} :keyword]
                [:linked   {:optional true} :keyword]
                [:from     {:optional true} :keyword]
                [:where    {:optional true} [:vector WhereClause]]
                [:join     {:optional true} :any]
                [:with     {:optional true} [:map-of :keyword WithProperty]]
                [:coalesce {:optional true} :any]
                [:agg      {:optional true} [:vector :keyword]]
                [:pluck    {:optional true} :keyword]
                [:union    {:optional true} [:vector Ref]]
                [:of       {:optional true} :keyword]
                [:limit    {:optional true} pos-int?]
                [:order-by {:optional true} :keyword]
                [:sort-dir {:optional true} [:enum :asc :desc]]]]])

(def ^:private VarSpec+
  [:and
   VarSpec
   [:fn {:error/fn (fn [{:keys [value]} _] (vars/type-contradiction value))}
    (fn [m] (nil? (vars/type-contradiction m)))]
   [:fn {:error/fn (fn [{:keys [value]} _] (vars/definition-problem value))}
    (fn [m] (nil? (vars/definition-problem m)))]])

(def ^:private DataVars
  [:and
   [:map-of :keyword VarSpec+]
   [:fn {:error/fn (fn [{:keys [value]} _] (sq/vars-problem value))}
    (fn [m] (nil? (sq/vars-problem m)))]])

(def ^:private Box
  [:map {:closed true}
   [:icon  {:optional true} :string]
   [:help  {:optional true} :string]
   [:w     {:optional true} :any]
   [:min-w {:optional true} :any]
   [:max-w {:optional true} :any]
   [:h     {:optional true} :any]
   [:min-h {:optional true} :any]
   [:max-h {:optional true} :any]
   [:grow? {:optional true} :boolean]
   [:flex   {:optional true} [:and number? [:> 0]]]
   [:border {:optional true} [:enum :none :bordered :raised :inset]]])

(def ^:private Padding
  [:or [:enum :none :compact :regular :large]
   [:map {:closed true}
    [:level  {:optional true} [:enum :none :compact :regular :large]]
    [:top    {:optional true} [:int {:min 0 :max 400}]]
    [:right  {:optional true} [:int {:min 0 :max 400}]]
    [:bottom {:optional true} [:int {:min 0 :max 400}]]
    [:left   {:optional true} [:int {:min 0 :max 400}]]
    [:inner  {:optional true} [:enum :none :bordered :raised :inset]]]])

(def ^:private Background
  [:or [:enum :1 :2 :3 :4 :5 :transparent]
   [:re #"^(#([0-9a-fA-F]{3}|[0-9a-fA-F]{6})|[a-z-]+[1-5]|white|black)$"]])

(def ^:private HeaderColor [:or [:enum :1 :2 :3 :4 :5] :string])

(defn- items-of
  [item]
  [:multi {:dispatch (fn [x] (if (vector? x) :items :ref))}
   [:items [:vector item]]
   [:ref   Ref]])

(defn- closed-keys
  [& ks]
  (into [:map {:closed true}] (map (fn [k] [k {:optional true} :any])) ks))

(def ^:private ColSpec
  (closed-keys :label :prop :fmt :align :badge :cell-class :cell-style :render :list-role
               :conditional-format))

(def ^:private EditColSpec
  (closed-keys :label :prop :fmt :align :badge :cell-class :cell-style :render
               :edit :param))

(def ^:private Effect
  (into [:multi {:dispatch (fn [m] (if (and (map? m) (some? (ce/spec (:do m))))
                                     (:do m)
                                     ::unknown-do))}]
        (conj (mapv (fn [d]
                      [d (into [:map {:closed true} [:do [:= d]]]
                               (map (fn [{:keys [key required]}]
                                      (if required [key :any] [key {:optional true} :any])))
                               (ce/fields d))])
                    ce/dos)
              [::unknown-do
               [:fn {:error/message (str "unknown :do — an effect is one of "
                                         (str/join ", " (map str ce/dos)))}
                (constantly false)]])))

(def ^:private Event
  [:and
   [:multi {:dispatch (fn [x] (if (sequential? x) :many :one))}
    [:one  Effect]
    [:many [:vector Effect]]]
   [:fn {:error/fn (fn [{:keys [value]} _] (ce/message value))}
    (fn [x] (ce/sound? x))]])

(def ^:private Intent (into [:enum] ci/intents))

(def ^:private PropertyCard
  [:or :keyword
   [:and
    [:map {:closed true}
     [:prop  {:optional true} :keyword]
     [:link  {:optional true} :keyword]
     [:agg   {:optional true} [:enum :count :distinct-count :sum :avg :min :max]]
     [:of    {:optional true} :keyword]
     [:label {:optional true} :string]
     [:fmt   {:optional true} :keyword]
     [:icon  {:optional true} :string]
     [:conditional-format {:optional true} [:vector :map]]]
    [:fn {:error/message (str "a card shows a :prop (of this object, or with :link of the "
                              "one object a to-one link reaches), or a :link with :agg "
                              "(:count needs nothing more; :distinct-count, :sum, :avg, "
                              ":min and :max name the linked property in :of)")}
     (fn [{:keys [prop link agg of]}]
       (cond agg  (and link (not prop) (or (= :count agg) (some? of)))
             :else (and (some? prop) (nil? of))))]]])

(def ^:private widget-config-schemas
  {[:object-table :cols]     (items-of ColSpec)
   [:action-table :cols]     (items-of EditColSpec)
   [:kpi-row :items]         (items-of (conj (closed-keys :label :value :sub :accent :fmt
                                                          :description :conditional-format
                                                          :secondary :sparkline)
                                             [:on-click {:optional true} Event]))
   [:kpi-row :display]       [:enum :card :tag :list]
   [:kpi-row :direction]     [:enum :horizontal :vertical]
   [:kpi-row :template]      [:enum :stacked :side-by-side]
   [:kpi-row :size]          [:enum :compact :regular :large]
   [:kpi :display]           [:enum :card :tag]
   [:kpi :on-click]          Event
   [:button :intent]         Intent
   [:hyperlink :intent]      Intent
   [:hyperlink :url-type]    [:enum :property :hardcoded :templated]
   [:hyperlink :open-in]     [:enum :same-tab :new-tab :popup]
   [:hyperlink :hide-if-null?] :boolean
   [:action-button :intent]  Intent
   [:action-form :intent]    Intent
   [:action-table :intent]   Intent
   [:order-list :intent]     Intent
   [:export :intent]         Intent
   [:bars :items]            (items-of (closed-keys :label :frac :value :muted?))
   [:timeline :items]        (items-of (closed-keys :at :title :body :tone))
   [:tabs :items]            (items-of (closed-keys :id :label :count :badge :icon :on-click
                                                    :visible :when-false))
   [:tabs :design]           [:enum :outline :block :prominent :grouped]
   [:tabs :size]             [:enum :small :default :large]
   [:tabs :direction]        [:enum :horizontal :vertical]
   [:tabs :alignment]        [:enum :left :center]
   [:button-group :button-type] [:enum :inline :menu :two-part]
   [:button-group :intent]   Intent
   [:inline-action :layout]  [:enum :form :table]
   [:inline-action :invalid-state] [:enum :disabled :hidden]
   [:inline-action :on-done] Event
   [:qr-reader :mode]        [:enum :single :multiple]
   [:qr-reader :intent]      Intent
   [:media-uploader :intent] Intent
   [:media-uploader :capture] [:enum :environment :user]
   [:iframe :mode]           [:enum :url :messages :bidirectional]
   [:app-pairing :auto-open] [:enum :disabled :enabled :only-initially :only-on-change]
   [:stepper :steps]         (items-of (closed-keys :id :label))
   [:filter-pills :items]    (items-of (closed-keys :value :label :count))
   [:select-input :items]    (items-of (closed-keys :value :label :disabled? :on-select))
   [:select-input :on-change] Event
   [:combo-input :items]     (items-of (closed-keys :value :label))
   [:button :on-click]       Event
   [:action-button :on-done] Event
   [:action-form :on-done]   Event
   [:action-form :on-cancel] Event
   [:action-form :output]    [:map {:closed true} [:var :keyword]]
   [:action-button :output]  [:map {:closed true} [:var :keyword]]
   [:object-table :on-row-click] Event
   [:object-table :wrap?]    :boolean
   [:object-table :lines]    [:int {:min 1 :max 10}]
   [:object-table :frozen-cols] [:int {:min 0 :max 5}]
   [:object-table :fit-columns?] :boolean
   [:action-table :on-row-click] Event
   [:image-annotation :on-create] Event
   [:polygon :on-close]      Event
   [:polygon :on-shape-click] Event
   [:polygon :on-import]     Event
   [:polygon :extent]        (closed-keys :w :h)
   [:polygon :layers]        (items-of (closed-keys :rows :points :label :class
                                                    :labels? :tooltips? :conditional-format
                                                    :except :except-key))
   [:polygon :pick]          [:enum :points :geojson]
   [:property-list :props]   (items-of [:or :keyword (closed-keys :label :prop :fmt)])
   [:property-list :exclude] [:vector :keyword]
   [:property-list :label-position] [:enum :beside :below]
   [:property-list :columns] [:int {:min 1 :max 4}]
   [:property-cards :cards]  [:multi {:dispatch #(if (keyword? %) :prominent :cards)}
                              [:prominent [:= :prominent]]
                              [:cards (items-of PropertyCard)]]
   [:property-cards :display] [:enum :card :tag]
   [:object-view :interface] [:map-of :keyword [:or [:map {:closed true} [:var :keyword]]
                                                [:map {:closed true} [:val :keyword]]]]
   [:object-view :initial-tab] :keyword
   [:object-view :panel-behavior] [:enum :instance :adaptive :set]
   [:object-view :hide-tabs?] :boolean
   [:data-freshness :items]  [:vector
                              [:map {:closed true}
                               [:object-type [:or :keyword :string]]
                               [:sources {:optional true}
                                [:vector
                                 [:map {:closed true}
                                  [:resource [:or :keyword :string]]
                                  [:override-name {:optional true} :string]]]]]]
   [:object-view :hide-actions?] :boolean
   [:matrix :owner-type]     :keyword
   [:matrix :children-conn]  :keyword
   [:matrix :other-type]     :keyword
   [:object-view :initial-tab-on-switch?] :boolean})

(let [roster (into #{} (for [{:keys [widget params]} cat/widget-specs
                             {:keys [key]} params]
                         [widget key]))
      stray  (remove roster (keys widget-config-schemas))]
  (assert (empty? stray)
          (str "widget-config-schemas types a param the catalog does not have: "
               (pr-str (vec stray)))))

(def ^:private node-key-schemas
  {:title   [:maybe :string]
   :hidden? :boolean
   :min-size [:enum :mittel :breit :weit]
   :bg      Background
   :when    :any
   :vars    DataVars
   :box     Box
   :class   :string})

(def ^:private Binding
  [:and :map [:fn {:error/message "a binding form — {:val v}, {:item f}, …"} bind/reference?]])

(defn- spec-schema
  [{:keys [kind] :as p}]
  (when (= :bool kind)
    (if (= :any (cat/param-binds p)) [:or :boolean Binding] :boolean)))

(defn- by-key [params] (coll/index-by :key params))

(def ^:private node-params
  (by-key (concat cat/rider-params cat/common-params)))

(defn- leaf
  [{k :widget :keys [open? params]}]
  (let [ps (merge node-params (by-key params))]
    (into [:map {:closed (not open?)} [:widget [:= k]]]
          (map (fn [[ck p]] [ck {:optional true}
                             (or (node-key-schemas ck) (get widget-config-schemas [k ck])
                                 (spec-schema p) :any)]))
          ps)))

(defn- closest-names
  [x names]
  (let [t (name x)]
    (->> names
         (map (fn [n] (let [s (name n)]
                        [n (+ (* 3 (count (take-while true? (map = t s))))
                              (count (filter (set t) s))
                              (- (abs (- (count t) (count s)))))])))
         (sort-by (comp - second))
         (take 3)
         (map first))))

(defn unknown-name-message
  [what names x]
  (if (or (keyword? x) (string? x) (symbol? x))
    (str "unknown " what " " (pr-str x) " — meant " (str/join ", " (closest-names x names))
         "? one of: " (str/join ", " (sort (map name names))))
    (str what " is missing — one of: " (str/join ", " (sort (map name names))))))

(def ^:private WidgetMulti
  (into [:multi {:dispatch :widget
                 :error/fn (fn [{:keys [value]} _]
                             (unknown-name-message ":widget" sdk-widget-names
                                                   (when (map? value) (:widget value))))}]
        (map (fn [spec] [(:widget spec) (leaf spec)]))
        cat/widget-specs))

(def ^:private Widget* (delay (compile-grammar WidgetMulti)))

(def ^:private container-key-schemas
  {:subtitle [:maybe :string]
   :fill?    :boolean
   :device   [:enum :desktop :mobile]
   :items    [:multi {:dispatch (fn [x] (if (vector? x) :nodes :ref))}
              [:nodes [:vector [:ref ::node]]]
              [:ref   Ref]]
   :body     [:ref ::node]
   :tabs     [:vector [:ref ::pane]]
   :pages    [:vector [:ref ::pane]]
   :variants [:vector [:ref ::pane]]
   :view-as  [:vector [:map {:closed true}
                       [:id    :keyword]
                       [:label {:optional true} :string]]]
   :default  :any
   :header   [:map {:closed true}
              [:hidden?      {:optional true} :boolean]
              [:title        {:optional true} :string]
              [:title-color  {:optional true} :string]
              [:logo         {:optional true}
               [:map {:closed true}
                [:icon       {:optional true} :string]
                [:icon-color {:optional true} :string]
                [:image      {:optional true} :string]
                [:height     {:optional true} [:int {:min 8 :max 400}]]
                [:position   {:optional true}
                 [:enum :left :center :right :top :bottom]]
                [:collapsed-image {:optional true} :string]]]
              [:favorite?    {:optional true} :boolean]
              [:bg           {:optional true} HeaderColor]
              [:orientation  {:optional true} [:enum :horizontal :vertical]]
              [:height       {:optional true} [:int {:min 24 :max 400}]]
              [:width        {:optional true} [:int {:min 48 :max 800}]]
              [:collapsible? {:optional true} :boolean]
              [:collapsed?   {:optional true} :boolean]
              [:items        {:optional true} [:vector [:ref ::node]]]]
   :padding  Padding
   :select   Ref
   :selected Ref
   :detail   Ref
   :scenario Ref
   :key      :any
   :arrange  [:enum :rows :columns :flow :grid]
   :max-columns    [:int {:min 1 :max 12}]
   :min-card-width [:int {:min 80 :max 1200}]
   :tag      [:ref ::node]
   :group-by    :keyword
   :group-label :any
   :group-head  [:ref ::node]
   :sort     :any
   :limit    :int
   :empty    [:ref ::node]
   :scroll?  :boolean
   :overlay  :keyword
   :visible  Ref
   :as       [:enum :modal :drawer]
   :side     [:enum :left :right]
   :backdrop? :boolean
   :size     :any
   :on-close Event
   :title    [:maybe :string]
   :when     :any
   :hidden?  :boolean
   :collapse [:or :boolean [:map {:closed true} [:var :keyword]]]
   :bg       Background
   :class    :string
   :box      Box
   :vars     DataVars})

(def ^:private container-params
  (merge (by-key (concat (mapcat :children cat/container-specs) cat/rider-params
                         cat/common-params cat/root-params))
         (by-key (mapcat :params cat/container-specs))))

(let [stray (remove (set (keys container-params)) (keys container-key-schemas))]
  (assert (empty? stray)
          (str "container-key-schemas types a key the catalog does not have: "
               (pr-str (vec stray)))))

(defn- container-schema
  []
  (into [:map {:closed true} [:kind (into [:enum] sdk-container-kinds)]]
        (map (fn [[k p]] [k {:optional true}
                          (or (get container-key-schemas k) (spec-schema p) :any)]))
        container-params))

(defn data-node []
  [:schema
   {:registry
    {::node      [:multi {:dispatch (fn [m] (if (contains? m :kind) :container :widget))}
                  [:container [:ref ::container]]
                  [:widget    [:ref ::widget]]]
     ::widget    @Widget*
     ::pane      [:map {:closed true}
                  [:id         {:optional true} :keyword]
                  [:label      {:optional true} :string]
                  [:icon       {:optional true} :string]
                  [:visibility {:optional true}
                   [:map {:closed true}
                    [:roles    {:optional true} [:vector :keyword]]
                    [:profiles {:optional true} [:vector :keyword]]
                    [:when     {:optional true} :any]
                    [:link     {:optional true} spec/RefId]]]
                  [:count      {:optional true} [:or :int Ref]]
                  [:padding    {:optional true} Padding]
                  [:body  [:ref ::node]]]
     ::container (container-schema)}}
   ::node])

(defn- view-body [] (data-node))

(def view-factors
  [:full :panel :panel-set :mobile])

(defn body-vars-problem
  [body view]
  (when (map? body)
    (or (some->> (refs/problems body view) (str/join " · "))
        (sq/vars-problem (sq/layout-specs body)))))

(defn reference-message
  [ps]
  (when (seq ps)
    (str/join " · " (map (fn [{:keys [error path]}] (str error " — bei " (pr-str path))) ps))))

(defn- view-reference-problem
  [view]
  (or (reference-message
       (mapcat (fn [ff]
                 (map #(update % :path (partial into [ff]))
                      (cat/reference-problems (get view ff) {(refs/reserved-for ff) {}})))
               view-factors))
      (some (fn [ff]
              (some->> (body-vars-problem (get view ff) {:object-type (:object-type view)
                                                         :form-factor ff})
                       (str (name ff) ": ")))
            view-factors)))

(def ^:private SidebarItem
  (let [common [[:title {:optional true} :string]
                [:icon  {:optional true} :string]]]
    [:multi {:dispatch (fn [m] (cond (not (map? m))        ::other
                                     (contains? m :app)    :app
                                     (contains? m :action) :action
                                     (contains? m :url)    :url
                                     :else                 ::other))}
     [:app (into [:map {:closed true}
                  [:app    [:or :keyword :string]]
                  [:inputs {:optional true} [:map-of :keyword :any]]
                  [:mode   {:optional true} [:enum :card :compact]]]
                 common)]
     [:action (into [:map {:closed true}
                     [:action     :keyword]
                     [:parameters {:optional true} [:vector :keyword]]]
                    common)]
     [:url (into [:map {:closed true}
                  [:url    :string]
                  [:params {:optional true} [:map-of :keyword :keyword]]]
                 [[:title :string] [:icon {:optional true} :string]])]
     [::other [:fn {:error/message (str "a sidebar item is {:app …}, {:action …} or"
                                        " {:url \"…{{objectId}}…\" :title …}")}
               (constantly false)]]]))

(def ^:private Sidebar
  [:vector
   [:map {:closed true}
    [:title      :string]
    [:visibility {:optional true} [:map {:closed true}
                                   [:roles    {:optional true} [:vector :keyword]]
                                   [:profiles {:optional true} [:vector :keyword]]]]
    [:items      [:vector SidebarItem]]]])

(def LiveSpec
  [:or :boolean
   [:map {:closed true}
    [:types       {:optional true} [:vector spec/RefId]]
    [:object-sets {:optional true}
     [:vector [:or :keyword [:map {:closed true}
                             [:from spec/RefId]
                             [:where {:optional true} [:vector :any]]]]]]
    [:min-seconds {:optional true} [:and number? [:>= 1]]]]])

(defn- view-file []
  (let [body (view-body)]
    [:and
     [:map {:closed true}
      [:object-type spec/RefId]
      [:project     {:optional true} :keyword]
      [:auto-publish {:optional true} :boolean]
      [:sidebar     {:optional true} Sidebar]
      [:live        {:optional true} LiveSpec]
      [:aggregate-types {:optional true} [:vector spec/RefId]]
      [:functions   {:optional true} [:vector spec/RefId]]
      [:full        {:optional true} body]
      [:panel       {:optional true} body]
      [:panel-set   {:optional true} body]
      [:mobile      {:optional true} body]]
     [:fn {:error/message (str ":sidebar belongs to the full view — author :full beside it"
                               " (the applications sidebar shows on the full"
                               " Object View only)")}
      (fn [v] (or (not (:sidebar v)) (:full v)))]]))

(def ViewFile*
  (delay (compile-grammar
          [:and
           (view-file)
           [:fn {:error/fn (fn [{:keys [value]} _] (view-reference-problem value))}
            (fn [v] (nil? (view-reference-problem v)))]])))

(defn actions-in-layout
  [layout]
  (let [acc (volatile! [])]
    (walk/postwalk
     (fn [x]
       (when (map? x)
         (when (#{:action-button :action-form :action-table :comments :button-group
                  :inline-action :media-uploader :combo-input :number-input}
                (some-> (:widget x) name keyword))
           (doseq [{a :action} (cat/widget-action-entries x)
                   :when (or (keyword? a) (string? a))]
             (vswap! acc conj (keyword (name a))))))
       x)
     layout)
    (distinct @acc)))

(defn with-derived-actions
  [layout]
  (let [declared (vec (:actions layout))
        named    (set (map #(some-> (:action %) name keyword) declared))
        extra    (remove named (actions-in-layout layout))]
    (cond-> layout
      (seq extra) (assoc :actions (into declared (map (fn [a] {:action a})) extra)))))

(defn without-derived-actions
  [layout]
  (cond-> layout (map? layout) (dissoc :actions)))

(defn- with-sidebar-actions
  [body sidebar]
  (let [named (set (map #(some-> (:action %) name keyword) (:actions body)))
        extra (for [g sidebar it (:items g)
                    :let [a (:action it)]
                    :when (and a (not (named a)))]
                (cond-> {:action a} (seq (:parameters it)) (assoc :parameters (:parameters it))))]
    (cond-> body
      (seq extra) (assoc :actions (into (vec (:actions body)) (distinct) extra)))))

(defn entv->schema
  [{:keys [object-type project aggregate-types functions] :as raw}]
  (let [ot-kw (keyword (name object-type))]
    {:applications
     (into {}
           (for [factor view-factors
                 :let   [body (get raw factor)]
                 :when  body]
             [(keyword (str (name ot-kw) "-view-" (name factor)))
              (cond-> {:object-type object-type
                       :form-factor factor
                       :layout      (with-derived-actions
                                      (cond-> body
                                        (= :full factor) (with-sidebar-actions (:sidebar raw))
                                        (seq aggregate-types)
                                        (assoc :aggregate-types aggregate-types)
                                        (seq functions) (assoc :functions functions)))}
                (and (= :full factor) (seq (:sidebar raw))) (assoc :sidebar (:sidebar raw))
                (contains? raw :live) (assoc :live (:live raw))
                (:auto-publish raw) (assoc :auto-publish true)
                project (assoc :project project))]))}))
