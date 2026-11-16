(ns sutra-ui.state.catalog
  (:require [clojure.string :as str]
            [sutra-ui.state.db :as db]
            [sutra.types.rid :as rid]
            [sutra.types.schema-view :as sv]
            [sutra.types.schema-reach :as reach]
            [sutra.types.link-side :as ls]
            [sutra.kit.remote :as rd]
            [sutra.types.transform-io :as tio]
            [sutra.viz.lineage-projection :as lp]
            [sutra-ui.api :as api]
            [sutra-ui.feature :as feature]))

(defonce canonical-schema
  (db/cursor [:catalog :canonical-schema] {:object-types {} :link-types {} :actions {}}))

(defonce schema
  (db/cursor [:catalog :schema] {:objects {} :object-types {} :links [] :interfaces {}}))

(defonce schema-derived
  (db/cursor [:catalog :schema-derived] {:implementers {} :endpoint-types {}}))

(defonce
  rid->name (db/cursor [:catalog :rid->name] rd/loading))

(defonce name->rid (db/cursor [:catalog :name->rid] {}))

(defn failed
  [c sentence]
  (cond-> c sentence (update :rid->name rd/after-failure sentence)))

(defn name-of
  [x]
  (rid/resolve-rid->name (rd/value-or @rid->name {}) x))

(defn value-type-label
  [t]
  (cond
    (nil? t)     nil
    (keyword? t) (name t)
    (string? t)  t
    (vector? t)  (case (first t)
                   :array   (str "array<" (value-type-label (second t)) ">")
                   :decimal (str "decimal(" (second t) "," (nth t 2) ")")
                   (pr-str t))
    :else        (str t)))

(defn rid-of
  [kinds-key name-kw]
  (rid/resolve-name->rid @name->rid #{kinds-key} name-kw))

(defonce schema-version (db/cursor [:catalog :schema-version] 0))

(defn bump-version! [] (db/edit! schema-version inc))

(defn- views-of
  [canon]
  (into {} (for [[nm spec] (:graphs canon)]
             [(str (:id spec))
              {:id          (:id spec)
               :name        (clojure.core/name nm)
               :area        (:area spec)
               :mode        (:mode spec)
               :description (:description spec)
               :project     (:project spec)
               :branch      (:branch spec)}])))

(defn- with-schema-view
  [c]
  (-> c
      (assoc :schema (sv/view-shape (:canonical-schema c)))
      (update :schema-version inc)))

(defn adopt
  [c canon derived]
  (if-not canon
    c
    (let [kept (reach/keep-bodies (:canonical-schema c) (rid/name-keyed canon))]
      (-> c
          (assoc :rid->name (rid/rid->name-index canon)
                 :name->rid (rid/name->rid-index canon)
                 :canonical-schema kept
                 :hydrating #{}
                 :views (views-of kept)
                 :schema-derived (or derived {:implementers {} :endpoint-types {}}))
          with-schema-view))))

(defn apply-canonical!
  ([canon] (apply-canonical! canon nil))
  ([canon derived] (db/send! [:sutra.catalog/adopt canon derived])))

(db/reg :sutra.catalog/adopt
        (fn [db [_ canon derived]] {:db (update db :catalog adopt canon derived)}))

(db/reg :sutra.catalog/hydrate
        (fn [db [_ section nm]]
          (let [spec (get-in db [:catalog :canonical-schema section nm])
                rid  (some-> (:id spec) str)]
            (when (and rid (seq (get spec reach/elsewhere-key))
                       (not (contains? (get-in db [:catalog :hydrating]) rid)))
              {:db (update-in db [:catalog :hydrating] (fnil conj #{}) rid)
               :fx [[:call {:call [api/get! reach/body-endpoint {:rid rid}]
                            :ok   [:sutra.catalog/hydrated section nm]
                            :err  [:sutra.catalog/not-hydrated rid]}]]}))))

(db/reg :sutra.catalog/hydrated
        (fn [db [_ section nm resp]]
          {:db (update db :catalog
                       #(-> %
                            (update-in [:canonical-schema section nm]
                                       (fn [s] (reach/with-bodies section s (:spec resp))))
                            with-schema-view))}))

(db/reg :sutra.catalog/not-hydrated
        (fn [db [_ rid]] {:db (update-in db [:catalog :hydrating] disj rid)}))

(db/reg :sutra.catalog/views
        (fn [db _]
          {:db (update db :catalog #(assoc % :views (views-of (:canonical-schema %))))}))

(defn hydrate-body!
  [section nm]
  (db/send! [:sutra.catalog/hydrate section nm]))

(defn bodies-here?
  [section nm]
  (empty? (get-in @canonical-schema [section nm reach/elsewhere-key])))

(defn project-of
  [rid]
  (some-> rid name-of))

(defn link-id
  [{rid :id nm :name from :from to :to card :cardinality}]
  (or rid
      (str (name nm) "|" (name from) "|" (name to) "|" (name card))))

(defn link-spec
  [lname]
  (merge (get-in @canonical-schema [:link-types lname])
         (some #(when (= (:name %) lname) %) (:links @schema))))

(defn- side-label [lname c dir]
  (let [fwd? (= :to dir)
        lbl  (or (not-empty (get c (if fwd? :forward-name :reverse-name)))
                 (name (ls/api-name lname c dir)))
        self? (and (some? (:from c)) (= (:from c) (:to c)))
        twin  (or (not-empty (get c (if fwd? :reverse-name :forward-name)))
                  (name (ls/api-name lname c (if fwd? :from :to))))]
    (if (and self? (not fwd?) (= lbl twin)) (str lbl " ←") lbl)))

(defn link-side
  [lname dir]
  (let [c (link-spec lname)]
    {:api-name (keyword (name (ls/api-name lname c dir)))
     :link     lname
     :dir      dir
     :near     (if (= :to dir) (:from c) (:to c))
     :far      (if (= :to dir) (:to c) (:from c))
     :label    (side-label lname c dir)}))

(defn link-sides
  [sch type-kw]
  (let [tbl (into {} (for [{lname :name :keys [from to]} (:links sch)
                           :when lname]
                       [lname (merge (select-keys (link-spec lname)
                                                  [:forward-api-name :reverse-api-name])
                                     {:from from :to to})]))]
    (map #(link-side (:link %) (:dir %)) (ls/table-sides tbl type-kw))))

(defonce ^:private side-cache (atom [nil nil {}]))

(defn- cached-side [lname dir]
  (let [sch @schema canon @canonical-schema
        [s c m] @side-cache
        m (if (and (identical? s sch) (identical? c canon)) m {})]
    (or (get m [lname dir])
        (let [side (link-side lname dir)]
          (reset! side-cache [sch canon (assoc m [lname dir] side)])
          side))))

(defn edge-side
  [l id]
  (when-let [lname (name-of (:link-type-ref l))]
    (cached-side lname (if (= (str id) (str (:from-id l))) :to :from))))

(defn object-type-groups
  []
  (let [obj-names (set (keys (:object-types @schema)))
        keep-members (fn [spec]
                       (update spec :members
                               (fn [ms] (filterv obj-names (or ms [])))))]
    (reduce-kv (fn [acc g-kw spec] (assoc acc g-kw (keep-members spec)))
               {}
               (:object-type-groups @canonical-schema))))

(defn groups-of
  [obj-kw]
  (vec (for [[g-kw spec] (object-type-groups)
             :when (some #(= % obj-kw) (:members spec))]
         g-kw)))

(defn project-list
  []
  (:projects @canonical-schema))

(defn open-resource!
  [family nm]
  (hydrate-body! family nm)
  (let [spec (get-in @canonical-schema [family nm])]
    (when-let [o (or (feature/opener family spec) (feature/opener :default spec))]
      ((:open! o) nm spec))))

(defonce views        (db/cursor [:catalog :views] {}))

(defn graph-mode [v] (some-> (:mode v) keyword))

(defn graphs-of
  [mode]
  (filter #(= mode (graph-mode %)) (vals @views)))

(defn refresh-views! [] (db/send! [:sutra.catalog/views]))

(defn view-spec-by-rid
  [view-rid]
  (some (fn [[nm spec]] (when (= (str (:id spec)) (str view-rid))
                          (assoc spec :name nm)))
        (:graphs @canonical-schema)))

(defn object-view-app
  [object-type form-factor]
  (let [app (some (fn [[k spec]]
                    (when (and (= (some-> (:object-type spec) name keyword) object-type)
                               (= (:form-factor spec) form-factor))
                      (assoc spec :application-name k)))
                  (:applications @canonical-schema))]
    (when app (hydrate-body! :applications (:application-name app)))
    app))

(defn instance-display-name
  ([row] (instance-display-name row (get-in @schema [:object-types (:object-type row) :title-key])))
  ([row tk]
   (let [vs    (:values row)
         tv    (when tk (get vs tk))
         title (when (and (some? tv) (not (str/blank? (str tv)))) (str tv))
         nm    (or title
                   (some (fn [k]
                           (let [v (get vs k)]
                             (when (and (string? v) (not (str/blank? v))) v)))
                         [:name :hostname :port-id :title :label]))]
     (or nm (subs (str (:id row)) 0 8)))))

(def dataset-node-id       (partial lp/node-id :dataset))

(defn declared-actions
  ([] (declared-actions @canonical-schema))
  ([sch] (:actions sch)))

(defn node-columns
  [kind kw]
  (let [sch @canonical-schema
        nms (fn [xs k] (vec (keep #(some-> (get % k) name) xs)))]
    (case (keyword kind)
      :dataset     (nms (get-in sch [:datasets kw :columns]) :attribute-name)
      :object-type (nms (get-in sch [:object-types kw :attribute-list]) :attribute-name)
      :transform
      (let [spec (get-in sch [:transforms kw])
            outs (tio/outputs-of spec)]
        (vec (distinct
              (concat (vec (keep #(some-> % name) (:output-columns spec)))
                      (mapcat (fn [o]
                                (case (:kind o)
                                  :object-type (nms (get-in sch [:object-types (:object-type o)
                                                                 :attribute-list])
                                                    :attribute-name)
                                  :dataset     (nms (get-in sch [:datasets (:dataset o) :columns])
                                                    :attribute-name)
                                  []))
                              outs)))))
      [])))

(defn node-project
  [kind kw]
  (let [sch @canonical-schema
        sec (case (keyword kind)
              :dataset :datasets :object-type :object-types
              :transform :transforms :datasource :sources
              :action :actions :application :applications nil)]
    (or (some-> (get-in sch [sec kw :project]) name) "Default scope")))
