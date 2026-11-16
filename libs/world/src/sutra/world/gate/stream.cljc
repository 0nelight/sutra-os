(ns sutra.world.gate.stream
  (:require [sutra.world.db :as db]
            [sutra.world.gate.base :as b]
            [sutra.world.index :as ix]
            [sutra.world.view :as view]
            [sutra.types.entities :as ents]
            [sutra.types.refs :as refs]))

(defn- unscoped
  [db sec x]
  (cond
    (nil? x)                                  nil
    (contains? (get (db/world db) sec) x)     x
    :else
    (let [hits   (db/keys-of (db/nodes db :by-name-str [sec (ix/nm x)]))
          scopes (map #(db/scope-of db (db/entry db [sec %])) hits)]
      (cond (empty? hits)        nil
            (apply = scopes)     (last hits)
            :else                (let [xs (keep (fn [[k s]] (when (nil? s) k))
                                                (map vector hits scopes))]
                                   (when (= 1 (count xs)) (first xs)))))))

(defn- stream-dataset [db x]
  (when-let [r (unscoped db :datasets x)]
    (when (:stream (db/entry db [:datasets r])) r)))

(defn- derive-of [e] (get-in e [:stream :derive]))

(defn- type-outputs [e]
  (filter #(and (map? %) (= :object-type (:kind %))) (ix/outputs-of e)))

(defn- authored-outputs
  [e]
  (filter map? (ix/outputs-of e)))

(defn- edited-types
  [a]
  (distinct (for [[targets v] (refs/ref-values ents/action-edit-refs a)
                  :when (= :object-types (first targets))]
              v)))

(def ^:private stream-refs
  {:object-types (fn [e] (when-some [d (get-in e [:backing :dataset])] [[:backing (ix/nm d)]]))
   :transforms   (fn [e] (concat (for [o (type-outputs e) :when (some? (:dataset o))]
                                   [:out-dataset (ix/nm (:dataset o))])
                                 (for [o (authored-outputs e) :when (some? (:object-type o))]
                                   [:supplies (ix/nm (:object-type o))])))
   :actions      (fn [e] (for [v (edited-types e) :when (some? v)] [:edits (ix/nm v)]))})

(def fact-rows
  [{:id :derives :sections #{:datasets}
    :facts (fn [_ e] (let [{:keys [from function]} (derive-of e)]
                       (cond-> []
                         (some? from)     (conj [:from (ix/nm from)])
                         (some? function) (conj [:function (ix/nm function)]))))}
   {:id :stream-refs :sections (set (keys stream-refs))
    :facts (fn [sec e] ((get stream-refs sec) e))}])

(defn- derive-problem [db ds]
  (when-let [e (db/entry db [:datasets ds])]
    (let [{:keys [from function] :as d} (derive-of e)
          src (when from (stream-dataset db from))
          why (cond
                (nil? d)   nil
                (nil? src) (str ":derive :from " (pr-str from) " names no stream")
                (= src ds) ":derive reads the stream it writes"
                (not (when function (unscoped db :functions function)))
                (str ":derive :function " (pr-str function) " names no function"))]
      (when why [{:dataset (or (:dataset-name e) ds) :reason why}]))))

(defn- deriving
  [db0 db1 [sec k]]
  (when-let [fact ({:datasets :from :functions :function} sec)]
    (for [n (into (b/names-of db0 sec k) (b/names-of db1 sec k))
          d [db0 db1]
          [_ ds] (b/facts d :derives [fact n])]
      ds)))

(def ^:private stream-derivations-valid
  {:id :stream-derivations-valid? :rule :dataset/stream-derive :scope :local
   :keys (fn [db0 db1 changes]
           (let [moved (into (set (map first changes)) (b/scope-dependents db0 db1 changes))]
             (-> (view/touched changes :datasets)
                 (into (mapcat #(deriving db0 db1 %)) moved))))
   :all (b/section-keys :datasets)
   :check derive-problem})

(def ^:private consistencies #{:exactly-once :at-least-once})

(def ^:private timestamp-types #{:timestamp :datetime :instant :date-time})

(def ^:private attribute-lists [:attribute-list :attributes :property-list :properties :columns])

(defn- attribute-type
  [e a]
  (some (fn [item]
          (let [[n t] (cond (map? item)        [(:attribute-name item) (:attribute-type item)]
                            (sequential? item) item
                            :else              [item nil])]
            (when (= n a) t)))
        (some #(not-empty (get e %)) attribute-lists)))

(defn- primary-dataset
  [db ot]
  (let [outs (for [k (db/keys-of (db/nodes db :edges [:output :object-type ot]))
                   o (type-outputs (db/entry db [:transforms k]))
                   :when (= ot (:object-type o))]
               o)]
    (:dataset (or (first (remove #(= :supplement (:role %)) outs)) (first outs)))))

(defn- settings
  [db ot]
  (when-let [e (db/entry db [:object-types ot])]
    (let [bk (:backing e)
          ds (if bk (:dataset bk) (primary-dataset db ot))]
      (when-let [s (some->> ds (stream-dataset db))]
        {:stream           s
         :direct?          (true? (:direct bk))
         :consistency      (or (some-> (:consistency bk) keyword) :exactly-once)
         :source-timestamp (some-> (:source-timestamp bk) keyword)}))))

(defn- type-key [db t]
  (if (contains? (get (db/world db) :object-types) t) t (unscoped db :object-types t)))

(defn- shown [db ot]
  (or (when (db/rid? ot) (some-> (db/entry db [:object-types ot]) :object-type-name)) ot))

(defn- setting-problem [db ot]
  (when-let [e (db/entry db [:object-types ot])]
    (let [bk  (:backing e)
          s   (settings db ot)
          ts  (some-> (:source-timestamp bk) keyword)
          t   (when ts (attribute-type e ts))
          why (cond
                (and (nil? s) (some #(contains? bk %) [:direct :consistency :source-timestamp]))
                (str "sets :direct, :consistency or :source-timestamp on its :backing,"
                     " which only a stream-backed type takes")

                (and s (contains? bk :consistency)
                     (not (contains? consistencies (keyword (:consistency bk)))))
                (str ":consistency " (pr-str (:consistency bk)) " is none of "
                     (pr-str (sort consistencies)))

                (and ts (not (:direct? s)))
                (str ":source-timestamp orders direct writes — set :direct true on the"
                     " :backing, or drop it")

                (and ts (not (contains? timestamp-types
                                        (if (sequential? t) (first t) (some-> t keyword)))))
                (str ":source-timestamp " (pr-str ts) " names no timestamp property of"
                     " the type"))]
      (when why [{:object-type (shown db ot) :reason why}]))))

(defn- supplement-problems [db tk]
  (when-let [t (db/entry db [:transforms tk])]
    (for [o  (authored-outputs t)
          :let [ot (type-key db (:object-type o))]
          :when (and ot (settings db ot) (= :supplement (some-> (:role o) keyword)))]
      {:transform (or (:transform-name t) tk) :object-type (shown db ot)})))

(defn- edit-problems [db ak]
  (when-let [a (db/entry db [:actions ak])]
    (for [v (edited-types a)
          :let [ot (type-key db v) s (when ot (settings db ot))]
          :when (and s (not (:direct? s)))]
      {:action (or (:action-name a) ak) :object-type (shown db ot)})))

(defn- by-fact [db0 db1 fact names]
  (for [n names d [db0 db1] node (b/facts d :stream-refs [fact n])] node))

(defn- backed-types
  [db0 db1 changes]
  (let [moved    (b/scope-dependents db0 db1 changes)
        datasets (into (view/touched changes :datasets)
                       (keep (fn [[sec k]] (when (= :datasets sec) k))) moved)
        names    (into #{} (mapcat #(into (b/names-of db0 :datasets %)
                                          (b/names-of db1 :datasets %)))
                       datasets)
        outputs  (fn [ts] (for [t ts d [db0 db1] o (type-outputs (db/entry d [:transforms t]))
                                :when (some? (:object-type o))]
                            (:object-type o)))]
    (-> (view/touched changes :object-types)
        (into (outputs (view/touched changes :transforms)))
        (into (map second) (by-fact db0 db1 :backing names))
        (into (outputs (map second (by-fact db0 db1 :out-dataset names)))))))

(defn- type-names
  [db0 db1 changes]
  (let [moved (keep (fn [[sec k]] (when (= :object-types sec) k))
                    (b/scope-dependents db0 db1 changes))]
    (into #{} (mapcat #(into (b/names-of db0 :object-types %) (b/names-of db1 :object-types %)))
          (concat (backed-types db0 db1 changes) moved))))

(def ^:private stream-backing-settings-valid
  {:id :stream-backing-settings-valid? :rule :object-type/stream-backing-setting :scope :local
   :keys backed-types
   :all (b/section-keys :object-types)
   :check setting-problem})

(def ^:private stream-types-single-datasource
  {:id :stream-types-single-datasource? :rule :object-type/stream-supplement :scope :local
   :keys (fn [db0 db1 changes]
           (into (view/touched changes :transforms)
                 (map second) (by-fact db0 db1 :supplies (type-names db0 db1 changes))))
   :all (b/section-keys :transforms)
   :check supplement-problems})

(def ^:private stream-types-unedited
  {:id :stream-types-unedited? :rule :object-type/edit-on-stream :scope :local
   :keys (fn [db0 db1 changes]
           (into (view/touched changes :actions)
                 (map second) (by-fact db0 db1 :edits (type-names db0 db1 changes))))
   :all (b/section-keys :actions)
   :check edit-problems})

(def rows
  [stream-types-unedited
   stream-types-single-datasource
   stream-backing-settings-valid
   stream-derivations-valid])
