(ns sutra.world.gate
  (:require [clojure.string :as str]
            [sutra.world.db :as db]
            [sutra.world.gate.base :as b]
            [sutra.world.gate.lint :as lint]
            [sutra.world.gate.marking :as marking]
            [sutra.world.gate.media :as media]
            [sutra.world.gate.near :as near]
            [sutra.world.gate.space :as space]
            [sutra.world.gate.stream :as stream]
            [sutra.world.gate.structure :as structure]
            [sutra.world.index :as ix]
            [sutra.world.policy :as policy]
            [sutra.world.view :as view]
            [sutra.types.entities :as ents]
            [sutra.types.kinds :as kinds]
            [sutra.types.refs :as refs]
            [sutra.types.table :as tbl]))

(def ^:private named b/named)
(def ^:private section-keys b/section-keys)
(def ^:private both b/both)

(defn- dedup-by [f xs]
  (first (reduce (fn [[acc seen] x]
                   (let [k (f x)] (if (contains? seen k) [acc seen] [(conj acc x) (conj seen k)])))
                 [[] #{}] xs)))

(defn attr-row [x]
  (select-keys (cond (map? x)        x
                     (keyword? x)    {:attribute-name x}
                     (sequential? x) (let [[n t opts] x]
                                       (cond-> {:attribute-name n :attribute-type t}
                                         (map? opts) (assoc :opts opts))))
               [:attribute-name :attribute-type :opts]))

(defn effective-interface [ifs anc i]
  (let [chain (cons i (get anc i))]
    (assoc (get ifs i)
           :property-list (dedup-by :attribute-name (mapcat #(:property-list (get ifs %)) chain))
           :link-constraints (dedup-by :link-name
                                       (mapcat #(or (:link-constraints (get ifs %)) []) chain)))))

(defn effective-implements [ifs anc ot]
  (into [] (comp (filter #(contains? ifs %)) (mapcat #(cons % (get anc %))) (distinct))
        (or (:implements ot) [])))

(defn contribute-props [ifs anc impls attr-list]
  (let [own (into #{} (map :attribute-name) attr-list)]
    (into (vec attr-list)
          (dedup-by :attribute-name
                    (for [i (effective-implements ifs anc {:implements impls})
                          p (:property-list (effective-interface ifs anc i))
                          :when (not (own (:attribute-name p)))]
                      p)))))

(def ^:private folds
  {"ä" "ae" "ö" "oe" "ü" "ue" "Ä" "Ae" "Ö" "Oe" "Ü" "Ue" "ß" "ss" "ø" "oe" "Ø" "Oe"})

(defn fold
  [a]
  (let [t (name a)]
    (keyword (if (re-find #"[äöüÄÖÜßøØ?!-]" t)
               (-> (reduce-kv str/replace t folds) (str/replace #"[?!]" "") (str/replace "-" "_"))
               t))))

(defn- ifs-anc [db] [(get (db/world db) :interfaces) (db/view db :ancestors)])

(defn- implementers
  [db is]
  (into #{} (comp (mapcat #(view/descendants db %))
                  (mapcat #(db/keys-of (db/nodes db :edges [:implements %]))))
        is))

(defn- types-touched
  [db0 db1 changes]
  (let [is (view/touched changes :interfaces)]
    (-> (view/touched changes :object-types)
        (into (implementers db0 is))
        (into (implementers db1 is)))))

(defn- type-gate
  [id rule f]
  {:id id :rule rule :scope :local
   :keys types-touched
   :all (section-keys :object-types)
   :check (fn [db k]
            (when-let [ot (db/entry db [:object-types k])]
              (let [[ifs anc] (ifs-anc db)] (f db k ot ifs anc))))})

(defn- own-props [ot ifs anc]
  (contribute-props ifs anc (:implements ot) (:attribute-list ot)))

(defn- title-key [_ k ot ifs anc]
  (when-let [tk (:title-key ot)]
    (let [available (into #{} (map :attribute-name) (own-props ot ifs anc))]
      (when-not (contains? available tk)
        [{:entity-class :object-types :entity (named :object-types k ot)
          :title-key tk :available (vec (sort available))}]))))

(defn- columns [_ k ot ifs anc]
  (for [[col group] (group-by fold (distinct (keep :attribute-name (own-props ot ifs anc))))
        :let [problem (cond (> (count group) 1)                       :fold-collision
                            (not (re-matches #"[A-Za-z0-9_]+" (name col))) :illegal-column)]
        :when problem]
    {:entity-class :object-types :entity (named :object-types k ot)
     :problem problem :column col :attributes (vec (sort group))}))

(defn- implements-known [_ k ot ifs _]
  (for [i (or (:implements ot) []) :when (not (contains? ifs i))]
    {:entity-class :object-types :entity k :interface i}))

(defn- property-superset [_ k ot ifs anc]
  (let [types (into {} (map (comp (juxt :attribute-name :attribute-type) attr-row))
                    (:attribute-list ot))]
    (for [i (effective-implements ifs anc ot)
          {:keys [attribute-name attribute-type]} (:property-list (get ifs i))
          :let [got (get types attribute-name ::missing)]
          :when (not= got attribute-type)]
      (cond-> {:entity-class :object-types :entity k :interface i
               :attribute attribute-name :want attribute-type}
        (not= got ::missing) (assoc :got got)))))

(defn- interfaces-agree [member per-iface]
  (fn [_ k ot ifs anc]
    (let [mine (effective-implements ifs anc ot)]
      (when (>= (count mine) 2)
        (for [[n entries] (group-by first
                                    (mapcat (fn [i] (for [[n x] (per-iface (get ifs i))] [n [i x]]))
                                            mine))
              :let [by-iface (into {} (map second) entries)]
              :when (> (count (set (vals by-iface))) 1)]
          {:entity-class :object-types :entity k member n :by-interface by-iface})))))

(defn constraint-card [c]
  (get {:one :one :many-to-one :one :one-to-one :one
        :many :many :one-to-many :many :many-to-many :many}
       (some-> c name keyword)))

(defn- far-card [c dir]
  (get {[:to :many-to-one] :one [:to :one-to-one] :one [:from :one-to-many] :one
        [:from :one-to-one] :one [:to :one-to-many] :many [:to :many-to-many] :many
        [:from :many-to-one] :many [:from :many-to-many] :many}
       [dir (some-> c name keyword)]))

(defn- type-kinds [W T]
  (loop [acc #{T} todo (seq (get-in W [:object-types T :implements]))]
    (if-let [[i & more] todo]
      (if (acc i)
        (recur acc more)
        (recur (conj acc i) (seq (concat more (get-in W [:interfaces i :extends])))))
      acc)))

(defn- iface-chain [ifs i]
  (loop [acc [] seen #{} todo [i]]
    (if-let [[x & more] (seq todo)]
      (if (or (seen x) (not (contains? ifs x)))
        (recur acc seen more)
        (recur (conj acc x) (conj seen x) (concat more (get-in ifs [x :extends]))))
      acc)))

(defn- side [W L dir]
  (let [lt (get-in W [:link-types L]) to? (= :to dir)]
    {:link L :dir dir
     :name (or (get lt (if to? :forward-api-name :reverse-api-name)) (:link-type-name lt) L)
     :near (get lt (if to? :from-object-type :to-object-type))
     :far  (get lt (if to? :to-object-type :from-object-type))}))

(defn- link-key [db h]
  (if (contains? (get (db/world db) :link-types) h)
    h
    (first (db/keys-of (db/nodes db :by-name [:link-types h])))))

(defn- constraint-sides
  [db T i c]
  (let [W     (db/world db)
        chain (iface-chain (:interfaces W) i)]
    (if-let [{:keys [to-object-type to-interface cardinality]}
             (some (fn [x] (some #(when (= c (:link-name %)) %)
                                 (get-in W [:interfaces x :link-constraints])))
                   chain)]
      (let [expected (or to-object-type to-interface)
            want     (constraint-card cardinality)
            ks       (type-kinds W T)
            reaches? (fn [far] (or (= far expected)
                                   (and to-interface (contains? (type-kinds W far) to-interface))))
            named    (link-key db c)
            cands    (cond-> (into #{} (for [x chain [_ l] (db/nodes db :edges [:satisfies x c])]
                                         l))
                       (contains? (:link-types W) named) (conj named))]
        (vec (for [L   (sort-by str cands)
                   dir [:to :from]
                   :let [s (side W L dir)]
                   :when (and (contains? ks (:near s))
                              (reaches? (:far s))
                              (or (nil? want)
                                  (= want (far-card (get-in W [:link-types L :cardinality]) dir))))]
               s)))
      [])))

(defn- link-constraints [db k ot ifs anc]
  (let [lts (get (db/world db) :link-types)]
    (for [i (effective-implements ifs anc ot)
          {:keys [link-name to-object-type to-interface cardinality required]}
          (or (:link-constraints (get ifs i)) [])
          :when (true? required)
          :when (empty? (constraint-sides db k i link-name))
          :let [by-name (some->> (db/nodes db :by-name [:link-types link-name])
                                 db/keys-of last (get lts))
                any?    (or (some? by-name)
                            (seq (db/nodes db :edges [:satisfies-kw i link-name])))]]
      {:object-type k :interface i :link-name link-name :missing? (not any?)
       :reason (if any? :unsatisfied-constraint :no-such-link-type)
       :expected {:from k :to (or to-object-type to-interface)
                  :cardinality (constraint-card cardinality)}
       :actual by-name})))

(defn- link-ends [db l]
  (when-let [e (db/entry db [:link-types l])] (keep e [:from-object-type :to-object-type])))

(defn- end-types [db x]
  (if (contains? (get (db/world db) :interfaces) x) (implementers db #{x}) #{x}))

(defn- link-interfaces
  [db l]
  (when-let [e (db/entry db [:link-types l])]
    (concat (keep #(when (map? %) (:interface %))
                  (let [s (:satisfies e)] (if (sequential? s) s [s])))
            (db/keys-of (db/nodes db :edges [:constraint (:link-type-name e)])))))

(defn- link-touched
  [db0 db1 changes]
  (let [lts (view/touched changes :link-types)
        ts  (-> (types-touched db0 db1 changes)
                (into (both (fn [d l] (implementers d (link-interfaces d l))) db0 db1 lts)))
        xs (into (both link-ends db0 db1 lts)
                 (both (fn [d t] (for [[_ l] (db/nodes d :edges [:link-end t])
                                       end (link-ends d l)]
                                   end))
                       db0 db1 ts))]
    (into ts (both end-types db0 db1 xs))))

(defn- group-key [db sec e]
  (when-some [n (get e (ix/name-field sec))]
    (if (= :link-types sec)
      [n (:from-object-type e) (:to-object-type e) (db/scope-of db e)]
      [n (db/scope-of db e)])))

(defn- duplicates [db [sec gk]]
  (let [es (keep #(let [e (db/entry db %)] (when (= gk (group-key db sec e)) e))
                 (db/nodes db :by-name [sec (first gk)]))]
    (when (> (count es) 1)
      [{:entity-class sec :entity (first gk) :rids (vec (sort-by str (map :id es)))}])))

(def ^:private names-unique
  {:id :names-unique? :rule :schema/duplicate-name :scope :local
   :keys (fn [db0 db1 changes]
           (into #{}
                 (for [[sec :as node] (into (set (map first changes))
                                            (b/scope-dependents db0 db1 changes))
                       :when (ix/name-field sec)
                       d [db0 db1]
                       :let [gk (some->> (db/entry d node) (group-key d sec))]
                       :when gk]
                   [sec gk])))
   :all (fn [db] (for [sec (keys ix/name-field) [_ e] (get (db/world db) sec)
                       :let [gk (group-key db sec e)] :when gk]
                   [sec gk]))
   :check duplicates})

(defn- strip-typed-outputs [t]
  (let [strip #(cond-> % (kinds/is? :typed-output? (:kind %)) (dissoc :object-type :link-type))]
    (cond-> t
      (map? (:output t))         (update :output strip)
      (sequential? (:outputs t)) (update :outputs #(mapv strip %)))))

(defn- hits [sec e]
  (concat (refs/ref-values (get ents/refs sec) (cond-> e (= :transforms sec) strip-typed-outputs))
          (when (= :actions sec) (refs/ref-values ents/action-refs e))))

(defn- dangling [db [sec k :as node]]
  (when-let [e (db/entry db node)]
    (let [W (db/world db)]
      (for [[targets v] (hits sec e)
            :when (and (db/rid? v) (not (some #(contains? (get W %) v) targets)))]
        {:in sec :entity k :ref v :targets targets}))))

(def ^:private no-dangling-refs
  {:id :no-dangling-refs? :rule :schema/dangling-reference :scope :local
   :keys (fn [db0 db1 changes]
           (let [touched (set (map first changes))]
             (into (into #{} (filter (comp ents/refs first)) touched)
                   (both (fn [d [_ k]] (db/nodes d :refs-in k)) db0 db1 touched))))
   :all (fn [db] (for [sec (keys ents/refs) [k _] (get (db/world db) sec)] [sec k]))
   :check dangling})

(defn- model-key [m] (if (keyword? m) (subs (str m) 1) (str m)))

(def ^:private models-registered
  {:id :models-registered? :rule :transform/unregistered-model :scope :local
   :keys (fn [db0 db1 changes]
           (into (view/touched changes :transforms)
                 (both (fn [d m] (db/keys-of (db/nodes d :edges [:model (model-key m)])))
                       db0 db1 (view/touched changes :llm-models))))
   :all (section-keys :transforms)
   :check (fn [db k]
            (when-let [t (db/entry db [:transforms k])]
              (let [ms (get (db/world db) :llm-models)]
                (for [[_ in] (:inputs t)
                      :when (= :language-model (:kind in))
                      :let [s (str (:model in))]
                      :when (not (or (contains? ms s) (contains? ms (keyword s))))]
                  {:entity-class :transforms :entity (:transform-name t) :model (:model in)}))))})

(defn- writes? [t kind k]
  (some #(and (= kind (:kind %)) (= k (get % kind))
              (or (not= :object-type kind) (not= :supplement (:role %))))
        (ix/outputs-of t)))

(def ^:private unresolved-input
  (tbl/per-key "kern gate 8 (unresolved-input)" kinds/kinds
               {:transform   (fn [W _ in] (not (contains? (:transforms W) (:transform in))))
                :object-type (fn [W t in] (or (not (contains? (:object-types W) (:object-type in)))
                                              (writes? t :object-type (:object-type in))))
                :dataset     (fn [_ t in] (writes? t :dataset (:dataset in)))
                #{:link-type :media-set :media-set-items :media :language-model :datasource
                  :export :scalar}
                nil}))

(def ^:private inputs-resolve
  {:id :compute-graph-inputs :gate :compute-graph-valid?
   :rule :transform/compute-graph-invalid :scope :local
   :keys (fn [db0 db1 changes]
           (into (view/touched changes :transforms)
                 (concat
                  (both (fn [d k] (db/keys-of (db/nodes d :edges [:input :transform k])))
                        db0 db1 (view/touched changes :transforms))
                  (both (fn [d k] (db/keys-of (db/nodes d :edges [:input :object-type k])))
                        db0 db1 (view/touched changes :object-types)))))
   :all (section-keys :transforms)
   :check (fn [db k]
            (when-let [t (db/entry db [:transforms k])]
              (let [W (db/world db)]
                (for [in (ix/inputs-of t)
                      :when (tbl/on unresolved-input (:kind in) W t in)]
                  {:entity-class :transforms :entity (named :transforms k t) :input in}))))})

(defn- kahn [deps]
  (loop [remaining deps]
    (let [ready (into [] (filter #(empty? (get remaining %))) (keys remaining))]
      (cond (empty? remaining) nil
            (empty? ready)     (set (keys remaining))
            :else (recur (into {} (map (fn [[k d]] [k (apply disj d ready)]))
                               (apply dissoc remaining ready)))))))

(defn- deps-of
  [db k]
  (let [ts (get (db/world db) :transforms)]
    (into #{} (comp (filter #(= :transform (:kind %)))
                    (map :transform)
                    (filter #(contains? ts %)))
          (vals (:inputs (get ts k))))))

(defn- downstream
  [db ks]
  (view/reach (fn [k] (db/keys-of (db/nodes db :edges [:input :transform k]))) ks))

(def ^:private residual
  {:id    :cycle-residual
   :reads #{:transforms}
   :init  (fn [db]
            (let [ts (keys (get (db/world db) :transforms))]
              (or (kahn (into {} (map (fn [k] [k (deps-of db k)])) ts)) #{})))
   :step  (fn [old changes db0 db1]
            (let [ts (view/touched changes :transforms)]
              (if (empty? ts)
                old
                (let [moved (into (set (downstream db0 ts)) (downstream db1 ts))
                      reach (view/reach #(deps-of db1 %)
                                        (filter #(db/entry db1 [:transforms %]) moved))
                      now   (or (kahn (into {} (map (fn [k] [k (deps-of db1 k)])) reach)) #{})]
                  (into (reduce disj old moved) (filter moved) now)))))})

(def ^:private acyclic
  {:id :compute-graph-cycle :gate :compute-graph-valid?
   :rule :transform/compute-graph-invalid :scope :local
   :keys (fn [db0 db1 changes]
           (let [r1 (db/view db1 :cycle-residual)]
             (when (or (not= (db/view db0 :cycle-residual) r1)
                       (some r1 (view/touched changes :transforms)))
               [::graph])))
   :all (fn [_] [::graph])
   :check (fn [db _]
            (when-let [c (not-empty (db/view db :cycle-residual))]
              (let [ts (get (db/world db) :transforms)]
                [{:entity-class :transforms
                  :entity (vec (sort-by str (map #(named :transforms % (get ts %)) c)))
                  :cycle? true}])))})

(defn- value-type [db ref]
  (or (some->> (db/nodes db :by-name [:value-types ref]) db/keys-of last
               (vector :value-types) (db/entry db))
      (when-let [vt (db/entry db [:value-types ref])]
        (when-not (:value-type-name vt) vt))))

(def ^:private shared-property-value-types
  {:id :shared-property-value-types? :rule :shared-property/value-type :scope :local
   :keys (fn [db0 db1 changes]
           (into (view/touched changes :shared-properties)
                 (both (fn [d v]
                         (for [n (cons v (keep #(some-> (db/entry % [:value-types v])
                                                        :value-type-name)
                                               [db0 db1]))
                               [_ k] (db/nodes d :edges [:value-type (ix/kw n)])]
                           k))
                       db0 db1 (view/touched changes :value-types))))
   :all (section-keys :shared-properties)
   :check (fn [db k]
            (when-let [sp (db/entry db [:shared-properties k])]
              (when-let [ref (some-> (:value-type sp) ix/kw)]
                (let [vt (value-type db ref)]
                  (when (or (nil? vt) (not= (ix/kw (:type vt)) (ix/kw (:type sp))))
                    [(cond-> {:entity-class :shared-properties
                              :entity (or (:shared-property-name sp) k)
                              :value-type ref :type (:type sp)}
                       vt (assoc :base (ix/kw (:type vt))))])))))})

(defn- iface-gate [id rule keys-fn f]
  {:id id :rule rule :scope :local
   :keys keys-fn
   :all (section-keys :interfaces)
   :check (fn [db k] (when-let [i (db/entry db [:interfaces k])] (f db k i)))})

(def ^:private extends-known
  (iface-gate :extends-known? :interface/unknown-extends
              (fn [db0 db1 changes]
                (let [is (view/touched changes :interfaces)]
                  (into is (both (fn [d i] (db/keys-of (db/nodes d :edges [:extends i])))
                                 db0 db1 is))))
              (fn [db k i]
                (for [p (or (:extends i) [])
                      :when (not (contains? (get (db/world db) :interfaces) p))]
                  {:entity-class :interfaces :entity k :parent p}))))

(def ^:private extends-acyclic
  (iface-gate :extends-acyclic? :interface/extends-cycle
              (fn [db0 db1 changes]
                (both view/descendants db0 db1 (view/touched changes :interfaces)))
              (fn [db k _]
                (when (contains? (get (db/view db :ancestors) k) k)
                  [{:entity-class :interfaces :entity k}]))))

(def rows
  (into
   [names-unique
   no-dangling-refs
   models-registered
   inputs-resolve
   acyclic
   shared-property-value-types
   (type-gate :title-keys-valid? :object-type/unknown-title-key title-key)
   (type-gate :columns-valid? :object-type/illegal-column columns)
   (type-gate :implements-known? :interface/unknown-implements implements-known)
   (type-gate :property-superset? :interface/property-superset property-superset)
   (assoc (type-gate :link-constraints-met? :interface/link-constraint link-constraints)
          :keys link-touched)
   (type-gate :interface-properties-agree? :interface/property-conflict
              (interfaces-agree :attribute
                                (fn [i] (for [p (:property-list i)]
                                          [(:attribute-name p) (:attribute-type p)]))))
   (type-gate :interface-links-agree? :interface/link-conflict
              (interfaces-agree :link-name
                                (fn [i] (for [c (or (:link-constraints i) [])]
                                          [(:link-name c)
                                           {:to (or (:to-object-type c) (:to-interface c))
                                            :cardinality (constraint-card (:cardinality c))}]))))
   extends-known
   extends-acyclic]
   (concat space/rows marking/rows stream/rows)))

(def facts
  (b/facts-view (concat space/fact-rows marking/fact-rows media/fact-rows
                        stream/fact-rows near/fact-rows lint/fact-rows
                        structure/fact-rows)))

(def near-specs
  (into near/specs lint/specs))

(def ^:private near-facts (concat near/fact-rows lint/fact-rows))

(defn- run [db {:keys [check]} ks]
  (into {} (keep (fn [k] (let [ps (vec (check db k))] (when (seq ps) [k ps])))) ks))

(defn findings-of
  [gs]
  {:id    :findings
   :reads :all
   :init  (fn [db] (into {} (map (fn [g] [(:id g) (run db g ((:all g) db))]))
                         (remove :frame gs)))
   :step  (fn [old changes db0 db1]
            (reduce (fn [m g]
                      (let [ks (set ((:keys g) db0 db1 changes))]
                        (if (empty? ks)
                          m
                          (update m (:id g) #(merge (apply dissoc % ks) (run db1 g ks))))))
                    old (remove :frame gs)))})

(def findings (findings-of rows))

(def ^:private before-findings
  (into [facts residual] policy/rows))

(def views
  (conj before-findings findings))

(defn rows-with
  [{:keys [media?] :as checks}]
  (-> (structure/rows checks)
      (into rows)
      (into (when media? (media/rows media?)))
      (into (near/rows checks near-specs near-facts))))

(defn views-with
  [checks]
  (conj before-findings (near/near-view near-specs near-facts)
        (findings-of (rows-with checks))))

(defn report-of
  [gs db]
  (vec (for [g  (remove :silent gs)
             :let [by-key (get (db/view db :findings) (:id g))]
             p  (cond
                  (:frame g)    ((:frame g) db)
                  (:collapse g) (when (seq by-key) [{}])
                  :else         (mapcat val by-key))]
         (assoc p
                :rule (if (:lint g) (:rule p) (:rule g))
                :gate (:gate g (:id g))
                :severity (:severity p :error)))))

(defn- lint-key [f] [(:rule f) (str (:entity f))])

(defn report-since
  [gs db0 db1]
  (let [lint?     #(= :lint-gate-clean? (:gate %))
        inherited (into #{} (comp (filter lint?) (map lint-key)) (report-of gs db0))]
    (into [] (remove #(and (lint? %) (contains? inherited (lint-key %)))) (report-of gs db1))))

(defn accepted-since
  [gs db0 db1]
  (and (every? #(empty? (get (db/view db1 :findings) (:id %))) (filter :silent gs))
       (every? #(empty? ((:frame %) db1)) (filter :frame gs))
       (not-any? #(not= :schema/structure (:rule %)) (report-since gs db0 db1))))
