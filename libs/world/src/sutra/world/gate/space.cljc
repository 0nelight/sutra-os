(ns sutra.world.gate.space
  (:require [sutra.world.db :as db]
            [sutra.world.delta :as d]
            [sutra.world.gate.base :as b]
            [sutra.world.index :as ix]
            [sutra.world.view :as view]
            [sutra.types.entities :as ents]
            [sutra.types.refs :as refs]
            [sutra.types.table :as tbl]))

(def ^:private default-space :space/default)
(def ^:private default-org :org/default)

(defn- member-ids [e]
  (keep #(some-> (if (map? %) (:principal %) %) str) (:members e)))

(def fact-rows
  [{:id :principals :sections #{:organizations}
    :facts (fn [_ e] (for [p (member-ids e)] [:principal p]))}])

(defn- principals [db o] (member-ids (db/entry db [:organizations o])))

(def ^:private primary-organization-unique
  {:id :primary-organization-unique? :rule :organization/primary-twice :scope :local
   :keys (fn [db0 db1 changes]
           (b/both principals db0 db1 (view/touched changes :organizations)))
   :all (fn [db] (map (comp second key) (get (db/view db :gate-facts) :principals)))
   :check (fn [db p]
            (let [os (b/facts db :principals [:principal p])]
              (when (< 1 (count os))
                [{:principal p :organizations (vec (sort (map (comp str second) os)))}])))})

(defn- org-key [db x] (b/key-of db :organizations x))

(defn- space-organizations
  [db s]
  (let [e    (db/entry db [:spaces (b/key-of db :spaces s)])
        home (or (org-key db (:organization e))
                 (some #(org-key db %) (:organizations e))
                 default-org)]
    (into (if (= default-org home) #{} #{home})
          (keep #(org-key db %))
          (concat (:organizations e) (:shared-with e)))))

(defn- outside-space [db p]
  (when-let [e (db/entry db [:projects p])]
    (let [s     (or (b/key-of db :spaces (:space e)) default-space)
          extra (remove (space-organizations db s) (distinct (keep #(org-key db %)
                                                                   (:shared-with e))))]
      (when (seq extra)
        [{:project p :space s :organizations (vec (sort (map str extra)))}]))))

(defn- space-projects [db0 db1 s]
  (into #{} (for [n (into (b/names-of db0 :spaces s) (b/names-of db1 :spaces s))
                  d [db0 db1]
                  [sec p] (db/nodes d :members [:space n])
                  :when (= :projects sec)]
              p)))

(def ^:private project-organizations-in-space
  {:id :project-organizations-in-space? :rule :project/organization-outside-space
   :scope :local
   :keys (fn [db0 db1 changes]
           (let [by-org (for [o (view/touched changes :organizations)
                              node (b/referrers db0 db1 [:organizations o])]
                          node)
                 spaces (into (view/touched changes :spaces)
                              (keep (fn [[sec k]] (when (= :spaces sec) k))) by-org)]
             (-> (view/touched changes :projects)
                 (into (keep (fn [[sec k]] (when (= :projects sec) k))) by-org)
                 (into (mapcat #(space-projects db0 db1 %)) spaces))))
   :all (b/section-keys :projects)
   :check outside-space})

(def ^:private ontology-section?
  (tbl/per-key "kern gate ontology-references-local?" ents/sections
               {#{:object-types :link-types :actions :interfaces :functions :shared-properties
                  :value-types :object-type-groups}
                true
                #{:agents :applications :collections :custom-applications :datasets
                  :eval-suites :exports :graphs :llm-models :markings :media-sets :object-sets
                  :organizations :pipelines :projections :projects :repositories :roles
                  :sources :spaces :templates :transforms :workspaces}
                false}))

(def ^:private ontology-sections (into #{} (keep (fn [[s v]] (when v s))) ontology-section?))

(defn- ontology-of [db e] (or (db/scope-of db e) default-space))

(defn- target
  [db t v own]
  (if (db/rid? v)
    v
    (let [hits (db/keys-of (db/nodes db :by-name-str [t (ix/nm v)]))
          in   (fn [s] (let [xs (filter #(= s (db/scope-of db (db/entry db [t %]))) hits)]
                         (when (= 1 (count xs)) (first xs))))]
      (cond (empty? hits)                                              nil
            (apply = (map #(db/scope-of db (db/entry db [t %])) hits)) (last hits)
            :else                                                      (or (in own) (in nil))))))

(defn- cross-references [db [sec r]]
  (when-let [e (db/entry db [sec r])]
    (let [own (ontology-of db e)]
      (for [[targets v] (refs/ref-values (get ents/refs sec) e)
            t targets
            :when (contains? ontology-sections t)
            :let [tr (target db t v own)
                  te (some->> tr (vector t) (db/entry db))]
            :when te
            :let [other (ontology-of db te)]
            :when (and (not= other own) (not= other default-space))]
        {:section sec :resource r :ontology own :references tr :in other}))))

(def ^:private ontology-references-local
  {:id :ontology-references-local? :rule :ontology/cross-reference :scope :local
   :keys (fn [db0 db1 changes]
           (let [moved (into (set (d/nodes changes)) (b/scope-dependents db0 db1 changes))]
             (into #{} (filter (comp ontology-sections first))
                   (into moved (mapcat #(b/referrers db0 db1 %)) moved))))
   :all (fn [db] (for [sec ontology-sections [k _] (get (db/world db) sec)] [sec k]))
   :check cross-references})

(def rows
  [ontology-references-local
   project-organizations-in-space
   primary-organization-unique])
