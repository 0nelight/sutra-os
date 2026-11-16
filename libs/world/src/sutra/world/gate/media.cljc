(ns sutra.world.gate.media
  (:require [sutra.world.db :as db]
            [sutra.world.gate.base :as b]
            [sutra.world.index :as ix]
            [sutra.world.view :as view]
            [sutra.types.entities :as ents]
            [sutra.types.table :as tbl]))

(def ^:private list-keys [:attribute-list :attributes :property-list :properties :columns])

(defn- attr-row [item]
  (cond (map? item)        item
        (keyword? item)    {:attribute-name item}
        (sequential? item) (let [[n t opts] item]
                             (cond-> {:attribute-name n :attribute-type t}
                               (map? opts) (assoc :opts opts)))))

(defn- attr-rows
  [e]
  (into [] (keep attr-row) (some (fn [k] (let [v (get e k)] (when (seq v) v))) list-keys)))

(defn- set-of [a] (or (:media-set a) (:media-set (:opts a))))

(def ^:private holds-media?
  (tbl/per-key "kern gate media-sets-declared?" ents/sections
               {#{:object-types :datasets} true
                #{:actions :agents :applications :collections :custom-applications
                  :eval-suites :exports :functions :graphs :interfaces :link-types :llm-models
                  :markings :media-sets :object-sets :object-type-groups :organizations
                  :pipelines :projections :projects :repositories :roles :shared-properties
                  :sources :spaces :templates :transforms :value-types :workspaces}
                false}))

(def ^:private holders (into #{} (keep (fn [[s v]] (when v s))) holds-media?))

(def fact-rows
  [{:id :media-set-refs :sections holders
    :facts (fn [_ e] (for [a (attr-rows e) :let [ms (set-of a)] :when ms] [:media-set ms]))}])

(defn- undeclared [media?]
  (fn [db [sec k]]
    (when-let [e (db/entry db [sec k])]
      (for [a (attr-rows e)
            :when (media? (:attribute-type a))
            :let [ms (set-of a)]
            :when (and ms (empty? (db/nodes db :by-name [:media-sets ms])))]
        {:entity-class sec :entity (get e (ix/name-field sec))
         :attribute (:attribute-name a) :media-set ms}))))

(defn- media-set-names [db k]
  (some-> (db/entry db [:media-sets k]) :media-set-name vector))

(defn- media-sets-declared [media?]
  {:id :media-sets-declared? :rule :media-set/undeclared :scope :local
   :keys (fn [db0 db1 changes]
           (let [ms (view/touched changes :media-sets)]
             (-> (into #{} (filter (comp holders first)) (map first changes))
                 (into (for [n (b/both media-set-names db0 db1 ms)
                             d [db0 db1]
                             node (b/facts d :media-set-refs [:media-set n])]
                         node)))))
   :all (fn [db] (for [sec holders [k _] (get (db/world db) sec)] [sec k]))
   :check (undeclared media?)})

(defn- source-problem [db k]
  (when-let [ms (db/entry db [:media-sets k])]
    (let [nm  (:media-set-name ms)
          src (db/entry db [:sources (:source ms)])
          why (cond
                (and (or (:virtual ms) (:sync ms)) (nil? (:source ms)))
                "names no :source"
                (and (:source ms) (nil? src))
                (str "names the source " (pr-str (:source ms)) ", which does not exist")
                (and (:virtual ms) (seq (remove (set (:markings ms)) (:markings src))))
                "lacks markings its source carries, and would show its files to anyone")]
      (when why
        [{:entity-class :media-sets :entity nm
          :message (str "Media set " (pr-str nm) " " why)}]))))

(def ^:private media-set-sources
  {:id :media-set-sources? :rule :media-set/source :scope :local
   :keys (fn [db0 db1 changes]
           (into (view/touched changes :media-sets)
                 (for [s (view/touched changes :sources)
                       d [db0 db1]
                       [sec k] (db/nodes d :refs-in s)
                       :when (= :media-sets sec)]
                   k)))
   :all (b/section-keys :media-sets)
   :check source-problem})

(defn rows
  [media?]
  [media-set-sources (media-sets-declared media?)])
