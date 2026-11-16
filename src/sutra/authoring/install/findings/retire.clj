(ns sutra.authoring.install.findings.retire
  (:require [sutra.kit.coll :as coll]
            [sutra.catalog.retirement :as retirement]
            [sutra.catalog.lookup :as lookup]
            [sutra.types.expr :as expr]
            [clojure.string :as str]
            [sutra.authoring.install.findings :as fnd]
            [sutra.types.entities :as ents]
            [sutra.world.index :as kix]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]
            [sutra.types.transform-io :as tio]))

(set! *warn-on-reflection* true)

(defn resolver
  [current ix]
  (let [spaces (delay (sch/spaces-of current))
        scope  (::sch/scope (meta current))]
    {:rid-of  (fn [kk x]
                (if (or (nil? x) (rid/rid? x))
                  x
                  (let [n (keyword (rid/->name x))]
                    (rid/resolve-name->rid
                     {kk {n (sch/name-entry @spaces scope :name->rid kk n
                                            (for [r ((:names ix) kk (kix/nm (name n)))]
                                              [r (get-in current [kk r])]))}}
                     [kk] n))))
     :name-of (fn [r]
                (when (rid/rid? r)
                  (when-let [kk (rid/tag->kinds-key (rid/rid-kind r))]
                    (get-in current [kk r (rid/name-field-of kk)]))))}))

(defn retiring
  [current res plan]
  (for [{:keys [kind entity outcome]} plan
        :when (= :deleted outcome)
        :let [kk (if (= :action kind) :actions (get rid/tag->kinds-key kind))
              id (if (= :actions kk)
                   entity
                   ((:rid-of res) kk entity))]
        :when (and kk id)]
    {:kind kind :entity entity :kinds-key kk :id id
     :status (or (some-> (get-in current [kk id :status]) keyword) :experimental)}))

(defn- display-name [res entity]
  (rid/->name (or ((:name-of res) entity) entity)))

(defn- restated-away?
  [idx plan by-id {:keys [in entity ref]}]
  (let [nm   (display-name idx entity)
        g    (by-id ref)
        args (some (fn [{:keys [kind outcome] :as e}]
                     (when (and (= :updated outcome)
                                (= in (ents/tag->section kind))
                                (= nm (rid/->name (:entity e))))
                       (:args e)))
                   plan)
        hit? (fn [x] (and (or (keyword? x) (string? x))
                          (or (= x ref) (= (rid/->name x) (rid/->name (:entity g))))))]
    (boolean (and g args (not (some hit? (tree-seq coll? seq args)))))))

(defn- rollers
  [current going]
  (let [links-going (filter #(= :link-types (:kinds-key %)) going)
        types-going (into #{} (keep #(when (= :object-types (:kinds-key %)) (:id %))) going)
        link-by-ref (into {} (mapcat (fn [g] [[(:id g) g] [(:entity g) g]
                                              [(rid/->name (:entity g)) g]]))
                          links-going)]
    (when (seq links-going)
      (for [[ot-rid ot] (:object-types current)
            :when (not (types-going ot-rid))
            [attr ex] (:derived (lookup/attrs-index current ot-rid))
            l (expr/link-refs ex)
            :let [g (or (get link-by-ref l) (get link-by-ref (rid/->name l)))]
            :when g]
        [g (str "derived " (rid/->name (or (:object-type-name ot) ot-rid))
                "." (name attr))]))))

(defn- producers-of
  [current idx g rs]
  (when (= :datasets (:kinds-key g))
    (vec (distinct
          (for [{:keys [in entity]} rs
                :when (and (= :transforms in)
                           (some #(= (:id g) (:dataset %))
                                 (tio/outputs-of (get-in current [:transforms entity]))))]
            (display-name idx entity))))))

(defn- retire-findings
  [current plan ix]
  (let [idx   (resolver current ix)
        going (retiring current idx plan)
        by-id (coll/index-by :id going)
        refs  (when (seq going)
                (remove (partial restated-away? idx plan by-id)
                        (retirement/retirement-blockers
                         current (reduce (fn [m {:keys [kinds-key id]}]
                                           (update m kinds-key (fnil conj #{}) id))
                                         {} going)
                         ix)))
        link-ends (fn [g] (let [lt (get-in current [:link-types (:id g)])]
                            (keep #(get by-id %) [(:from-object-type lt) (:to-object-type lt)])))]
    (concat
     (for [[g ds] (group-by first (rollers current going))
           owner  (distinct (cons g (link-ends g)))]
       {:rule :retire-referenced :entity-class (:kind owner) :entity (:entity owner)
        :referrers (vec (distinct (map second ds)))
        ::fnd/ctx {:form :rolls-up :link g}})
     (for [g going :when (contains? retirement/protected-statuses (:status g))]
       {:rule :retire-protected :entity-class (:kind g) :entity (:entity g)
        ::fnd/ctx {:status (:status g)}})
     (for [[ref rs] (group-by :ref refs)
           :let [g (by-id ref)]
           :when g]
       {:rule :retire-referenced :entity-class (:kind g) :entity (:entity g)
        :referrers (vec (distinct
                         (for [{:keys [in entity]} rs]
                           (str (name (get rid/kinds-key->tag in in)) " "
                                (display-name idx entity)))))
        ::fnd/ctx {:form :points-at :producers (producers-of current idx g rs)}}))))

(defn- label [kind entity] (str (name kind) " " (name entity)))

(def retire
  {:find (fn [{:keys [current plan ix] :as d}]
           (retire-findings current plan (or ix (:sutra.authoring.install.diff/ix d))))
   :says
   {:retire-referenced
    (fn [{:keys [entity-class entity referrers] {:keys [form link producers]} ::fnd/ctx}]
      (str (label entity-class entity) " is left out of the tree, but "
           (str/join ", " referrers)
           (case form
             :rolls-up  (str " rolls up over " (label (:kind link) (:entity link))
                             " — drop or rewrite that property first")
             :points-at " still point(s) at it — retire or repoint the referrer first")
           (when (seq producers)
             (str ". " (str/join ", " producers) " produces it: remove the producer FIRST"
                  " — the dataset stays, readable and no longer built — then delete the"
                  " dataset in a later push (code first, then the dataset)"))))

    :retire-protected
    (fn [{:keys [entity-class entity] {:keys [status]} ::fnd/ctx}]
      (str (label entity-class entity) " is left out of the tree but is " status
           " — an :active/:endorsed resource is not removed by omission."
           " Put it back and set `:status :deprecated` (with `:deprecation"
           " {:reason … :deadline … :replacement …}`) or `:status :experimental`"
           " on it, push that, then leave it out"))}})

(defn retire-problems
  [current plan ix]
  (fnd/scan retire {:current current :plan plan :ix ix}))

(defn orphaning
  [current going ix]
  (let [gone   (into #{} (map :id) going)
        leaves (into #{} (keep #(when (= :transforms (:kinds-key %)) (:id %))) going)
        after  (reduce (fn [s {:keys [kinds-key id]}] (update s kinds-key dissoc id))
                       current going)
        ks     (into #{} (comp (mapcat (:referring ix))
                               (keep (fn [[sec k]] (when (= :transforms sec) k))))
                     gone)
        among  (assoc after :transforms (select-keys (:transforms after) ks))]
    (vec (sort (for [[t-rid refs] (retirement/orphaned-producers among)
                     :when (and (not (leaves t-rid)) (some gone refs))]
                 (rid/->name (or (get-in current [:transforms t-rid :transform-name])
                                 t-rid)))))))
