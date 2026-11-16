(ns sutra.world.gate.near
  (:require [sutra.world.db :as db]
            [sutra.world.delta :as d]
            [sutra.world.gate.base :as b]
            [sutra.world.index :as ix]
            [sutra.world.view :as view]
            [sutra.types.kinds :as kinds]))

(def none {:nodes #{} :facts #{}})

(defn- seqable [x] (cond (nil? x) [] (sequential? x) x :else [x]))

(defn see
  [r nodes]
  (update r :nodes into (filter #(some? (second %))) nodes))

(defn lookup [db [index fact]]
  (if (vector? index) (b/facts db (second index) fact) (db/nodes db index fact)))

(defn ask
  [r db facts]
  (reduce (fn [r f] (-> r (update :facts conj f) (update :nodes into (lookup db f)))) r facts))

(defn ask-first
  [r db f]
  (-> r
      (update :facts conj f)
      (see (take 1 (sort-by (comp str second) (lookup db f))))))

(defn found
  [db facts]
  (db/keys-of (into #{} (mapcat #(lookup db %)) facts)))

(defn world-of
  [db r]
  (reduce (fn [W node] (if-some [e (db/entry db node)] (assoc-in W node e) W))
          {} (sort-by str (:nodes r))))

(defn leaves
  [e]
  (into #{} (filter #(or (keyword? %) (string? %) (symbol? %))) (tree-seq coll? seq e)))

(defn- output-names
  [t]
  (if (seq (:outputs t))
    (keep #(when (map? %) (:name %)) (:outputs t))
    (when (some? (:output t)) [(:transform-name t)])))

(def fact-rows
  [{:id :function-names :sections #{:transforms}
    :facts (fn [_ e] (when-some [n (:function-name e)] [[:function (ix/nm n)]]))}
   {:id :output-names :sections #{:transforms}
    :facts (fn [_ e] (for [n (output-names e) :when (some? n)] [:output n]))}])

(defn- link-near
  [db l]
  (let [e     (db/entry db [:link-types l])
        prods [[:edges [:output :link-type l]]]
        r     (-> none
                  (see [[:link-types l]])
                  (see (for [x (keep e [:from-object-type :to-object-type])
                             s [:object-types :interfaces]]
                         [s x]))
                  (ask db prods))]
    (reduce (fn [r p]
              (let [t  (db/entry db [:transforms p])
                    fk (when (map? (:input t)) (:object-type (:input t)))]
                (cond-> (see r (for [s (seqable (:sources t)) :when (map? s)]
                                 [:object-types (:from-type s)]))
                  (some? fk) (-> (see [[:object-types fk]])
                                 (ask db [[:edges [:output :object-type fk]]])))))
            r (found db prods))))

(defn- sides-near
  [db t]
  (let [ifs (view/reach #(seqable (:extends (db/entry db [:interfaces %])))
                        (seqable (:implements (db/entry db [:object-types t]))))]
    (-> none
        (see (cons [:object-types t] (for [i ifs] [:interfaces i])))
        (ask db (for [x (cons t ifs)] [:edges [:link-end x]])))))

(defn- namesakes
  [own sec name-field db k]
  (let [f  (when-some [n (get (db/entry db [sec k]) name-field)] [:by-name [sec n]])
        ks (into #{k} (keep (fn [[s x]] (when (= sec s) x))) (when f (lookup db f)))]
    (reduce (fn [r x] (own db x r)) (ask none db (when f [f])) (sort-by str ks))))

(defn- transform-own
  [db k r]
  (let [t     (db/entry db [:transforms k])
        chain (view/reach (fn [x] (let [in (:input (db/entry db [:transforms x]))]
                                    (when (and (map? in) (= :transform (:kind in)))
                                      [(:transform in)])))
                          [k])
        in    (when (map? (:input t)) (:input t))
        outs  (filter map? (ix/outputs-of t))]
    (-> r
        (see (for [x chain] [:transforms x]))
        (see (when (= :object-type (:kind in)) [[:object-types (:object-type in)]]))
        (see (keep kinds/output-slot outs))
        (see (for [o outs :when (= :object-type (:kind o))] [:object-types (:object-type o)]))
        (ask db (for [o outs
                      :let [kind (:kind o)]
                      :when (kinds/is? :typed-output? kind)]
                  [:edges [:output kind (get o kind)]]))
        (ask db (for [n (output-names t) :when (some? n)]
                  [[:gate-facts :output-names] [:output n]])))))

(defn- transform-near
  [db k]
  (namesakes transform-own :transforms :transform-name db k))

(def ^:private named-types [:object-types :link-types :interfaces])

(defn- action-own
  [db k r]
  (reduce (fn [r v]
            (-> r
                (see (for [s named-types] [s v]))
                (ask db (cons [:by-name-str [:functions (ix/nm v)]]
                              (cons [[:gate-facts :function-names] [:function (ix/nm v)]]
                                    (for [s named-types] [:by-name [s v]]))))))
          (see r [[:actions k]])
          (leaves (db/entry db [:actions k]))))

(defn- action-near
  [db k]
  (namesakes action-own :actions :action-name db k))

(defn- object-set-near
  [db k]
  (see none (cons [:object-sets k]
                  (for [v (leaves (db/entry db [:object-sets k])) s named-types] [s v]))))

(def specs
  [{:id :link-types-valid? :rule :link-type/invalid :section :link-types
    :near link-near :check :link-types}
   {:id :link-sides-unique :gate :link-types-valid? :rule :link-type/invalid
    :section :object-types :near sides-near :check :link-sides}
   {:id :actions-valid? :rule :action/invalid :section :actions
    :near action-near :check :actions}
   {:id :object-sets-valid? :rule :object-set/invalid :section :object-sets
    :near object-set-near :check :object-sets :collapse true}
   {:id :transforms-valid? :rule :transform/invalid :section :transforms
    :near transform-near :check :transforms}])

(defn- reread
  [m id k r]
  (ix/refile-record m [:reads id] {[:by-node id] :nodes [:by-fact id] :facts} k r))

(defn- moved
  [facts changes db0 db1]
  (let [nodes (d/nodes changes)]
    [nodes (into (ix/moved (db/world db0) (db/world db1) nodes)
                 (b/moved-facts facts db0 db1 nodes))]))

(defn- moved-keys
  [old {:keys [id section]} changes [nodes facts]]
  (-> (view/touched changes section)
      (into (mapcat #(get-in old [:by-node id %])) nodes)
      (into (mapcat #(get-in old [:by-fact id %])) facts)))

(defn near-view
  [specs facts]
  {:id    :near
   :reads :all
   :init  (fn [db]
            (reduce (fn [m {:keys [id section near]}]
                      (reduce (fn [m k] (reread m id k (near db k)))
                              m ((b/section-keys section) db)))
                    {} specs))
   :step  (fn [old changes db0 db1]
            (let [mv (moved facts changes db0 db1)]
              (reduce (fn [m {:keys [id section near] :as s}]
                        (reduce (fn [m k]
                                  (reread m id k (when (db/entry db1 [section k])
                                                   (near db1 k))))
                                m (moved-keys old s changes mv)))
                      old specs)))})

(defn- row [facts check {:keys [id section] :as s}]
  (merge (select-keys s [:id :gate :rule :collapse :lint])
         {:scope :local
          :keys  (fn [db0 db1 changes]
                   (moved-keys (db/view db0 :near) s changes (moved facts changes db0 db1)))
          :all   (b/section-keys section)
          :check (fn [db k]
                   (when (db/entry db [section k])
                     (check (world-of db (get-in (db/view db :near) [:reads id k])) k)))}))

(defn rows
  [checks specs facts]
  (into [] (keep (fn [s] (when-let [c (get checks (:check s))] (row facts c s)))) specs))
