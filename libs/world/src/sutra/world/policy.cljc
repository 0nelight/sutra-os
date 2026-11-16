(ns sutra.world.policy
  (:refer-clojure :exclude [parents])
  (:require [sutra.world.db :as db]
            [sutra.world.delta :as d]
            [sutra.world.index :as ix]
            [sutra.world.view :as view]))

(def default-space :space/default)
(def default-org :org/default)

(defn resolve-in
  [db sec x]
  (when (some? x)
    (if (contains? (get (db/world db) sec) x)
      x
      (first (db/keys-of (db/nodes db :by-name-str [sec (ix/nm x)]))))))

(defn- principals-of [xs] (keep #(some-> (if (map? %) (:principal %) %) str) xs))

(defn- container-orgs [e] (concat (:shared-with e) (:organizations e)))

(defn- input-ref [got x]
  (when (map? x)
    (or (:dataset x) (:object-type x) (:transform x) (:link-type x)
        (some->> (:datasource x) (got :sources)))))

(defn- own
  [got c e]
  (concat [[:container c]]
          (for [{:keys [principal role]} (:grants e) :when (and principal role)]
            [:grant (str principal) role c])
          (for [m (:markings e)] [:mark c m])
          (for [x (container-orgs e) :let [o (got :organizations x)] :when o]
            [:shared-into c o])))

(defn- resource
  [got r e]
  (concat (for [m (:markings e)] [:mark r m])
          (when-let [p (got :projects (:project e))] [[:resource-of r p]])))

(defn- stops [got x y input]
  (concat (for [m (:stop-propagating input)] [:stop-propagating x y (keyword (name m))])
          (for [o (:stop-requiring input) :let [o (got :organizations o)] :when o]
            [:stop-requiring x y o])))

(defn- pulled
  [got x e]
  (when-let [s (some->> (or (get-in e [:external-source :datasource]) (get-in e [:virtual :source]))
                        (got :sources))]
    [[:derived-from x s]]))

(defn- source-wants [e]
  (for [x [(get-in e [:external-source :datasource]) (get-in e [:virtual :source])]]
    [:sources x]))

(def ^:private sections
  {:organizations
   {:wants  (fn [e] (for [x (container-orgs e)] [:organizations x]))
    :stated (fn [got o e]
              (concat (own got o e) [[:organization o]]
                      (for [p (principals-of (concat (:members e) (:guests e)))]
                        [:org-member p o])))}
   :spaces
   {:wants  (fn [e] (for [x (concat [(:organization e)] (:organizations e) (:shared-with e))]
                      [:organizations x]))
    :stated (fn [got s e]
              (concat (own got s e)
                      [[:parent s (or (got :organizations (:organization e))
                                      (some #(got :organizations %) (:organizations e))
                                      default-org)]]))}
   :projects
   {:wants  (fn [e] (cons [:spaces (:space e)] (for [x (container-orgs e)] [:organizations x])))
    :stated (fn [got p e]
              (concat (own got p e) [[:parent p (or (got :spaces (:space e)) default-space)]]))}
   :sources
   {:wants  (fn [e] [[:projects (:project e)]])
    :stated resource}
   :object-types
   {:wants  (fn [e] (cons [:projects (:project e)] (source-wants e)))
    :stated (fn [got ot e]
              (concat (resource got ot e) (pulled got ot e)
                      (when-let [ds (or (:dataset e) (get-in e [:backing :dataset]))]
                        [[:derived-from ot ds] [:derived-from ds ot]])))}
   :datasets
   {:wants  (fn [e] (concat [[:projects (:project e)]
                             [:datasets (get-in e [:restricted-view :dataset])]
                             [:datasets (get-in e [:stream :derive :from])]]
                            (source-wants e)
                            (for [o (get-in e [:stream :derive :stop-requiring])]
                              [:organizations o])))
    :stated (fn [got ds e]
              (concat (resource got ds e) (pulled got ds e)
                      (when-let [y (got :datasets (get-in e [:restricted-view :dataset]))]
                        [[:derived-from ds y]])
                      (let [dv (get-in e [:stream :derive])]
                        (when-let [y (got :datasets (:from dv))]
                          (cons [:derived-from ds y] (stops got ds y dv))))
                      (when-let [ot (:object-type e)]
                        [[:derived-from ot ds] [:derived-from ds ot]])))}
   :transforms
   {:wants  (fn [e] (concat [[:projects (:project e)]]
                            (for [in (ix/inputs-of e) :when (map? in)
                                  w (cons [:sources (:datasource in)]
                                          (for [o (:stop-requiring in)] [:organizations o]))]
                              w)
                            (for [s (:external-systems e)] [:sources s])
                            (for [o (ix/outputs-of e) :when (map? o)] [:sources (:datasource o)])))
    :stated (fn [got t e]
              (concat (resource got t e)
                      (for [in (ix/inputs-of e)
                            :let [y (input-ref got in)] :when y
                            f (cons [:derived-from t y] (stops got t y in))]
                        f)
                      (for [s (:external-systems e) :let [y (got :sources s)] :when y]
                        [:derived-from t y])
                      (for [o (ix/outputs-of e) :let [x (input-ref got o)] :when x]
                        [:derived-from x t])))}
   :markings
   {:wants  (fn [_] [])
    :stated (fn [_ m e]
              (let [k (keyword (name m))]
                (concat (for [g (:cleared e)] [:clears (str g) k])
                        (when (and (:disjunctive e) (:category e))
                          [[:disjunctive k (str (:category e))]]))))}})

(def frame
  [[:container default-org] [:container default-space] [:parent default-space default-org]])

(defn- statement
  [db [sec k :as node]]
  (let [e (db/entry db node)]
    (when-let [{:keys [wants stated]} (and (map? e) (get sections sec))]
      (let [ws  (into [] (filter (comp some? second)) (wants e))
            got (into {} (map (fn [[s x]] [[s x] (resolve-in db s x)])) ws)]
        {:facts (set (stated (fn [s x] (get got [s x])) k e))
         :asks  (into #{} (map (fn [[s x]] [s (ix/nm x)])) ws)}))))

(defn- index-keys [f] (map-indexed (fn [i v] [(first f) i v]) (rest f)))

(defn- count-in [fv f]
  (let [n (inc (get-in fv [:n f] 0))]
    (cond-> (assoc-in fv [:n f] n)
      (= 1 n) (update :at ix/add f (index-keys f)))))

(defn- count-out [fv f]
  (let [n (dec (get-in fv [:n f]))]
    (if (pos? n)
      (assoc-in fv [:n f] n)
      (-> fv (update :n dissoc f) (update :at ix/retract f (index-keys f))))))

(defn- restate [fv node st]
  (let [old (get-in fv [:of node])
        st  (when (or (seq (:facts st)) (seq (:asks st))) st)]
    (-> (reduce count-out fv (:facts old))
        (as-> fv (reduce count-in fv (:facts st)))
        (ix/refile-record [:of] {[:asked] :asks} node st))))

(defn restated
  [fv changes db0 db1]
  (let [touched (filter (fn [[sec]] (contains? sections sec)) (d/nodes changes))
        names   (for [[sec k :as node] (d/nodes changes)
                      :let [nf (ix/name-field sec)]
                      n (cons k (when nf [(get (db/entry db0 node) nf)
                                          (get (db/entry db1 node) nf)]))
                      :when (some? n)]
                  [sec (ix/nm n)])]
    (into (set touched) (mapcat #(get-in fv [:asked %])) names)))

(def facts
  {:id    :policy/facts
   :reads (set (keys sections))
   :init  (fn [d]
            (reduce (fn [fv node] (restate fv node (statement d node)))
                    (reduce count-in {:n {} :at {}} frame)
                    (for [sec (keys sections) [k _] (get (db/world d) sec)] [sec k])))
   :step  (fn [old changes db0 db1]
            (reduce (fn [fv node] (restate fv node (statement db1 node)))
                    old (restated old changes db0 db1)))})

(defn at
  [fv rel i v]
  (get-in fv [:at [rel i v]] #{}))

(defn holds? [fv f] (contains? (:n fv) f))

(defn container? [fv x] (holds? fv [:container x]))

(defn parents [fv c] (map #(nth % 2) (at fv :parent 0 c)))

(defn within
  [fv c]
  (filterv #(container? fv %) (view/reach #(parents fv %) [c])))

(defn projects-of [fv r] (map #(nth % 2) (at fv :resource-of 0 r)))

(defn lineage-up [fv x] (map #(nth % 2) (at fv :derived-from 0 x)))

(defn down
  [fv x]
  (concat (map second (at fv :parent 1 x))
          (map second (at fv :resource-of 1 x))
          (map second (at fv :derived-from 1 x))))

(defn homed
  [fv c]
  (filter #(holds? fv [:organization %]) (within fv c)))

(defn walled
  [fv x]
  (let [of-container (fn [c]
                       (let [w (within fv c) h (filter #(holds? fv [:organization %]) w)]
                         (into (set h)
                               (when (seq h)
                                 (mapcat (fn [a] (map #(nth % 2) (at fv :shared-into 0 a))) w)))))]
    (into (of-container x) (mapcat of-container) (projects-of fv x))))

(defn- stopped [fv rel x y]
  (into #{} (keep (fn [[_ _ y' v]] (when (= y y') v))) (at fv rel 0 x)))

(defn- value
  [fv val x]
  (let [up (lineage-up fv x)]
    {:marked   (-> (into #{} (map #(nth % 2)) (at fv :mark 0 x))
                   (into (mapcat #(:marked (val %))) (parents fv x))
                   (into (mapcat #(:marked (val %))) (projects-of fv x))
                   (into (mapcat (fn [y]
                                   (let [s (stopped fv :stop-propagating x y)]
                                     (remove s (:marked (val y))))))
                         up))
     :requires (-> (if (seq (homed fv x)) #{x} #{})
                   (into (mapcat #(:requires (val %))) (projects-of fv x))
                   (into (mapcat (fn [y]
                                   (let [s (stopped fv :stop-requiring x y)]
                                     (filter (fn [g] (some #(not (s %)) (walled fv g)))
                                             (:requires (val y))))))
                         up))}))

(def ^:private none {:marked #{} :requires #{}})

(defn- of-eff [eff x]
  {:marked (get-in eff [:marked x] #{}) :requires (get-in eff [:requires x] #{})})

(defn- settle
  [fv eff A]
  (let [vals (loop [vals {} todo (vec A)]
               (if-let [x (peek todo)]
                 (let [val (fn [y] (if (contains? A y) (get vals y none) (of-eff eff y)))
                       v   (value fv val x)]
                   (if (= v (get vals x none))
                     (recur (assoc vals x v) (pop todo))
                     (recur (assoc vals x v) (into (pop todo) (filter #(contains? A %))
                                                   (down fv x)))))
                 vals))]
    (reduce-kv (fn [eff x {:keys [marked requires]}]
                 (cond-> eff
                   (seq marked)        (assoc-in [:marked x] marked)
                   (empty? marked)     (update :marked dissoc x)
                   (seq requires)      (assoc-in [:requires x] requires)
                   (empty? requires)   (update :requires dissoc x)))
               eff vals)))

(def ^:private dependent
  {:mark 0 :parent 0 :resource-of 0 :derived-from 0 :stop-propagating 0 :stop-requiring 0
   :shared-into 0 :organization 0 :container 0})

(defn- seeds [fv0 fv1 nodes]
  (for [n nodes
        :let [a (get-in fv0 [:of n :facts] #{}) b (get-in fv1 [:of n :facts] #{})]
        :when (not= a b)
        f (concat (remove b a) (remove a b))
        :let [i (dependent (first f))] :when i]
    (nth f (inc i))))

(defn moved
  [changes db0 db1]
  (let [fv0 (db/view db0 :policy/facts)
        fv1 (db/view db1 :policy/facts)
        ss  (seeds fv0 fv1 (restated fv0 changes db0 db1))]
    (set (view/reach #(down fv1 %) ss))))

(def effective
  {:id    :policy/effective
   :reads (set (keys sections))
   :init  (fn [d]
            (let [fv    (db/view d :policy/facts)
                  roots (for [f (keys (:n fv)) :when (#{:mark :container} (first f))] (second f))]
              (settle fv {:marked {} :requires {}} (set (view/reach #(down fv %) roots)))))
   :step  (fn [old changes db0 db1]
            (let [A (moved changes db0 db1)]
              (if (empty? A) old (settle (db/view db1 :policy/facts) old A))))})

(defn marked [d x] (get-in (db/view d :policy/effective) [:marked x] #{}))

(defn requires [d x] (get-in (db/view d :policy/effective) [:requires x] #{}))

(defn- class-of [m e]
  (when (and (map? e) (:category e) (some? (:rank e)))
    [(keyword (name m)) {:category (str (:category e)) :rank (long (:rank e))}]))

(def classes
  {:id    :policy/classes
   :reads #{:markings}
   :init  (fn [d]
            (reduce (fn [cl [m e]]
                      (if-let [[k {:keys [category rank] :as c}] (class-of m e)]
                        (-> cl (assoc-in [:of k] c) (assoc-in [:in category k] rank))
                        cl))
                    {:of {} :in {}} (get (db/world d) :markings)))
   :step  (fn [old changes _ db1]
            (reduce (fn [cl m]
                      (let [k  (keyword (name m))
                            c0 (get-in cl [:of k])
                            cl (cond-> (update cl :of dissoc k)
                                 c0 (update-in [:in (:category c0)] dissoc k))
                            cl (cond-> cl
                                 (and c0 (empty? (get-in cl [:in (:category c0)])))
                                 (update :in dissoc (:category c0)))
                            [_ c] (class-of m (db/entry db1 [:markings m]))]
                        (cond-> cl
                          c (-> (assoc-in [:of k] c) (assoc-in [:in (:category c) k] (:rank c))))))
                    old (view/touched changes :markings)))})

(defn dominated
  [d n]
  (let [{:keys [of in]} (db/view d :policy/classes)]
    (when-let [{:keys [category rank]} (get of n)]
      (for [[m k] (get in category) :when (and (not= m n) (>= rank k))] m))))

(def rows
  [facts effective classes])
