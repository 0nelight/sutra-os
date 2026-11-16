(ns sutra.service.access
  (:require [clojure.set :as set]
            [sutra.service.rights :as r]
            [sutra.world.db :as db]
            [sutra.world.policy :as p]
            [sutra.world.view :as view]))

(defn- facts [d] (db/view d :policy/facts))

(defn- fact-changes
  [{:keys [db-before db-after delta]} rel]
  (let [fv0 (facts db-before)
        fv1 (facts db-after)
        fs  (into #{} (for [n (p/restated fv0 delta db-before db-after)
                            f (concat (get-in fv0 [:of n :facts]) (get-in fv1 [:of n :facts]))
                            :when (= rel (first f))]
                        f))]
    [(into #{} (comp (filter #(and (p/holds? fv0 %) (not (p/holds? fv1 %)))) (map #(subvec % 1)))
           fs)
     (into #{} (comp (filter #(and (p/holds? fv1 %) (not (p/holds? fv0 %)))) (map #(subvec % 1)))
           fs)]))

(defn- downstream
  [{:keys [db-before db-after]} xs]
  (into #{} (mapcat #(view/reach (fn [x] (p/down (facts %) x)) xs)) [db-before db-after]))

(defn- reached
  [{:keys [db-before db-after delta] :as step}]
  (downstream step (p/moved delta db-before db-after)))

(defn- held
  [{:keys [db-before db-after] :as step} ops0 ops1 reached]
  (let [roles   (into #{} (filter #(not= (get ops0 %) (get ops1 %)))
                      (concat (keys ops0) (keys ops1)))
        [gone come] (fact-changes step :grant)
        granted (concat (map #(nth % 2) (concat gone come))
                        (for [d [db-before db-after] ro roles f (p/at (facts d) :grant 1 ro)]
                          (nth f 3)))]
    (into #{} (filter #(or (p/container? (facts db-before) %) (p/container? (facts db-after) %)))
          (into reached (downstream step granted)))))

(defn- holds
  [d ops c]
  (let [fv (facts d)]
    (into #{} (for [a (p/within fv c) [_ who role] (p/at fv :grant 2 a) o (get ops role)]
                [who o c]))))

(defn- pairs [f d xs] (into #{} (for [x xs v (f d x)] [x v])))

(defn- finding [rel change xs]
  (map (fn [t] {:relation rel :fact t :change change}) (sort-by str xs)))

(defn- walls
  [d xs]
  (let [fv (facts d)]
    (into {} (keep (fn [x] (let [w (p/walled fv x)] (when (seq w) [x w])))) xs)))

(defn access-delta
  [{:keys [db-before db-after] :as step} roles0 roles1]
  (let [xs      (reached step)
        ops0    (r/role-ops roles0)
        ops1    (r/role-ops roles1)
        cs      (held step ops0 ops1 xs)
        diff    (fn [b a] [(set/difference a b) (set/difference b a)])
        [ha hr] (diff (into #{} (mapcat #(holds db-before ops0 %)) cs)
                      (into #{} (mapcat #(holds db-after ops1 %)) cs))
        [ma mr] (diff (pairs p/marked db-before xs) (pairs p/marked db-after xs))
        [ra rr] (diff (pairs p/requires db-before xs) (pairs p/requires db-after xs))
        [orm oa] (fact-changes step :org-member)
        wb      (walls db-before xs)
        wa      (walls db-after xs)
        wall    (for [x (sort-by str (set/union (set (keys wb)) (set (keys wa))))
                      :let [b (get wb x #{}) a (get wa x #{})]
                      :when (not= a b)
                      :let [opened (or (empty? a) (seq (set/difference a b)))
                            closed (or (empty? b) (seq (set/difference b a)))]]
                  {:relation :walled :fact [x (vec (sort-by str a))] :from (vec (sort-by str b))
                   :change (cond (empty? a) :removed (empty? b) :added :else :changed)
                   :expands? (boolean (and opened (seq b)))
                   :restricts? (boolean (and closed (seq a)))})]
    {:expands   (vec (concat (finding :holds :added ha) (finding :marked :removed mr)
                             (filter :expands? wall) (finding :org-member :added oa)
                             (finding :requires :removed rr)))
     :restricts (vec (concat (finding :holds :removed hr) (finding :marked :added ma)
                             (filter :restricts? wall) (finding :org-member :removed orm)
                             (finding :requires :added ra)))}))

(defn lifted
  [delta db-before]
  (let [wall #(p/walled (facts db-before) %)
        at   (fn [m x k vs] (update-in m [x k] (fnil into #{}) vs))]
    (reduce (fn [m {:keys [relation fact from]}]
              (case relation
                :marked   (at m (first fact) :markings [(second fact)])
                :requires (at m (first fact) :organizations (not-empty (wall (second fact))))
                :walled   (at m (first fact) :organizations from)
                m))
            {}
            (:expands delta))))

(defn applied
  [{:keys [db-before db-after] :as step}]
  (let [fv0 (facts db-before) fv1 (facts db-after)]
    (into {}
          (for [x (reached step)
                :when (p/container? fv1 x)
                :let [new (disj (set/difference (p/walled fv1 x) (p/walled fv0 x)) x)]
                :when (seq new)]
            [x new]))))
