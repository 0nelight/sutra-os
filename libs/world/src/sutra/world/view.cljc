(ns sutra.world.view
  (:refer-clojure :exclude [ancestors descendants])
  (:require [sutra.world.db :as db]
            [sutra.world.delta :as d]
            [sutra.world.index :as ix]))

(defn touched
  [changes sec]
  (into #{} (keep (fn [[s k]] (when (= s sec) k))) (d/nodes changes)))

(defn reach
  [succ seeds]
  (loop [order [] seen #{} todo (vec seeds) i 0]
    (if (< i (count todo))
      (let [x (nth todo i)]
        (if (contains? seen x)
          (recur order seen todo (inc i))
          (recur (conj order x) (conj seen x) (into todo (succ x)) (inc i))))
      order)))

(defn recompute
  [m ks f]
  (reduce (fn [m k] (if-some [v (f k)] (assoc m k v) (dissoc m k))) m ks))

(defn recompute-set
  [s ks keep?]
  (reduce (fn [s k] (if (keep? k) (conj s k) (disj s k))) s ks))

(defn entry-view
  [id sec f]
  {:id    id
   :reads #{sec}
   :init  (fn [db] (into {} (map (fn [[k e]] [k (f db k e)])) (get (db/world db) sec)))
   :step  (fn [old changes _ db1]
            (recompute old (touched changes sec)
                       #(some->> (db/entry db1 [sec %]) (f db1 %))))})

(defn fact-index
  ([facts-of W] (fact-index facts-of vector W))
  ([facts-of member W]
   (reduce-kv (fn [idx sec f]
                (let [es (get W sec)]
                  (if (map? es)
                    (reduce-kv (fn [idx k e]
                                 (cond-> idx (map? e) (ix/add (member sec k) (distinct (f k e)))))
                               idx es)
                    idx)))
              {} facts-of)))

(defn refiled
  [idx facts-of member changes db0 db1]
  (let [of (fn [sec k e] (when (map? e) (distinct ((facts-of sec) k e))))]
    (reduce-kv (fn [idx sec _]
                 (reduce (fn [idx k]
                           (ix/refile idx (member sec k)
                                      (of sec k (db/entry db0 [sec k]))
                                      (of sec k (db/entry db1 [sec k]))))
                         idx (touched changes sec)))
               idx facts-of)))

(defn facts
  ([id facts-of] (facts id facts-of vector))
  ([id facts-of member]
   {:id    id
    :reads (set (keys facts-of))
    :init  (fn [d] (fact-index facts-of member (db/world d)))
    :step  (fn [old changes db0 db1] (refiled old facts-of member changes db0 db1))}))

(defn descendants
  [db i]
  (reach #(db/keys-of (db/nodes db :edges [:extends %])) [i]))

(defn ancestors-of
  [db i]
  (let [ifs (get (db/world db) :interfaces)
        ext #(get-in ifs [% :extends])]
    (into #{} (filter #(contains? ifs %)) (reach ext (ext i)))))

(defn interfaces-moved
  [changes db0 db1]
  (into #{} (mapcat #(concat (descendants db0 %) (descendants db1 %)))
        (touched changes :interfaces)))

(def ancestors
  {:id    :ancestors
   :reads #{:interfaces}
   :init  (fn [db] (into {} (map (fn [[i _]] [i (ancestors-of db i)]))
                         (get (db/world db) :interfaces)))
   :step  (fn [old changes db0 db1]
            (recompute old (interfaces-moved changes db0 db1)
                       #(when (db/entry db1 [:interfaces %]) (ancestors-of db1 %))))})

(def rows [ancestors])
