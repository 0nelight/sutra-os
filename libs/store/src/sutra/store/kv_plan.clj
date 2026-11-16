(ns sutra.store.kv-plan
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [sutra.store.model :as model]))

(set! *warn-on-reflection* true)

(def ^:private sep "\u001f")

(def dropped-mark :sutra.store.kv-engine/dropped)

(defn spaces [prefix]
  {:ns (keyword prefix "ns") :objects (keyword prefix "objects") :links (keyword prefix "links")})

(defn part [ns K] (str ns sep (pr-str K)))

(defn- ns-prefix [ns] (str ns sep))

(defn key-of [id] (pr-str id))

(defn meta-key [sp ns] [(:ns sp) "" ns])

(defn hist-keys [sp sec ns K ids] (map #(vector (sec sp) (part ns K) (key-of %)) ids))

(defn by-key [read] (into {} (map (fn [[[_ _ k] v]] [k v])) read))

(defn own-kinds
  [ns parts]
  (into [] (comp (filter #(str/starts-with? % (ns-prefix ns)))
                 (map #(edn/read-string (subs % (count (ns-prefix ns))))))
        parts))

(defn exists? [ns m]
  (if (= "main" ns) (not= dropped-mark m) (and (some? m) (not= dropped-mark m))))

(defn root-of [m K as-of]
  (when-not (model/detached-at? (:detached m) K as-of) (:root m)))

(defn section
  [own root-hist as-of]
  (reduce-kv (fn [acc k vs]
               (if-let [[v] (model/own-at vs (:rev as-of))]
                 (if v (assoc acc k v) (dissoc acc k))
                 acc))
             (model/live root-hist (:root-rev as-of)) own))

(defn rows [sig T ids section]
  (let [m (update-vals section #(model/projected sig T %))]
    (if (nil? ids) (vec (vals m)) (into [] (keep #(get m (key-of %))) ids))))

(defn links [where section] (filterv (model/matches where) (vals section)))

(defn appended
  [sp sec ns K rev old entries]
  (into {} (for [[id v] entries :let [k (key-of id)]]
             [[(sec sp) (part ns K) k] (conj (get old k []) [rev v])])))

(defn revision [[k _ a b]]
  (case k
    :rename-attribute #(model/renamed % a b)
    :drop-attribute   #(model/dropped % a)
    :clear-attribute  #(model/cleared % a)))

(defn revised
  [sp ns rev T own f]
  (appended sp :objects ns T rev own
            (for [[_ vs] own
                  :let [v (model/at vs nil) v' (some-> v f)]
                  :when (and v (not= v v'))]
              [(:id v) v'])))

(defn cleared [sp sec ns K ks] (into {} (for [k ks] [[(sec sp) (part ns K) k] nil])))

(defn detached
  [sp sec ns K rev m own root-hist]
  (assoc (appended sp sec ns K rev own
                   (for [[k v] (model/live root-hist nil) :when (not (contains? own k))]
                     [(:id v) v]))
         (meta-key sp ns) (update m :detached #(merge {K rev} %))))

(defn forked
  [sp from to m held]
  (if-not (:root m)
    {(meta-key sp to) {:root from}}
    (into {(meta-key sp to) (select-keys m [:root :detached])}
          (for [[[sec K] kvs] held [k v] kvs] [[(sec sp) (part to K) k] v]))))

(defn dropped
  [sp ns held]
  (into {(meta-key sp ns) dropped-mark}
        (for [[[sec K] ks] held k ks] [[(sec sp) (part ns K) k] nil])))

(defn inspect
  [m held sig]
  {:missing   {:object-types [] :link-types []}
   :orphans   (vec (for [[sec in] [[:objects :types] [:links :links]]
                         K (get held sec)
                         :when (not (contains? (get sig in) K))]
                     {:name (str K)}))
   :local     (if (:root m) (set (keys (:detached m))) (set (:objects held)))
   :inherited (if (:root m)
                (into #{} (remove (set (keys (:detached m)))) (keys (:types sig)))
                #{})})
