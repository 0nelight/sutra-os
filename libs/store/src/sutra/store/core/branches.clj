(ns sutra.store.core.branches
  (:require [sutra.store.api :as api]
            [sutra.store.memo :as memo]
            [sutra.store.core :as-alias core]
            [sutra.store.core.kv :refer [branch-k kv-get kv-read kv-scan rev-k rev-key]]
            [sutra.store.core.reads :refer [caches]]))

(set! *warn-on-reflection* true)

(defn branch-row [db h b] (kv-get db h (branch-k b)))

(defn live!
  [b row]
  (when (or (nil? row) (:retired? row))
    (api/refuse! :no-branch (str "no branch " (pr-str b)) {:branch b}))
  row)

(defn live-branch [db h b] (live! b (branch-row db h b)))

(defn bases-ks
  [{:keys [memo]} b]
  (map branch-k (first (memo/hit memo ::core/bases b))))

(defn root-of
  ([db h b row] (root-of db h b row nil))
  ([{:keys [memo] :as db} h b row kv]
   (loop [x b r row bases []]
     (let [base (get-in r [:base :branch])]
       (if (and base (< (count bases) 64))
         (recur base (if-let [[_ v] (find kv (branch-k base))] v (branch-row db h base))
                (conj bases base))
         (do (when (not= bases (first (memo/hit memo ::core/bases b)))
               (memo/put! memo caches ::core/bases [[b bases]]))
             (when (not= x b) [x (live! x r)])))))))

(defn live-root
  [db h b]
  (first (root-of db h b (branch-row db h b))))

(defn clocks-of
  [db h b]
  (let [root (live-root db h b)]
    (reduce (fn [m p] (merge-with max m (into {} (kv-scan db h [:clock p] nil))))
            {}
            (cond-> [b] root (conj root)))))

(defn- walked
  [walk known? exhausted? commits]
  (let [step  (fn [{:keys [open] :as w} [r parents]]
                (if (contains? open r)
                  (let [open (into (disj open r) (remove known?) parents)
                        w    (-> w (assoc :open open) (assoc-in [:entries r] parents))]
                    (cond-> w (empty? open) reduced))
                  w))
        w     (reduce step walk commits)
        floor (if exhausted? -1 (first (last commits)))
        lost  (filter #(> % floor) (:open w))]
    (-> w (update :open #(reduce disj % lost)) (update :lost into lost))))

(defn- histories
  [{:keys [entries lost]} known]
  (let [union (fn [a b]
                (let [[small big] (if (< (count a) (count b)) [a b] [b a])]
                  (into big (remove big) small)))]
    (reduce (fn [sets r]
              (assoc sets
                     r (conj (reduce union #{} (map #(or (get sets %) (known %)) (entries r)))
                             r)))
            (zipmap lost (map hash-set lost))
            (sort (keys entries)))))

(defn- worth-keeping
  [rev walked-revs cap]
  (let [stride (max 64 (quot (* 4 (count walked-revs)) cap))]
    (conj (set (take-nth stride (sort walked-revs))) rev)))

(defn- walk-history
  [db h rev entry known?]
  (loop [walk (walked {:entries {} :open #{rev} :lost #{}} known? false
                      [[rev (:parents entry)]])
         page 256]
    (if (empty? (:open walk))
      walk
      (let [kvs (kv-scan db h [:rev ""] {:upto (rev-key (apply max (:open walk)))
                                         :reverse? true :limit page})]
        (recur (walked walk known? (< (count kvs) page)
                       (map (fn [[k e]] [(parse-long k) (:parents e)]) kvs))
               (min 8192 (* 2 page)))))))

(defn ancestry
  [{:keys [memo] :as db} h rev]
  (let [known (get @memo ::core/ancestry {})]
    (or (known rev)
        (if-let [entry (kv-get db h (rev-k rev))]
          (let [walk (walk-history db h rev entry known)
                sets (histories walk known)
                cap  (get-in caches [::core/ancestry :cap])]
            (memo/put! memo caches ::core/ancestry
                       (select-keys sets (worth-keeping rev (keys (:entries walk)) cap)))
            (get sets rev))
          #{rev}))))

(def ^:private fork-chain-max
  64)

(defn- shown-ancestor?
  [db h b head target]
  (when-let [tb (some-> (kv-get db h (rev-k target)) :branch str)]
    (loop [b (str b), ceiling head, depth 0]
      (when (< depth fork-chain-max)
        (if (= tb b)
          (when (<= target ceiling) true)
          (let [{:keys [branch rev]} (:base (branch-row db h b))]
            (when (and branch rev)
              (recur (str branch) (min ceiling rev) (inc depth)))))))))

(defn ancestor?
  [{:keys [memo] :as db} h b head target]
  (or (= head target)
      (true? (shown-ancestor? db h b head target))
      (if-let [[on] (memo/hit memo ::core/ancestry head)]
        (contains? on target)
        (loop [seen #{} todo #{head}]
          (let [todo (into #{} (remove #(or (seen %) (< % target))) todo)]
            (cond
              (contains? todo target) true
              (empty? todo)           false
              :else (let [es (kv-read db h (map rev-k todo))]
                      (recur (into seen todo) (into #{} (mapcat (comp :parents val)) es)))))))))
