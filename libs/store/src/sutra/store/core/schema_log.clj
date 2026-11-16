(ns sutra.store.core.schema-log
  (:require [sutra.store.api :as api]
            [sutra.store.core.commit :refer [apply!]]
            [sutra.store.core.engine :refer [begin commit! lock! rollback!]]
            [sutra.store.core.kv :refer [branch-k clock-k kv-fx kv-get kv-pages kv-read
                                         rev-key schema-log-k schema-logged-k]])
  (:import (java.util ArrayDeque Arrays)))

(set! *warn-on-reflection* true)

(defn- with-read [db f]
  (let [h (begin db :read nil)]
    (try (f h) (finally (rollback! db h)))))

(defn- under-lock [db b f]
  (let [h (begin db :write nil)]
    (try
      (lock! db h [[:branch b]])
      (let [r (f h)] (commit! db h) r)
      (catch Throwable t (rollback! db h) (throw t)))))

(defn- grow ^longs [^longs a ^long r]
  (if (< r (alength a))
    a
    (let [b (Arrays/copyOf a (int (max (inc r) (* 2 (alength a)))))]
      (Arrays/fill b (alength a) (alength b) -1)
      b)))

(def ^:private empty-graph
  {:p1 (long-array 0) :p2 (long-array 0) :more {} :schema {} :last nil :top 0})

(defn- absorb [{:keys [p1 p2] :as g} [k {:keys [rev parents schema? branch]}]]
  (let [[a b & cs] (remove nil? parents)
        r  (long rev)
        ^longs p1 (grow p1 r)
        ^longs p2 (grow p2 r)]
    (aset p1 (int r) (long (or a -1)))
    (aset p2 (int r) (long (or b -1)))
    (cond-> (assoc g :p1 p1 :p2 p2 :last k :top (max r (long (:top g))))
      (seq cs) (assoc-in [:more r] (vec cs))
      schema?  (assoc-in [:schema r] (str branch)))))

(defn- scan
  [db h g]
  (reduce absorb g (kv-pages db h [:rev ""] (if-let [k (:last g)] {:after k} {}))))

(defn own-commits
  [{:keys [^longs p1 ^longs p2 more schema]} b head]
  (let [n     (alength p1)
        seen  (boolean-array n)
        stack (ArrayDeque.)]
    (.push stack (long head))
    (loop [out (transient [])]
      (if (.isEmpty stack)
        (sort (persistent! out))
        (let [r (long (.pop stack))]
          (if (or (neg? r) (>= r n) (aget seen r))
            (recur out)
            (do (aset seen r true)
                (doseq [p (list* (aget p1 r) (aget p2 r) (get more r))
                        :when (>= (long p) 0)]
                  (.push stack (long p)))
                (recur (cond-> out (= (str b) (get schema r)) (conj! r))))))))))

(defn- log! [db h b revs through]
  (apply! db h (conj (mapv #(kv-fx (schema-log-k b %) %) revs)
                     (kv-fx (schema-logged-k b) through))))

(defn- fill!
  [db g b]
  (under-lock db b
              (fn [h]
                (let [g   (scan db h g)
                      row (kv-get db h (branch-k b))]
                  (if (or (nil? row) (:retired? row) (kv-get db h (schema-logged-k b)))
                    [g nil]
                    (let [revs (own-commits g b (:rev row))]
                      (log! db h b revs (:top g))
                      [g (vec revs)]))))))

(defn- catch-up!
  [db h b through]
  (let [tick (kv-get db h (clock-k b [:schema]))
        {:keys [revs top]}
        (reduce (fn [acc [_ {:keys [rev schema? branch]}]]
                  (cond-> (assoc acc :top rev)
                    (and schema? (= (str b) (str branch))) (update :revs conj rev)))
                {:revs [] :top through}
                (kv-pages db h [:rev ""] {:after (rev-key through)}))]
    (when (and tick (> tick through) (not (some #{tick} revs)))
      (api/refuse! :unindexed (str "the schema log of " b " cannot be repaired: no revision "
                                   "entry holds its schema commit " tick)
                   {:branch b :tick tick}))
    (log! db h b revs top)
    revs))

(defn repair!
  [db b]
  (if (with-read db #(kv-get db % (schema-logged-k b)))
    (under-lock db b #(catch-up! db % b (kv-get db % (schema-logged-k b))))
    (or (second (fill! db (with-read db #(scan db % empty-graph)) b)) [])))

(defn- unmarked [db h]
  (let [rows (kv-pages db h [:branch ""] {})
        live (keep (fn [[b row]] (when-not (:retired? row) b)) rows)
        done (kv-read db h (map schema-logged-k live))]
    (vec (remove #(get done (schema-logged-k %)) live))))

(defn index!
  [db]
  (when-let [bs (seq (with-read db #(unmarked db %)))]
    (let [g0 (with-read db #(scan db % empty-graph))]
      (second (reduce (fn [[g out] b]
                        (let [[g revs] (fill! db g b)]
                          [g (cond-> out revs (assoc b (count revs)))]))
                      [g0 {}] bs)))))
