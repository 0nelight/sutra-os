(ns sutra.store.core.log-kind
  (:require [sutra.store.core.commit :refer [apply!]]
            [sutra.store.core.engine :refer [begin commit! lock! rollback!]]
            [sutra.store.core.kv :refer [branch-k kinded-through kv-fx kv-get kv-pages kv-read
                                         kv-scan log-indexed-k log-key log-kind-k
                                         log-kinded-k]]))

(set! *warn-on-reflection* true)

(def ^:private page-size 2000)

(defn- with-read [db f]
  (let [h (begin db :read nil)]
    (try (f h) (finally (rollback! db h)))))

(defn- under-lock [db b f]
  (let [h (begin db :write nil)]
    (try
      (lock! db h [[:branch b]])
      (let [r (f h)] (commit! db h) r)
      (catch Throwable t (rollback! db h) (throw t)))))

(defn- clear!
  [db b]
  (while (= page-size (under-lock db b
                              (fn [h]
                                (let [ks (kv-scan db h [:log-kind b]
                                                  {:limit page-size :keys-only? true})]
                                  (when (seq ks)
                                    (apply! db h (mapv #(kv-fx [:log-kind b (first %)] nil) ks)))
                                  (count ks)))))))

(defn- extend!
  [db b base from]
  (loop [after from n 0]
    (let [r (under-lock
             db b
             (fn [h]
               (let [row (kv-get db h (branch-k b))]
                 (when (and row (not (:retired? row)) (= base (:base row)))
                   (let [kvs   (kv-scan db h [:log-of b]
                                        (cond-> {:limit page-size} after (assoc :after after)))
                         done? (< (count kvs) page-size)
                         fx    (cond-> (mapv (fn [[_ e]]
                                               (kv-fx (log-kind-k b (:kind e) (:rev e) (:index e))
                                                      e))
                                             kvs)
                                 done? (conj (kv-fx (log-kinded-k b)
                                                    {:rev (:rev row) :base base})))]
                     (apply! db h fx)
                     {:last (or (first (peek kvs)) after) :n (count kvs) :done? done?})))))]
      (cond
        (nil? r)     nil
        (:done? r)   (+ n (:n r))
        :else        (recur (:last r) (+ n (long (:n r))))))))

(defn- index-branch! [db b]
  (let [[row mark] (with-read db #(let [kv (kv-read db % [(branch-k b) (log-kinded-k b)])]
                                    [(get kv (branch-k b)) (get kv (log-kinded-k b))]))
        m          (kinded-through row mark)]
    (cond
      (= m (:rev row)) nil
      m                (extend! db b (:base row) (log-key m Integer/MAX_VALUE))
      :else            (do (clear! db b) (extend! db b (:base row) nil)))))

(defn index!
  [db]
  (when (with-read db #(kv-get db % log-indexed-k))
    (let [bs (with-read db #(into [] (keep (fn [[b row]] (when-not (:retired? row) b)))
                                  (kv-pages db % [:branch ""] {})))]
      (into {} (keep (fn [b] (when-let [n (index-branch! db b)] [b n]))) bs))))
