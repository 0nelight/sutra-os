(ns sutra.store.core.inherited-edits
  (:require [clojure.string :as str]
            [sutra.store.core.branches :refer [live-root]]
            [sutra.store.core.commit :refer [apply!]]
            [sutra.store.core.engine :refer [begin commit! lock! rollback!]]
            [sutra.store.core.kv :refer [branch-k kv-fx kv-get kv-pages]]))

(set! *warn-on-reflection* true)

(def ^:private done-k [:store "" "edits-inherited-compacted"])

(defn- with-read [db f]
  (let [h (begin db :read nil)]
    (try (f h) (finally (rollback! db h)))))

(defn redundant
  [own of-root detached]
  (let [kept? (fn [k] (some #(str/starts-with? k (str % "|")) detached))]
    (into [] (keep (fn [[k v]] (when (and (= v (get of-root k)) (not (kept? k))) k))) own)))

(defn- compact-branch! [db b]
  (let [h (begin db :write nil)]
    (try
      (lock! db h [[:branch b]])
      (let [row  (kv-get db h (branch-k b))
            root (when (and row (not (:retired? row))) (live-root db h b))
            ks   (when root
                   (redundant (kv-pages db h [:edited b] {})
                              (into {} (kv-pages db h [:edited root] {}))
                              (map first (kv-pages db h [:detached b] {}))))]
        (when (seq ks) (apply! db h (mapv #(kv-fx [:edited b %] nil) ks)))
        (commit! db h)
        (count ks))
      (catch Throwable t (rollback! db h) (throw t)))))

(defn compact!
  [db]
  (when-not (with-read db #(kv-get db % done-k))
    (let [bs  (with-read db #(into [] (keep (fn [[b row]] (when-not (:retired? row) b)))
                                   (kv-pages db % [:branch ""] {})))
          out (into {} (keep (fn [b] (let [n (compact-branch! db b)] (when (pos? n) [b n]))))
                    bs)
          h   (begin db :write nil)]
      (try (apply! db h [(kv-fx done-k true)]) (commit! db h)
           (catch Throwable t (rollback! db h) (throw t)))
      out)))
