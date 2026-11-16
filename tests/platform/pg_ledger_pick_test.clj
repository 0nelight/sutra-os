(ns ^:integration platform.pg-ledger-pick-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [sutra.builds.ledger :as ledger]
            [sutra.store.access.scope :as ctx]
            [sutra.store.control :as ctl]
            [sutra.store.core.engine :as engine]
            [sutra.store.storage.command :as cmd]
            [support.pod :as pod]))

(use-fixtures :once pod/with-stores-only)

(defn- do! [effects-of & args]
  (let [fx (apply effects-of args)]
    (cmd/execute! (ctx/store) (if (map? fx) [fx] fx))))

(defn- recency [r] (or (:ended-at r) (:started-at r) ""))

(defn- newest-per [k rows]
  (reduce (fn [m r]
            (let [v (some-> (get r k) str not-empty)]
              (cond-> m
                (and v (or (not (contains? m v)) (pos? (compare (recency r) (recency (get m v))))))
                (assoc v r))))
          {} rows))

(defn- at [day h] (format "2026-09-%02dT%02d:00:00Z" day h))

(deftest the-store-picks-what-the-whole-ledger-said
  (let [b   (str "pick-" (random-uuid))
        rid (fn [kind i] (format "ri.sutra.main.%s.%08d-0000-0000-0000-000000000000" kind i))
        ts  (mapv #(rid "transform" %) (range 4))
        ds  (mapv #(rid "dataset" %) (range 3))]
    (doseq [i (range 24)
            :let [t   (ts (mod i 4))
                  run (str "r" i)
                  jr  (ledger/job-rid run t)
                  day (inc (mod (* 7 i) 23))]]
      (do! ledger/open-job {:job-rid jr :run-id run :transform-rid t :branch b :attempt 1
                            :at (at day 1)})
      (case (mod i 3)
        0 (do! ledger/finish-job {:job-rid jr :outputs [] :snapshot-id "c" :rows-out i
                                  :at (at day (+ 2 (mod i 5)))})
        1 (do! ledger/fail-job {:job-rid jr :error (str "boom " i) :at (at day 3)})
        2 nil))
    (doseq [i (range 15)
            :let [day (inc (mod (* 5 i) 23))]]
      (ledger/record-txn! (ctx/store) {:dataset-rid (ds (mod i 3)) :branch b :run-id "r"
                                       :job-rid "j" :snapshot-id "c" :type :APPEND
                                       :status :COMMITTED :row-count i :row-delta 1
                                       :started-at (at day 4) :ended-at (at day 5)}))
    (binding [engine/*verify* true]
      (let [jobs  (mapv #(update % :status ledger/read-job-state)
                        (ctl/list (ctx/store) :ledger-jobs {:branch b}))
            cols  [:status :rows-out :error :run-id :code-sha :started-at :ended-at :jobspec-id]
            want  (fn [rs] (update-vals (newest-per :transform-rid rs) #(select-keys % cols)))
            got   (ledger/last-jobs-per-transform (ctx/store) b)]
        (is (= (want jobs) (:any got)))
        (is (= (want (filter #(= "COMPLETED" (:status %)) jobs)) (:ok got)))
        (is (= (want (filter #(= "FAILED" (:status %)) jobs)) (:failed got)))
        (is (= (set (keys (want jobs)))
               (set (keys (ledger/last-job-per-transform (ctx/store) b))))))
      (let [txns (ctl/list (ctx/store) :ledger-txns {:branch b})
            got  (ledger/last-txn-per-dataset (ctx/store) b)]
        (doseq [[d rs] (group-by :dataset-rid txns)
                :let [newest (get (newest-per :dataset-rid rs) d)
                      g      (get got d)]]
          (is (= (:row-count newest) (:row_count g)) d)
          (is (= (reduce #(if (neg? (compare %2 %1)) %2 %1) (map :started-at rs)) (:created g)))
          (is (= (count rs) (long (:txns g)))))))
    (doseq [t [:ledger-jobs :ledger-txns]] (ctl/delete! (ctx/store) t {:branch b}))))
