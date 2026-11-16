(ns ^:unit platform.store-ancestry-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.store.core.branches :as branches]
            [sutra.store.core.kv :refer [rev-k rev-key]]
            [sutra.store.memo :as memo]
            [sutra.store.spi :as spi]))

(defn- store
  [commits]
  (let [calls (atom 0)
        rows  (into (sorted-map) (map (fn [[r ps]] [(rev-key r) {:rev r :parents ps}])) commits)]
    {:memo   (atom {})
     :calls  calls
     :engine (reify spi/Engine
               (-kv-read [_ _ ks]
                 (swap! calls inc)
                 (into {} (keep (fn [[_ _ k :as kk]] (when-let [v (rows k)] [kk v]))) ks))
               (-kv-scan [_ _ _ _ {:keys [upto limit]}]
                 (swap! calls inc)
                 (vec (take limit (rsubseq rows <= upto)))))}))

(defn- chain [n] (into {0 []} (map (fn [r] [r [(dec r)]])) (range 1 (inc n))))

(defn- calls-for [db rev]
  (reset! (:calls db) 0)
  (branches/ancestry db nil rev)
  @(:calls db))

(deftest the-history-is-what-the-parents-say
  (testing "a fork, its own commits and a merge, with commits of another branch in between"
    (let [db (store {0 [] 1 [0] 2 [1] 3 [1] 4 [2] 5 [3] 6 [4 5] 7 [2]})]
      (is (= #{0 1 2 4} (branches/ancestry db nil 4)))
      (is (= #{0 1 3 5} (branches/ancestry db nil 5)))
      (is (= #{0 1 2 3 4 5 6} (branches/ancestry db nil 6)))
      (is (= #{0 1 2 7} (branches/ancestry db nil 7)))))
  (testing "the same answers walking from nothing known, the merge first"
    (let [db (store {0 [] 1 [0] 2 [1] 3 [1] 4 [2] 5 [3] 6 [4 5] 7 [2]})]
      (is (= #{0 1 2 3 4 5 6} (branches/ancestry db nil 6)))))
  (testing "a revision no commit holds has only itself, and is not remembered"
    (let [db (store {0 []})]
      (is (= #{9} (branches/ancestry db nil 9)))
      (is (nil? (memo/hit (:memo db) :sutra.store.core/ancestry 9)))))
  (testing "a parent no commit holds is on the history all the same"
    (let [db (store {0 [] 5 [0 3]})]
      (is (= #{0 3 5} (branches/ancestry db nil 5))))))

(deftest a-long-history-costs-pages-once
  (let [n  100000
        db (store (chain n))]
    (testing "walking it from nothing reads pages, not one key per generation"
      (is (< (calls-for db n) 30))
      (is (= (inc n) (count (branches/ancestry db nil n)))))
    (testing "the table keeps far fewer than the walk saw"
      (is (<= (count (:sutra.store.core/ancestry @(:memo db))) 2048)))))

(deftest a-full-table-does-not-forget-the-current-head
  (let [n  20000
        db (store (chain n))]
    (branches/ancestry db nil 10000)
    (testing "new heads one after the other, far past the cap: never a second walk"
      (is (every? #(<= (calls-for db %) 1) (range 10001 (inc n)))))
    (is (<= (count (:sutra.store.core/ancestry @(:memo db))) 8192))))

(deftest an-old-head-stops-at-the-nearest-kept-set
  (let [n  50000
        db (store (assoc (chain n) 50001 [100]))]
    (branches/ancestry db nil n)
    (testing "a branch forked far below the head walks a stride, not to the root"
      (is (<= (calls-for db 50001) 2))
      (is (= 102 (count (branches/ancestry db nil 50001)))))))

(deftest a-table-past-its-cap
  (let [spec {:new {:cap 4 :newest true} :old {:cap 4 :clears #{:c}}}]
    (testing ":newest keeps its greatest keys and what came in"
      (is (= {4 :x 0 :y}
             (:new (memo/admitted {:new {1 :x 2 :x 3 :x 4 :x}} spec :new [[0 :y]])))))
    (testing "any other table starts over, with what it clears"
      (is (= {:old {9 :y} :c {}}
             (memo/admitted {:old {1 :x 2 :x 3 :x 4 :x} :c {1 1}} spec :old [[9 :y]]))))))
