(ns ^:integration platform.singleton-lease-test
  (:require [sutra.store.access.scope :as ctx]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.coord.api :as coord]
            [sutra.builds.run.leader :as leader]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-stores-only)

(defn- other-node-acquire
  [n]
  (coord/acquire! (ctx/coord) {:name n :owner "other-node" :ttl-ms 60000}))

(deftest one-holder-and-a-clean-handover
  (try
    (let [{:keys [leader? epoch]} (leader/start!)]
      (is leader? "a lone node leads at once")
      (is (pos? epoch))
      (is (nil? (other-node-acquire "leader")) "a second node cannot take it while it is held")
      (leader/stop!)
      (testing "after stop! the lease is free for the successor — no timeout to wait out"
        (let [l (other-node-acquire "leader")]
          (is (some? l))
          (coord/release! (ctx/coord) l)))
      (testing "re-acquiring moves the epoch, so each tenure is distinguishable"
        (is (< epoch (:epoch (leader/start!))))))
    (finally (leader/stop!))))

(deftest an-armed-node-without-the-lease-is-not-leader
  (let [l (other-node-acquire "leader")]
    (try
      (is (some? l))
      (let [{:keys [leader?]} (leader/start!)]
        (is (false? leader?) "the singleton loops stand down on this node")
        (is (nil? (leader/as-leader (constantly :ran))) "and nothing runs as leader"))
      (finally
        (leader/stop!)
        (coord/release! (ctx/coord) l)))))

(deftest with-lock-refuses-while-held-elsewhere
  (let [calls (atom [])
        n     (leader/lock-name "install" "main")]
    (leader/with-lock (ctx/coord) n
      (fn []
        (is (nil? (other-node-acquire n)) "another node sees the lock held")
        (swap! calls conj :outer))
      #(swap! calls conj :outer-busy))
    (is (= [:outer] @calls))
    (let [l (other-node-acquire n)]
      (is (some? l) "released after the body")
      (try
        (leader/with-lock (ctx/coord) n #(swap! calls conj :inner) #(swap! calls conj :busy))
        (is (= :busy (peek @calls)) "a lock another node holds is busy here")
        (finally (coord/release! (ctx/coord) l))))
    (leader/with-lock (ctx/coord) n #(swap! calls conj :again) #(swap! calls conj :still-busy))
    (is (= :again (peek @calls)))))
