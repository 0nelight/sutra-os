(ns ^:unit store.machine-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.kit.anomaly :as anomaly]
            [sutra.store.api :as api]
            [sutra.store.core.engine]
            [sutra.store.conformance :as store-conf]
            [sutra.store.machine :as machine]
            [sutra.store.mem :as mem]))

(def ^:private authority {:system? true})
(def ^:private actor {:principal "machine-test"})

(def ^:private world store-conf/world)

(defn- at-head!
  [db tx]
  (let [b (with-open [s (api/snapshot db {:branch "main"})] (api/basis s))]
    (api/transact! db (merge {:basis b :actor actor} tx))))

(defn- elsewhere!
  [db tx]
  @(future (at-head! db tx)))

(defn- seeded []
  (doto (mem/store mem/empty-seed {:collections store-conf/collections})
    (at-head! {:schema {:authored world :migration []} :ops store-conf/seed})))

(defn- size [db n]
  (with-open [s (api/snapshot db {:branch "main"})]
    (get-in (first (api/read s authority [:objects store-conf/widget [(store-conf/id n)]]))
            [:values :size])))

(defn- grow
  [db n f & [{:keys [fences before]}]]
  (machine/command! db {:fences fences}
                    (fn [s]
                      (some-> before (apply []))
                      (let [o (first (api/read s authority
                                               [:objects store-conf/widget [(store-conf/id n)]]))]
                        {:result o
                         :tx {:actor  actor
                              :ops    [{:op :update-object :type store-conf/widget
                                        :payload
                                          {:id (store-conf/id n)
                                           :values {:size (f (get-in o [:values :size]))}}}]
                              :fences [[:object store-conf/widget (store-conf/id n)
                                        (api/object-version o)]]}}))))

(deftest a-commit-whose-read-moved-is-decided-again
  (let [db       (seeded)
        attempts (atom 0)]
    (grow db 1 #(inc (or % 0))
          {:before #(when (= 1 (swap! attempts inc))
                      (elsewhere! db {:ops [{:op :update-object :type store-conf/widget
                                           :payload {:id (store-conf/id 1) :values {:size 5}}}]}))})
    (is (= 2 @attempts) "the first decision read a value that moved before its commit")
    (is (= 6 (size db 1)) "the second decision saw the moved value — no lost update")))

(deftest an-unrelated-commit-does-not-disturb-a-commit
  (let [db       (seeded)
        attempts (atom 0)]
    (grow db 2 (constantly 1)
          {:before #(when (= 1 (swap! attempts inc))
                      (elsewhere! db
                                  {:ops
                                     [(store-conf/create
                                       store-conf/other 70 {:name "elsewhere"})]}))})
    (is (= 1 @attempts) "a write to another object trips no fence")))

(deftest command-is-read-decide-write
  (let [db   (seeded)
        bump (fn []
               (machine/command! db {}
                                 (fn [s]
                                   (let [v (api/read s nil [:records/get :conformance/setting :n])]
                                     {:result (inc (or v 0))
                                      :tx {:actor actor
                                           :records [{:coll :conformance/setting :key :n
                                                      :expect (if v v api/absent)
                                                      :value (inc (or v 0))}]}}))))]
    (is (= [1 2 3] (mapv (comp :result (fn [_] (bump))) (range 3))))
    (testing "concurrent bumps each land once"
      (let [fs (doall (repeatedly 8 #(future (bump))))]
        (run! deref fs)
        (with-open [s (api/snapshot db {:branch "main"})]
          (is (= 11 (api/read s nil [:records/get :conformance/setting :n]))))))))

(deftest a-commit-carries-the-lease-fences-it-is-given
  (let [db (seeded)]
    (at-head! db {:fences [[:lease "leader" 3]]})
    (testing "a commit fenced on a superseded epoch is refused, not retried"
      (let [e (try (grow db 1 (constantly 9) {:fences [[:lease "leader" 2]]}) nil
                   (catch clojure.lang.ExceptionInfo e (ex-data e)))]
        (is (= :fenced (:store/refusal e)))
        (is (not= 9 (size db 1)))))
    (testing "the current epoch holds"
      (grow db 1 (constantly 9) {:fences [[:lease "leader" 3]]})
      (is (= 9 (size db 1))))))

(defn- pooled
  [db n]
  (let [pool (java.util.concurrent.Semaphore. n)
        take! #(when-not (.tryAcquire pool 1 java.util.concurrent.TimeUnit/SECONDS)
                 (throw (ex-info "connection is not available" {})))]
    (reify api/Db
      (-snapshot [_ b]
        (take!)
        (let [s (try (api/-snapshot db b) (catch Throwable t (.release pool) (throw t)))]
          (reify
            api/Snapshot
            (-basis [_] (api/-basis s))
            (-answer [_ a q] (api/-answer s a q))
            (-overlay [_ ops] (api/-overlay s ops))
            (-close [_] (try (api/-close s) (finally (.release pool))))
            java.io.Closeable
            (close [_] (try (api/-close s) (finally (.release pool)))))))
      (-prepare [_ tx]
        (take!)
        (let [p (try (api/-prepare db tx) (catch Throwable t (.release pool) (throw t)))]
          (if (anomaly/refusal? p)
            (do (.release pool) p)
            (reify api/Prepared
              (-commit! [_] (try (api/-commit! p) (finally (.release pool))))
              (-abort! [_] (try (api/-abort! p) (finally (.release pool))))))))
      (-branch! [_ op] (api/-branch! db op))
      (-read-at [_ b a q] (api/-read-at db b a q)))))

(deftest commands-in-parallel-share-a-pool-of-one
  (let [db   (pooled (seeded) 1)
        n0   (or (size db 3) 0)
        runs (doall (for [_ (range 8)] (future (grow db 3 #(inc (or % 0))))))]
    (is (every? some? (map deref runs)) "no command holds its read while it commits")
    (is (= (+ n0 8) (size db 3)))))

(deftest a-thread-holds-one-handle-per-lane
  (let [db (seeded)]
    (binding [sutra.store.core.engine/*one-handle-per-lane* true]
      (with-open [_ (api/snapshot db {:branch "main" :lane :auth})]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"nested connection acquisition"
                              (with-open [_ (api/snapshot db {:branch "main" :lane :auth})]))
            "a second read beside the first")
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"nested connection acquisition"
                              (at-head! db {:lane :auth :ops store-conf/seed}))
            "a commit beside the read")
        (with-open [_ (api/snapshot db {:branch "main"})]
          (is true "another lane is another pool")))
      (is (some? (grow db 4 #(inc (or % 0)))) "a command closes its read before it commits"))))
