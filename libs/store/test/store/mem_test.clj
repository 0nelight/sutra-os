(ns ^:unit store.mem-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.store.api :as api]
            [sutra.store.conformance :as store-conf]
            [sutra.store.mem :as mem]
            [sutra.store.core]
            [sutra.store.core.engine]
            [sutra.store.spi]
            [sutra.store.guard]))

(def ^:private driver
  {:engine mem/engine
   :db     (fn [engine collections]
             (doto (sutra.store.core/db engine {:collections collections})
               sutra.store.core/adopt!))})

(deftest mem-conforms-to-the-db-contract
  (binding [sutra.store.core.engine/*one-handle-per-lane* false]
    (store-conf/check-store driver)
    (store-conf/check-drops-with #(mem/store mem/empty-seed {:collections %}))))

(deftest the-engine-is-the-only-driver-code
  (testing "mem speaks the spi; sutra.store.core supplies the Db"
    (is (satisfies? sutra.store.spi/Engine (mem/engine)))
    (is (satisfies? api/Db (mem/store)))))

(defn- refusal [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest core-catches-a-misbehaving-engine
  (let [leaky (let [e (mem/engine)]
                (reify sutra.store.spi/Engine
                  (-profile [_] (sutra.store.spi/-profile e))
                  (-begin [_ m l] (sutra.store.spi/-begin e m l))
                  (-lock! [_ h ks] (sutra.store.spi/-lock! e h ks))
                  (-next-rev [_ h] (sutra.store.spi/-next-rev e h))
                  (-commit! [_ h] (sutra.store.spi/-commit! e h))
                  (-rollback! [_ h] (sutra.store.spi/-rollback! e h))
                  (-kv-read [_ h ks] (sutra.store.spi/-kv-read e h ks))
                  (-kv-write! [_ h w] (sutra.store.spi/-kv-write! e h w))
                  (-kv-scan [_ h s p o] (sutra.store.spi/-kv-scan e h s p o))
                  (-ns-exists? [_ h n] (sutra.store.spi/-ns-exists? e h n))
                  (-ns-fork! [_ h f t c] (sutra.store.spi/-ns-fork! e h f t c))
                  (-ns-drop! [_ h n] (sutra.store.spi/-ns-drop! e h n))
                  (-layout! [_ h c ch] (sutra.store.spi/-layout! e h c ch))
                  (-rows [_ h c T ids] (mapv #(assoc-in % [:values :markings] nil)
                                             (sutra.store.spi/-rows e h c T ids)))
                  (-write-rows! [_ h c T r] (sutra.store.spi/-write-rows! e h c T r))
                  (-delete-rows! [_ h c T i] (sutra.store.spi/-delete-rows! e h c T i))
                  (-links [_ h c L w] (sutra.store.spi/-links e h c L w))
                  (-write-links! [_ h c L l] (sutra.store.spi/-write-links! e h c L l))
                  (-delete-links! [_ h c L i] (sutra.store.spi/-delete-links! e h c L i))
                  (-inspect [_ h c] (sutra.store.spi/-inspect e h c))))
        db    (binding [sutra.store.guard/*on* false]
                (doto (sutra.store.core/db leaky) sutra.store.core/adopt!))]
    (api/transact! db {:basis {:branch "main" :rev 0} :actor {:principal "t"}
                       :schema {:authored store-conf/world :migration []} :ops store-conf/seed})
    (testing "a row whose values carry a system column is the engine's bug, reported as such"
      (let [d (refusal #(with-open [s (api/snapshot db {:branch "main"})]
                          (api/read s {:system? true} [:objects store-conf/widget])))]
        (is (= :unavailable (:store/refusal d)))
        (is (= :row (:store/engine-contract d)))))))

(deftest the-world-is-kept-as-data
  (let [db (mem/store)]
    (testing "a world that would not read back equal is refused before the engine sees it"
      (is (= :world-verbatim
             (:store/rule
              (refusal #(api/transact! db {:basis {:branch "main" :rev 0} :actor {:principal "t"}
                                           :schema {:authored {:object-types {"x" {:f (Object.)}}}
                                                    :migration []}}))))))))

(defn- executor-db
  [pushdown profile]
  (doto (sutra.store.core/db (mem/engine) {:executors [{:pushdown pushdown :profile profile}]})
    sutra.store.core/adopt!
    (api/transact! {:basis {:branch "main" :rev 0} :actor {:principal "t"}
                    :schema {:authored store-conf/world :migration []} :ops store-conf/seed})))

(defn- head [db] (with-open [s (api/snapshot db {:branch "main"})] (api/basis s)))

(defn- tx! [db ops] (api/transact! db {:basis (head db) :actor {:principal "t"} :ops ops}))

(defn- scanner
  [answer]
  (reify sutra.store.spi/Pushdown
    (-scan [_ _ _ _] (answer))
    (-execute [_ _ _ _] sutra.store.spi/decline)
    (-explain [_ _ _ _] {})))

(def ^:private scans-rows
  {:consistency :transactional :latency {:startup-ms 0}
   :scan {:sources #{:rows} :preds {:= :exact} :project? true}})

(deftest an-executor-is-held-to-the-storage-contract
  (testing "an executor that answers a key's holders wrongly is refused under *verify*"
    (let [db (executor-db (scanner (constantly [])) scans-rows)]
      (tx! db [(store-conf/create store-conf/keyed 60 {:code "K"})])
      (let [d (binding [sutra.store.core.engine/*verify* true]
                (refusal #(tx! db [(store-conf/create store-conf/keyed 61 {:code "K"})])))]
        (is (= :unavailable (:store/refusal d)))
        (is (= :pushdown (:store/engine-contract d)))
        (is (= :scan (:kind d)))
        (is (seq (get-in d [:diff :only-reference])) "the holder it left out"))))
  (testing "an answer of another shape is refused without *verify*: the executor goes through Checked"
    (let [db (executor-db (scanner (constantly :oops)) scans-rows)
          d  (refusal #(tx! db [(store-conf/create store-conf/keyed 62 {:code "L"})]))]
      (is (= :unavailable (:store/refusal d)))
      (is (= :driver/answer (:store/engine-contract d)))))
  (testing "an executor's row the authority may not see is refused"
    (let [marked {:id (store-conf/id 1) :object-type-ref store-conf/widget :values {:name "alpha"}
                  :markings #{:secret}}
          db     (executor-db (reify sutra.store.spi/Pushdown
                                (-scan [_ _ _ _] [])
                                (-execute [_ _ _ _] [marked])
                                (-explain [_ _ _ _] {}))
                              {:consistency :transactional :latency {:startup-ms 0}
                               :plan {:ops #{:scan}}})
          d      (refusal #(with-open [s (api/snapshot db {:branch "main"})]
                             (api/read s {:denied-markings #{:secret}}
                                       [:select {:root store-conf/widget}])))]
      (is (= :unavailable (:store/refusal d)))
      (is (= :select (:store/engine-contract d)))))
  (testing "an executor errors as a refusal, not a raw exception"
    (let [db (executor-db (scanner #(throw (RuntimeException. "down"))) scans-rows)
          d  (refusal #(tx! db [(store-conf/create store-conf/keyed 63 {:code "M"})]))]
      (is (= :unavailable (:store/refusal d)))
      (is (= :scan (:store/engine-op d))))))
