(ns ^:integration platform.flaw-admission-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.builds.run.lock :as bl]
            [sutra.store.control :as ctl]
            [sutra.store.access.scope :as ctx]
            [support.pod :as pod])
  (:import (java.util.concurrent CyclicBarrier TimeUnit)))

(use-fixtures :once pod/with-mem-stores-only)

(deftest a-write-meets-only-who-sees-it
  (let [claim (fn [branch & {:keys [reads writes]}]
                {:branch branch :reads (set reads) :writes (set writes)})]
    (testing "on one branch a write meets reads and writes"
      (is (bl/conflicts? (claim "b" :writes ["x"]) (claim "b" :writes ["x"])))
      (is (bl/conflicts? (claim "b" :writes ["x"]) (claim "b" :reads ["x"]))))
    (testing "a branch reads main live, so main's write meets its read"
      (is (bl/conflicts? (claim "main" :writes ["x"]) (claim "b" :reads ["x"])))
      (is (bl/conflicts? (claim "b" :reads ["x"]) (claim "main" :writes ["x"])))
      (is (bl/conflicts? (claim nil :writes ["x"]) (claim "b" :reads ["x"]))))
    (testing "a branch's write stays on the branch"
      (is (not (bl/conflicts? (claim "b" :writes ["x"]) (claim "main" :reads ["x"]))))
      (is (not (bl/conflicts? (claim "b" :writes ["x"]) (claim "main" :writes ["x"]))))
      (is (not (bl/conflicts? (claim "b" :writes ["x"]) (claim "c" :writes ["x"]))))
      (is (not (bl/conflicts? (claim "b" :writes ["x"]) (claim "c" :reads ["x"])))))
    (testing "a write outside the instance is shared by every branch"
      (is (bl/conflicts? (claim "b" :writes ["external/e"])
                         (claim "c" :writes ["external/e"]))))))

(deftest a-waiting-claim-can-be-handed-back
  (let [store (ctx/store)
        ahead (str (random-uuid))
        me    (str (random-uuid))
        c     {:branch "main" :reads #{} :writes #{"hb"}}
        held  (bl/acquire! store (assoc c :run-id ahead))
        wait  (future (try (bl/acquire! store (assoc c :run-id me))
                           (catch Exception e (ex-data e))))]
    (try
      (is held)
      (while (not (bl/waiting? me)) (Thread/sleep 10))
      (bl/hand-back! me)
      (is (:handed-back (deref wait 15000 :timeout)))
      (is (not (bl/waiting? me)))
      (is (nil? (ctl/get store :build-claims {:run-id me})) "the claim is gone")
      (finally
        (bl/release! store ahead)
        (bl/release! store me)))))

(defn- claim! [store run-id ds]
  (ctl/insert! store :build-claims {:run-id run-id :status "queued" :reads #{} :writes #{ds}
                                  :owner "test" :queued-at ctl/now}))

(deftest over-admitted
  (let [store  (ctx/store)
        a   (str (random-uuid))
        b   (str (random-uuid))
        bar (CyclicBarrier. 2)]
    (claim! store a "ds-a")
    (claim! store b "ds-b")
    (try
      (let [rs (mapv deref
                     (mapv (fn [id]
                             (future (.await bar 5 TimeUnit/SECONDS)
                                     (first (ctl/plan! store [(#'bl/admitting id 1 #{} #{})]))))
                           [a b]))]
        (is (= 1 (count (filter :admit? rs))) (pr-str rs))
        (is (= 1 (count (:running (ctl/get store :build-slots {:name "builds"}))))))
      (finally
        (bl/release! store a)
        (bl/release! store b)))))
