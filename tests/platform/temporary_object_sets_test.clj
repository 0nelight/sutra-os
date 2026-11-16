(ns ^:unit platform.temporary-object-sets-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.objects.saved-sets :as temp-sets]
            [support.mem :as mem]))

(deftest an-object-set-round-trips-and-is-creator-scoped
  (let [store  (mem/store)
        rid "ri.sutra.main.object-set.mem-test-1"
        ir  {:op :static :ids ["a" "b"]}]
    (temp-sets/create! store {:rid rid :ir ir :kind :static
                           :created-by "alice" :branch "main" :ttl-seconds 3600})
    (is (= ir (:ir (temp-sets/fetch store rid "alice"))))
    (testing "the creator's listing sees it"
      (is (some #(= rid (:rid %)) (temp-sets/list-for store "alice"))))
    (testing "another principal cannot see it at all"
      (is (nil? (temp-sets/fetch store rid "mallory"))))
    (testing "revoke is creator-scoped and idempotent"
      (is (false? (temp-sets/revoke! store rid "mallory")))
      (is (true?  (temp-sets/revoke! store rid "alice")))
      (is (false? (temp-sets/revoke! store rid "alice")))
      (is (nil? (temp-sets/fetch store rid "alice"))))))

(deftest an-object-set-expires-on-the-store-clock
  (let [t     (atom (java.util.Date. 0))
        store    (mem/store #(deref t))
        rid   "ri.sutra.main.object-set.mem-test-expiring"
        later (fn [secs] (reset! t (java.util.Date. (* 1000 secs))))]
    (temp-sets/create! store {:rid rid :ir {:op :static :ids []} :kind :static
                           :created-by "alice" :branch "main" :ttl-seconds 60})
    (is (some? (temp-sets/fetch store rid "alice")))
    (later 61)
    (is (nil? (temp-sets/fetch store rid "alice")) "past its ttl it reads as absent")
    (is (empty? (temp-sets/list-for store "alice")))
    (is (= [1] (temp-sets/sweep-expired! store)))
    (is (= [0] (temp-sets/sweep-expired! store)) "the sweep removed it")))
