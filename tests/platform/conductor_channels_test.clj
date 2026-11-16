(ns ^:unit platform.conductor-channels-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.foundation.contracts :as ct]
            [sutra.builds.ledger.facts :as facts]))

(defn- contract-keys []
  (->> (get ct/registry :sutra.foundation.contracts/job-result)
       rest
       (map #(if (vector? %) (first %) %))
       set))

(defn- fold [fs notes]
  (reduce (fn [f args] (apply facts/fold-note f args)) fs notes))

(deftest channel-table-is-the-job-result-contract
  (is (= (contract-keys) (set (map :key facts/channels)))
      "a channel the contract does not name travels back unchecked"))

(deftest only-keyed-channels-are-job-facts
  (is (every? :by (filter :job-fact? facts/channels))
      "a Job fact is read at [channel transform] — an unkeyed channel has no such slot"))

(deftest each-channel-folds-its-own-way
  (let [r (fold (facts/empty-facts true)
                [[:rows-in :t 3] [:rows-in :t 4]
                 [:code-sha :t "a"] [:code-sha :t "b"]
                 [:job-log :t "one"] [:job-log :t "two"]
                 [:dropped-cols :t #{:x}] [:dropped-cols :t #{:y}]
                 [:bootstrapped :t] [:bootstrapped :t]])]
    (is (= {:t 7} (:rows-in r))          "rows sum")
    (is (= {:t "b"} (:code-sha r))       "sha is the last one")
    (is (= {:t "one\ntwo"} (:job-log r)) "the log appends, it does not replace")
    (is (= {:t #{:x :y}} (:dropped-cols r)) "dropped columns union")
    (is (= #{:t} (:bootstrapped r))      "a set conj is idempotent")
    (is (ct/valid? :sutra.foundation.contracts/job-result r))))

(deftest an-empty-note-contributes-nothing
  (let [fs (facts/empty-facts true)]
    (is (= fs (fold fs [[:job-log :t "   "] [:job-log nil "text"]
                        [:dropped-cols :t #{}] [:rows-in nil 5]])))))

(deftest trace-is-opt-in
  (testing "no trace channel unless the build asked for one"
    (let [fs (facts/empty-facts false)]
      (is (= fs (facts/fold-note fs :trace {:kind :x})))
      (is (not (contains? fs :trace)))))
  (is (= [{:kind :x}] (:trace (facts/fold-note (facts/empty-facts true) :trace {:kind :x})))))

(deftest job-facts-carry-the-marked-channels
  (let [f (facts/job-facts (fold (facts/empty-facts false) [[:rows-in :t 2] [:job-log :t "hi"]])
                           :t "2026-01-01T00:00:00Z")]
    (is (= (set (conj facts/job-fact-keys :at)) (set (keys f))))
    (is (= "2026-01-01T00:00:00Z" (:at f)))
    (is (= 2 (:rows-in f)))
    (is (= "hi" (:job-log f)))))

(deftest notes-fold-in-order-as-data
  (let [notes [[:rows-in :t 2] [:job-log :t "a"] [:rows-in :t 3] [:bootstrapped nil :t]]
        r     (facts/fold-notes (facts/empty-facts false) notes)]
    (is (= (fold (facts/empty-facts false)
                 [[:rows-in :t 2] [:job-log :t "a"] [:rows-in :t 3] [:bootstrapped :t]])
           r))
    (is (= {:t 5} (:rows-in r)))))

(deftest input-sigs-keep-the-first-seen
  (is (= {:t {:in "s1"}}
         (:input-sigs (facts/fold-notes (facts/empty-facts false)
                                        [[:input-sigs :t {:in "s1"}] [:input-sigs :t {:in "s2"}]])))
      "a retry's later signature does not replace what the job started on"))

(deftest a-note-on-no-channel-is-a-bug
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no channel :rows-out"
                        (facts/fold-note (facts/empty-facts false) :rows-out :t 1))))

(deftest deltas-fold-by-their-own-table
  (let [acc (facts/start facts/deltas false)
        r   (-> acc
                (facts/fold-delta {:obj-decls [:a] :rows {:x [1]} :staged {:x "ds"}})
                (facts/fold-delta {:obj-decls [:b] :rows {:y [2]} :staged nil}))]
    (is (= (set (map :key facts/deltas)) (set (keys acc))))
    (is (= [:a :b] (:obj-decls r)) "decls concatenate")
    (is (= {:x [1] :y [2]} (:rows r)) "indexes merge")
    (is (= {:x "ds"} (:staged r)) "a node that staged nothing leaves the staging as it was")
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no channel :bogus"
                          (facts/fold-delta acc {:bogus 1})))))

(deftest no-facts-no-job-facts
  (is (= {:at "t" :rows-in nil :code-sha nil :pending-observed nil :job-log nil :job-spans nil}
         (facts/job-facts nil :t "t"))))
