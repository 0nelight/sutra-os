(ns ^:unit platform.build-schedule-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [sutra.builds.run.lock :as lock]
            [sutra.builds.run.schedule :as schedule]))

(def ^:private limits
  (gen/hash-map :cap (gen/choose 1 5) :waiting-max (gen/choose 0 6)))

(def ^:private event
  (gen/one-of
   [(gen/return {:kind :poll})
    (gen/fmap (fn [id] {:kind :taken :request {:run-id (str "r" id)}}) (gen/choose 0 30))
    (gen/fmap (fn [id] {:kind :finished :run-id (str "r" id)}) (gen/choose 0 30))
    (gen/fmap (fn [id] {:kind :wait :run-id (str "r" id)}) (gen/choose 0 30))
    (gen/fmap (fn [id] {:kind :admit :run-id (str "r" id)}) (gen/choose 0 30))
    (gen/return {:kind :quiesce})]))

(defn- takes? [effects] (some #(= [:take] %) effects))

(defn- busy [{:keys [running]} waiting] (count (remove waiting running)))

(defn- run-lane
  [lim events]
  (loop [s schedule/initial waiting #{} [e & more] events now 0 seen []]
    (if-not e
      seen
      (let [now (+ now 1000)]
        (case (:kind e)
          :wait  (recur s (cond-> waiting (contains? (:running s) (:run-id e))
                                  (conj (:run-id e)))
                        more now seen)
          :admit (recur s (disj waiting (:run-id e)) more now seen)
          (let [e'        (merge e lim {:waiting waiting :now now})
                [s' effs] (schedule/step s e')]
            (recur s' (into #{} (filter (:running s')) waiting) more now
                   (conj seen [s e' effs]))))))))

(deftest waiting-runs-hold-no-places
  (is (:pass?
       (tc/quick-check
        300
        (prop/for-all
         [lim limits
          n-busy (gen/choose 0 6)
          n-wait (gen/choose 0 6)]
         (let [busy-ids (map #(str "b" %) (range n-busy))
               wait-ids (map #(str "w" %) (range n-wait))
               state    (assoc schedule/initial :running (set (concat busy-ids wait-ids)))
               e        (merge lim {:kind :poll :now 1 :waiting (set wait-ids)})
               alone    (merge e {:waiting #{}})
               without  (assoc schedule/initial :running (set busy-ids))]
           (= (boolean (takes? (second (schedule/step state e))))
              (and (< n-busy (:cap lim)) (< n-wait (:waiting-max lim)))
              (boolean (and (takes? (second (schedule/step without alone)))
                            (< n-wait (:waiting-max lim))))))))))
  (testing "the lane takes again once a run it holds goes to wait"
    (let [s (assoc schedule/initial :running #{"a"})
          e {:kind :poll :now 1 :cap 1 :waiting-max 4}]
      (is (not (takes? (second (schedule/step s (assoc e :waiting #{}))))))
      (is (takes? (second (schedule/step s (assoc e :waiting #{"a"}))))))))

(deftest the-lane-never-takes-past-its-places
  (is (:pass?
       (tc/quick-check
        300
        (prop/for-all
         [lim limits
          events (gen/vector event 0 60)]
         (every? (fn [[s {:keys [waiting]} effs]]
                   (let [s' (first (schedule/step s {:kind :none}))]
                     (or (not (takes? effs))
                         (and (not (:stopping? s'))
                              (< (busy s waiting) (:cap lim))
                              (< (count (filter waiting (:running s))) (:waiting-max lim))))))
                 (run-lane lim events)))))))

(deftest a-drain-hands-back-every-run-and-takes-nothing-more
  (is (:pass?
       (tc/quick-check
        200
        (prop/for-all
         [lim limits
          events (gen/vector event 0 40)]
         (let [seen (run-lane lim events)
               q    (some #(when (= :quiesce (:kind (second %))) %) seen)]
           (or (nil? q)
               (let [[s _ effs] q
                     after (drop-while #(not= q %) seen)]
                 (and (= (set (keep #(when (= :hand-back (first %)) (second %)) effs))
                         (:running s))
                      (not-any? (fn [[_ _ e]] (takes? e)) after))))))))))

(deftest tidy-once-a-minute
  (let [e {:kind :poll :cap 1 :waiting-max 1 :waiting #{}}
        [s1 f1] (schedule/step schedule/initial (assoc e :now 100000))
        [s2 f2] (schedule/step s1 (assoc e :now 120000))
        [_ f3]  (schedule/step s2 (assoc e :now 170000))]
    (is (= [[:tidy] [:take]] f1))
    (is (= [[:take]] f2))
    (is (= [[:tidy] [:take]] f3))))

(deftest a-waiting-claim-is-a-step-of-its-own
  (let [queued {:state :queued :blockers ["r1"]}
        seen   (fn [m] (merge {:admission queued :handed-back? false :waiters 1} m))
        [s1 e1] (lock/wait-step {:announced? false} (seen {}))
        [_ e2]  (lock/wait-step s1 (seen {:waiters 3}))]
    (testing "the first round waits, says so once and sleeps"
      (is (= [[:wait ["r1"]] [:announce ["r1"]] [:sleep (lock/poll-after 1)]] e1))
      (is (= [[:wait ["r1"]] [:sleep (lock/poll-after 3)]] e2)))
    (testing "admission ends the wait, cancel and hand-back fail it"
      (is (= [[:admitted]] (second (lock/wait-step s1 (seen {:admission {:state :admitted}})))))
      (is (= [[:fail :cancelled]]
             (second (lock/wait-step s1 (seen {:admission {:state :cancelled}})))))
      (is (= [[:fail :handed-back]] (second (lock/wait-step s1 (seen {:handed-back? true})))))
      (is (= [[:admitted]]
             (second (lock/wait-step s1 (seen {:admission {:state :admitted}
                                                :handed-back? true}))))
          "an admitted claim runs even if it was to be handed back"))))
