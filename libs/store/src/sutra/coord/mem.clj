(ns sutra.coord.mem
  (:require [sutra.coord.api :as coord]))

(set! *warn-on-reflection* true)

(defn- live? [now {:keys [until]}] (and until (< now (.getTime ^java.util.Date until))))

(defn- lease [name owner epoch until-ms]
  {:name name :owner owner :epoch epoch :until (java.util.Date. (long until-ms))})

(defrecord MemCoordinator [state clock]
  coord/Coordinator
  (-acquire! [_this {:keys [name owner ttl-ms wait-ms]}]
    (let [deadline (+ (clock) (or wait-ms 0))]
      (loop []
        (let [now (clock)
              got (get (swap! state
                              (fn [leases]
                                (let [{:keys [held epoch]} (get leases name)
                                      mine? (and held (= owner (:owner held)) (live? now held))]
                                  (cond
                                    mine?
                                      (assoc-in leases [name :held]
                                                (assoc held
                                                       :until
                                                         (java.util.Date. (long (+ now ttl-ms)))))
                                    (live? now held) leases
                                    :else            (let [e (inc (or epoch 0))]
                                                       (assoc leases
                                                              name
                                                                {:held
                                                                   (lease name owner e
                                                                          (+ now ttl-ms))
                                                                 :epoch e}))))))
                       name)]
          (cond
            (and (:held got) (= owner (:owner (:held got))) (live? now (:held got))) (:held got)
            (< (clock) deadline) (do (Thread/sleep 10) (recur))
            :else nil)))))
  (-renew! [_ {:keys [name owner epoch]} ttl-ms]
    (let [now  (clock)
          ok?  (fn [held]
                 (and held (= owner (:owner held)) (= epoch (:epoch held)) (live? now held)))
          [before after] (swap-vals! state
                                     (fn [leases]
                                       (if (ok? (get-in leases [name :held]))
                                         (assoc-in leases [name :held :until]
                                                   (java.util.Date. (long (+ now ttl-ms))))
                                         leases)))]
      (when (ok? (get-in before [name :held])) (get-in after [name :held]))))
  (-release! [_ {:keys [name owner epoch]}]
    (let [mine?    (fn [held] (and held (= owner (:owner held)) (= epoch (:epoch held))))
          [before] (swap-vals! state
                               (fn [leases]
                                 (if (mine? (get-in leases [name :held]))
                                   (update leases name dissoc :held)
                                   leases)))]
      (boolean (mine? (get-in before [name :held])))))
  (-holder [_ name]
    (let [held (get-in @state [name :held])]
      (when (live? (clock) held) held)))
  (-revoke! [_ name]
    (get-in (swap! state
                   (fn [leases]
                     (-> leases (update name dissoc :held) (update-in [name :epoch] (fnil inc 0)))))
            [name :epoch])))

(defn coordinator
  ([] (coordinator {}))
  ([{:keys [clock] :or {clock #(System/currentTimeMillis)}}]
   (->MemCoordinator (atom {}) clock)))
