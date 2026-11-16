(ns sutra.kit.rate-limit
  (:refer-clojure :exclude [reset!]))

(set! *warn-on-reflection* true)

(def default-max-keys 10000)

(defn- level
  [{:keys [capacity per-ms]} {:keys [units at]} now]
  (if (nil? at)
    (double capacity)
    (min (double capacity)
         (+ (double units) (* (double capacity) (/ (double (max 0 (- now at))) per-ms))))))

(defn- pruned
  [state policy now]
  (if (<= (count state) (long (:max-keys policy default-max-keys)))
    state
    (into {} (remove (fn [[_ b]] (>= (level policy b now) (:capacity policy)))) state)))

(defn wait-ms
  [state policy k now amount]
  (let [have (level policy (get state k) now)]
    (if (>= have amount)
      0
      (long (Math/ceil (/ (* (- amount have) (:per-ms policy))
                          (double (:capacity policy))))))))

(defn spend
  [state policy k now amount]
  (let [wait (wait-ms state policy k now amount)]
    (if (zero? wait)
      {:state    (pruned (assoc state k {:units (- (level policy (get state k) now) amount)
                                         :at now})
                         policy now)
       :allowed? true
       :retry-ms 0}
      {:state state :allowed? false :retry-ms wait})))

(defn charge
  [state policy k now amount]
  (pruned (assoc state k {:units (- (level policy (get state k) now) amount) :at now})
          policy now))

(defn forget
  [state k]
  (dissoc state k))

(defn limiter
  [policy now-ms]
  {:policy policy :state (atom {}) :now-ms now-ms})

(defn spend!
  ([lim k] (spend! lim k 1))
  ([{:keys [policy state now-ms]} k amount]
   (let [now   (now-ms)
         taken (volatile! nil)]
     (swap! state (fn [s] (let [r (spend s policy k now amount)] (vreset! taken r) (:state r))))
     (when-not (:allowed? @taken) (:retry-ms @taken)))))

(defn charge!
  [{:keys [policy state now-ms]} k amount]
  (swap! state charge policy k (now-ms) amount)
  nil)

(defn reset!
  [{:keys [state]}]
  (clojure.core/reset! state {})
  nil)
