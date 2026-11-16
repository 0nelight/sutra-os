(ns sutra.certify
  (:require [clojure.test :as t]
            [sutra.store.shapes :as shapes]))

(set! *warn-on-reflection* true)

(defn failed [{:keys [duty msg data]}] {:status :failed :duty duty :msg msg :data data})

(defn fail [duty msg data] (failed {:duty duty :msg msg :data data}))

(def proven {:status :proven})

(defn proven? [result] (= :proven (:status result)))

(defn rng ^java.util.Random [seed i] (java.util.Random. (hash [seed i])))

(defn rnd [^java.util.Random r n] (.nextInt r (int n)))

(defn pick [r xs] (nth (vec xs) (rnd r (count xs))))

(def sides [:driver :reference])

(defn- differs [duty]
  (fn [_ _ a b]
    (when (not= a b)
      {:duty duty :msg "the driver answered otherwise than the reference"
       :data {:driver a :reference b}})))

(defn- checked [shape cmd]
  (when (and shape (not (shapes/valid? shape cmd)))
    (throw (ex-info "a program drew a command outside its shape"
                    {:shape shape :command cmd :explain (shapes/explain shape cmd)})))
  cmd)

(defn program
  [{:keys [duty shape steps seed init gen script admit run advance observe judge]
    :or   {steps 150 seed 1 admit (fn [_ c] c) advance (fn [s _ _] s)
           observe (fn [_ _ _ _ a] a)}}
   x]
  (let [judge (or judge (differs duty))
        steps (if script (count script) steps)]
    (loop [i 0 state init ran []]
      (if (= i steps)
        {:status :proven :steps steps}
        (let [drawn (if script (nth script i) (gen (rng seed i) state))
              cmd   (some->> (admit state drawn) (checked shape))]
          (if (nil? cmd)
            (recur (inc i) state ran)
            (let [ran (conj ran cmd)
                  res (try (let [as     (mapv #(run % (x %) state cmd) sides)
                                 state' (advance state cmd as)]
                             [state' (mapv #(observe %1 (x %1) state' cmd %2) sides as)])
                           (catch InterruptedException e (throw e))
                           (catch Throwable t t))]
              (if (instance? Throwable res)
                (fail :duty/failures "a valid call threw"
                      {:step i :command cmd :error (ex-message res) :error-data (ex-data res)
                       :ran ran})
                (let [[state' [a b]] res]
                  (if-let [f (judge state cmd a b)]
                    (failed (update f :data merge {:step i :command cmd :ran ran}))
                    (recur (inc i) state' ran)))))))))))

(defn suite
  [duty msg f]
  (let [failures (atom [])]
    (binding [t/report (fn [m] (when (#{:fail :error} (:type m))
                                 (swap! failures conj (select-keys m [:type :message :expected
                                                                      :actual]))))]
      (t/with-test-out (f)))
    (if (seq @failures)
      (fail duty msg {:failures (vec (take 5 @failures)) :count (count @failures)})
      proven)))

(defn certification
  [checks arg]
  (let [results (into {} (for [[k f] checks]
                           [k (try (f arg)
                                   (catch InterruptedException e (throw e))
                                   (catch Throwable x
                                     (fail :duty/failures "the check could not run"
                                           {:error (ex-message x)})))]))]
    {:passed? (every? proven? (vals results)) :results results}))
