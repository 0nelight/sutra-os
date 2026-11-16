(ns sutra.foundation.profile.report
  (:require [sutra.foundation.profile :as prof]
            [sutra.observe :as obs]))

(set! *warn-on-reflection* true)

(defn- ms [ns]
  (/ (Math/round (/ (double ns) 1e5)) 10.0))

(defn- pct [v total]
  (when (pos? (long total))
    (/ (Math/round (* 1000.0 (/ (double v) (double total)))) 10.0)))

(defn- self-samples
  [threads]
  (for [[_ {:keys [name stacks samples]}] threads
        :let [by-id (vec stacks)]
        [sid _ cpu] samples
        :when (and sid (pos? (long (or cpu 0))))]
    {:thread name :label (first (first (nth by-id sid nil))) :cpu (long cpu)}))

(defn- tally
  [rows k total limit]
  (->> rows
       (reduce (fn [m r] (update m (get r k) (fnil + 0) (:cpu r))) {})
       (sort-by (comp - val))
       (take limit)
       (mapv (fn [[a v]] {k a :cpu-ms (ms v) :pct (pct v total)}))))

(defn busiest
  [{:keys [threads]} {:keys [limit]}]
  (let [limit (or limit 15)
        per   (vec (self-samples threads))
        total (reduce + 0 (map :cpu per))]
    {:cpu-ms (ms total)
     :by-layer (->> per
                    (reduce (fn [m r]
                              (update m (obs/layer-name (prof/layer-of (:label r)))
                                      (fnil + 0) (:cpu r)))
                            {})
                    (sort-by (comp - val))
                    (mapv (fn [[a v]] {:category a :cpu-ms (ms v) :pct (pct v total)})))
     :by-self-frame (tally per :label total limit)
     :by-thread     (tally per :thread total limit)}))

(defn marker-summary
  [{:keys [markers]}]
  (->> markers
       (reduce (fn [m {:keys [name start end data]}]
                 (let [m (-> m
                             (update-in [name :count] (fnil inc 0))
                             (update-in [name :ms] (fnil + 0.0)
                                        (- (double end) (double start))))]
                   (reduce (fn [m k]
                             (cond-> m
                               (number? (get data k))
                               (update-in [name :sizes k] (fnil + 0) (get data k))))
                           m
                           (get obs/size-fields name []))))
               {})
       (sort-by (comp - :ms val))
       (mapv (fn [[nm v]]
               (cond-> {:marker nm :count (:count v)
                        :total-ms (/ (Math/round (* 10.0 (double (:ms v)))) 10.0)}
                 (seq (:sizes v))
                 (assoc :sizes (into {} (remove (comp zero? val)) (:sizes v))))))))
