(ns sutra.capps.metric
  (:require [clojure.string :as str]
            [sutra.kit.coerce :refer [->kw]]))

(defn group-class
  [{:keys [display direction size template]}]
  (let [d (or (->kw display) :card)]
    (str/join " "
              (cond-> ["capp-kpi-row" (str "capp-kpi-row--" (name d))]
                (= :vertical (->kw direction)) (conj "capp-kpi-row--vertical")
                size     (conj (str "capp-kpi-row--" (name (->kw size))))
                template (conj (str "capp-kpi-row--" (name (->kw template))))))))

(def ^:private hour-ms 3600000)
(def ^:private unit-ms
  {:minute 60000 :hour hour-ms :day (* 24 hour-ms) :week (* 7 24 hour-ms)
   :month (* 30 24 hour-ms) :year (* 365 24 hour-ms)})

(defn window
  [time-range now-ms]
  (cond
    (map? time-range)
    (if-let [n (:last time-range)]
      {:from-ms (- now-ms (* n (get unit-ms (->kw (:unit time-range)) (:day unit-ms))))}
      (select-keys time-range [:from :until]))

    :else
    (case (->kw time-range)
      :hour {:from-ms (- now-ms hour-ms)}
      :day  {:from-ms (- now-ms (:day unit-ms))}
      :week {:from-ms (- now-ms (:week unit-ms))}
      {})))

(defn series-read
  [{:keys [range] :as spark} now-ms]
  (merge (select-keys spark [:find :where :property :bucket :rollup])
         (window range now-ms)))

(defn first-series-values
  [answer]
  (into [] (comp (map second) (filter number?)) (:points (first (:series answer)))))

(defn baseline-value
  [{:keys [value prop object summary]} values]
  (cond
    (number? value) value
    prop            (let [row (if (map? object) object (first object))
                          x   (or (get row (->kw prop)) (get (:values row) (->kw prop)))]
                      (when (number? x) x))
    (seq values)    (case (->kw summary)
                      :first (first values)
                      :last  (peek (vec values))
                      :min   (reduce min values)
                      :max   (reduce max values)
                      :avg   (/ (reduce + values) (count values))
                      nil)))
