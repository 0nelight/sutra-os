(ns sutra.capps.gantt
  (:require [clojure.string :as str]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.capps.table :as tbl]))

(defn ms
  [x]
  (cond
    (number? x)           x
    (str/blank? (str x))  nil
    :else
    #?(:cljs (let [t (.getTime (js/Date. (str x)))] (when-not (js/isNaN t) t))
       :clj  (try (.toEpochMilli (java.time.Instant/parse (str x)))
                  (catch Exception _
                    (try (-> (java.time.LocalDate/parse (subs (str x) 0 10))
                             (.atStartOfDay java.time.ZoneOffset/UTC)
                             .toInstant .toEpochMilli)
                         (catch Exception _ nil)))))))

(defn- event-of
  [{:keys [event-type start end at label no-end end-fallback group-by color color-prop
           conditional-format hover look-of layer-index on-select]}
   row]
  (let [range?  (not= :discrete (->kw event-type))
        s       (ms ((tbl/field-fn (if range? start at)) row))
        e       (when range? (ms ((tbl/field-fn end) row)))
        missing (->kw no-end)
        e       (or e (when (= :fallback missing) (ms end-fallback)))
        look    (or (when conditional-format
                      (tbl/cond-format conditional-format (or (:values row) row)))
                    (when (and color-prop look-of) (:look (look-of row color-prop))))]
    (when (and s (or (not range?) e (= :point missing)))
      {:start   s
       :end     (if (and range? e) (max s e) s)
       :point?  (or (not range?) (nil? e))
       :label   (some-> (tbl/field-fn label) (apply [row]) str)
       :color   (or (:color look) color)
       :hover   (mapv (fn [p] [p ((tbl/field-fn p) row)]) hover)
       :groups  (mapv (fn [p] (str ((tbl/field-fn p) row))) group-by)
       :layer   layer-index
       :on-select on-select
       :row     row})))

(defn events
  [layers]
  (vec (mapcat (fn [i {:keys [rows] :as layer}]
                 (keep #(event-of (assoc layer :layer-index i) %) rows))
               (range) layers)))

(defn- smart-lanes
  [evs]
  (:out
   (reduce (fn [{:keys [ends] :as acc} {:keys [start end] :as ev}]
             (let [lane (or (first (keep-indexed (fn [i e] (when (<= e start) i)) ends))
                            (count ends))]
               (-> acc
                   (assoc-in [:ends lane] end)
                   (update :out conj (assoc ev :lane lane)))))
           {:ends [] :out []}
           (sort-by :start evs))))

(defn- laned
  [stacking evs]
  (case (->kw stacking)
    :overlap (mapv #(assoc % :lane 0) evs)
    :new-rows (vec (map-indexed (fn [i e] (assoc e :lane i)) (sort-by :start evs)))
    (smart-lanes evs)))

(defn- row-order
  [ordering groups]
  (if (= :first-start (->kw ordering))
    (sort-by (fn [[_ evs]] (apply min (map :start evs))) groups)
    (sort-by (fn [[g _]] (str/join "/" g)) groups)))

(def ^:private hour 3600000)
(def ^:private day (* 24 hour))

(defn- utc
  [y m d h]
  #?(:cljs (js/Date.UTC y m d h)
     :clj  (-> (java.time.LocalDateTime/of (int y) 1 1 0 0)
               (.plusMonths (long m))
               (.plusDays (dec (long d)))
               (.plusHours (long h))
               (.toInstant java.time.ZoneOffset/UTC) .toEpochMilli)))

(defn- ymdh
  [t]
  #?(:cljs (let [d (js/Date. t)]
             [(.getUTCFullYear d) (.getUTCMonth d) (.getUTCDate d) (.getUTCHours d)
              (.getUTCDay d)])
     :clj  (let [d (java.time.ZonedDateTime/ofInstant (java.time.Instant/ofEpochMilli t)
                                                      java.time.ZoneOffset/UTC)]
             [(.getYear d) (dec (.getMonthValue d)) (.getDayOfMonth d) (.getHour d)
              (mod (.getValue (.getDayOfWeek d)) 7)])))

(defn- first-tick
  [unit t {:keys [start-month start-day start-hour]}]
  (let [[y m d h wd] (ymdh t)
        sm (or start-month 0) sd (or start-day 1) sh (or start-hour 0)]
    (case unit
      :year    (let [c (utc y sm 1 0)] (if (<= c t) c (utc (dec y) sm 1 0)))
      :quarter (let [q (- m (mod (- m sm) 3))] (utc y q 1 0))
      :month   (let [c (utc y m sd 0)] (if (<= c t) c (utc y (dec m) sd 0)))
      :week    (let [c (- (utc y m d 0) (* day (mod (- wd sd) 7)))] c)
      :day     (let [c (utc y m d sh)] (if (<= c t) c (- c day)))
      :hour    (utc y m d h))))

(defn- next-tick
  [unit t]
  (let [[y m d] (ymdh t)]
    (case unit
      :year    (utc (inc y) m d 0)
      :quarter (utc y (+ m 3) d 0)
      :month   (utc y (inc m) d 0)
      :week    (+ t (* 7 day))
      :day     (+ t day)
      :hour    (+ t hour))))

(defn- auto-unit
  [span]
  (cond (< span (* 2 day))   :hour
        (< span (* 60 day))  :day
        (< span (* 400 day)) :month
        :else                :year))

(defn ticks
  [{:keys [grid-unit] :as cfg} lo hi]
  (when (not= :none (->kw grid-unit))
    (let [unit (or (->kw grid-unit) (auto-unit (- hi lo)))]
      (take 400 (take-while #(<= % hi)
                            (iterate #(next-tick unit %) (first-tick unit lo cfg)))))))

(defn- pct [lo hi t] (if (= lo hi) 0 (* 100.0 (/ (- t lo) (- hi lo)))))

(defn layout
  [{:keys [layers stacking row-ordering start end show-now? now] :as cfg}]
  (let [evs  (events layers)
        ts   (mapcat (juxt :start :end) evs)
        lo   (or (ms start) (when (seq ts) (apply min ts)))
        hi   (or (ms end) (when (seq ts) (apply max ts)))]
    (when (and lo hi)
      (let [hi     (if (= lo hi) (+ lo day) hi)
            shown  (filter #(and (<= (:start %) hi) (>= (:end %) lo)) evs)
            place  (fn [{:keys [start end] :as e}]
                     (let [s (max start lo) f (min end hi)]
                       (assoc e :x (pct lo hi s)
                              :w (if (:point? e) 0 (max 0.2 (- (pct lo hi f) (pct lo hi s)))))))
            groups (row-order row-ordering (group-by :groups shown))]
        {:lo    lo
         :hi    hi
         :rows  (mapv (fn [[g gevs]]
                        (let [es (laned stacking gevs)]
                          {:group  (str/join " / " g)
                           :lanes  (inc (apply max 0 (map :lane es)))
                           :events (mapv place es)}))
                      groups)
         :ticks (mapv (fn [t] {:t t :x (pct lo hi t)})
                      (filter #(>= % lo) (ticks cfg lo hi)))
         :now-x (when (and show-now? now (<= lo now hi)) (pct lo hi now))}))))

(defn next-selection
  [selected row {:keys [multi? add?]}]
  (let [on? (some #{row} selected)]
    (cond
      (and multi? add? on?) (vec (remove #{row} selected))
      (and multi? add?)     (conj (vec selected) row)
      on?                   []
      :else                 [row])))
