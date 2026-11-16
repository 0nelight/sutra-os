(ns sutra.capps.flow-view
  (:require [clojure.string :as str]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.capps.table :as tbl]
            [sutra.capps.timeline-layers :as timeline-layers]))

(defn- truthy-state
  [{:keys [completed] :as step} i cur-i]
  (if (contains? step :completed) (boolean completed) (< i cur-i)))

(defn steps
  [{:keys [steps current type]}]
  (let [steps  (vec steps)
        ids    (mapv :id steps)
        cur-i  (or (first (keep-indexed (fn [i id] (when (= (str id) (str current)) i)) ids)) 0)
        done   (map-indexed (fn [i s] (truthy-state s i cur-i)) steps)
        linear (not= :non-linear (->kw type))
        reach  (if linear
                 (count (take-while true? done))
                 (count steps))]
    (mapv (fn [i s done?]
            (assoc s
                   :number (inc i)
                   :done? done?
                   :active? (= i cur-i)
                   :clickable? (or (not linear) (<= i reach))))
          (range) steps done)))

(defn step-marker
  [{:keys [number icon done?]} {:keys [template show-number? type]}]
  (let [icons? (= :icons (->kw template))]
    (cond
      (and icons? show-number? (not= :non-linear (->kw type)))
      (str number " " (or icon ""))
      icons? (or icon (if done? "✓" number))
      done?  "✓"
      :else  number)))

(defn stages
  [{:keys [rows status at statuses label]}]
  (let [sf    (tbl/field-fn status)
        af    (tbl/field-fn at)
        lf    (tbl/field-fn label)
        seen  (sort-by #(str (af %)) rows)
        first-of (reduce (fn [m r] (let [s (str (sf r))] (if (contains? m s) m (assoc m s r))))
                         {} seen)
        order (if (seq statuses)
                (map str statuses)
                (distinct (map (comp str sf) seen)))
        last-st (some-> (last seen) sf str)
        cur-i (.indexOf (vec order) last-st)]
    (vec (map-indexed
          (fn [i s]
            (let [r (get first-of s)]
              {:status s
               :at     (some-> r af str)
               :label  (or (when (and lf r) (some-> (lf r) str)) s)
               :state  (cond (= s last-st) :current
                             (and (not (neg? cur-i)) (< i cur-i)) :done
                             r :done
                             :else :pending)}))
          order))))

(defn- ms
  [x]
  (when-not (str/blank? (str x))
    #?(:cljs (let [t (.getTime (js/Date. (str x)))] (when-not (js/isNaN t) t))
       :clj  (try (.toEpochMilli (java.time.Instant/parse (str x)))
                  (catch Exception _
                    (try (-> (java.time.LocalDate/parse (subs (str x) 0 10))
                             (.atStartOfDay java.time.ZoneOffset/UTC)
                             .toInstant .toEpochMilli)
                         (catch Exception _ nil)))))))

(defn gap-text
  [a b]
  (when-let [d (when (and (ms a) (ms b)) (abs (- (ms b) (ms a))))]
    (let [mins (quot d 60000)]
      (cond (< mins 60)    (str mins " Min.")
            (< mins 1440)  (str (quot mins 60) " Std.")
            :else          (str (quot mins 1440) " Tage")))))

(defn- layer-with-title
  [{:keys [title-mode title-of] :as layer}]
  (cond-> layer
    (and title-of (or (= :object (->kw title-mode))
                      (and (nil? title-mode) (not (:title-prop layer)) (not (:title layer)))))
    (assoc :title-prop title-of :title nil)))

(defn- dressed
  [{:keys [color conditional-format icon on-select]} i {:keys [row] :as ev}]
  (let [look (tbl/cond-format conditional-format (or (:values row) row))]
    (cond-> (assoc ev :layer-index i)
      color                  (assoc :color color)
      (:color look)          (assoc :color (:color look))
      (:icon look)           (assoc :icon (:icon look))
      (and icon (not= :none (->kw icon)) (nil? (:icon look))) (assoc :icon icon)
      (= :none (->kw icon))  (assoc :no-icon? true)
      (some? on-select)      (assoc :on-select on-select))))

(defn timeline-events
  [{:keys [layers order hidden]}]
  (let [evs  (->> (map-indexed vector layers)
                  (remove (fn [[i _]] (contains? hidden i)))
                  (mapcat (fn [[i l]]
                            (map #(dressed l i (assoc % ::t (:sort %) ::i i))
                                 (timeline-layers/layer-events (layer-with-title l)))))
                  (sort-by (juxt ::t ::i)))
        evs  (if (= :newest (->kw order)) (reverse evs) evs)]
    (mapv (fn [prev ev]
            (cond-> (dissoc ev ::t ::i)
              prev (assoc :gap (gap-text (::t prev) (::t ev)))))
          (cons nil evs) evs)))

(defn action-log-layers
  [{:keys [objects logs]}]
  (let [ids (into #{} (mapcat (fn [o] (keep (fn [x] (some-> x str))
                                            [(:id o) (:pk o) (:primary-key o)])))
                  objects)]
    (mapv (fn [{:keys [rows target-prop at properties label]}]
            (let [tf (tbl/field-fn target-prop)]
              {:label label
               :at    at
               :body  properties
               :rows  (filterv #(let [v (tf %)]
                                  (if (sequential? v)
                                    (some (fn [x] (contains? ids (str x))) v)
                                    (contains? ids (str v))))
                               rows)}))
          logs)))
