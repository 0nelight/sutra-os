(ns sutra.capps.edits
  (:require [clojure.string :as str]))

(def ^:private kind->tone
  {"delete" :danger "create" :ok "update" :info})

(defn actor
  [row]
  (or (some-> (:edited_by_name row) str str/trim not-empty)
      (let [p (some-> (:edited_by row) str str/trim not-empty)]
        (if (= "sutra.store.world" p) "System (Migration)" p))
      "System"))

(defn when-str
  [v]
  (when-let [s (some-> v str not-empty)]
    (if-let [[_ d t] (re-find #"^(\d{4}-\d{2}-\d{2})[T ](\d{2}:\d{2})" s)]
      (str d " " t)
      s)))

(defn changed
  [row props]
  (let [vals (:values row)
        ks   (if (seq props)
               (filter (set (map keyword props)) (keys vals))
               (sort-by name (keys vals)))]
    (into [] (map (fn [k] [k (get vals k)])) ks)))

(defn- render-val [v]
  (cond (nil? v) "—" (boolean? v) (if v "Ja" "Nein") :else (str v)))

(defn- change-str
  [previous [k v]]
  (str (name k) ": "
       (when (contains? previous k) (str (render-val (get previous k)) " → "))
       (render-val v)))

(defn ->timeline
  [rows {:keys [props]}]
  (into []
        (map (fn [row]
               (let [ch (changed row props)]
                 {:at    (when-str (:updated_at row))
                  :title (cond-> (actor row)
                           (:action row) (str " · " (:action row)))
                  :tone  (get kind->tone (some-> (:edit_kind row) str) :info)
                  :body  (when (seq ch)
                           (str/join " · " (map (partial change-str (:previous row)) ch)))})))
        rows))

(defn- ms [v]
  (when v
    (let [n #?(:clj  (if (inst? v) (inst-ms v) (.toEpochMilli (java.time.Instant/parse (str v))))
               :cljs (.getTime (js/Date. v)))]
      (when-not #?(:clj false :cljs (js/isNaN n)) n))))

(defn- edit-label [{:keys [agent edited-by edited-by-name]}]
  (or agent
      (actor {:edited_by edited-by :edited_by_name edited-by-name})))

(defn object-timeline
  [{:keys [edits runs]}]
  (let [es   (keep #(when-let [t (ms (:edited-at %))] (assoc % ::t t)) edits)
        rs   (keep #(when-let [a (ms (:started-at %))]
                      (assoc % ::a a ::b (or (ms (:ended-at %)) a)))
                   runs)
        ts   (concat (map ::t es) (map ::a rs) (map ::b rs))]
    (when (seq ts)
      (let [t0    (apply min ts)
            t1    (apply max ts)
            pct   (fn [t] (if (= t0 t1) 50 (* 100.0 (/ (- t t0) (- t1 t0)))))
            marks (group-by edit-label es)
            bars  (group-by #(or (:agent %) (:run %)) rs)]
        {:from    t0
         :to      t1
         :tracks  (vec (for [label (distinct (concat (keys marks) (keys bars)))]
                         {:label label
                          :marks (mapv (fn [e] {:left  (pct (::t e))
                                                :title (str (when-str (:edited-at e))
                                                            " · " (:edit-kind e)
                                                            (when (:action e)
                                                              (str " · " (:action e)))
                                                            (when (:agent-run e)
                                                              (str " · " (:agent-run e))))
                                                :tone  (get kind->tone (:edit-kind e) :info)})
                                       (get marks label))
                          :bars  (mapv (fn [r] {:left  (pct (::a r))
                                                :width (max 1 (- (pct (::b r)) (pct (::a r))))
                                                :title (str (:run r) " · " (:tokens r) " Tokens")})
                                       (get bars label))}))
         :summary {:edits       (count es)
                   :runs        (count rs)
                   :tokens      (reduce + 0 (keep :tokens rs))
                   :runtime-ms  (when (seq rs)
                                  (- (apply max (map ::b rs)) (apply min (map ::a rs))))
                   :coverage    (when (seq es)
                                  (Math/round
                                   (* 100.0 (/ (count (filter :agent-run es)) (count es)))))}}))))
