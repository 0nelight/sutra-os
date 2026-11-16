(ns sutra.store.core.resources
  (:require [clojure.edn :as edn]
            [sutra.store.core.kv :refer [enc kv-fx rev-key]]))

(set! *warn-on-reflection* true)

(def frame-id [:frame])

(defn entries
  [W]
  (reduce-kv (fn [m s v]
               (if (and (map? v) (seq v))
                 (into (assoc-in m [frame-id s] {}) (for [[k e] v] [[s k] e]))
                 (assoc-in m [frame-id s] v)))
             {frame-id {}} W))

(defn world
  [es]
  (reduce-kv (fn [W id v] (if (= id frame-id) W (assoc-in W id v)))
             (get es frame-id {}) es))

(defn value
  [W id]
  (if (= id frame-id)
    (update-vals W #(if (map? %) {} %))
    (let [[s k] id m (get W s)]
      (if (and (map? m) (contains? m k)) (get m k) ::absent))))

(defn ids-of
  [nodes]
  (into [] (comp (map #(if (= :frame (first %)) frame-id %)) (distinct)) nodes))

(defn only
  [W ids]
  (into {} (for [id ids :let [v (value W id)] :when (not= ::absent v)] [id v])))

(defn changed
  [es0 es]
  (into [] (filter #(not= (get es0 % ::absent) (get es % ::absent)))
        (into (set (keys es0)) (keys es))))

(defn node-k [id r] [:wnode (enc id) (rev-key r)])
(defn head-k [b id] [:whead b (enc id)])
(defn log-k [b r] [:wlog b (rev-key r)])
(defn hist-k [b id r] [:whist b (str (enc id) "|" (rev-key r))])
(defn format-k [b] [:wformat "" b])
(defn state-k [b r] [:wstate b (rev-key r)])

(def checkpoint-every 32)

(defn redo
  [state {:keys [changes]}]
  (reduce-kv (fn [s id [_ to]] (if to (assoc s id to) (dissoc s id))) state changes))

(defn hist-range
  [id]
  {:after (str (enc id) "|") :upto (str (enc id) "|" (rev-key Long/MAX_VALUE))})

(defn read-id [k] (edn/read-string k))
(defn read-rev [k] (parse-long k))

(defn commit-effects
  [b r {:keys [kept es0 es heads all-heads]}]
  (let [ids (if kept (changed es0 es) (keys es))
        chs (into {} (for [id ids
                           :let [v (get es id ::absent)]]
                       [id [(when kept (get heads id)) (when (not= ::absent v) r)]]))
        e   (cond-> {:changes chs} (not kept) (assoc :converted true))
        st  (cond (not kept) (update-vals chs second)
                  all-heads  (redo all-heads e))]
    (when (or (seq chs) (not kept))
      (concat
       (for [[id [_ to]] chs :when to] (kv-fx (node-k id r) [(get es id)]))
       (for [[id [_ to]] chs] (kv-fx (head-k b id) to))
       (for [[id ch] chs] (kv-fx (hist-k b id r) ch))
       [(kv-fx (log-k b r) e)
        (kv-fx (format-k b) (if st {:since (or (:since kept) r) :n 0}
                                (update kept :n inc)))]
       (when st [(kv-fx (state-k b r) st)])))))

(defn state-due?
  [kept]
  (and kept (>= (inc (:n kept)) checkpoint-every)))

(defn fork-effects
  [b f r heads es]
  (let [st (or heads (update-vals es (constantly r)))]
    (concat (when-not heads (for [[id v] es] (kv-fx (node-k id r) [v])))
            (for [[id rv] st] (kv-fx (head-k b id) rv))
            [(kv-fx (log-k b r) {:fork {:from f :rev r}})
             (kv-fx (format-k b) {:since r :n 0})]
            (when-not heads [(kv-fx (state-k b r) st)]))))

