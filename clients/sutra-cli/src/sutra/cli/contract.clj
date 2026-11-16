(ns sutra.cli.contract
  (:require [sutra.parser.step-verbs :as sv]
            [sutra.cli.dispatch :as dispatch]
            [sutra.cli.hints :as hints]
            [sutra.cli.table :as table]
            [clojure.string :as str]))

(defn- result
  [m]
  (merge {:state nil :errors [] :warnings [] :did nil :next []} m))

(defn blocked?
  [res]
  (boolean (seq (:errors res))))

(def ^:private tools
  (delay (into {} (map (juxt :name identity))
               (:tools ((requiring-resolve 'sutra.cli.spec-dump/tool-manifest))))))

(defn- tool [verb] (@tools (name verb)))

(defn- step-form
  [verb]
  (some (fn [[v arity form]] (when (= v verb) [(not= :req arity) form])) sv/steps))

(defn- required-args [verb]
  (let [[opt? form] (step-form (keyword verb))]
    (cond
      (or (nil? form) opt?)                       []
      (and (vector? form) (= :map (first form)))
      (vec (for [[k a] (remove map? (rest form))
                 :when (not (and (map? a) (:optional a)))]
             (name k)))
      (= :map form)                               []
      :else                                       ["payload"])))

(defn- enum-of [verb k] (get-in (tool verb) [:input_schema :properties (keyword k) :enum]))

(defn- levenshtein [s t]
  (let [n (count t)]
    (loop [i 1, prev (vec (range (inc n)))]
      (if (> i (count s))
        (peek prev)
        (recur (inc i)
               (reduce (fn [cur j]
                         (conj cur
                               (if (= (nth s (dec i)) (nth t (dec j)))
                                 (nth prev (dec j))
                                 (inc (min (nth prev j)
                                           (peek cur)
                                           (nth prev (dec j)))))))
                       [i]
                       (range 1 (inc n))))))))

(defn suggest-verb
  [verb]
  (let [v (name verb)]
    (->> sv/verbs
         (map (fn [c] [c (levenshtein v (name c))]))
         (filter #(<= (second %) 2))
         (sort-by second)
         (map first)
         (take 3)
         vec)))

(defn- missing-required
  [verb args]
  (let [present (into #{} (map name) (keys args))]
    (remove present (required-args verb))))

(defn- fill-in
  [verb missing]
  (mapv (fn [k]
          (cond-> {:need (keyword k)}
            (seq (enum-of verb k)) (assoc :options (enum-of verb k))))
        missing))

(defn- describe
  [verb args]
  (str "· " (name verb) (when (seq args) (str " " (pr-str args)))))

(defn- next-for
  [verb]
  (mapv (fn [s] {:verb (keyword s)}) (:see-also (hints/verb-hint verb))))

(defn- normalize
  [command]
  (if (map? command) [(:verb command) (:args command)] command))

(defn- trivially-valid?
  [verb args]
  (let [[_ form] (step-form verb)]
    (and (map? args) (empty? args)
         (or (= :map form) (and (vector? form) (#{:map :map-of} (first form)))))))

(defn run
  [ctx command]
  (let [[verb args] (normalize command)]
    (cond
      (not (or (contains? sv/verbs verb) (table/client-verbs verb)))
      (let [near (suggest-verb verb)]
        (result {:errors [{:code :unknown-verb
                           :msg  (str "no verb " verb)
                           :fix  (when (seq near)
                                   (str "did you mean " (str/join " / " (map name near)) "?"))}]
                 :next   (mapv (fn [s] {:verb s}) near)}))

      (seq (missing-required verb args))
      (let [miss (missing-required verb args)]
        (result {:errors (mapv (fn [k] {:code :missing-arg
                                        :msg  (str "missing required arg :" k)}) miss)
                 :next   (fill-in verb miss)}))

      :else
      (let [validated (try (when-not (or (table/client-verbs verb) (trivially-valid? verb args))
                             ((requiring-resolve 'sutra.parser.entr/validate-step!)
                              verb args :contract))
                           (catch Exception e e))]
        (if (instance? Exception validated)
          (result {:errors [{:code :invalid-args
                             :msg  (or (not-empty (.getMessage ^Exception validated))
                                       "invalid args")}]})
          (let [ret (dispatch/step! ctx [verb args])]
            (result {:state ret
                     :did   (describe verb args)
                     :next  (next-for verb)})))))))
