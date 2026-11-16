(ns sutra.types.retention)

(set! *warn-on-reflection* true)

(def selector-kinds
  #{:older-than-days :view-count :transaction-count :only-in-views-older-than-days
    :latest-view-older-than-days})

(def policy-schema
  [:map
   [:name :string]
   [:datasets [:or [:= :all] [:sequential :string]]]
   [:selectors [:sequential [:tuple (into [:enum] selector-kinds) nat-int?]]]
   [:allow-latest-view? {:optional true} :boolean]
   [:recommended? {:optional true} :boolean]])

(def recommended
  [{:name "recommended/old-views" :datasets :all
    :selectors [[:view-count 10] [:older-than-days 30]] :recommended? true}
   {:name "recommended/superseded" :datasets :all
    :selectors [[:latest-view-older-than-days 30]]
    :allow-latest-view? true :recommended? true}])

(def grace-days
  7)

(def ^:private day-ms (* 24 60 60 1000))

(defn- at-ms [t] (some-> (:at t) (#(if (inst? %) (inst-ms %) (long %)))))

(defn transactions
  [history]
  (let [asc     (sort-by :version history)
        typed?  (comp some? :kind)
        first-t (some #(when (typed? %) (:version %)) asc)]
    (into []
          (comp (filter #(or (typed? %) (nil? first-t) (< (:version %) first-t)))
                (map #(update % :kind (fnil keyword :unknown))))
          asc)))

(defn- with-views
  [txns]
  (first (reduce (fn [[out v] t]
                   (let [v (if (and (= :snapshot (:kind t)) (seq out)) (inc v) v)]
                     [(conj out (assoc t :view v)) v]))
                 [[] 0]
                 txns)))

(defn- view-closed-ms
  [txns]
  (into {} (keep (fn [[v ts]] (some->> (keep at-ms ts) seq (reduce max) (vector v))))
        (group-by :view txns)))

(defn- data-containing? [t] (not= :delete (:kind t)))

(defn- selects?
  [{:keys [now-ms latest-view closed recent]} t [kind n]]
  (let [cut (- now-ms (* (long n) day-ms))]
    (case kind
      :older-than-days (some-> (at-ms t) (< cut))
      :view-count (<= (+ (:view t) (long n)) latest-view)
      :transaction-count (not (contains? (get recent n) (:version t)))
      :only-in-views-older-than-days (some-> (get closed (:view t)) (< cut))
      :latest-view-older-than-days (and (= (:view t) latest-view)
                                        (some-> (at-ms t) (< cut))))))

(defn applies?
  [{:keys [datasets]} {:keys [rid name]}]
  (or (= :all datasets) (boolean (some #{(str rid) (str name)} (map str datasets)))))

(defn plan
  [policies ds history now-ms]
  (let [txns   (with-views (transactions history))
        latest (or (:view (peek txns)) 0)
        counts (into #{} (keep (fn [[k n]] (when (= :transaction-count k) n)))
                     (mapcat :selectors policies))
        ctx    {:now-ms now-ms :latest-view latest :closed (view-closed-ms txns)
                :recent (into {} (for [n counts]
                                   [n (into #{} (comp (filter data-containing?) (map :version)
                                                      (take n))
                                            (rseq txns))]))}
        ps     (filterv #(applies? % ds) policies)
        by     (fn [t] (into #{} (comp (filter (fn [{:keys [selectors allow-latest-view?]}]
                                                 (and (seq selectors)
                                                      (or allow-latest-view?
                                                          (< (:view t) latest))
                                                      (every? #(selects? ctx t %) selectors))))
                                       (map :name))
                             ps))
        gone   (into [] (comp (map (fn [t] (assoc t :by (by t)))) (take-while (comp seq :by)))
                     (butlast txns))
        kept   (get txns (count gone))
        due    (- now-ms (* (long grace-days) day-ms))
        was    (assoc ctx :now-ms due)
        expiring (fn [t]
                   (and (= latest (:view t))
                        (some-> (at-ms t) (< due))
                        (some (fn [{:keys [selectors allow-latest-view? recommended?]}]
                                (and allow-latest-view? (not recommended?) (seq selectors)
                                     (every? #(selects? was t %) selectors)))
                              ps)))
        through (some->> gone (drop-while #(not= latest (:view %))) (take-while expiring)
                         last :version)]
    (if (and (seq gone) kept)
      (cond-> {:horizon (:version kept) :deleted (count gone) :by (into #{} (mapcat :by) gone)}
        through (assoc :expire-through through))
      {:horizon nil :deleted 0 :by #{}})))
