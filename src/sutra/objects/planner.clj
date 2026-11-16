(ns sutra.objects.planner
  (:require [sutra.kit.refusal :as refusal]
            [sutra.types.object-set :as os]
            [sutra.catalog.grants :as grants]))

(set! *warn-on-reflection* true)

(defn- types-read
  [schema ir]
  (let [nodes    (os/nodes ir)
        resolved (try (os/resolve-sides schema ir)
                      (catch clojure.lang.ExceptionInfo _ ir))]
    (into (into #{} (keep #(os/result-type schema %)) nodes)
          (keep :link)
          (os/nodes resolved))))

(defn- authorize-plan-reads! [auth schema ir]
  (doseq [t (types-read schema ir)]
    (grants/authorize-read! auth
                       (or (get-in schema [:object-types t])
                           (get-in schema [:link-types t]))
                       t)))

(def default-caps
  {:max-hops                  3
   :max-nodes                 64
   :max-static-members        10000
   :max-page                  10000
   :search-around-cardinality 100000
   :max-derive-objects        100000})

(defn- bound
  [node {:keys [stats-fn marked? exact-only?] :as opts}]
  (case (:op node)
    :static  {:kind :exact :n (count (:ids node))}
    :scan    (when-not (:legacy node)
               (when-let [n (when stats-fn (stats-fn (:root node)))]
                 {:kind (if marked? :max :exact) :n n}))
    :order   (bound (:in node) opts)
    :project (bound (:in node) opts)
    (when-not exact-only?
      (case (:op node)
        :filter  (when-let [b (bound (:in node) opts)]
                   (assoc b :kind :max))
        :page    (let [b (bound (:in node) opts)]
                   (if-let [lim (:limit node)]
                     {:kind :max :n (if b (min lim (:n b)) lim)}
                     b))
        :union   (let [bs (mapv #(bound % opts) (:in node))]
                   (when (every? some? bs)
                     {:kind :max :n (reduce + (map :n bs))}))
        :intersect (let [bs (keep #(bound % opts) (:in node))]
                     (when (seq bs)
                       {:kind :max :n (reduce min (map :n bs))}))
        :subtract  (when-let [b (bound (first (:in node)) opts)]
                     (assoc b :kind :max))
        nil))))

(defn- hop-input-bounds
  [ir opts]
  (into []
        (comp (filter #(= :search-around (:op %)))
              (map (fn [h] {:hop h :bound (bound (:in h) opts)})))
        (os/nodes ir)))

(defn- traversal-start-bound
  [ir hop-bounds]
  (when (and (seq hop-bounds) (some? (os/source ir)))
    (:bound (peek hop-bounds))))

(def ^:private column-ops #{:with :compute})

(def ^:private cap-measures
  [[:max-hops           (fn [{:keys [ir]}] [(os/hops ir)])]
   [:max-nodes          (fn [{:keys [nodes]}] [(count (remove (comp column-ops :op) nodes))])]
   [:max-static-members (fn [{:keys [nodes]}]
                          (keep #(when (= :static (:op %)) (count (:ids %))) nodes))]
   [:max-page           (fn [{:keys [nodes]}]
                          (keep #(when (= :page (:op %)) (or (:limit %) 0)) nodes))]
   [:search-around-cardinality
    (fn [{:keys [hop-bounds]}]
      (keep #(when (= :exact (get-in % [:bound :kind])) (get-in % [:bound :n]))
            hop-bounds))]])

(defn- cap-violations
  [ir caps hop-bounds]
  (let [ctx {:ir ir :nodes (os/nodes ir) :hop-bounds hop-bounds}]
    (into []
          (keep (fn [[cap measure]]
                  (let [limit  (cap caps)
                        actual (reduce max 0 (measure ctx))]
                    (when (> actual limit)
                      {:cap cap :limit limit :actual actual}))))
          cap-measures)))

(def engine-rules
  [[(constantly true) :store
    "the serving tables are store-resident: the store reads them in place, with no
     materialization to weigh"]])

(def engine-available?
  {:store (constantly true)})

(defn- fallback-engine
  []
  :store)

(defn- choose-engine
  ([estimate] (choose-engine estimate nil))
  ([estimate requested]
   (if requested
     (let [avail (engine-available? requested)]
       (cond
         (nil? avail)
         (refusal/refuse! :refusal.objects/unknown-engine
                          {:engine requested}
                          {:known (vec (sort (keys engine-available?)))})

         (avail)
         {:engine requested :requested requested
          :reason "requested by the caller"}

         :else
         (let [fb (fallback-engine)]
           {:engine fb :requested requested :declined requested
            :reason (str (name requested) " was requested but is not enabled "
                         "here; running on " (name fb))})))
     (let [[_ engine reason] (first (filter (fn [[pred]] (pred estimate))
                                            engine-rules))]
       (if ((engine-available? engine (constantly false)))
         {:engine engine :reason reason}
         (let [fb (fallback-engine)]
           {:engine fb :declined engine
            :reason (str "would run on " (name engine) " (" reason ") — but that "
                         "engine is not enabled here; running on " (name fb))}))))))

(def ^:private presence-signals
  {:bounded?   :page
   :filtered?  :filter
   :aggregate? :aggregate})

(defn- estimate
  [ir start-bound]
  (let [ns- (os/nodes ir)
        has? (fn [op] (boolean (some #(= op (:op %)) ns-)))]
    (cond-> (into {:nodes      (count ns-)
                   :hops       (os/hops ir)
                   :statements 1
                   :scans      (into [] (comp (filter #(= :scan (:op %))) (map :root)) ns-)}
                  (map (fn [[signal op]] [signal (has? op)]))
                  presence-signals)
      start-bound (assoc :start-bound start-bound))))

(defn plan
  ([schema ir] (plan schema ir nil))
  ([schema ir {:keys [accuracy caps stats-fn authority exact-only?] req-engine :engine
               :or {accuracy :exact}}]
   (os/assert-readable! ir)
   (when-not (contains? #{:exact :approximate} accuracy)
     (refusal/refuse! :refusal.objects/unknown-accuracy {:accuracy accuracy}))
   (authorize-plan-reads! authority schema ir)
   (let [caps    (merge default-caps caps)
         marked? (boolean (seq (grants/denied-markings authority)))
         hbounds (hop-input-bounds ir {:stats-fn stats-fn :marked? marked?
                                       :exact-only? exact-only?})
         viol    (cap-violations ir caps hbounds)
         _       (when (seq viol)
                   (refusal/refuse! :refusal.objects/plan-caps-exceeded
                                    {:violations viol :caps caps}
                                    {:caps-hit (mapv :cap viol)}))
         est     (estimate ir (traversal-start-bound ir hbounds))
         flat?   (os/flat? ir)
         engine  (choose-engine est req-engine)]
     (cond-> {:ir       ir
              :strategy (if flat? :flat :composed)
              :root     (os/result-type schema ir)
              :engine   (:engine engine)
              :engine-reason (:reason engine)
              :accuracy :exact
              :caps     caps
              :cap-status :within
              :estimate est
              :authority-bound? true}
       (not= :exact accuracy) (assoc :accuracy-requested accuracy)
       (:requested engine)    (assoc :engine-requested (:requested engine))
       (:declined engine)     (assoc :engine-declined (:declined engine))))))

(defn plan-qt
  ([schema qt] (plan-qt schema qt nil))
  ([schema qt opts] (plan schema (os/qt->ir qt) opts)))

(defn describe
  [{:keys [strategy engine engine-reason engine-requested engine-declined
           accuracy accuracy-requested root estimate caps cap-status]}]
  (cond-> {:strategy   strategy
           :engine     engine
           :reason     engine-reason
           :accuracy   accuracy
           :root       root
           :caps       caps
           :cap-status cap-status
           :estimate   estimate}
    accuracy-requested (assoc :accuracy-requested accuracy-requested)
    engine-requested   (assoc :engine-requested engine-requested)
    engine-declined    (assoc :engine-declined engine-declined)))
