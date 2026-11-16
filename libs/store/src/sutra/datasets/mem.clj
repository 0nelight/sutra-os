(ns sutra.datasets.mem
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.datasets.api :as datasets]
            [sutra.datasets.landing :as landing]))

(set! *warn-on-reflection* true)

(defn- k [{:keys [branch dataset]}] [branch dataset])

(defn- own-or-root
  [{:keys [logs parents]} {:keys [branch dataset]}]
  (loop [b branch]
    (or (get logs [b dataset])
        (when-let [p (get parents b)] (recur p)))))

(defn- state-at
  [log v]
  (or (:rows (last (if v (filter #(<= (:version %) v) log) log))) {}))

(defn- versioned-log
  [state addr v]
  (let [log (own-or-root state addr)]
    (when (and (some? v) (empty? log))
      (anomaly/raise! :incorrect
                      (str "dataset " (:dataset addr) " was never written: no version " v
                           " to read")
                      {:store/refusal :as-of-unsupported :at v}))
    log))

(defn- keyword-row [row] (update-keys row #(keyword (name %))))

(defn- landed
  [before pk kind rows]
  (reduce (fn [m [step _ step-rows {:keys [full-refresh?]}]]
            (case step
              (:fork :ensure-columns) m
              :retract-inherited      {}
              :merge  (into (if full-refresh? {} m)
                            (map (juxt #(str (get % pk)) identity)) step-rows)
              :retain (select-keys m (map #(str (get % pk)) rows))))
          before
          (landing/plan {:fork? (not= :snapshot kind) :replace? (= :snapshot kind)
                         :groups [[nil rows]]})))

(defn- horizon-of
  [{:keys [marks parents]} {:keys [branch dataset]}]
  (reduce max 0 (for [b (take-while some? (iterate parents branch))
                      {:keys [v]} (get marks [b dataset])]
                  v)))

(defn- swept-log
  [log target]
  (let [keep-from (or (last (keep-indexed (fn [i e] (when (<= (:version e) target) i)) log))
                      0)]
    (subvec (vec log) keep-from)))

(defrecord MemDatasets [state clock]
  datasets/Datasets
  (-profile [_] {:history-kinds? true})
  (-query [_ _]
    (throw (ex-info "this datasets driver has no transform-language engine"
                    {:datasets/unsupported :query})))
  (-layout! [_ branch {:keys [old new drop-unclaimed? head]}]
    (when drop-unclaimed?
      (swap! state update :logs
             (fn [logs]
               (apply dissoc logs
                      (for [ds (remove (set (keys (if head (head) new))) (keys old))]
                        [branch ds])))))
    nil)
  (-list [_ branch {:keys [prefix]}]
    (let [{:keys [logs parents]} @state
          chain (take-while some? (iterate parents branch))]
      (vec (sort (distinct (for [[[b ds] _] logs
                                 :when (some #{b} chain)
                                 :when (or (nil? prefix) (.startsWith (str ds) (str prefix)))]
                             ds))))))
  (-inspect [_ branch decls]
    (let [{:keys [logs scratch]} @state
          own (into {} (for [[[b ds] log] logs :when (= b branch)] [ds log]))]
      {:missing (vec (sort (remove #(contains? own %) (keys decls))))
       :orphans (vec (for [[ds log] (sort-by key own)
                           :when (and (not (contains? decls ds))
                                      (not (contains? scratch [branch ds])))]
                       {:name ds :rows (count (:rows (peek log)))}))}))
  (-write! [_ addr {:keys [kind rows ids all? meta] :as tx}]
    (let [pk  (keyword (or (:pk tx) "id"))
          at  (java.util.Date. (long (clock)))
          out (atom nil)]
      (swap! state
             (fn [{:keys [version] :as tables}]
               (let [v      (inc version)
                     before (state-at (own-or-root tables addr) nil)
                     rows   (mapv keyword-row rows)
                     after  (if (= :delete kind)
                              (if all? {} (apply dissoc before (map str ids)))
                              (landed before pk kind rows))
                     entry  {:version v :kind kind :rows after :n (count (or rows ids []))
                             :meta meta
                             :at at}]
                 (reset! out {:version v :rows (:n entry)})
                 (-> tables
                     (cond-> (:scratch? addr) (update :scratch (fnil conj #{}) (k addr)))
                     (assoc :version v)
                     (update-in [:logs (k addr)]
                                #(conj (or % (own-or-root tables addr) []) entry))))))
      @out))
  (-read [_ addr {:keys [at ids columns limit]}]
    (let [rows (state-at (versioned-log @state addr at) at)
          rows (if ids (keep #(get rows (str %)) ids) (map val (sort-by key rows)))
          rows (cond->> rows
                 columns (map #(select-keys % (map keyword columns)))
                 limit   (take limit))]
      (vec rows)))
  (-columns [_ addr]
    (into {}
          (comp (mapcat (comp vals :rows)) (mapcat keys) (map (fn [k] [(name k) nil])))
          (versioned-log @state addr nil)))
  (-count [_ addr {:keys [at]}]
    (count (state-at (versioned-log @state addr at) at)))
  (-version [_ _] (:version @state))
  (-delta [_ addr lo hi]
    (let [log (versioned-log @state addr hi)
          a   (state-at log lo)
          b   (state-at log hi)
          in  (filter #(and (> (:version %) (or lo 0)) (<= (:version %) hi)) log)]
      {:added    (into #{} (remove (set (keys a))) (keys b))
       :removed  (into #{} (remove (set (keys b))) (keys a))
       :changed  (into #{} (filter #(and (contains? a %) (not= (get a %) (get b %)))) (keys b))
       :replaced? (boolean (some #(= :snapshot (:kind %)) in))}))
  (-horizon [_ addr] (horizon-of @state addr))
  (-retire-below! [_ addr v]
    (let [at (long (clock))]
      (swap! state (fn [st]
                     (cond-> st
                       (> v (horizon-of st addr))
                       (update-in [:marks (k addr)] (fnil conj []) {:v v :at at}))))
      {:horizon (horizon-of @state addr)}))
  (-sweep! [_ addr {:keys [grace-ms] :or {grace-ms 0}}]
    (let [due (- (long (clock)) (long grace-ms))
          out (atom {:swept 0 :rows 0})]
      (swap! state
             (fn [st]
               (let [target (reduce max 0 (for [{:keys [v at]} (get-in st [:marks (k addr)])
                                                :when (<= at due)]
                                            v))
                     log    (get-in st [:logs (k addr)])]
                 (if (or (<= target (long (get-in st [:swept (k addr)] 0))) (nil? log))
                   (do (reset! out {:swept 0 :rows 0}) st)
                   (let [kept (swept-log log target)]
                     (reset! out {:swept target :rows (- (count log) (count kept))})
                     (-> st
                         (assoc-in [:logs (k addr)] kept)
                         (assoc-in [:swept (k addr)] target)))))))
      @out))
  (-expire! [this addr through]
    (let [log  (own-or-root @state addr)
          last-write (fn [id] (reduce (fn [found {:keys [version rows]}]
                                        (if (and (contains? rows id)
                                                 (not= (get rows id)
                                                       (get (state-at log (dec version)) id)))
                                          version
                                          found))
                                      0 log))
          gone (filterv #(<= (last-write %) through) (keys (state-at log nil)))]
      (if (seq gone)
        (let [{:keys [version]} (datasets/-write! this addr
                                                  {:kind :delete :pk (or (:pk addr) "id")
                                                   :ids gone})]
          {:version version :rows (count gone)})
        {:version (:version @state) :rows 0})))
  (-history [_ addr {:keys [before limit]}]
    (cond->> (->> (own-or-root @state addr)
                  (remove #(and before (>= (:version %) before)))
                  reverse
                  (map #(-> (select-keys % [:version :kind :at :meta]) (assoc :rows (:n %)))))
      limit (take limit)
      true  vec))
  (-restore! [this addr v]
    (let [rows (vals (state-at (versioned-log @state addr v) v))]
      (select-keys (datasets/-write! this addr
                                     {:kind :snapshot :pk (or (:pk addr) "id") :rows (vec rows)})
                   [:version])))
  (-fork! [_ from to] (swap! state assoc-in [:parents to] from) nil)
  (-drop! [_ branch]
    (swap! state (fn [tables] (-> tables
                                  (update :logs #(into {} (remove (fn [[[b _] _]] (= b branch))) %))
                                  (update :parents dissoc branch))))
    nil)
  (-drop-dataset! [_ addr] (swap! state update :logs dissoc (k addr)) nil))

(defn datasets
  ([] (datasets {}))
  ([{:keys [clock] :or {clock #(System/currentTimeMillis)}}]
   (->MemDatasets (atom {:version 0 :logs {} :parents {}}) clock)))
